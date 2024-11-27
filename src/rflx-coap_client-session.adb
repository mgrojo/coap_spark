with Ada.Numerics.Discrete_Random;
with Ada.Strings.UTF_Encoding;
with Ada.Unchecked_Conversion;
with RFLX.CoAP.Option_Sequence;
with RFLX.CoAP.Option_Type;
with RFLX.RFLX_Arithmetic;
with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types.Operations;

package body RFLX.CoAP_Client.Session
   with SPARK_Mode
is

   use type RFLX.CoAP.Option_Extended16_Type;
   use type RFLX.RFLX_Arithmetic.U64;
   use type RFLX.RFLX_Builtin_Types.Index;
   use type RFLX.RFLX_Builtin_Types.Length;
   use type RFLX.RFLX_Builtin_Types.Bit_Length;

   -- The random number generators cannot be proved by SPARK.
   package Random with SPARK_Mode => Off is
      package Bytes is
         new Ada.Numerics.Discrete_Random (RFLX_Types.Byte);
      Byte_Generator : Bytes.Generator;

      package ID is
         new Ada.Numerics.Discrete_Random (RFLX.CoAP.Message_ID_Type);
      ID_Generator : ID.Generator;
   end Random;

   package body Random with SPARK_Mode => Off is
   begin
      Bytes.Reset (Byte_Generator);
      ID.Reset (ID_Generator);
   end Random;

   subtype Base_Delta_Type is
      RFLX.CoAP.Option_Extended16_Type range 0 .. 12;
   subtype Extended8_Delta_Type is
      RFLX.CoAP.Option_Extended16_Type range 13 .. 268;
   subtype Extended16_Delta_Type is
      RFLX.CoAP.Option_Extended16_Type
      range 269 .. RFLX.CoAP.Option_Extended16_Type'Last;

   Max_Option_Value_Length : constant :=
      Extended16_Delta_Type'First + RFLX.CoAP.Option_Extended16_Type'Last;

   subtype Option_Value_Length is RFLX.RFLX_Builtin_Types.Length
      range 0 .. Max_Option_Value_Length;

   subtype Base_Length_Type is
      Option_Value_Length range 0 .. 12;
   subtype Extended8_Length_Type is
      Option_Value_Length range 13 .. 268;
   subtype Extended16_Length_Type is
      Option_Value_Length
      range 269 .. Option_Value_Length'Last;

   subtype Option_Bytes is RFLX.RFLX_Types.Bytes
      with Dynamic_Predicate => Option_Bytes'Length <= Max_Option_Value_Length;

   procedure Get_Method
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Method_Code) is
   begin
      -- TODO: Implement this procedure
      RFLX_Result := RFLX.CoAP.Get;
   end Get_Method;

   procedure Get_New_Message_ID
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Message_ID_Type)
      with SPARK_Mode => Off
   is
      use type RFLX.CoAP.Message_ID_Type;
   begin
      if State.Is_First_Message then
         -- Quoting the RFC:
         --  It is strongly recommended that the initial
         --  value of the variable (e.g., on startup) be randomized, in order
         --  to make successful off-path attacks on the protocol less likely.
         State.Is_First_Message := False;
         State.Current_Message_ID := Random.ID.Random (Random.ID_Generator);
      elsif State.Current_Message_ID = RFLX.CoAP.Message_ID_Type'Last then
         State.Current_Message_ID := RFLX.CoAP.Message_ID_Type'First;
      else
         State.Current_Message_ID := State.Current_Message_ID + 1;
      end if;
      RFLX_Result := State.Current_Message_ID;
   end Get_New_Message_ID;

   procedure Get_Random_Token
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Token_Data.Structure)
      with SPARK_Mode => Off
   is
   begin

      for I in RFLX_Result.Token'Range loop
         RFLX_Result.Token (I) := Random.Bytes.Random (Random.Byte_Generator);
      end loop;

      RFLX_Result :=
        (Length => RFLX_Result.Token'Length,
         Unused => RFLX.CoAP_Client.Unused_4'First,
         Token  => RFLX_Result.Token);
   end Get_Random_Token;

   function To_Option_Extended16_Type (Option : RFLX.CoAP.Option_Numbers)
      return RFLX.CoAP.Option_Extended16_Type
   is (RFLX.CoAP.Option_Extended16_Type (RFLX.CoAP.To_Base_Integer (Option)));

   function To_Option_Base (Value : RFLX.CoAP.Option_Extended16_Type)
      return RFLX.CoAP.Option_Base_Type
   is (case Value is
         when Base_Delta_Type => RFLX.CoAP.Option_Base_Type (Value),
         when Extended8_Delta_Type => 13,
         when Extended16_Delta_Type => 14);

   function To_Option_Base (Value : Option_Value_Length)
      return RFLX.CoAP.Option_Base_Type
   is (case Value is
         when Base_Length_Type => RFLX.CoAP.Option_Base_Type (Value),
         when Extended8_Length_Type => 13,
         when Extended16_Length_Type => 14);

   -- See figure 8 in RFC7252
   function Option_Byte_Size
     (Option : RFLX.CoAP.Option_Numbers;
      Option_Length : Option_Value_Length)
      return RFLX.RFLX_Builtin_Types.Length
   is
      (1 +
         (case To_Option_Extended16_Type (Option) is
            when Base_Delta_Type => 0,
            when Extended8_Delta_Type => 1,
            when Extended16_Delta_Type => 2) +
         (case Option_Length is
            when Base_Length_Type => 0,
            when Extended8_Length_Type => 1,
            when Extended16_Length_Type => 2) + Option_Length);

   function To_Bit_Size (Value : RFLX.RFLX_Builtin_Types.Length)
                          return RFLX.RFLX_Types.Bit_Length
   is (RFLX.RFLX_Types.Bit_Length (Value) * 8);

   procedure Add_Option
     (Option : RFLX.CoAP.Option_Numbers;
      Value : Option_Bytes;
      Current_Delta : in out RFLX.CoAP.Option_Extended16_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context)
      with Pre =>
            RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt) and then
            RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt) and then
            To_Option_Extended16_Type (Option) >= Current_Delta and then
            RFLX.CoAP.Option_Sequence.Available_Space (Option_Sequence_Cxt) >=
            To_Bit_Size (Option_Byte_Size (Option, Value'Length)),
         Post => To_Option_Extended16_Type (Option) = Current_Delta and then
            RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt) and then
            RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt) >=
               Option_Byte_Size (Option, Value'Length)
   is
      Option_Delta : constant RFLX.CoAP.Option_Extended16_Type :=
        To_Option_Extended16_Type (Option) - Current_Delta;

      Option_Length : constant Option_Value_Length :=
        Option_Value_Length (Value'Length);

      Option_Cxt : RFLX.CoAP.Option_Type.Context;
      Option_Buffer : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'
        (1 .. RFLX.RFLX_Builtin_Types.Index
               (Option_Byte_Size (Option, Value'Length)) => 0);
   begin

      RFLX.CoAP.Option_Type.Initialize
         (Ctx => Option_Cxt,
          Buffer => Option_Buffer);

      RFLX.CoAP.Option_Type.Set_Option_Delta
         (Ctx => Option_Cxt,
          Val => To_Option_Base (Option_Delta));

      RFLX.CoAP.Option_Type.Set_Option_Length
         (Ctx => Option_Cxt,
          Val => To_Option_Base (Option_Length));

      case RFLX.CoAP.Option_Type.Get_Option_Delta (Option_Cxt) is
         when 13 =>
            RFLX.CoAP.Option_Type.Set_Option_Delta_Extended8
              (Ctx => Option_Cxt,
               Val => RFLX.CoAP.Option_Extended8_Type (Option_Delta - 13));
         when 14 =>
            RFLX.CoAP.Option_Type.Set_Option_Delta_Extended16
              (Ctx => Option_Cxt,
               Val => RFLX.CoAP.Option_Extended16_Type (Option_Delta - 269));
         when others =>
            null;
      end case;

      case RFLX.CoAP.Option_Type.Get_Option_Length (Option_Cxt) is
         when 13 =>
            RFLX.CoAP.Option_Type.Set_Option_Length_Extended8
              (Ctx => Option_Cxt,
               Val => RFLX.CoAP.Option_Extended8_Type (Option_Length - 13));
         when 14 =>
            RFLX.CoAP.Option_Type.Set_Option_Length_Extended16
              (Ctx => Option_Cxt,
               Val => RFLX.CoAP.Option_Extended16_Type (Option_Length - 269));
         when others =>
            null;
      end case;

      RFLX.CoAP.Option_Type.Set_Option_Value
        (Ctx => Option_Cxt,
         Data => Value);

      RFLX.CoAP.Option_Sequence.Append_Element (Ctx => Option_Sequence_Cxt,
                                                Element_Ctx => Option_Cxt);

      Current_Delta :=
        To_Option_Extended16_Type (Option);

      RFLX.CoAP.Option_Type.Take_Buffer
         (Ctx => Option_Cxt,
          Buffer => Option_Buffer);
      pragma Unreferenced (Option_Cxt);

      RFLX.RFLX_Types.Free (Option_Buffer);

   end Add_Option;

   procedure Add_String_Option
     (Option : RFLX.CoAP.Option_Numbers;
      Value : Ada.Strings.UTF_Encoding.UTF_8_String;
      Current_Delta : in out RFLX.CoAP.Option_Extended16_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context)
      with Pre =>
         RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt) and then
         RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt) and then
            To_Option_Extended16_Type (Option) >= Current_Delta and then
            Value'First <= Value'Last and then
            Value'Length <= Max_Option_Value_Length,
         Post => To_Option_Extended16_Type (Option) = Current_Delta
            and then
            RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
            and then
            RFLX.CoAP.Option_Sequence.Has_Element (Option_Sequence_Cxt)
            and then
            RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt) > 0
   is
      Value_Bytes : Option_Bytes
          (RFLX_Builtin_Types.Index (Value'First) ..
               RFLX_Builtin_Types.Index (Value'Last)) :=
        (others => 0);
   begin

      for I in Value_Bytes'Range loop

         RFLX.RFLX_Types.Operations.Insert
           (Val => RFLX.RFLX_Types.Base_Integer
                (Character'Pos (Value (Natural (I)))),
            Buffer => Value_Bytes,
            First => I,
            Last => I,
            Off => 0,
            Size => Character'Size,
            BO => RFLX.RFLX_Types.High_Order_First);
      end loop;

      Add_Option (Option => Option,
                  Value => Value_Bytes,
                  Current_Delta => Current_Delta,
                  Option_Sequence_Cxt => Option_Sequence_Cxt);

   end Add_String_Option;

   procedure Add_Uint_Option
     (Option : RFLX.CoAP.Option_Numbers;
      Value : RFLX.RFLX_Arithmetic.U64;
      Current_Delta : in out RFLX.CoAP.Option_Extended16_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context)
      with
         Pre => RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt)
            and then
               RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
            and then
               To_Option_Extended16_Type (Option) >= Current_Delta,
         Post => RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
            and then
               RFLX.CoAP.Option_Sequence.Has_Element (Option_Sequence_Cxt)
            and then
               To_Option_Extended16_Type (Option) = Current_Delta
            and then
               RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt) > 0
   is
      subtype Possible_Sizes_In_Bytes is
         RFLX.RFLX_Builtin_Types.Index
            range 1 .. RFLX.RFLX_Arithmetic.U64'Max_Size_In_Storage_Elements;
      subtype Bytes_Subtype is RFLX.RFLX_Types.Bytes
                                 (Possible_Sizes_In_Bytes);
      Bytes_Value : Bytes_Subtype := (others => 0);
      Size_In_Bytes : RFLX.RFLX_Builtin_Types.Index := 1;
   begin

      -- Quote from RFC7252:
      --  An option definition may specify a range of permissible
      --  numbers of bytes; if it has a choice, a sender SHOULD
      --  represent the integer with as few bytes as possible, i.e.,
      --  without leading zero bytes. For example, the number 0 is
      --  represented with an empty option value (a zero-length
      --  sequence of bytes) and the number 1 by a single byte with
      --  the numerical value of 1 (bit combination 00000001 in most
      --  significant bit first notation).

      if Value = 0 then
         Add_Option (Option => Option,
                     Value => RFLX.RFLX_Types.Bytes'(1 .. 0 => 0),
                     Current_Delta => Current_Delta,
                     Option_Sequence_Cxt => Option_Sequence_Cxt);
      else

         for I in Possible_Sizes_In_Bytes loop

            if I = Possible_Sizes_In_Bytes'Last
              or else Value < 2**(Natural (I) * 8)
            then
               Size_In_Bytes := I;
               exit;
            end if;
         end loop;

         RFLX.RFLX_Types.Operations.Insert
            (Val    => Value,
             Buffer => Bytes_Value,
             First  => Bytes_Value'First,
             Last   => Size_In_Bytes,
             Off    => 0,
             Size   => Positive (Size_In_Bytes) * 8,
             BO     => RFLX.RFLX_Types.High_Order_First);
      end if;

      Add_Option
        (Option => Option,
         Value => Bytes_Value (1 .. Size_In_Bytes),
         Current_Delta => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

   end Add_Uint_Option;

   procedure Get_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Options_And_Payload_Data.Structure)
   is
      use type RFLX.CoAP.Option_Numbers;

      Option_Sequence_Cxt : RFLX.CoAP.Option_Sequence.Context;
      Option_Sequence_Buffer : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'
        (1 .. RFLX.RFLX_Builtin_Types.Index
               (Max_Option_Value_Length) => 0);

      Hostname : constant Ada.Strings.UTF_Encoding.UTF_8_String := "coap.me";
      Path : constant Ada.Strings.UTF_Encoding.UTF_8_String := "test";
      Default_Port : constant := 5683; -- TODO move to an appropiate place

      Current_Delta : RFLX.CoAP.Option_Extended16_Type := 0;
   begin

      RFLX_Result := (Length              => 0,
                      Options_And_Payload => (others => 0));

      RFLX.CoAP.Option_Sequence.Initialize
        (Ctx => Option_Sequence_Cxt,
         Buffer => Option_Sequence_Buffer);

      Add_String_Option
         (Option => RFLX.CoAP.Uri_Host,
          Value => Hostname,
          Current_Delta => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

      pragma Assert (RFLX.CoAP.Uri_Port > RFLX.CoAP.Uri_Host);

      Add_Uint_Option
         (Option => RFLX.CoAP.Uri_Port,
          Value => Default_Port,
          Current_Delta => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

      pragma Assert (RFLX.CoAP.Uri_Path > RFLX.CoAP.Uri_Port);

      Add_String_Option
         (Option => RFLX.CoAP.Uri_Path,
          Value => Path,
          Current_Delta => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

      pragma Unreferenced (Current_Delta);

      declare
         Last : constant RFLX.RFLX_Builtin_Types.Index :=
           RFLX.RFLX_Builtin_Types.Index
             (RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt));
      begin
         if Last in RFLX_Result.Options_And_Payload'Range then
            RFLX.CoAP.Option_Sequence.Copy
               (Ctx => Option_Sequence_Cxt,
               Buffer => RFLX_Result.Options_And_Payload (1 .. Last));
            RFLX_Result.Length := RFLX.CoAP.Length_16 (Last);

            State.Current_Status := RFLX.CoAP_Client.Session_Environment.OK;
         else
            State.Current_Status :=
              RFLX.CoAP_Client.Session_Environment.Capacity_Error;
         end if;
      end;

      RFLX.CoAP.Option_Sequence.Take_Buffer
         (Ctx => Option_Sequence_Cxt,
          Buffer => Option_Sequence_Buffer);
      pragma Unreferenced (Option_Sequence_Cxt);

      RFLX.RFLX_Types.Free (Option_Sequence_Buffer);

   end Get_Options_And_Payload;

end RFLX.CoAP_Client.Session;
