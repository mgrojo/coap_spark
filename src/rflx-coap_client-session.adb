with Ada.Numerics.Discrete_Random;
with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with CoAP_SPARK.Content_Formats;
with CoAP_SPARK.Options;
with CoAP_SPARK.Utils;
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
   use type CoAP_SPARK.Options.Option_Format;
   use type RFLX.CoAP.Option_Numbers;

   -- The random number generators cannot be proved by SPARK.
   package Random
     with SPARK_Mode => Off
   is
      package Bytes is new Ada.Numerics.Discrete_Random (RFLX_Types.Byte);
      Byte_Generator : Bytes.Generator;

      package ID is new
        Ada.Numerics.Discrete_Random (RFLX.CoAP.Message_ID_Type);
      ID_Generator : ID.Generator;
   end Random;

   package body Random
     with SPARK_Mode => Off
   is
   begin
      Bytes.Reset (Byte_Generator);
      ID.Reset (ID_Generator);
   end Random;

   subtype Base_Delta_Type is RFLX.CoAP.Option_Extended16_Type range 0 .. 12;
   subtype Extended8_Delta_Type is
     RFLX.CoAP.Option_Extended16_Type range 13 .. 268;
   subtype Extended16_Delta_Type is
     RFLX.CoAP.Option_Extended16_Type
       range 269 .. RFLX.CoAP.Option_Extended16_Type'Last;

   Max_Option_Value_Length : constant :=
     Extended16_Delta_Type'First + RFLX.CoAP.Option_Extended16_Type'Last;

   subtype Option_Value_Length is
     RFLX.RFLX_Builtin_Types.Length range 0 .. Max_Option_Value_Length;

   subtype Base_Length_Type is Option_Value_Length range 0 .. 12;
   subtype Extended8_Length_Type is Option_Value_Length range 13 .. 268;
   subtype Extended16_Length_Type is
     Option_Value_Length range 269 .. Option_Value_Length'Last;

   subtype Option_Bytes is RFLX.RFLX_Types.Bytes
   with Dynamic_Predicate => Option_Bytes'Length <= Max_Option_Value_Length;

   procedure Get_Method
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Method_Code) is
   begin
      -- TODO: Implement this procedure
      RFLX_Result := RFLX.CoAP.Get;
      State.Current_Status := RFLX.CoAP_Client.Session_Environment.OK;

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

   function To_Option_Extended16_Type
     (Option : RFLX.CoAP.Option_Numbers)
      return RFLX.CoAP.Option_Extended16_Type
   is (RFLX.CoAP.Option_Extended16_Type (RFLX.CoAP.To_Base_Integer (Option)));

   function To_Option_Base
     (Value : RFLX.CoAP.Option_Extended16_Type)
      return RFLX.CoAP.Option_Base_Type
   is (case Value is
         when Base_Delta_Type => RFLX.CoAP.Option_Base_Type (Value),
         when Extended8_Delta_Type => 13,
         when Extended16_Delta_Type => 14);

   function To_Option_Base
     (Value : Option_Value_Length) return RFLX.CoAP.Option_Base_Type
   is (case Value is
         when Base_Length_Type => RFLX.CoAP.Option_Base_Type (Value),
         when Extended8_Length_Type => 13,
         when Extended16_Length_Type => 14);

   -- See figure 8 in RFC7252
   function Option_Byte_Size
     (Option : RFLX.CoAP.Option_Numbers; Option_Length : Option_Value_Length)
      return RFLX.RFLX_Builtin_Types.Length
   is (1
       + (case To_Option_Extended16_Type (Option) is
            when Base_Delta_Type => 0,
            when Extended8_Delta_Type => 1,
            when Extended16_Delta_Type => 2)
       + (case Option_Length is
            when Base_Length_Type => 0,
            when Extended8_Length_Type => 1,
            when Extended16_Length_Type => 2)
       + Option_Length);

   function To_Bit_Size
     (Value : RFLX.RFLX_Builtin_Types.Length) return RFLX.RFLX_Types.Bit_Length
   is (RFLX.RFLX_Types.Bit_Length (Value) * 8);
 
   procedure Add_Option
     (Option              : RFLX.CoAP.Option_Numbers;
      Value               : Option_Bytes;
      Current_Delta       : in out RFLX.CoAP.Option_Extended16_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context)
   with
     Pre =>
       Value'Length <= CoAP_SPARK.Options.Max_Option_Value_Length
       and then RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt)
       and then RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
       and then To_Option_Extended16_Type (Option) >= Current_Delta
       and then RFLX.CoAP.Option_Sequence.Available_Space (Option_Sequence_Cxt)
                >= To_Bit_Size (Option_Byte_Size (Option, Value'Length)),
     Post =>
       To_Option_Extended16_Type (Option) = Current_Delta
       and then RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
   is
      Option_Delta : constant RFLX.CoAP.Option_Extended16_Type :=
        To_Option_Extended16_Type (Option) - Current_Delta;

      Option_Length : constant Option_Value_Length :=
        Option_Value_Length (Value'Length);

      Option_Cxt    : RFLX.CoAP.Option_Type.Context;
      Option_Buffer : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'
          (1
           .. RFLX.RFLX_Builtin_Types.Index
                (Option_Byte_Size (Option, Value'Length)) => 0);
   begin

      RFLX.CoAP.Option_Type.Initialize
        (Ctx => Option_Cxt, Buffer => Option_Buffer);

      RFLX.CoAP.Option_Type.Set_Option_Delta
        (Ctx => Option_Cxt, Val => To_Option_Base (Option_Delta));

      RFLX.CoAP.Option_Type.Set_Option_Length
        (Ctx => Option_Cxt, Val => To_Option_Base (Option_Length));

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
        (Ctx => Option_Cxt, Data => Value);

      RFLX.CoAP.Option_Sequence.Append_Element
        (Ctx => Option_Sequence_Cxt, Element_Ctx => Option_Cxt);

      Current_Delta := To_Option_Extended16_Type (Option);

      RFLX.CoAP.Option_Type.Take_Buffer
        (Ctx => Option_Cxt, Buffer => Option_Buffer);
      pragma Unreferenced (Option_Cxt);

      RFLX.RFLX_Types.Free (Option_Buffer);

   end Add_Option;

   procedure Add_String_Option
     (Option              : RFLX.CoAP.Option_Numbers;
      Value               : Ada.Strings.UTF_Encoding.UTF_8_String;
      Current_Delta       : in out RFLX.CoAP.Option_Extended16_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context)
   with
     Pre =>
       CoAP_SPARK.Options.Option_Properties_Table (Option).Format
       = CoAP_SPARK.Options.UTF8_String
       and then RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt)
       and then RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
       and then To_Option_Extended16_Type (Option) >= Current_Delta
       and then Value'First <= Value'Last
       and then Value'Length <= CoAP_SPARK.Options.Max_Option_Value_Length
       and then RFLX.CoAP.Option_Sequence.Available_Space (Option_Sequence_Cxt)
                >= To_Bit_Size (Option_Byte_Size (Option, Value'Length)),
     Post =>
       To_Option_Extended16_Type (Option) = Current_Delta
       and then RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
       and then RFLX.CoAP.Option_Sequence.Has_Element (Option_Sequence_Cxt)
       and then RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt) > 0
   is
      Value_Bytes :
        Option_Bytes
          (RFLX_Builtin_Types.Index (Value'First)
           .. RFLX_Builtin_Types.Index (Value'Last)) := (others => 0);
   begin

      for I in Value_Bytes'Range loop

         RFLX.RFLX_Types.Operations.Insert
           (Val    =>
              RFLX.RFLX_Types.Base_Integer
                (Character'Pos (Value (Natural (I)))),
            Buffer => Value_Bytes,
            First  => I,
            Last   => I,
            Off    => 0,
            Size   => Character'Size,
            BO     => RFLX.RFLX_Types.High_Order_First);
      end loop;

      Add_Option
        (Option              => Option,
         Value               => Value_Bytes,
         Current_Delta       => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

   end Add_String_Option;

   procedure Add_Uint_Option
     (Option              : RFLX.CoAP.Option_Numbers;
      Value               : RFLX.RFLX_Arithmetic.U64;
      Current_Delta       : in out RFLX.CoAP.Option_Extended16_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context)
   with
     Pre =>
       CoAP_SPARK.Options.Option_Properties_Table (Option).Format
       = CoAP_SPARK.Options.UInt
       and then RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt)
       and then RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
       and then To_Option_Extended16_Type (Option) >= Current_Delta
       and then RFLX.CoAP.Option_Sequence.Available_Space (Option_Sequence_Cxt)
                >= To_Bit_Size
                     (Option_Byte_Size
                        (Option,
                         RFLX
                           .RFLX_Arithmetic
                           .U64'Max_Size_In_Storage_Elements)),
     Post =>
       RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
       and then RFLX.CoAP.Option_Sequence.Has_Element (Option_Sequence_Cxt)
       and then To_Option_Extended16_Type (Option) = Current_Delta
       and then RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt) > 0
   is
      subtype Possible_Sizes_In_Bytes is
        RFLX.RFLX_Builtin_Types.Index
          range 1 .. RFLX.RFLX_Arithmetic.U64'Max_Size_In_Storage_Elements;
      subtype Bytes_Subtype is RFLX.RFLX_Types.Bytes (Possible_Sizes_In_Bytes);
      Bytes_Value   : Bytes_Subtype := (others => 0);
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

      if Value /= 0 then

         for I in Possible_Sizes_In_Bytes loop

            if I = Possible_Sizes_In_Bytes'Last
              or else Value < 2 ** (Natural (I) * 8)
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
        (Option              => Option,
         Value               =>
           Bytes_Value (1 .. (if Value = 0 then 0 else Size_In_Bytes)),
         Current_Delta       => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

   end Add_Uint_Option;

   procedure Get_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Options_And_Payload_Data.Structure)
   is

      Option_Sequence_Cxt    : RFLX.CoAP.Option_Sequence.Context;
      Option_Sequence_Buffer : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'
          (1 .. RFLX.RFLX_Builtin_Types.Index (Max_Option_Value_Length) => 0);

      Hostname     : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        "localhost";
      Path         : constant Ada.Strings.UTF_Encoding.UTF_8_String := "/";
      Default_Port : constant := 5683; -- TODO move to an appropiate place

      Current_Delta : RFLX.CoAP.Option_Extended16_Type := 0;
   begin

      RFLX_Result := (Length => 0, Options_And_Payload => (others => 0));

      RFLX.CoAP.Option_Sequence.Initialize
        (Ctx => Option_Sequence_Cxt, Buffer => Option_Sequence_Buffer);

      Add_String_Option
        (Option              => RFLX.CoAP.Uri_Host,
         Value               => Hostname,
         Current_Delta       => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

      pragma Assert (RFLX.CoAP.Uri_Port > RFLX.CoAP.Uri_Host);

      Add_Uint_Option
        (Option              => RFLX.CoAP.Uri_Port,
         Value               => Default_Port,
         Current_Delta       => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

      pragma Assert (RFLX.CoAP.Uri_Path > RFLX.CoAP.Uri_Port);

      Add_String_Option
        (Option              => RFLX.CoAP.Uri_Path,
         Value               => Path,
         Current_Delta       => Current_Delta,
         Option_Sequence_Cxt => Option_Sequence_Cxt);

      pragma Unreferenced (Current_Delta);

      declare
         Last : constant RFLX.RFLX_Builtin_Types.Index :=
           RFLX.RFLX_Builtin_Types.Index
             (RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt));
      begin
         if Last in RFLX_Result.Options_And_Payload'Range then
            RFLX.CoAP.Option_Sequence.Copy
              (Ctx    => Option_Sequence_Cxt,
               Buffer => RFLX_Result.Options_And_Payload (1 .. Last));
            RFLX_Result.Length := RFLX.CoAP.Length_16 (Last);

            State.Current_Status := RFLX.CoAP_Client.Session_Environment.OK;
         else
            State.Current_Status :=
              RFLX.CoAP_Client.Session_Environment.Capacity_Error;
         end if;
      end;

      RFLX.CoAP.Option_Sequence.Take_Buffer
        (Ctx => Option_Sequence_Cxt, Buffer => Option_Sequence_Buffer);
      pragma Unreferenced (Option_Sequence_Cxt);

      RFLX.RFLX_Types.Free (Option_Sequence_Buffer);

   end Get_Options_And_Payload;

   procedure Put_Option
     (State          : in out RFLX.CoAP_Client.Session_Environment.State;
      Option_Cxt     : RFLX.CoAP.Option_Type.Context;
      Option_Delta   : in out RFLX.RFLX_Types.Base_Integer;
      End_Of_Options : out Boolean)
   with
     Pre =>
       RFLX.CoAP.Option_Type.Initialized (Option_Cxt)
       and then RFLX.CoAP.Option_Type.Has_Buffer (Option_Cxt)
   is
      Option_Length : Option_Value_Length;

   begin
      End_Of_Options := False;

      Option_Delta :=
        Option_Delta
        + RFLX.CoAP.To_Base_Integer
            (RFLX.CoAP.Option_Type.Get_Option_Delta (Option_Cxt));

      Option_Length :=
        Option_Value_Length
          (RFLX.CoAP.Option_Type.Get_Option_Length (Option_Cxt));

      case Option_Delta is
         when 13 =>
            Option_Delta :=
              13
              + RFLX.RFLX_Types.Base_Integer
                  (RFLX.CoAP.Option_Type.Get_Option_Delta_Extended8
                     (Option_Cxt));

         when 14 =>
            Option_Delta :=
              269
              + RFLX.RFLX_Types.Base_Integer
                  (RFLX.CoAP.Option_Type.Get_Option_Delta_Extended16
                     (Option_Cxt));

         when 15 =>
            if Option_Length = 15 then
               End_Of_Options := True;
               return;
            else
               State.Current_Status :=
                 CoAP_Client.Session_Environment.Malformed_Message;
               return;
            end if;

         when others =>
            null;
      end case;

      case Option_Length is
         when 13 =>
            Option_Length :=
              13
              + Option_Value_Length
                  (RFLX.CoAP.Option_Type.Get_Option_Length_Extended8
                     (Option_Cxt));

         when 14 =>
            Option_Length :=
              269
              + Option_Value_Length
                  (RFLX.CoAP.Option_Type.Get_Option_Length_Extended16
                     (Option_Cxt));

         when 15 =>
            State.Current_Status :=
              CoAP_Client.Session_Environment.Malformed_Message;
            return;

         when others =>
            null;
      end case;

      if Option_Length > CoAP_SPARK.Options.Max_Option_Value_Length then
         State.Current_Status :=
           CoAP_Client.Session_Environment.Malformed_Message;
         return;
      elsif Option_Length > 0 then
         declare
            Option_Value  : RFLX.RFLX_Types.Bytes_Ptr :=
              new RFLX.RFLX_Types.Bytes'
                (1 .. RFLX_Builtin_Types.Index (Option_Length) => 0);
            Option_Number : constant RFLX.CoAP.Option_Numbers :=
              RFLX.CoAP.To_Actual (Option_Delta);
         begin
            RFLX.CoAP.Option_Type.Get_Option_Value
              (Ctx => Option_Cxt, Data => Option_Value.all);

            Ada.Text_IO.Put_Line
              ("Option: "
               & Option_Number'Image
               & ", Length: "
               & Option_Length'Image
               & ", Value: "
               & CoAP_SPARK.Options.Image
                   (Format =>
                      CoAP_SPARK.Options.Option_Properties_Table
                        (Option_Number)
                        .Format,
                    Value  => Option_Value.all));

            if CoAP.To_Actual (Option_Delta) = CoAP.Content_Format then
               if Option_Length > CoAP_SPARK.Options.UInt_Bytes'Length then
                  State.Current_Status :=
                    RFLX.CoAP_Client.Session_Environment.Malformed_Message;
               else
                  State.Content_Format :=
                    CoAP_SPARK.Options.To_UInt (Value => Option_Value.all);
               end if;
            end if;

            RFLX.RFLX_Types.Free (Option_Value);
         end;
      else
         Ada.Text_IO.Put_Line
           ("Option: "
            & CoAP.Option_Numbers'Image (CoAP.To_Actual (Option_Delta))
            & ", no value");

         if CoAP.To_Actual (Option_Delta) = CoAP.Content_Format then
            State.Content_Format := 0;
         end if;
      end if;

   end Put_Option;

   procedure Put_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Data        : RFLX_Types.Bytes;
      RFLX_Result : out Boolean)
   is
      use type CoAP_Client.Session_Environment.Status_Type;
   begin

      if Data'Length = 0 then
         Ada.Text_IO.Put_Line ("Options and payload are empty");
      else
         declare
            Option_Sequence_Cxt : RFLX.CoAP.Option_Sequence.Context;
            Buffer              : RFLX.RFLX_Types.Bytes_Ptr :=
              new RFLX.RFLX_Types.Bytes'(Data);
            Option_Delta        : RFLX.RFLX_Types.Base_Integer := 0;
            Option_Cxt          : RFLX.CoAP.Option_Type.Context;
            End_Of_Options      : Boolean;
         begin

            RFLX.CoAP.Option_Sequence.Initialize
              (Ctx => Option_Sequence_Cxt, Buffer => Buffer);

            Read_Options :
            loop
               RFLX.CoAP.Option_Sequence.Switch
                 (Ctx => Option_Sequence_Cxt, Element_Ctx => Option_Cxt);

               RFLX.CoAP.Option_Type.Verify_Message (Ctx => Option_Cxt);

               if not RFLX.CoAP.Option_Type.Well_Formed_Message
                        (Ctx => Option_Cxt)
               then
                  State.Current_Status :=
                    RFLX.CoAP_Client.Session_Environment.Malformed_Message;
                  exit Read_Options;
               end if;
               Put_Option (State, Option_Cxt, Option_Delta, End_Of_Options);

               RFLX.CoAP.Option_Sequence.Update
                 (Ctx => Option_Sequence_Cxt, Element_Ctx => Option_Cxt);

               exit Read_Options when
                 End_Of_Options
                 or else not RFLX.CoAP.Option_Sequence.Has_Element
                               (Option_Sequence_Cxt);
            end loop Read_Options;

            if CoAP.Option_Sequence.Sequence_Last (Option_Sequence_Cxt)
              >= Option_Sequence_Cxt.Last
            then
               RFLX.CoAP.Option_Type.Take_Buffer
                 (Ctx => Option_Cxt, Buffer => Buffer);
               pragma Assert (not RFLX.CoAP.Option_Type.Has_Buffer
                                    (Option_Cxt));
               RFLX.RFLX_Types.Free (Buffer);
            else
               -- When there is something left to be read, it is the payload
               Print_Payload :
               declare
                  Payload_Marker_Offset : constant := 2;
                  First                 :
                    constant RFLX.RFLX_Builtin_Types.Index :=
                      RFLX.RFLX_Builtin_Types.Index
                        (CoAP.Option_Sequence.Sequence_Last
                           (Option_Sequence_Cxt)
                         / 8
                         + Payload_Marker_Offset);
                  Last                  :
                    constant RFLX.RFLX_Builtin_Types.Index :=
                      RFLX.RFLX_Builtin_Types.Index (Option_Cxt.Last / 8);
                  Payload_Format        :
                    constant CoAP_SPARK.Options.Option_Format :=
                      (if CoAP_SPARK.Content_Formats.Is_Text
                            (State.Content_Format)
                       then CoAP_SPARK.Options.UTF8_String
                       else CoAP_SPARK.Options.Opaque);
               begin

                  RFLX.CoAP.Option_Type.Take_Buffer
                    (Ctx => Option_Cxt, Buffer => Buffer);
                  pragma Assert (not RFLX.CoAP.Option_Type.Has_Buffer (Option_Cxt));

                  Ada.Text_IO.Put_Line
                    ("Content-Format: "
                     & CoAP_SPARK.Content_Formats.To_String
                         (State.Content_Format));

                  Ada.Text_IO.Put_Line
                    ("Payload: "
                     & CoAP_SPARK.Options.Image
                         (Format => Payload_Format,
                          Value  => Buffer (First .. Last)));

                  RFLX.RFLX_Types.Free (Buffer); 
               end Print_Payload;
            end if;
         end;
      end if;

      RFLX_Result :=
        State.Current_Status = RFLX.CoAP_Client.Session_Environment.OK;

   end Put_Options_And_Payload;

   procedure Put_Client_Error
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Error_Code  : RFLX.CoAP.Client_Error_Response;
      RFLX_Result : out Boolean)
   is
   begin
      Ada.Text_IO.Put_Line
        ("Server answered with client error: 4."
         & CoAP_SPARK.Utils.Padded_Image
            (Source => Error_Code'Enum_Rep, Count => 2)
         & " ("
         & RFLX.CoAP.Client_Error_Response'Image (Error_Code)
         & ")");
         
         RFLX_Result := True;
   end Put_Client_Error;

   procedure Put_Server_Error
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Error_Code  : RFLX.CoAP.Server_Error_Response;
      RFLX_Result : out Boolean)
   is
   begin
      Ada.Text_IO.Put_Line
        ("Server answered with server error: 5."
         & CoAP_SPARK.Utils.Padded_Image
             (Source => Error_Code'Enum_Rep, Count => 2)
         & " ("
         & RFLX.CoAP.Server_Error_Response'Image (Error_Code)
         & ")");
         
      RFLX_Result := True;
   end Put_Server_Error;

end RFLX.CoAP_Client.Session;
