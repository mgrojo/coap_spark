with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Conversion;
with RFLX.CoAP.Option_Sequence;
with RFLX.CoAP.Option_Type;
with RFLX.RFLX_Arithmetic;
with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;
with RFLX.RFLX_Types.Operations;

package body RFLX.Coap_Client.Session is

   package Random_Bytes is
      new Ada.Numerics.Discrete_Random (RFLX_Types.Byte);
   Byte_Generator : Random_Bytes.Generator;

   package Random_ID is
      new Ada.Numerics.Discrete_Random (RFLX.CoAP.Message_ID_Type);
   ID_Generator : Random_ID.Generator;

   procedure Get_Method
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Method_Code) is
   begin
      -- TODO: Implement this procedure
      RFLX_Result := RFLX.CoAP.Get;
   end Get_Method;

   procedure Get_New_Message_ID
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Message_ID_Type) is
      use type RFLX.CoAP.Message_ID_Type;
   begin
      if State.Is_First_Message then
         -- Quoting the RFC:
         --  It is strongly recommended that the initial
         --  value of the variable (e.g., on startup) be randomized, in order
         --  to make successful off-path attacks on the protocol less likely.
         State.Is_First_Message := False;
         State.Current_Message_ID := Random_ID.Random (ID_Generator);
      elsif State.Current_Message_ID = RFLX.CoAP.Message_ID_Type'Last then
         State.Current_Message_ID := RFLX.CoAP.Message_ID_Type'First;
      else
         State.Current_Message_ID := State.Current_Message_ID + 1;
      end if;
      RFLX_Result := State.Current_Message_ID;
   end Get_New_Message_ID;

   procedure Get_Random_Token
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Token_Data.Structure) is
   begin
      Random_Bytes.Reset (Byte_Generator);
      for I in RFLX_Result.Token'Range loop
         RFLX_Result.Token (I) := Random_Bytes.Random (Byte_Generator);
      end loop;

      RFLX_Result := (Length => RFLX_Result.Token'Length,
                      Unused => RFLX.CoAP_Client.Unused_4'First,
                      Token  => RFLX_Result.Token);
   end Get_Random_Token;

   procedure Add_Option
     (Option : RFLX.CoAP.Option_Numbers;
      Value : RFLX.RFLX_Types.Bytes;
      Current_Delta : in out RFLX.CoAP.Option_Base_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context) is

      Option_Delta : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.CoAP.To_Base_Integer (Option) -
         RFLX.CoAP.To_Base_Integer (Current_Delta);
      Option_Length : constant RFLX.RFLX_Types.Base_Integer :=
         RFLX.RFLX_Types.Base_Integer (Value'Length);
      Option_Cxt : RFLX.CoAP.Option_Type.Context;
      Option_Buffer : RFLX.RFLX_Types.Bytes_Ptr;
   begin
      RFLX.CoAP.Option_Type.Initialize
         (Ctx => Option_Cxt,
          Buffer => Option_Buffer);

      if Option_Delta > 269 then

         RFLX.CoAP.Option_Type.Set_Option_Delta
         (Ctx => Option_Cxt,
          Val => 14);

         RFLX.CoAP.Option_Type.Set_Option_Delta_Extended16
         (Ctx => Option_Cxt,
          Val => RFLX.CoAP.Option_Extended16_Type (Option_Delta - 269));

      elsif Option_Delta > 13 then

         RFLX.CoAP.Option_Type.Set_Option_Delta
            (Ctx => Option_Cxt,
             Val => 13);

         RFLX.CoAP.Option_Type.Set_Option_Delta_Extended8
            (Ctx => Option_Cxt,
             Val => RFLX.CoAP.Option_Extended8_Type (Option_Delta - 13));
      else
         RFLX.CoAP.Option_Type.Set_Option_Delta
            (Ctx => Option_Cxt,
             Val => RFLX.CoAP.Option_Base_Type (Option_Delta));
      end if;

      if Option_Length > 269 then

         RFLX.CoAP.Option_Type.Set_Option_Length
            (Ctx => Option_Cxt,
             Val => 14);
         RFLX.CoAP.Option_Type.Set_Option_Length_Extended16
            (Ctx => Option_Cxt,
             Val => RFLX.CoAP.Option_Extended16_Type (Option_Length - 269));
      elsif Option_Length > 13 then
         RFLX.CoAP.Option_Type.Set_Option_Length
            (Ctx => Option_Cxt,
             Val => 13);
         RFLX.CoAP.Option_Type.Set_Option_Length_Extended8
            (Ctx => Option_Cxt,
             Val => RFLX.CoAP.Option_Extended8_Type (Option_Length - 13));
      else
         RFLX.CoAP.Option_Type.Set_Option_Length
            (Ctx => Option_Cxt,
             Val => RFLX.CoAP.Option_Base_Type (Option_Length));
      end if;

      RFLX.CoAP.Option_Type.Set_Option_Value
        (Ctx => Option_Cxt,
         Data => Value);

      RFLX.CoAP.Option_Sequence.Append_Element (Ctx => Option_Sequence_Cxt,
                                                Element_Ctx => Option_Cxt);

      Current_Delta := RFLX.CoAP.Option_Base_Type
                         (RFLX.CoAP.To_Base_Integer (Option));

   end Add_Option;

   procedure Add_String_Option
     (Option : RFLX.CoAP.Option_Numbers;
      Value : String;
      Current_Delta : in out RFLX.CoAP.Option_Base_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context)
   is
      subtype String_Subtype is String (1 .. Value'Length);
      subtype Bytes_Subtype is RFLX.RFLX_Types.Bytes (1 .. Value'Length);
      function To_Bytes is new Ada.Unchecked_Conversion
         (Source => String_Subtype,
          Target => Bytes_Subtype);
   begin

      Add_Option (Option => Option,
                  Value => To_Bytes (Value),
                  Current_Delta => Current_Delta,
                  Option_Sequence_Cxt => Option_Sequence_Cxt);

   end Add_String_Option;

   procedure Add_Uint_Option
     (Option : RFLX.CoAP.Option_Numbers;
      Value : RFLX.RFLX_Arithmetic.U64;
      Current_Delta : in out RFLX.CoAP.Option_Base_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context)
   is
      use type RFLX.RFLX_Arithmetic.U64;
      use type RFLX.RFLX_Builtin_Types.Index;
      subtype Bytes_Subtype is RFLX.RFLX_Types.Bytes
                                 (1 .. RFLX.RFLX_Arithmetic.U64
                                         'Max_Size_In_Storage_Elements);
      Bytes_Value : Bytes_Subtype;
      Size_In_Bytes : RFLX.RFLX_Builtin_Types.Index;
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
                     Value => Bytes_Value (1 .. 0),
                     Current_Delta => Current_Delta,
                     Option_Sequence_Cxt => Option_Sequence_Cxt);
      else

         for I in Bytes_Value'Range loop
            if Value <= 2**(Positive (I) * 8) then
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

      Add_Option (Option => Option,
                  Value => Bytes_Value (1 .. Size_In_Bytes),
                  Current_Delta => Current_Delta,
                  Option_Sequence_Cxt => Option_Sequence_Cxt);

   end Add_Uint_Option;

   procedure Get_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Options_And_Payload_Data.Structure) is

      Option_Sequence_Cxt : RFLX.CoAP.Option_Sequence.Context;
      Option_Sequence_Buffer : RFLX.RFLX_Types.Bytes_Ptr;

      Hostname : constant String := "coap.me";
      Path : constant String := "test";
      Default_Port : constant := 5683; -- TODO move to an appropiate place
      Current_Delta : RFLX.CoAP.Option_Base_Type := 0;
   begin
      RFLX.CoAP.Option_Sequence.Initialize
        (Ctx => Option_Sequence_Cxt,
         Buffer => Option_Sequence_Buffer);

      Add_String_Option
         (Option => RFLX.CoAP.Uri_Host,
          Value => Hostname,
          Current_Delta => Current_Delta,
          Option_Sequence_Cxt => Option_Sequence_Cxt);

      Add_String_Option
         (Option => RFLX.CoAP.Uri_Path,
          Value => Path,
          Current_Delta => Current_Delta,
          Option_Sequence_Cxt => Option_Sequence_Cxt);

      Add_Uint_Option
         (Option => RFLX.CoAP.Uri_Port,
          Value => Default_Port,
          Current_Delta => Current_Delta,
          Option_Sequence_Cxt => Option_Sequence_Cxt);

      RFLX.CoAP.Option_Sequence.Copy
         (Ctx => Option_Sequence_Cxt,
          Buffer => RFLX_Result.Options_And_Payload
                      (1 .. RFLX.RFLX_Builtin_Types.Index
                              (RFLX.CoAP.Option_Sequence.Byte_Size
                                 (Option_Sequence_Cxt))));
      RFLX_Result.Length := RFLX.CoAP.Length_16
                              (RFLX.CoAP.Option_Sequence.Byte_Size
                                  (Option_Sequence_Cxt));

   end Get_Options_And_Payload;

begin
   Random_Bytes.Reset (Byte_Generator);
   Random_ID.Reset (ID_Generator);
end RFLX.CoAP_Client.Session;
