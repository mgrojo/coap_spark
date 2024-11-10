with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Conversion;

with RFLX.CoAP.Option_Sequence;
with RFLX.CoAP.Option_Type;
with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;

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

   procedure Add_String_Option
     (Option : RFLX.CoAP.Option_Numbers;
      Value : String;
      Current_Delta : in out RFLX.CoAP.Option_Base_Type;
      Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context) is

      subtype String_Subtype is String (1 .. Value'Length);
      subtype Bytes_Subtype is RFLX.RFLX_Types.Bytes (1 .. Value'Length);
      function To_Bytes is new Ada.Unchecked_Conversion
         (Source => String_Subtype,
          Target => Bytes_Subtype);
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
         Data => To_Bytes (Value));

      RFLX.CoAP.Option_Sequence.Append_Element (Ctx => Option_Sequence_Cxt,
                                                Element_Ctx => Option_Cxt);

      Current_Delta := RFLX.CoAP.Option_Base_Type
                         (RFLX.CoAP.To_Base_Integer (Option));

   end Add_String_Option;

   procedure Get_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Options_And_Payload_Data.Structure) is

      Option_Sequence_Cxt : RFLX.CoAP.Option_Sequence.Context;
      Option_Sequence_Buffer : RFLX.RFLX_Types.Bytes_Ptr;

      Hostname : constant String := "coap.me";
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
