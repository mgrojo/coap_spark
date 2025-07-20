with CoAP_SPARK.Messages;
with CoAP_SPARK.Messages.Encoding;
with CoAP_SPARK.Random;

package body RFLX.CoAP_Client.Session
   with SPARK_Mode
is

   procedure Get_Method
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Method_Code) is
   begin

      RFLX_Result := State.Method;
      State.Current_Status := RFLX.CoAP_Client.Session_Environment.OK;

   end Get_Method;

   procedure Get_New_Message_ID
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Message_ID_Type)
   is
      use type RFLX.CoAP.Message_ID_Type;
   begin
      if State.Is_First_Message then
         -- Quoting the RFC:
         --  It is strongly recommended that the initial
         --  value of the variable (e.g., on startup) be randomized, in order
         --  to make successful off-path attacks on the protocol less likely.
         State.Is_First_Message := False;
         CoAP_SPARK.Random.Get_Random_Message_ID (State.Current_Message_ID);
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
   is
      pragma Unreferenced (State);
   begin

      RFLX_Result :=
        (Length => RFLX_Result.Token'Length,
         Unused => RFLX.CoAP_Client.Unused_4'Last,
         Token => [others => 0]);

      for E of RFLX_Result.Token loop
         CoAP_SPARK.Random.Get_Random_Byte (E);
      end loop;

   end Get_Random_Token;

   procedure Are_Equal_Tokens
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Left        : RFLX_Types.Bytes;
      Right       : RFLX_Types.Bytes;
      RFLX_Result : out Boolean)
   is
      pragma Unreferenced (State);
      use type RFLX_Types.Bytes;
   begin
      RFLX_Result := Left = Right;
   end Are_Equal_Tokens;

   procedure Get_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Options_And_Payload_Data.Structure)
   is
   begin

      RFLX_Result := (Length => 0, Options_And_Payload => [others => 0]);

      CoAP_SPARK.Messages.Encoding.Encode_Options_And_Payload
        (Options_And_Payload => State.Request_Content,
         Status              => CoAP_SPARK.Status_Type (State.Current_Status),
         Encoded_Data        => RFLX_Result.Options_And_Payload,
         Encoded_Length      => RFLX_Result.Length);

   end Get_Options_And_Payload;


   procedure Put_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Data        : RFLX_Types.Bytes;
      RFLX_Result : out Boolean)
   is
      use type CoAP_Client.Session_Environment.Status_Type;
   begin

      CoAP_SPARK.Messages.Encoding.Decode_Options_And_Payload
        (Data            => Data,
         Status          => CoAP_SPARK.Status_Type (State.Current_Status),
         Decoded_Content => State.Response_Content);

      RFLX_Result :=
        State.Current_Status = RFLX.CoAP_Client.Session_Environment.OK;

   end Put_Options_And_Payload;

   procedure Put_Client_Error
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Error_Code  : RFLX.CoAP.Client_Error_Response;
      RFLX_Result : out Boolean)
   is
   begin
      State.Response_Codes :=
        (Code_Class => RFLX.CoAP.Client_Error,
         Client_Error_Code => Error_Code);

      RFLX_Result := True;
   end Put_Client_Error;

   procedure Put_Server_Error
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Error_Code  : RFLX.CoAP.Server_Error_Response;
      RFLX_Result : out Boolean)
   is
   begin
      State.Response_Codes :=
        (Code_Class => RFLX.CoAP.Server_Error,
         Server_Error_Code => Error_Code);

      RFLX_Result := True;
   end Put_Server_Error;

end RFLX.CoAP_Client.Session;
