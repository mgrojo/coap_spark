with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;

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
         State.Current_Message_ID := Random_ID.Random (Byte_Generator);
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

   procedure Get_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Options_And_Payload_Data.Structure) is
   begin
      Put_Line ("Get_Options_And_Payload");
      Set_Value_Empty (RFLX_Result.Options_And_Payload);
      RFLX_Result.Length := 0;
   end Get_Options_And_Payload;

begin
   Random_Bytes.Reset (Byte_Generator);
   Random_ID.Reset (ID_Generator);
end RFLX.CoAP_Client.Session;
