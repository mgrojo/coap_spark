with CoAP_SPARK.Messages;
with RFLX.CoAP;

package RFLX.CoAP_Server.Main_Loop_Environment
  with SPARK_Mode
is

   type Handle_Request_Callback is
     access procedure
       (Method           : RFLX.CoAP.Method_Code;
        Request_Content  : CoAP_SPARK.Messages.Content;
        Response_Codes   : out CoAP_SPARK.Messages.Response_Kind;
        Response_Content : out CoAP_SPARK.Messages.Content);

   type State is record
      Current_Status     : CoAP_SPARK.Status_Type := CoAP_SPARK.OK;
      Request_Handler    : Handle_Request_Callback := null;
      Is_First_Message   : Boolean := True;
   end record;

   --  Initialize State.
   --
   --  This procedure must be called before the start of the state machine to
   --  ensure that State is initialized.
   --
   procedure Initialize
     (Request_Handler : Handle_Request_Callback;
      Session_State   : out State)
   with
     Always_Terminates;


end RFLX.CoAP_Server.Main_Loop_Environment;
