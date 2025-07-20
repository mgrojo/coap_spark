package body RFLX.CoAP_Server.Main_Loop_Environment
  with SPARK_Mode
is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Request_Handler : Handle_Request_Callback; Session_State : out State) is
   begin

      Session_State :=
        (Current_Status   => CoAP_SPARK.OK,
         Request_Handler  => Request_Handler,
         Is_First_Message => True);
   end Initialize;

end RFLX.CoAP_Server.Main_Loop_Environment;
