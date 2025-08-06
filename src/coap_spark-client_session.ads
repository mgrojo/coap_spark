with RFLX.CoAP_Client.Session.FSM;
with CoAP_SPARK.Channel;

package CoAP_SPARK.Client_Session
   with SPARK_Mode
is

   package FSM renames RFLX.CoAP_Client.Session.FSM;

   procedure Run_Session_Loop
     (Ctx : in out FSM.Context;
      Skt : in out CoAP_SPARK.Channel.Socket_Type)
   with
      Pre =>
        FSM.Initialized (Ctx)
        and then CoAP_SPARK.Channel.Is_Valid (Skt),
      Post => FSM.Initialized (Ctx);

end CoAP_SPARK.Client_Session;