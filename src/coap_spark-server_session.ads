with CoAP_SPARK.Channel;

with RFLX.CoAP_Server.Main_Loop.FSM;

package CoAP_SPARK.Server_Session
   with SPARK_Mode
is

   package FSM renames RFLX.CoAP_Server.Main_Loop.FSM;

   procedure Run_Session_Loop
     (Ctx : in out FSM.Context; Skt : in out CoAP_SPARK.Channel.Socket_Type)
   with
     Pre  => FSM.Initialized (Ctx) and then CoAP_SPARK.Channel.Is_Valid (Skt),
     Post => FSM.Initialized (Ctx);

end CoAP_SPARK.Server_Session;