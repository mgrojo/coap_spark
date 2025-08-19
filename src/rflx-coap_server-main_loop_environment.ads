with CoAP_SPARK.Messages;
with RFLX.CoAP;

package RFLX.CoAP_Server.Main_Loop_Environment
  with SPARK_Mode
is

   type Server_Interface is interface;

   procedure Handle_Request
       (Server           : in out Server_Interface;
        Method           : RFLX.CoAP.Method_Code;
        Request_Content  : CoAP_SPARK.Messages.Content;
        Response_Codes   : out CoAP_SPARK.Messages.Response_Kind;
        Response_Content : out CoAP_SPARK.Messages.Content) is abstract
   with
      Pre'Class => not Response_Codes'Constrained;

   type Server_Class_Access is access Server_Interface'Class;

   type State is record
      Current_Status     : CoAP_SPARK.Status_Type := CoAP_SPARK.OK;
      Server             : Server_Class_Access := null;
      Is_First_Message   : Boolean := True;
   end record;

   --  Initialize State.
   --
   --  This procedure must be called before the start of the state machine to
   --  ensure that State is initialized.
   --
   procedure Initialize
     (Server        : in out Server_Class_Access;
      Session_State : out State)
   with
      Pre  => Server /= null,
      Post => Server = null,
      Global => null,
      Always_Terminates;

   function Is_Finalized (Session_State : State) return Boolean
     is (Session_State.Server = null);

   procedure Finalize (Session_State : in out State)
   with
      Post => Is_Finalized (Session_State);

end RFLX.CoAP_Server.Main_Loop_Environment;
