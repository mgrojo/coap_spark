with Ada.Unchecked_Deallocation;

package body RFLX.CoAP_Server.Main_Loop_Environment
  with SPARK_Mode
is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Server        : in out Server_Class_Access;
      Session_State : out State) is
   begin

      Session_State :=
        (Current_Status   => CoAP_SPARK.OK,
         Server           => Server,
         Is_First_Message => True);

      Server := null;
   end Initialize;

   procedure Free is new Ada.Unchecked_Deallocation
     (Server_Interface'Class, Server_Class_Access);

   procedure Finalize (Session_State : in out State) is
   begin
      if Session_State.Server /= null then
         Free (Session_State.Server);
      end if;
   end Finalize;

end RFLX.CoAP_Server.Main_Loop_Environment;
