with AUnit.Assertions; use AUnit.Assertions;

with CoAP_SPARK;

with RFLX.CoAP_Server.Main_Loop_Environment;
with RFLX.CoAP_Server.Options_And_Payload_Data;

package body RFLX.CoAP_Server.Main_Loop.Test is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test CoAP Server Get_Error_Options_And_Payload");
   end Name;

   overriding
   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      State  : RFLX.CoAP_Server.Main_Loop_Environment.State :=
        (Current_Status   => CoAP_SPARK.OK,
         Server           => null,
         Is_First_Message => True);
      Result : RFLX.CoAP_Server.Options_And_Payload_Data.Structure;
   begin

      RFLX.CoAP_Server.Main_Loop.Get_Error_Options_And_Payload
        (State, Result);

      Assert
        (CoAP_SPARK."=" (State.Current_Status, CoAP_SPARK.OK),
         Message => "Incorrect status after Get_Error_Options_And_Payload");

   end Run_Test;

end RFLX.CoAP_Server.Main_Loop.Test;