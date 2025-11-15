with AUnit.Simple_Test_Cases;

package RFLX.CoAP_Server.Main_Loop.Test is

   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding
   function Name (T : Test) return AUnit.Message_String;

   overriding
   procedure Run_Test (T : in out Test);

end RFLX.CoAP_Server.Main_Loop.Test;