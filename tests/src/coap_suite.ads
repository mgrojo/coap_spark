with AUnit.Test_Suites; use AUnit.Test_Suites;

package CoAP_Suite is

   function Suite return Access_Test_Suite;

   -- Workaround for problem in SPARKLib.
   -- See https://forum.ada-lang.io/t/where-is-sparklib/218/15
   procedure Check_Or_Fail
   with
     Always_Terminates => True,
     Export => True,
     Convention => Ada,
     External_Name => "check_or_fail";

end CoAP_Suite;