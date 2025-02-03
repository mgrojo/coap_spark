-- Workaround for problem in SPARKLib.
-- See https://forum.ada-lang.io/t/where-is-sparklib/218/15
package Workarounds is

   procedure Check_Or_Fail
   is null
   with
     Export => True,
     Convention => C,
     External_Name => "check_or_fail";

end Workarounds;