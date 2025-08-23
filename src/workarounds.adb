-- Workaround for problem in SPARKLib.
-- See https://forum.ada-lang.io/t/where-is-sparklib/218/15
package body Workarounds is

   procedure Check_Or_Fail
   is null;

end Workarounds;