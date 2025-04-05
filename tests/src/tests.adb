with Ada.Environment_Variables;
with AUnit.Reporter.Text;
with AUnit.Run;
with CoAP_Suite; use CoAP_Suite;

procedure Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter     : AUnit.Reporter.Text.Text_Reporter;
   --  Honor https://no-color.org/
   Use_Colors   : constant Boolean :=
      not (Ada.Environment_Variables.Exists ("NO_COLOR") and then
           Ada.Environment_Variables.Value ("NO_COLOR") /= "");
begin

   Reporter.Set_Use_ANSI_Colors (Use_Colors);
   Runner (Reporter);

   Check_Or_Fail;
end Tests;
