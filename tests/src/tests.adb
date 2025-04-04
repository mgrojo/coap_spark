with Ada.Command_Line;
with AUnit.Reporter.Text;
with AUnit.Reporter.XML;
with AUnit.Run;
with CoAP_Suite; use CoAP_Suite;

procedure Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter     : AUnit.Reporter.Text.Text_Reporter;
   XML_Reporter : AUnit.Reporter.XML.XML_Reporter;
begin
   if Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "--xml"
   then
      Runner (XML_Reporter);
   else
      Reporter.Set_Use_ANSI_Colors (True);
      Runner (Reporter);
   end if;
end Tests;
