
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with CoAP_SPARK.Content_Formats.Test;
with CoAP_SPARK.Options.Test;
with CoAP_SPARK.URI.Test;

package body CoAP_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Test_Case_Access'(new CoAP_SPARK.Content_Formats.Test.Test));
      Ret.Add_Test (Test_Case_Access'(new CoAP_SPARK.Options.Test.Test));
      Ret.Add_Test (Test_Case_Access'(new CoAP_SPARK.URI.Test.Test));
      Ret.Add_Test (Test_Case_Access'(new CoAP_SPARK.URI.Test.Compose_Test));
      return Ret;
   end Suite;

   procedure Check_Or_Fail is null;

end CoAP_Suite;
