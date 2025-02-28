with CoAP_SPARK.Options.Lists;

package CoAP_SPARK.Options.List_Sorting
   with SPARK_Mode
is

   -- These pragmas mirror the ones present in the generic package
   -- SPARK.Containers.Formal.Unbounded_Vectors.
   -- Otherwise, assertions will be executed, and they are meant for analysis
   -- only, not for run-time checking.
   -- This is a workaround that could be probably removed if a version of
   -- SPARKLib after this commit is used:
   -- https://github.com/AdaCore/SPARKlib/commit/5364ed32ced8638777fe5c1b879441c3ff1b5400
   pragma Assertion_Policy (Pre => Ignore);
   pragma Assertion_Policy (Post => Ignore);
   pragma Assertion_Policy (Contract_Cases => Ignore);

   package Instance is new
      CoAP_SPARK.Options.Lists.Generic_Sorting ("<" => CoAP_SPARK.Options."<");

end CoAP_SPARK.Options.List_Sorting;