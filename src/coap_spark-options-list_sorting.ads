with CoAP_SPARK.Options.Lists;

package CoAP_SPARK.Options.List_Sorting
   with SPARK_Mode
is

   -- These pragmas mirror the ones present in the generic package
   -- SPARK.Containers.Formal.Unbounded_Vectors.
   -- Otherwise, assertions will be executed, and they are meant for analysis
   -- only, not for run-time checking.
   pragma Assertion_Policy (Pre => Ignore);
   pragma Assertion_Policy (Post => Ignore);
   pragma Assertion_Policy (Contract_Cases => Ignore);

   package Instance is new
      CoAP_SPARK.Options.Lists.Generic_Sorting ("<" => CoAP_SPARK.Options."<");

end CoAP_SPARK.Options.List_Sorting;