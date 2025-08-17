with CoAP_SPARK.Options.Lists;

package CoAP_SPARK.Options.URI
  with SPARK_Mode
is

   subtype URI_Part is String
   with
     Dynamic_Predicate =>
       URI_Part'First = 1
       and then URI_Part'Length <= CoAP_SPARK.Max_URI_Part_Length;

   procedure Compose_Path_From_Options
     (Option_List : CoAP_SPARK.Options.Lists.Vector;
      Path        : out URI_Part;
      Last        : out Natural;
      Status      : out CoAP_SPARK.Status_Type)
   with
     Always_Terminates,
     Pre => Path'Length > 0,
     Post => Last in 0 .. Path'Last;

end CoAP_SPARK.Options.URI;



