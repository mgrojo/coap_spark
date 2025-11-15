with Ada.Strings.Unbounded;

with CoAP_SPARK.Options.Lists;

package CoAP_SPARK.Options.URI
  with SPARK_Mode
is

   package URI_Strings renames Ada.Strings.Unbounded;

   subtype URI_Part is URI_Strings.Unbounded_String
   with
     Dynamic_Predicate =>
       (URI_Strings.Length (URI_Part) <= CoAP_SPARK.Max_URI_Length);

   procedure Compose_Path_From_Options
     (Option_List : CoAP_SPARK.Options.Lists.Vector;
      Path        : out URI_Part;
      Status      : out CoAP_SPARK.Status_Type)
   with Always_Terminates, Global => null;

end CoAP_SPARK.Options.URI;



