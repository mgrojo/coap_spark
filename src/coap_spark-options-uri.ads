with Ada.Strings.Bounded.Hash;

with CoAP_SPARK.Options.Lists;

package CoAP_SPARK.Options.URI
  with SPARK_Mode
is

   package URI_Strings is
      new Ada.Strings.Bounded.Generic_Bounded_Length
         (Max => CoAP_SPARK.Max_URI_Length);

   subtype URI_Part is URI_Strings.Bounded_String;

   function Hash is new Ada.Strings.Bounded.Hash
      (URI_Strings);

   procedure Compose_Path_From_Options
     (Option_List : CoAP_SPARK.Options.Lists.Vector;
      Path        : out URI_Part;
      Status      : out CoAP_SPARK.Status_Type)
   with
     Always_Terminates,
     Global => null;

end CoAP_SPARK.Options.URI;



