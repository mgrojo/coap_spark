with CoAP_SPARK.Content_Formats;
with CoAP_SPARK.Options.URI;

with RFLX.RFLX_Types;

with SPARK.Containers.Formal.Unbounded_Ordered_Maps;

package CoAP_SPARK.Resources
   with SPARK_Mode
is

   subtype Resource_Index is RFLX.RFLX_Types.Index'Base range 0 .. CoAP_SPARK.Max_Payload_Length;

   type Resource_Type (Last : Resource_Index) is
   record
      Data   : RFLX.RFLX_Types.Bytes (1 .. Last);
      Format : CoAP_SPARK.Content_Formats.Content_Type := 0;
   end record;

   use type CoAP_SPARK.Options.URI.URI_Part;

   package Resource_Maps is new
     SPARK.Containers.Formal.Unbounded_Ordered_Maps
       (Key_Type     => CoAP_SPARK.Options.URI.URI_Part,
        Element_Type => Resource_Type);

   use type RFLX.RFLX_Types.Index;

   --  Create a resource with a given data and format.
   function To_Resource
     (Data   : RFLX.RFLX_Types.Bytes;
      Format : CoAP_SPARK.Content_Formats.Content_Type)
      return Resource_Type
   with Pre => Data'Length <= CoAP_SPARK.Max_Payload_Length,
        Post => To_Resource'Result.Last = Data'Length;

end CoAP_SPARK.Resources;