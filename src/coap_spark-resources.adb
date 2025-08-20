package body CoAP_SPARK.Resources
   with SPARK_Mode
is

   function To_Resource
     (Data   : RFLX.RFLX_Types.Bytes;
      Format : CoAP_SPARK.Content_Formats.Content_Type)
      return Resource_Type
   is
      Result : Resource_Type (Last => Data'Length);
   begin
      Result :=
        (Last => Data'Length,
         Data => Data,
         Format => Format);
      return Result;
   end To_Resource;

end CoAP_SPARK.Resources;