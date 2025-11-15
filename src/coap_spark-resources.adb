with CoAP_SPARK.Utils;

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
        (Last   => Data'Length,
         Data   => Data,
         Format => Format);
      return Result;
   end To_Resource;

   function To_Text_Resource
     (Text : String)
      return Resource_Type
   is
      Result : Resource_Type (Last => Text'Length);
   begin
      Result :=
        (Last   => Text'Length,
         Data   => CoAP_SPARK.Utils.Text_As_Bytes (Text),
         Format => CoAP_SPARK.Content_Formats.text.plain_charset_utf_8);
      return Result;
   end To_Text_Resource;

end CoAP_SPARK.Resources;