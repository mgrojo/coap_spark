with Ada.Text_IO;

with CoAP_SPARK.Content_Formats;
with CoAP_SPARK.Options.Text_IO;
with CoAP_SPARK.Utils;

package body CoAP_SPARK.Messages
  with SPARK_Mode
is

   -------------------
   -- Print_Content --
   -------------------
   procedure Print_Content (Item : Content) is
      Payload_Format : constant CoAP_SPARK.Options.Option_Format :=
        (if CoAP_SPARK.Content_Formats.Is_Text (Item.Format)
         then CoAP_SPARK.Options.UTF8_String
         else CoAP_SPARK.Options.Opaque);
   begin
      for Option of Item.Options loop
         CoAP_SPARK.Options.Text_IO.Print (Option);
      end loop;

      if Item.Payload not in null then
         Ada.Text_IO.Put ("Content-Format: ");
         Ada.Text_IO.Put_Line
           (CoAP_SPARK.Content_Formats.To_String (Item.Format));

         Ada.Text_IO.Put ("Payload: ");
         Ada.Text_IO.Put_Line
           (CoAP_SPARK.Options.Image
              (Format => Payload_Format, Value => Item.Payload.all));
      end if;
   end Print_Content;

   function Image (Item : Response_Kind) return String is
      Detail_Number : constant Integer :=
        (case Item.Code_Class is
           when RFLX.CoAP.Success => 0,
           when RFLX.CoAP.Client_Error =>
             RFLX.CoAP.Client_Error_Response'Enum_Rep (Item.Client_Error_Code),
           when RFLX.CoAP.Server_Error =>
             RFLX.CoAP.Server_Error_Response'Enum_Rep
               (Item.Server_Error_Code));

      Detail_Image : constant String :=
        (case Item.Code_Class is
           when RFLX.CoAP.Success => RFLX.CoAP.Success'Image,
           when RFLX.CoAP.Client_Error =>
             RFLX.CoAP.Client_Error_Response'Image (Item.Client_Error_Code),
           when RFLX.CoAP.Server_Error =>
             RFLX.CoAP.Server_Error_Response'Image (Item.Server_Error_Code));
   begin
      return
        CoAP_SPARK.Utils.Padded_Image
          (Source => Item.Code_Class'Enum_Rep, Count => 1)
        & "."
        & CoAP_SPARK.Utils.Padded_Image (Source => Detail_Number, Count => 2)
        & " (" & Detail_Image & ")";
   end Image;
end CoAP_SPARK.Messages;

