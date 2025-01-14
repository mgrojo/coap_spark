
with CoAP_SPARK.Content_Formats;
with CoAP_SPARK.Options.Text_IO;
with CoAP_SPARK.Utils;

package body CoAP_SPARK.Messages
  with SPARK_Mode
is

   -------------------
   -- Print_Content --
   -------------------
   procedure Print_Content
     (Item              : Content;
      General_Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug;
      Log_Level_Payload : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Info)
   is
      Payload_Format : constant CoAP_SPARK.Options.Option_Format :=
        (if CoAP_SPARK.Content_Formats.Is_Text (Item.Format)
         then CoAP_SPARK.Options.UTF8_String
         else CoAP_SPARK.Options.Opaque);
   begin
      for Option of Item.Options loop
         CoAP_SPARK.Options.Text_IO.Print (Option, General_Log_Level);
      end loop;

      if Item.Payload not in null then
         CoAP_SPARK.Log.Put ("Content-Format: ", Level => General_Log_Level);
         CoAP_SPARK.Log.Put_Line
           (CoAP_SPARK.Content_Formats.To_String (Item.Format),
            Level => General_Log_Level);

         CoAP_SPARK.Log.Put ("Payload: ", Level => General_Log_Level);
         CoAP_SPARK.Log.Put_Line
           (CoAP_SPARK.Options.Image
              (Format => Payload_Format, Value => Item.Payload.all),
            Level => Log_Level_Payload);
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

   procedure Finalize (Item : in out Content)
   is
   begin
      for I in
         CoAP_SPARK.Options.Lists.First_Index (Item.Options) ..
         CoAP_SPARK.Options.Lists.Last_Index (Item.Options)
      loop
         declare
            Option_Copy : CoAP_SPARK.Options.Option :=
              CoAP_SPARK.Options.Lists.Element (Item.Options, I);
         begin
            -- Make a swallow copy of the option to be able to use a variable
            -- to free the stored Option.
            --
            CoAP_SPARK.Options.Free (Option_Copy);
            pragma Assert (not CoAP_SPARK.Options.Has_Buffer (Option_Copy));
         end;
      end loop;
      CoAP_SPARK.Options.Lists.Clear (Item.Options);
      if Item.Payload not in null then
         RFLX.RFLX_Types.Free (Item.Payload);
      end if;
   end Finalize;

end CoAP_SPARK.Messages;

