package body CoAP_SPARK.Options.Text_IO
   with SPARK_Mode
is

   procedure Print
     (Item      : CoAP_SPARK.Options.Option;
      Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug)
   is
   begin
      CoAP_SPARK.Log.Put ("Option: ", Log_Level);
      CoAP_SPARK.Log.Put_Line (CoAP_SPARK.Options.Get_Number (Item)'Image);
      if CoAP_SPARK.Options.Has_Buffer (Item) then
         CoAP_SPARK.Log.Put ("  - Length: ", Log_Level);
         CoAP_SPARK.Log.Put_Line (CoAP_SPARK.Options.Get_Length (Item)'Image);
         CoAP_SPARK.Log.Put ("  - Value: ", Log_Level);
         CoAP_SPARK.Log.Put_Line
           (CoAP_SPARK.Options.Value_Image (Item), Log_Level);
      else
         CoAP_SPARK.Log.Put_Line ("  - Length: 0", Log_Level);
         CoAP_SPARK.Log.Put_Line ("  - Value: (empty)", Log_Level);
      end if;
   end Print;

   procedure Print
     (Item      : CoAP_SPARK.Options.Indefinite_Option;
      Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug)
   is
   begin
      CoAP_SPARK.Log.Put ("Option: ", Log_Level);
      CoAP_SPARK.Log.Put_Line (CoAP_SPARK.Options.Get_Number (Item)'Image);
      if CoAP_SPARK.Options.Get_Length (Item) > 0 then
         CoAP_SPARK.Log.Put ("  - Length: ", Log_Level);
         CoAP_SPARK.Log.Put_Line (CoAP_SPARK.Options.Get_Length (Item)'Image);
         CoAP_SPARK.Log.Put ("  - Value: ", Log_Level);
         CoAP_SPARK.Log.Put_Line
           (CoAP_SPARK.Options.Value_Image (Item), Log_Level);
      else
         CoAP_SPARK.Log.Put_Line ("  - Length: 0", Log_Level);
         CoAP_SPARK.Log.Put_Line ("  - Value: (empty)", Log_Level);
      end if;
   end Print;

   procedure Print
     (Item      : CoAP_SPARK.Options.Lists.Vector;
      Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug)
   is
   begin
      for Option of Item loop
         Print (Option, Log_Level);
      end loop;
   end Print;

end CoAP_SPARK.Options.Text_IO;
