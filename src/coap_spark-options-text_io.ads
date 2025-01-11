with CoAP_SPARK.Options.Lists;
with CoAP_SPARK.Log;

package CoAP_SPARK.Options.Text_IO with
   SPARK_Mode
is

   procedure Print
     (Item      : CoAP_SPARK.Options.Option;
      Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug);

   procedure Print
     (Item      : CoAP_SPARK.Options.Lists.Vector;
      Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug);


end CoAP_SPARK.Options.Text_IO;