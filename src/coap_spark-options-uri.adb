package body CoAP_SPARK.Options.URI
  with SPARK_Mode
is

   procedure Compose_Path_From_Options
     (Option_List : CoAP_SPARK.Options.Lists.Vector;
      Path        : out URI_Part;
      Last        : out Natural;
      Status      : out CoAP_SPARK.Status_Type)
   is
   begin
      Last := 0;

      for Option of Option_List loop
         pragma Loop_Invariant (Last <= Path'Last);
         if Option.Number = RFLX.CoAP.Uri_Path then
            declare
               Option_Value : constant String := "/" &
                 CoAP_SPARK.Options.Value_Image (Option);
            begin
               if Last + Option_Value'Length < Path'Last then
                  Path (Last + 1 .. Last + Option_Value'Length) :=
                    Option_Value;
                  Last := Last + Option_Value'Length;
               else
                  Status := CoAP_SPARK.Capacity_Error;
                  Last := 0;
                  return;
               end if;
            end;
         end if;
      end loop;

      Status := CoAP_SPARK.OK;
   end Compose_Path_From_Options;

end CoAP_SPARK.Options.URI;

