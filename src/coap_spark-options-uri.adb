package body CoAP_SPARK.Options.URI
  with SPARK_Mode
is

   procedure Compose_Path_From_Options
     (Option_List : CoAP_SPARK.Options.Lists.Vector;
      Path        : out URI_Part;
      Status      : out CoAP_SPARK.Status_Type) is
   begin
      Path := URI_Strings.To_Unbounded_String ("");

      for Option of Option_List loop
         if Option.Number = RFLX.CoAP.Uri_Path then
            declare
               Option_Value : constant String :=
                 "/" & CoAP_SPARK.Options.Value_Image (Option);
            begin
               if URI_Strings.Length (Path) + Option_Value'Length
                 < CoAP_SPARK.Max_URI_Length
               then
                  URI_Strings.Append (Path, Option_Value);
               else
                  Status := CoAP_SPARK.Capacity_Error;
                  return;
               end if;
            end;
         end if;
      end loop;

      Status := CoAP_SPARK.OK;
   end Compose_Path_From_Options;

end CoAP_SPARK.Options.URI;

