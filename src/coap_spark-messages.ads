with CoAP_SPARK.Options.Lists;
with Interfaces;
with RFLX.CoAP;
with RFLX.RFLX_Types;

package CoAP_SPARK.Messages
   with SPARK_Mode
is

   subtype Response_Code is RFLX.CoAP.Code_Class
                              range RFLX.CoAP.Success .. RFLX.CoAP.Server_Error;

   type Response_Kind (Code_Class : Response_Code := RFLX.CoAP.Success) is
      record
         case Code_Class is
            when RFLX.CoAP.Success =>
               null;
            when RFLX.CoAP.Client_Error =>
               Client_Error_Code : RFLX.CoAP.Client_Error_Response;
            when RFLX.CoAP.Server_Error =>
               Server_Error_Code : RFLX.CoAP.Server_Error_Response;
         end case;
      end record;

   type Content is record
      Options : CoAP_SPARK.Options.Lists.Vector
                          (CoAP_SPARK.Max_Number_Of_Options);
      Format : Interfaces.Unsigned_32 := 0;
      Payload : RFLX.RFLX_Types.Bytes_Ptr;
   end record;

   procedure Print_Content (Item : Content);

   function Image (Item : Response_Kind) return String;

end CoAP_SPARK.Messages;
