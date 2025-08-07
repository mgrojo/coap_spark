
package body Server_Handling
   with SPARK_Mode
is

   procedure Handle_Request
       (Method           : RFLX.CoAP.Method_Code;
        Request_Content  : CoAP_SPARK.Messages.Content;
        Response_Codes   : out CoAP_SPARK.Messages.Response_Kind;
        Response_Content : out CoAP_SPARK.Messages.Content)
   is
   begin
      -- Handle the request based on the method and content.
      case Method is
         when RFLX.CoAP.Get =>
            -- Handle GET request
            Response_Codes :=
              (Code_Class   => RFLX.CoAP.Success,
               Success_Code => RFLX.CoAP.Continue);
            Response_Content := Request_Content;

         when RFLX.CoAP.Post =>
            -- Handle POST request
            Response_Codes :=
              (Code_Class   => RFLX.CoAP.Success,
               Success_Code => RFLX.CoAP.Created);
            Response_Content := Request_Content;

         when others =>
            -- Handle other methods
            Response_Codes :=
              (Code_Class        => RFLX.CoAP.Server_Error,
               Server_Error_Code => RFLX.CoAP.Internal_Server_Error);
            CoAP_SPARK.Messages.Initialize_With_Text_Payload
              (Text => "Method not supported", Item => Response_Content);
      end case;
   end Handle_Request;
       
end Server_Handling;