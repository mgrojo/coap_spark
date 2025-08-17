with CoAP_SPARK.Options.URI;

package body Server_Handling
   with SPARK_Mode
is

   procedure Handle_Request
       (Method           : RFLX.CoAP.Method_Code;
        Request_Content  : CoAP_SPARK.Messages.Content;
        Response_Codes   : out CoAP_SPARK.Messages.Response_Kind;
        Response_Content : out CoAP_SPARK.Messages.Content)
   is
      use type CoAP_SPARK.Status_Type;
   begin
      -- Handle the request based on the method and content.
      case Method is
         when RFLX.CoAP.Get =>
            declare
               Path   :
                 CoAP_SPARK.Options.URI.URI_Part
                   (1 .. CoAP_SPARK.Max_URI_Part_Length);
               Last   : Natural;
               Status : CoAP_SPARK.Status_Type;
            begin
               -- Extract the URI path from the request content
               CoAP_SPARK.Options.URI.Compose_Path_From_Options
                 (Option_List => Request_Content.Options,
                  Path        => Path,
                  Last        => Last,
                  Status      => Status);

               if Status /= CoAP_SPARK.OK or else Last not in Path'Range then
                  Response_Codes :=
                    (Code_Class        => RFLX.CoAP.Client_Error,
                     Client_Error_Code => RFLX.CoAP.Bad_Request);
                  return;
               end if;

               -- Handle GET request
               Response_Codes :=
                 (Code_Class   => RFLX.CoAP.Success,
                  Success_Code => RFLX.CoAP.Continue);

               CoAP_SPARK.Messages.Initialize_With_Text_Payload
                 (Text => "GET request on " & Path (Path'First .. Last),
                  Item => Response_Content);
            end;

         when RFLX.CoAP.Post =>
            -- Handle POST request
            Response_Codes :=
              (Code_Class   => RFLX.CoAP.Success,
               Success_Code => RFLX.CoAP.Created);

            CoAP_SPARK.Messages.Initialize_With_Text_Payload
              (Text => "POST request handled", Item => Response_Content);

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