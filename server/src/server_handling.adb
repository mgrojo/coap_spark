with CoAP_SPARK.Options.Lists;
with CoAP_SPARK.Options.URI;

with RFLX.RFLX_Types;

package body Server_Handling
   with SPARK_Mode
is

   overriding
   procedure Handle_Request
       (Server           : in out Server_Implementation;
        Method           : RFLX.CoAP.Method_Code;
        Request_Content  : CoAP_SPARK.Messages.Content;
        Response_Codes   : out CoAP_SPARK.Messages.Response_Kind;
        Response_Content : out CoAP_SPARK.Messages.Content)
   is
      use type CoAP_SPARK.Status_Type;
      Stored_Resources : Resource_Maps.Map renames Server.Stored_Resources;
   begin
      -- Handle the request based on the method and content.
      case Method is
         when RFLX.CoAP.Get =>
            declare
               Path   : CoAP_SPARK.Options.URI.URI_Part;
               Status : CoAP_SPARK.Status_Type;
            begin
               -- Extract the URI path from the request content
               CoAP_SPARK.Options.URI.Compose_Path_From_Options
                 (Option_List => Request_Content.Options,
                  Path        => Path,
                  Status      => Status);

               if Status /= CoAP_SPARK.OK then
                  Response_Codes :=
                    (Code_Class        => RFLX.CoAP.Client_Error,
                     Client_Error_Code => RFLX.CoAP.Request_Entity_Too_Large);

                  CoAP_SPARK.Messages.Initialize_With_Text_Payload
                     (Text => "Path too long",
                      Item => Response_Content);
                  return;
               end if;

               if Resource_Maps.Contains (Stored_Resources, Path) then
                  -- Resource found, retrieve it
                  declare
                     Resource : constant CoAP_SPARK.Resources.Resource_Type :=
                        Resource_Maps.Element (Stored_Resources, Path);
                  begin
                     
                     Response_Codes :=
                       (Code_Class   => RFLX.CoAP.Success,
                        Success_Code => RFLX.CoAP.Content);

                     Response_Content := (Options =>
                                            CoAP_SPARK.Options.Lists.Empty_Vector,
                                          Format =>
                                            Resource.Format,
                                          Payload => new RFLX.RFLX_Types.Bytes' (Resource.Data));
                  end;
               else
                  -- Resource not found
                  Response_Codes :=
                    (Code_Class        => RFLX.CoAP.Client_Error,
                     Client_Error_Code => RFLX.CoAP.Not_Found);

                  CoAP_SPARK.Messages.Initialize_With_Text_Payload
                     (Text => "Resource not found",
                      Item => Response_Content);
               end if;
            end;  

         when RFLX.CoAP.Post =>
            declare
               Path   : CoAP_SPARK.Options.URI.URI_Part;
               Status : CoAP_SPARK.Status_Type;
            begin
               -- Extract the URI path from the request content
               CoAP_SPARK.Options.URI.Compose_Path_From_Options
                 (Option_List => Request_Content.Options,
                  Path        => Path,
                  Status      => Status);

               if Status /= CoAP_SPARK.OK then
                  Response_Codes :=
                    (Code_Class        => RFLX.CoAP.Client_Error,
                     Client_Error_Code => RFLX.CoAP.Request_Entity_Too_Large);

                  CoAP_SPARK.Messages.Initialize_With_Text_Payload
                     (Text => "Path too long",
                      Item => Response_Content);
                  return;
               end if;

               if not Resource_Maps.Contains (Stored_Resources, Path) then
                  -- Resource found, retrieve it
                  declare
                     Resource : constant CoAP_SPARK.Resources.Resource_Type := CoAP_SPARK.Resources.To_Resource
                         (Data   => Request_Content.Payload.all,
                          Format => Request_Content.Format);
                  begin
                     
                     Response_Codes :=
                       (Code_Class   => RFLX.CoAP.Success,
                        Success_Code => RFLX.CoAP.Created);

                     -- Add the resource to the stored resources
                     Resource_Maps.Insert (Stored_Resources, Path, Resource);

                     Response_Content := (Options =>
                                            CoAP_SPARK.Options.Lists.Empty_Vector,
                                          Format =>
                                            Resource.Format,
                                          Payload => new RFLX.RFLX_Types.Bytes' (Resource.Data));
                  end;
               else
                  -- Resource already exists
                  Response_Codes :=
                    (Code_Class        => RFLX.CoAP.Client_Error,
                     Client_Error_Code => RFLX.CoAP.Bad_Request);

                  CoAP_SPARK.Messages.Initialize_With_Text_Payload
                     (Text => "Resource already exists",
                      Item => Response_Content);
               end if;
            end;  

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