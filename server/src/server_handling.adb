with Ada.Containers;

with CoAP_SPARK.Options.Lists;
with CoAP_SPARK.Options.URI;

with Coverage;

with RFLX.RFLX_Types;

package body Server_Handling
  with SPARK_Mode
is

   function "-" (S : CoAP_SPARK.Options.URI.URI_Part) return String
   renames CoAP_SPARK.Options.URI.URI_Strings.To_String;

   overriding
   procedure Handle_Request
     (Server           : in out Server_Implementation;
      Method           : RFLX.CoAP.Method_Code;
      Request_Content  : CoAP_SPARK.Messages.Content;
      Response_Codes   : out CoAP_SPARK.Messages.Response_Kind;
      Response_Content : out CoAP_SPARK.Messages.Content)
   is
      use type Ada.Containers.Count_Type;
      use type CoAP_SPARK.Status_Type;
      use type CoAP_SPARK.Messages.Payload_Ptr;
      Stored_Resources : Resource_Maps.Map renames Server.Stored_Resources;
      Path             : CoAP_SPARK.Options.URI.URI_Part;
      Status           : CoAP_SPARK.Status_Type;
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
           (Text => "Path too long", Item => Response_Content);
         return;
      end if;

      -- Handle the request based on the method and content.
      case Method is
         when RFLX.CoAP.Get =>

            if Resource_Maps.Contains (Stored_Resources, -Path) then
               -- Resource found, retrieve it
               declare
                  Resource : constant CoAP_SPARK.Resources.Resource_Type :=
                    Resource_Maps.Element (Stored_Resources, -Path);
               begin

                  Response_Codes :=
                    (Code_Class   => RFLX.CoAP.Success,
                     Success_Code => RFLX.CoAP.Content);

                  Response_Content :=
                    (Options => CoAP_SPARK.Options.Lists.Empty_Vector,
                     Format  => Resource.Format,
                     Payload => new RFLX.RFLX_Types.Bytes'(Resource.Data));
               end;
            else
               -- Resource not found
               Response_Codes :=
                 (Code_Class        => RFLX.CoAP.Client_Error,
                  Client_Error_Code => RFLX.CoAP.Not_Found);

               CoAP_SPARK.Messages.Initialize_With_Text_Payload
                 (Text => "Resource not found", Item => Response_Content);
            end if;

         when RFLX.CoAP.Post =>

            if not Resource_Maps.Contains (Stored_Resources, -Path)
              and then Request_Content.Payload /= null
              and then Resource_Maps.Length (Stored_Resources)
                       < Ada.Containers.Count_Type'Last
            then
               -- Resource not found, create it
               declare
                  Resource : constant CoAP_SPARK.Resources.Resource_Type :=
                    CoAP_SPARK.Resources.To_Resource
                      (Data   => Request_Content.Payload.all,
                       Format => Request_Content.Format);
               begin

                  Response_Codes :=
                    (Code_Class   => RFLX.CoAP.Success,
                     Success_Code => RFLX.CoAP.Created);

                  -- Add the resource to the stored resources
                  Resource_Maps.Insert (Stored_Resources, -Path, Resource);

                  Response_Content :=
                    (Options => CoAP_SPARK.Options.Lists.Empty_Vector,
                     Format  => Resource.Format,
                     Payload => new RFLX.RFLX_Types.Bytes'(Resource.Data));
               end;
            elsif Resource_Maps.Length (Stored_Resources)
              = Ada.Containers.Count_Type'Last
            then
               -- Resource limit reached
               Response_Codes :=
                 (Code_Class        => RFLX.CoAP.Client_Error,
                  Client_Error_Code => RFLX.CoAP.Not_Acceptable);

               CoAP_SPARK.Messages.Initialize_With_Text_Payload
                 (Text => "Resource storage limit reached",
                  Item => Response_Content);
            else
               -- Resource already exists or payload is not provided
               Response_Codes :=
                 (Code_Class        => RFLX.CoAP.Client_Error,
                  Client_Error_Code => RFLX.CoAP.Conflict);

               CoAP_SPARK.Messages.Initialize_With_Text_Payload
                 (Text =>
                    (if Request_Content.Payload = null
                     then "Payload for resource not provided"
                     else "Resource already exists"),
                  Item => Response_Content);
            end if;

         when RFLX.CoAP.Delete =>

            if Resource_Maps.Contains (Stored_Resources, -Path) then
               -- Resource found, delete it
               Resource_Maps.Delete (Stored_Resources, -Path);

               Response_Codes :=
                 (Code_Class   => RFLX.CoAP.Success,
                  Success_Code => RFLX.CoAP.Deleted);

               CoAP_SPARK.Messages.Initialize_With_Text_Payload
                 (Text => "Resource deleted", Item => Response_Content);
            else
               -- Resource not found
               Response_Codes :=
                 (Code_Class        => RFLX.CoAP.Client_Error,
                  Client_Error_Code => RFLX.CoAP.Not_Found);

               CoAP_SPARK.Messages.Initialize_With_Text_Payload
                 (Text => "Resource not found", Item => Response_Content);
            end if;

         when RFLX.CoAP.Fetch =>

            Response_Codes :=
              (Code_Class   => RFLX.CoAP.Success,
               Success_Code => RFLX.CoAP.Content);

            CoAP_SPARK.Messages.Initialize_With_Text_Payload
               (Text => "Coverage report dumped", Item => Response_Content);

            -- Dump coverage data when Fetch method is called. This allows
            -- to collect coverage data during testing, because if the process
            -- is killed, no coverage report is generated.
            -- Gnatprove thinks Dump has no effect, because we cannot define
            -- the Global aspect as the two implementations of Coverage
            -- have no way to define an abstract state (a null procedure and
            -- a C function). So we disable the warning.
            pragma Warnings (Off, "subprogram ""Dump"" has no effect");
            Coverage.Dump;
            pragma Warnings (On, "subprogram ""Dump"" has no effect");

         when others =>
            -- Handle other methods
            Response_Codes :=
              (Code_Class        => RFLX.CoAP.Client_Error,
               Client_Error_Code => RFLX.CoAP.Method_Not_Allowed);
            CoAP_SPARK.Messages.Initialize_With_Text_Payload
              (Text => "Method not supported", Item => Response_Content);
      end case;
   end Handle_Request;

end Server_Handling;
