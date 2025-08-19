with CoAP_SPARK.Messages;
with CoAP_SPARK.Resources;

with RFLX.CoAP;
with RFLX.CoAP_Server.Main_Loop_Environment;

package Server_Handling
with
   SPARK_Mode
is
   pragma Elaborate_Body;

   package Resource_Maps renames CoAP_SPARK.Resources.Resource_Maps;

   type Server_Implementation is
     new RFLX.CoAP_Server.Main_Loop_Environment.Server_Interface
   with record
      Stored_Resources : Resource_Maps.Map;
   end record;

   overriding
   procedure Handle_Request
       (Server           : in out Server_Implementation;
        Method           : RFLX.CoAP.Method_Code;
        Request_Content  : CoAP_SPARK.Messages.Content;
        Response_Codes   : out CoAP_SPARK.Messages.Response_Kind;
        Response_Content : out CoAP_SPARK.Messages.Content)
   with
      Pre'Class => not Response_Codes'Constrained;
       
end Server_Handling;