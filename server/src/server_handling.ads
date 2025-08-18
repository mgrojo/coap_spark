with CoAP_SPARK.Messages;

with RFLX.CoAP;

package Server_Handling
with
   SPARK_Mode,
   Abstract_State => Resources,
   Initializes => Resources
is

   procedure Handle_Request
       (Method           : RFLX.CoAP.Method_Code;
        Request_Content  : CoAP_SPARK.Messages.Content;
        Response_Codes   : out CoAP_SPARK.Messages.Response_Kind;
        Response_Content : out CoAP_SPARK.Messages.Content)
   with
      Pre => not Response_Codes'Constrained,
      Global => (In_Out => Resources);
       
end Server_Handling;