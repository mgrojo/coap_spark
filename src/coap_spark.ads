package CoAP_SPARK
   with SPARK_Mode
is

   Max_Number_Of_Options : constant := 255;

   Default_Port : constant := 5683;
   Secure_Port : constant := 5684;

   Max_URI_Length : constant := 1034;

   Max_URI_Part_Length : constant := 255;

   Max_Payload_Length : constant := 1024;

   Default_Scheme : constant String := "coap";

   Secure_Scheme : constant String := "coaps";

   type Status_Type is
     (OK,
      Capacity_Error,
      Invalid_Request,
      Malformed_Message,
      Unknown_Critical_Option,
      Communication_Problems,
      Unexpected_Case);

end CoAP_SPARK;
