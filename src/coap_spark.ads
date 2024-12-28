package CoAP_SPARK
   with SPARK_Mode
is

   Max_Number_Of_Options : constant := 255;

   Default_Port : constant := 5683;

   Max_URI_Length : constant := 1034;

   Max_URI_Part_Length : constant := 255;

   Default_Scheme : constant String := "coap";

   Default_Secure_Scheme : constant String := "coaps";
end CoAP_SPARK;
