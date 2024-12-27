with Interfaces;

package CoAP_SPARK.URI
   with SPARK_Mode
is

   subtype URI_Length is Natural range 0 .. CoAP_SPARK.Max_URI_Length;

   type URI (Length : URI_Length) is private;

   Scheme_Suffix : constant String := "://";
   Host_Suffix   : constant String := ":";

   function Create
     (Scheme : String := Default_Scheme;
      Host   : String;
      Port   : Interfaces.Unsigned_16 := CoAP_SPARK.Default_Port;
      Path   : String := "/";
      Query  : String := "") return URI
   with
      Pre =>
         Scheme'Length in 0 .. Default_Secure_Scheme'Length and then
         (Scheme = Default_Scheme or else
          Scheme = Default_Secure_Scheme or else
          Scheme = "") and then
         Host'Length in 1 .. Max_URI_Part_Length and then
         Path'Length in 1 .. Max_URI_Part_Length and then
         Query'Length in 0 .. Max_URI_Part_Length and then
         Scheme'Length + Host'Length +
         Port'Image'Length + Path'Length +
         Query'Length <= CoAP_SPARK.Max_URI_Length,
      Post => Create'Result.Length =
       Scheme'Length + Scheme_Suffix'Length + Host'Length +
       Port'Image'Length + Path'Length + Query'Length;

   function Create (URI_String : String) return URI
     with
      Pre => URI_String'Length <= CoAP_SPARK.Max_URI_Length,
      Post => Create'Result.Length = URI_String'Length;

   function Scheme (Self : URI) return String;

   function Host (Self : URI) return String;

   function Port (Self : URI) return Interfaces.Unsigned_16;

   function Path (Self : URI) return String;

   function Query (Self : URI) return String;

private

   type URI (Length : URI_Length) is record
      URI_String  : String (1 .. Length);
      Scheme_Last : URI_Length;
      Host_Last   : URI_Length;
      Port_Last   : URI_Length;
      Path_Last   : URI_Length;
   end record;

end CoAP_SPARK.URI;