with Interfaces;

package CoAP_SPARK.URI
   with SPARK_Mode
is

   subtype URI_Length is Natural range 0 .. CoAP_SPARK.Max_URI_Length;

   type URI (Length : URI_Length) is private;

   Scheme_Suffix : constant String := "://";
   Host_Suffix   : constant String := ":";

   function Is_Valid (Self : URI) return Boolean;

   function Is_Valid_Scheme (Scheme : String) return Boolean is
      (Scheme = Default_Scheme or else Scheme = Secure_Scheme);

   function Create
     (Scheme : String := Default_Scheme;
      Host   : String;
      Port   : Interfaces.Unsigned_16 := CoAP_SPARK.Default_Port;
      Path   : String := "/";
      Query  : String := "") return URI
   with
      Pre => Is_Valid_Scheme (Scheme) and then
         Host'Length in 1 .. Max_URI_Part_Length and then
         Path'Length in 1 .. Max_URI_Part_Length and then
         Query'Length in 0 .. Max_URI_Part_Length and then
         Scheme'Length + Host'Length +
         Port'Image'Length + Path'Length +
         Query'Length <= CoAP_SPARK.Max_URI_Length,
      Post => Create'Result.Length =
       Scheme'Length + Scheme_Suffix'Length + Host'Length +
       Port'Image'Length + Path'Length + Query'Length and then
       Is_Valid (Create'Result);

   function Create (URI_String : String) return URI
     with
      Pre => URI_String'Length <= CoAP_SPARK.Max_URI_Length,
      Post => Create'Result.Length = URI_String'Length;

   function Scheme (Self : URI) return String;

   function Host (Self : URI) return String
     with Pre => Is_Valid (Self);

   function Port (Self : URI) return Interfaces.Unsigned_16
     with Pre => Is_Valid (Self);

   function Path (Self : URI) return String
     with Pre => Is_Valid (Self);

   function Query (Self : URI) return String
     with Pre => Is_Valid (Self);

private

   type URI (Length : URI_Length) is record
      URI_String  : String (1 .. Length);
      Scheme_Last : URI_Length;
      Host_Last   : URI_Length;
      Port_Last   : URI_Length;
      Path_Last   : URI_Length;
   end record;

   function Is_Valid (Self : URI) return Boolean
     is (Self.Scheme_Last < Self.Host_Last
         and then Is_Valid_Scheme (Self.URI_String (1 .. Self.Scheme_Last))
         and then Self.Host_Last <= Self.Port_Last
         and then Self.Port_Last <= Self.Path_Last
         and then Self.Path_Last <= Self.URI_String'Last);

end CoAP_SPARK.URI;