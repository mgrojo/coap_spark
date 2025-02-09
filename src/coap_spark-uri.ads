with Interfaces;

package CoAP_SPARK.URI
   with SPARK_Mode
is

   subtype URI_Length is Natural range 0 .. CoAP_SPARK.Max_URI_Length;

   type URI (Length : URI_Length) is private;

   Scheme_Suffix : constant String := "://";
   Host_Suffix   : constant String := ":";
   Query_Prefix  : constant String := "?";

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
         Scheme'First = 1 and then
         Host'Length in 1 .. Max_URI_Part_Length and then
         Path'Length in 1 .. Max_URI_Part_Length and then
         Query'Length in 0 .. Max_URI_Part_Length and then
         Scheme'Length + Scheme_Suffix'Length + Host'Length + Host_Suffix'Length +
         Interfaces.Unsigned_16'Width + Path'Length + Query_Prefix'Length +
         Query'Length <= CoAP_SPARK.Max_URI_Length,
      Post => Create'Result.Length <=
       Scheme'Length + Scheme_Suffix'Length + Host'Length + Host_Suffix'Length +
         Interfaces.Unsigned_16'Width + Path'Length + Query_Prefix'Length +
         Query'Length and then
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

   function Has_Valid_Lengths (Self : URI) return Boolean
     is (Host (Self)'Length <= Max_URI_Part_Length and then
         Path (Self)'Length <= Max_URI_Part_Length and then
         Query (Self)'Length <= Max_URI_Part_Length)
     with Pre => Is_Valid (Self);

private

   type URI (Length : URI_Length) is record
      URI_String  : String (1 .. Length);
      Valid       : Boolean := False;
      Scheme_Last : URI_Length;
      Host_Last   : URI_Length;
      Port_Last   : URI_Length;
      Path_Last   : URI_Length;
   end record;

   function Is_Valid (Self : URI) return Boolean
   is (Self.Valid);

end CoAP_SPARK.URI;