with AUnit.Assertions; use AUnit.Assertions;

package body CoAP_SPARK.URI.Test is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test CoAP URI parsing");
   end Name;

   overriding
   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      Valid_URI   : constant URI :=
        CoAP_SPARK.URI.Create ("coap://valid.url/t?foo=1&bar=a");
      Invalid_URI : constant URI := CoAP_SPARK.URI.Create ("copa:/bad:port//");
      HTTP_URI    : constant URI :=
        CoAP_SPARK.URI.Create ("http://example.org/");
   begin

      Assert
        (CoAP_SPARK.URI.Is_Valid (Valid_URI),
         Message => "Valid URI is invalid");

      Assert
        (not CoAP_SPARK.URI.Is_Valid (Invalid_URI),
         Message => "Invalid URI is valid");

      Assert
        (not CoAP_SPARK.URI.Is_Valid (HTTP_URI),
         Message => "URI with invalid scheme is valid");

   end Run_Test;

   overriding
   function Name (T : Compose_Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test CoAP URI composition");
   end Name;

   overriding
   procedure Run_Test (T : in out Compose_Test) is
      pragma Unreferenced (T);
      Valid_URI : constant URI :=
        CoAP_SPARK.URI.Create
          (Scheme => "coap",
           Host   => "valid.url",
           Path   => "t",
           Query  => "foo=1&bar=a");
   begin

      Assert
        (CoAP_SPARK.URI.Is_Valid (Valid_URI),
         Message => "Valid URI is invalid");

   end Run_Test;

end CoAP_SPARK.URI.Test;
