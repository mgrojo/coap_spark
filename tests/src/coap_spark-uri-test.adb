with AUnit.Assertions; use AUnit.Assertions;

package body CoAP_SPARK.URI.Test is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test CoAP URI package");
   end Name;

   overriding
   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      Valid_URI   : constant URI := Create ("coap://valid.url/t?foo=1&bar=a");
      Invalid_URI : constant URI := Create ("copa:/bad:port//");
      HTTP_URI    : constant URI := Create ("http://example.org/");
   begin

      Assert
        (Is_Valid (Valid_URI),
         Message => "Valid URI is invalid");

      Assert
        (not Is_Valid (Invalid_URI),
         Message => "Invalid URI is valid");

      Assert
        (not Is_Valid (HTTP_URI),
         Message => "URI with invalid scheme is valid");

   end Run_Test;

end CoAP_SPARK.URI.Test;
