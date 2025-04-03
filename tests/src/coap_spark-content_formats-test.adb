with AUnit.Assertions; use AUnit.Assertions;

package body CoAP_SPARK.Content_Formats.Test is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Content Formats");
   end Name;

   overriding
   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      for Content_Format
        in CoAP_SPARK.Content_Formats.text.plain_charset_utf_8
           .. CoAP_SPARK.Content_Formats.image.svg_Plus_xml
      loop
         declare
            MIME_Type : constant String :=
              CoAP_SPARK.Content_Formats.To_String
                (Interfaces.Unsigned_32 (Content_Format));
         begin
            Assert
              ((for all E of MIME_Type => E in 'a' .. 'z' |  '0' .. '9' |
               '/' | '-' | ';' | ' ' | '=' | '"' | '+' | '.'),
               Message  => "Incorrect String format: " & MIME_Type);
         end;
      end loop;
   end Run_Test;

end CoAP_SPARK.Content_Formats.Test;
