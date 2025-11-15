with Ada.Strings.Fixed;

with AUnit.Assertions; use AUnit.Assertions;

package body CoAP_SPARK.Content_Formats.Test is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Content Formats");
   end Name;

   function Contains (Source : String; Pattern : String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Source, Pattern) > 0;
   end Contains;

   overriding
   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      for Content_Format in
        CoAP_SPARK.Content_Formats.text.plain_charset_utf_8
        .. CoAP_SPARK.Content_Formats.image.svg_Plus_xml
      loop
         declare
            MIME_Type : constant String :=
              CoAP_SPARK.Content_Formats.To_String (Content_Format);
            Is_Text : constant Boolean :=
               CoAP_SPARK.Content_Formats.Is_Text (Content_Format);
         begin
            Assert
              ((for all E of MIME_Type =>
                  E
                  in 'a' .. 'z'
                   | '0' .. '9'
                   | '/'
                   | '-'
                   | ';'
                   | ' '
                   | '='
                   | '"'
                   | '+'
                   | '.'),
               Message => "Incorrect String format: " & MIME_Type);
            Assert
              ((if Contains (MIME_Type, "text") or else
                  Contains (MIME_Type, "json") or else
                  Contains (MIME_Type, "xml") then Is_Text),
               Message => "Is_Text mismatch for: " & MIME_Type);
            Assert
              ((if Contains (MIME_Type, "cbor") then not Is_Text),
               Message => "not Is_Text mismatch for: " & MIME_Type);
         end;
      end loop;
   end Run_Test;

end CoAP_SPARK.Content_Formats.Test;

