with AUnit.Assertions; use AUnit.Assertions;

package body CoAP_SPARK.Log.Test is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test CoAP Log");
   end Name;

   overriding
   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin

      Set_Level (New_Level => Fatal);

      Put_Line ("OK. Should be seen.", Fatal);
      Put ("Current Level: ", Fatal);
      Put (Fatal'Image, Fatal);
      New_Line (Fatal);

      for Level in reverse Debug .. Error loop

         Put_Line ("NO OK. Should not be seen.", Level);
         Put ("Level: ", Level);
         Put (Level'Image, Level);
         New_Line (Level);

         Set_Level (New_Level => Level);

         Put_Line ("OK. Should be seen.", Level);
         Put ("Current Level: ", Level);
         Put (Level'Image, Level);
         New_Line (Level);

      end loop;

      Assert
        (True,
         Message => "Nothing to assert. Just check the output.");
         -- The output should not contain "NO OK. Should not be seen."

   end Run_Test;

end CoAP_SPARK.Log.Test;
