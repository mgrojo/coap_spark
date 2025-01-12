with Ada.Text_IO;

-- A simple logging package that allows to output messages of different levels
-- to the standard output or the standard error.
package CoAP_SPARK.Log with
  SPARK_Mode,
  Abstract_State    => Active_Level,
  Initializes       => Active_Level,
  Always_Terminates
is

   type Level_Type is (Debug, Info, Warning, Error, Fatal);

   subtype Problem_Type is Level_Type range Warning .. Fatal;

   -- Set the level of messages to be output.
   -- Any message with a level greater than or equal to the level set will be
   -- output.
   procedure Set_Level (New_Level : Level_Type) with
     Global => (Output => Active_Level);

   -- Output a message to the console if the level is active.
   -- The message is not terminated with a newline.
   -- The messages goes to the standard error when it is a Problem_Type,
   -- otherwise it goes to the standard output.
   procedure Put (Item : String; Level : Level_Type := Debug) with
      Global => (Input => Active_Level,
                 In_Out => Ada.Text_IO.File_System);

   -- Output a message to the console if the level is active.
   -- The message is terminated with a newline.
   -- The messages goes to the standard error when it is a Problem_Type,
   -- otherwise it goes to the standard output.
   procedure Put_Line
     (Item : String; Level : Level_Type := Debug) with
      Global => (Input => Active_Level,
                 In_Out => Ada.Text_IO.File_System);

   -- Output a newline with the same conditions as Put_Line.
   procedure New_Line
     (Level : Level_Type := Debug) with
      Global => (Input => Active_Level,
                 In_Out => Ada.Text_IO.File_System);

end CoAP_SPARK.Log;