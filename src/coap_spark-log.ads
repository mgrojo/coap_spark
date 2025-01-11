with Ada.Text_IO;

package CoAP_SPARK.Log with
  SPARK_Mode,
  Abstract_State    => Active_Level,
  Initializes       => Active_Level,
  Always_Terminates
is

   type Level_Type is (Debug, Info, Warning, Error, Fatal);

   procedure Set_Level (New_Level : Level_Type) with
     Global => (Output => Active_Level);

   procedure Put (Item : String; Level : Level_Type := Debug) with
      Global => (Input => Active_Level,
                 In_Out => Ada.Text_IO.File_System);

   procedure Put_Line
     (Item : String; Level : Level_Type := Debug) with
      Global => (Input => Active_Level,
                 In_Out => Ada.Text_IO.File_System);

   procedure New_Line
     (Level : Level_Type := Debug) with
      Global => (Input => Active_Level,
                 In_Out => Ada.Text_IO.File_System);

end CoAP_SPARK.Log;