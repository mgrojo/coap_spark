package body CoAP_SPARK.Log with
   SPARK_Mode,
   Refined_State => (Active_Level => Current_Level)
is

   Current_Level : Level_Type := Info;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (New_Level : Level_Type) is
   begin
      Current_Level := New_Level;
   end Set_Level;

   procedure Put_Error_Line (Item : String);
   procedure Put_Error (Item : String);
   procedure New_Error_Line;

   -- "Standard_Error" is not allowed in SPARK, so we wrap it in procedures
   -- whose implementations are not checked by SPARK.
   procedure Put_Error_Line (Item : String)
      with SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Item);
   end Put_Error_Line;

   procedure Put_Error (Item : String)
      with SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Item);
   end Put_Error;

   procedure New_Error_Line
      with SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end New_Error_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Item : String; Level : Level_Type := Debug) is
   begin
      if Level >= Current_Level then
         if Level in Problem_Type then
            Put_Error (Item);
         else
            Ada.Text_IO.Put (Item);
         end if;
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : String; Level : Level_Type := Debug) is
   begin
      if Level >= Current_Level then
         if Level in Problem_Type then
            Put_Error_Line (Item);
         else
            Ada.Text_IO.Put_Line (Item);
         end if;
      end if;
   end Put_Line;

   procedure New_Line
     (Level : Level_Type := Debug) is
   begin
      if Level >= Current_Level then
         if Level in Problem_Type then
            New_Error_Line;
         else
            Ada.Text_IO.New_Line;
         end if;
      end if;
   end New_Line;

end CoAP_SPARK.Log;
