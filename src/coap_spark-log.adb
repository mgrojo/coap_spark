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

   ---------
   -- Put --
   ---------

   procedure Put (Item : String; Level : Level_Type := Debug) is
   begin
      if Level >= Current_Level then
         if Level in Problem_Type then
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Item);
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
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Item);
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
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         else
            Ada.Text_IO.New_Line;
         end if;
      end if;
   end New_Line;

end CoAP_SPARK.Log;
