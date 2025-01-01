with Ada.Text_IO;

package body CoAP_SPARK.Options.Text_IO
   with SPARK_Mode
is

   -----------
   -- Print --
   -----------

   procedure Print (Item : CoAP_SPARK.Options.Option)
   is
   begin
      Ada.Text_IO.Put ("Option: ");
      Ada.Text_IO.Put_Line (CoAP_SPARK.Options.Get_Number (Item)'Image);
      if CoAP_SPARK.Options.Has_Buffer (Item) then
         Ada.Text_IO.Put ("  - Length: ");
         Ada.Text_IO.Put_Line (CoAP_SPARK.Options.Get_Length (Item)'Image);
         Ada.Text_IO.Put ("  - Value: ");
         Ada.Text_IO.Put_Line
           (CoAP_SPARK.Options.Value_Image (Item));
      else
         Ada.Text_IO.Put_Line ("  - Length: 0");
         Ada.Text_IO.Put_Line ("  - Value: (empty)");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Item : CoAP_SPARK.Options.Lists.Vector) is
   begin
      for Option of Item loop
         Print (Option);
      end loop;
   end Print;

end CoAP_SPARK.Options.Text_IO;
