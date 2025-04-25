with SPARK_Terminal;
with CoAP_SPARK.Log;

package body CoAP_Secure
   with SPARK_Mode => On
is

   -- We have to wrap the Update procedure because it has a contract
   -- that cannot be satisfied in SPARK when the buffer in Item has enough
   -- space to hold the string but Strlen will return a lower length value
   -- because the buffer is filled with null characters.
   -- We add the buffer length as a parameter to the Update procedure in order
   -- to check that the string fits in the buffer. We have to trust the C side
   -- to pass a correct value for Item_Length, though.
   procedure Update
     (Item        : Interfaces.C.Strings.chars_ptr;
      Item_Length : Interfaces.C.unsigned;
      Str         : String)
   with
     Pre    =>
       Item /= Interfaces.C.Strings.Null_Ptr
         and then Str'Length <= Interfaces.C.unsigned'Last
         and then Str'Length <= Item_Length,
     Global => (In_Out => Interfaces.C.Strings.C_Memory);

   procedure Update
     (Item        : Interfaces.C.Strings.chars_ptr;
      Item_Length : Interfaces.C.unsigned;
      Str         : String)
   with SPARK_Mode => Off
   is
      pragma Unreferenced (Item_Length);
   begin
      Interfaces.C.Strings.Update
        (Item => Item, Offset => 0, Str => Str, Check => False);
   end Update;

   function PSK_Client_Callback
     (Unused         : WolfSSL.WolfSSL_Type;
      Hint           : Interfaces.C.Strings.chars_ptr;
      Identity       : Interfaces.C.Strings.chars_ptr;
      Id_Max_Length  : Interfaces.C.unsigned;
      Key            : Interfaces.C.Strings.chars_ptr;
      Key_Max_Length : Interfaces.C.unsigned) return Interfaces.C.unsigned
   is
      Hint_String : constant String := Interfaces.C.Strings.Value (Hint);
 
      Identity_Index, Key_Index : Natural := 0;
   begin

      CoAP_SPARK.Log.Put ("Hint: ");
      CoAP_SPARK.Log.Put_Line (Hint_String);

      for I in 1 .. SPARK_Terminal.Argument_Count - 2 loop

         if SPARK_Terminal.Argument (I) = "-u" then
            Identity_Index := I + 1;
         elsif SPARK_Terminal.Argument (I) = "-k" then
            Key_Index := I + 1;
         end if;
         pragma Loop_Invariant (Identity_Index <= SPARK_Terminal.Argument_Count);
         pragma Loop_Invariant (Key_Index <= SPARK_Terminal.Argument_Count);
      end loop;

      if Identity_Index = 0 or Key_Index = 0 then
         -- Missing arguments for key or identity, nothing is returned
         return 0;
      end if;

      declare
         Identity_String : constant String :=
           SPARK_Terminal.Argument (Identity_Index);
         Key_String      : constant String :=
           SPARK_Terminal.Argument (Key_Index);
      begin

         if Identity_String'Length > Id_Max_Length then
            CoAP_SPARK.Log.Put_Line
              ("Identity too long", CoAP_SPARK.Log.Error);
            return 0;
         end if;

         Update
           (Item        => Identity,
            Item_Length => Id_Max_Length,
            Str         => Identity_String);

         if Key_String'Length > Key_Max_Length then
            CoAP_SPARK.Log.Put_Line
              ("Key too long", CoAP_SPARK.Log.Error);
            return 0;
         end if;

         Update
           (Item        => Key,
            Item_Length => Key_Max_Length,
            Str         => Key_String);

         return Key_String'Length;
      end;
   end PSK_Client_Callback;

   procedure Initialize (Socket : out CoAP_SPARK.Channel.Socket_Type)
   with SPARK_Mode => Off
   -- Access to function with side effects is not allowed in SPARK
   is
   begin
      CoAP_SPARK.Channel.Initialize
        (Socket       => Socket,
         PSK_Callback => CoAP_Secure.PSK_Client_Callback'Access);
   end Initialize;
   
end CoAP_Secure;