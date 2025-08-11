with SPARK_Terminal;
with CoAP_SPARK.Log;

package body Secure_Server
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


   function PSK_Server_Callback
     (Unused         : WolfSSL.WolfSSL_Type;
      Identity       : Interfaces.C.Strings.chars_ptr;
      Key            : Interfaces.C.Strings.chars_ptr;
      Key_Max_Length : Interfaces.C.unsigned) return Interfaces.C.unsigned
   is
      Identity_Index, Key_Index : Natural := 0;
   begin

      for I in 1 .. SPARK_Terminal.Argument_Count - 1 loop

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

         if Interfaces.C.Strings.Value
            (Item   => Identity,
            Length => Identity_String'Length) /= Identity_String 
         then
            CoAP_SPARK.Log.Put_Line
              ("Identity not known", CoAP_SPARK.Log.Debug);
            return 0;
         end if;

         if Key_Max_Length < Key_String'Length then
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
   end PSK_Server_Callback;

   procedure Initialize
     (Socket : out CoAP_SPARK.Channel.Socket_Type;
      Port   : CoAP_SPARK.Channel.Port_Type)
   with
     SPARK_Mode => Off
     -- Access to function with side effects is not allowed in SPARK
   is
   begin
      CoAP_SPARK.Channel.Initialize
        (Socket              => Socket,
         PSK_Server_Callback => Secure_Server.PSK_Server_Callback'Access,
         Port                => Port,
         Server              => True);
   end Initialize;
   
end Secure_Server;