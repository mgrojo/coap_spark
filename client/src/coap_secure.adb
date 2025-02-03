with Ada.Command_Line;
with CoAP_SPARK.Log;

package body CoAP_Secure
   with SPARK_Mode => Off
is

   function PSK_Client_Callback
     (Unused         : WolfSSL.WolfSSL_Type;
      Hint           : Interfaces.C.Strings.chars_ptr;
      Identity       : Interfaces.C.Strings.chars_ptr;
      Id_Max_Length  : Interfaces.C.unsigned;
      Key            : Interfaces.C.Strings.chars_ptr;
      Key_Max_Length : Interfaces.C.unsigned) return Interfaces.C.unsigned
   is
      use type Interfaces.C.unsigned;

      Hint_String : constant String := Interfaces.C.Strings.Value (Hint);
 
      Identity_Index, Key_Index : Natural := 0;
   begin

      CoAP_SPARK.Log.Put_Line ("Hint: " & Hint_String);

      for I in 1 .. Ada.Command_Line.Argument_Count - 2 loop

         if Ada.Command_Line.Argument (I) = "-u" then
            Identity_Index := I + 1;
         elsif Ada.Command_Line.Argument (I) = "-k" then
            Key_Index := I + 1;
         end if;
      end loop;

      if Identity_Index = 0 or Key_Index = 0 then
         -- Missing arguments for key or identity, nothing is returned
         return 0;
      end if;

      declare
         Identity_String : constant String :=
           Ada.Command_Line.Argument (Identity_Index);
         Key_String      : constant String :=
           Ada.Command_Line.Argument (Key_Index);
      begin

         pragma Assert (Id_Max_Length >= Identity_String'Length);

         Interfaces.C.Strings.Update
           (Item   => Identity,
            Offset => 0,
            Str    => Identity_String,
            Check  => False);

         pragma Assert (Key_Max_Length >= Key_String'Length);

         Interfaces.C.Strings.Update
           (Item => Key, Offset => 0, Str => Key_String, Check => False);

         return Key_String'Length;
      end;
   end PSK_Client_Callback;

   procedure Initialize (Socket : out CoAP_SPARK.Channel.Socket_Type)
   is
   begin
      CoAP_SPARK.Channel.Initialize
        (Socket       => Socket,
         PSK_Callback => CoAP_Secure.PSK_Client_Callback'Access);
   end Initialize;
   
end CoAP_Secure;