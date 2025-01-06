with Ada.Text_IO;

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

      Hint_String     : constant String := Interfaces.C.Strings.Value (Hint);
      -- TODO Pass through the command line.
      Identity_String : constant String := "Client_identity";
      Key_String      : constant String := "1234abcd567890";
   begin

      Ada.Text_IO.Put_Line ("Hint: " & Hint_String);

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
   end PSK_Client_Callback;

end CoAP_Secure;