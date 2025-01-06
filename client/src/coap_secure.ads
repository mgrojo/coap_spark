
with Interfaces.C.Strings;

with WolfSSL;

package CoAP_Secure
   with SPARK_Mode
is

   function PSK_Client_Callback
     (Unused         : WolfSSL.WolfSSL_Type;
      Hint           : Interfaces.C.Strings.chars_ptr;
      Identity       : Interfaces.C.Strings.chars_ptr;
      Id_Max_Length  : Interfaces.C.unsigned;
      Key            : Interfaces.C.Strings.chars_ptr;
      Key_Max_Length : Interfaces.C.unsigned) return Interfaces.C.unsigned
   with Convention => C;

end CoAP_Secure;