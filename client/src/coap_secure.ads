
with CoAP_SPARK.Channel;

with Interfaces.C.Strings;

with WolfSSL;

-- This package is used to define the PSK callback function for the WolfSSL
-- library. The body cannot be in SPARK mode because it uses the C callback.
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
   with Convention => C,
      Side_Effects;

   procedure Initialize (Socket : out CoAP_SPARK.Channel.Socket_Type) with
      Global =>
         null;

end CoAP_Secure;