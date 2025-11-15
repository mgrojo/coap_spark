
with CoAP_SPARK.Channel;

with Interfaces.C.Strings;

with WolfSSL;

-- This package is used to define the PSK callback function for the WolfSSL
-- library. The body cannot be in SPARK mode because it uses the C callback.
package Secure_Server
   with SPARK_Mode
is

   use type Interfaces.C.unsigned;
   use type Interfaces.C.Strings.chars_ptr;

   function PSK_Server_Callback
     (Unused         : WolfSSL.WolfSSL_Type;
      Identity       : Interfaces.C.Strings.chars_ptr;
      Key            : Interfaces.C.Strings.chars_ptr;
      Key_Max_Length : Interfaces.C.unsigned) return Interfaces.C.unsigned
   with
     Pre => 
        Identity /= Interfaces.C.Strings.Null_Ptr and then
        Key /= Interfaces.C.Strings.Null_Ptr,
     Convention => C,
     Side_Effects;

   procedure Initialize
     (Socket : out CoAP_SPARK.Channel.Socket_Type;
      Port   : CoAP_SPARK.Channel.Port_Type)
   with Global => null;

end Secure_Server;