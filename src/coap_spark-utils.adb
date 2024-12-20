with Ada.Strings.Fixed;

package body CoAP_SPARK.Utils
  with SPARK_Mode
is

   function Padded_Image
     (Source : Integer; Count : Natural; Pad : Character := '0') return String
   is
   begin
      return
        Ada.Strings.Fixed.Tail
          (Source =>
             Ada.Strings.Fixed.Trim
               (Source => Integer'Image (Source),
                Side   => Ada.Strings.Left),
           Count  => Count,
           Pad    => Pad);
   end Padded_Image;

end CoAP_SPARK.Utils;
