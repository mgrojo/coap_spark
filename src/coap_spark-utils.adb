with Ada.Strings.Fixed;
with RFLX.RFLX_Types.Operations;

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

   procedure Copy_String
     (Source : String; Target : out RFLX.RFLX_Types.Bytes)
   is
   begin
      for I in Target'Range loop

         RFLX.RFLX_Types.Operations.Insert
           (Val    =>
              RFLX.RFLX_Types.Base_Integer
                (Character'Pos (Source (Natural (I)))),
            Buffer => Target, First => I, Last => I, Off => 0,
            Size   => Character'Size, BO => RFLX.RFLX_Types.High_Order_First);
      end loop;
   end Copy_String;

end CoAP_SPARK.Utils;
