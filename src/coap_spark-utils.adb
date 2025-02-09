with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with RFLX.RFLX_Builtin_Types;

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

   procedure Copy_String (Source : String; Target : out RFLX.RFLX_Types.Bytes)
   is
      use type RFLX.RFLX_Builtin_Types.Index;
   begin
      for I in Target'Range loop
         Target (I) :=
           RFLX.RFLX_Types.Byte
             (Character'Pos (Source (Natural (I - Target'First) + Source'First)));
      end loop;
   end Copy_String;

   function Count (Source : String; Char : Character) return Natural
   is (Ada.Strings.Fixed.Count
         (Source => Source, Set => Ada.Strings.Maps.To_Set (Char)))
         with SPARK_Mode => Off;

end CoAP_SPARK.Utils;
