with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;

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

   function Value (Method : String) return RFLX.CoAP.Method_Code is
      (RFLX.CoAP.Method_Code'Value (Method)) with SPARK_Mode => Off;

   function Text_As_Bytes (Text : String) return RFLX.RFLX_Types.Bytes
   is
      use type RFLX.RFLX_Types.Index;
      function To_Byte
        is new Ada.Unchecked_Conversion (Character, RFLX.RFLX_Types.Byte);
      Data : RFLX.RFLX_Types.Bytes (1 .. Text'Length);
   begin
      for I in Data'Range loop
         Data (I) :=
           To_Byte (Text (Text'First - 1 + Integer (I)));
      end loop;
      return Data;
   end Text_As_Bytes;

end CoAP_SPARK.Utils;
