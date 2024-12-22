
with Ada.Strings.Fixed;

package body CoAP_SPARK.Options
   with SPARK_Mode
is

   function Image
     (Format : Option_Format; Value : RFLX.RFLX_Types.Bytes)
      return String
   is
      use type RFLX.RFLX_Types.Index;
   begin

      if Value'Length = 0 then
         return "";
      end if;

      case Format is
         when Empty =>
            return "";
         when UTF8_String =>
            declare
               Result :
                 String (Integer (Value'First) .. Integer (Value'Last)) :=
                   (others => ' ');
            begin
               for I in Value'Range loop
                  Result (Positive (I)) := Character'Val (Value (I));
               end loop;
               return Result;
            end;

         when UInt =>
            return Interfaces.Unsigned_32'Image (To_UInt (Value));

         when Opaque | Unknown =>
            declare
               Result : String (1 ..
                                Value'Length * RFLX.RFLX_Types.Byte'Width) :=
                 (others => ' ');
               Index  : Positive := Result'First;
            begin
               for I in Value'Range loop
                  Result (Index .. Index + RFLX.RFLX_Types.Byte'Width - 1) :=
                    Ada.Strings.Fixed.Tail
                      (Source => Value (I)'Image,
                       Count  => RFLX.RFLX_Types.Byte'Width);
                  if I /= Value'Last then
                     Index := Index + RFLX.RFLX_Types.Byte'Width;
                  end if;
               end loop;
               return Result;
            end;
      end case;

   end Image;

   -- See RFC7252, Section "3.2. Option Value Formats"
   function To_UInt (Value : UInt_Bytes) return Interfaces.Unsigned_32
   is
      use type Interfaces.Unsigned_32;
      Result : Interfaces.Unsigned_32 := 0;
   begin
      for I in Value'Range loop
         Result :=
           Result * 2 ** RFLX.RFLX_Types.Byte'Size
           + Interfaces.Unsigned_32 (Value (I));
      end loop;
      return Result;
   end To_UInt;

end CoAP_SPARK.Options;