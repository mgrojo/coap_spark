with Ada.Strings.Fixed;
with RFLX.RFLX_Arithmetic;
with RFLX.RFLX_Types.Operations;

package body CoAP_SPARK.Options with
  SPARK_Mode
is

   use type RFLX.RFLX_Builtin_Types.Index;

   procedure New_String_Option
     (Number : RFLX.CoAP.Option_Numbers; Value : String; Result : out Option)
   is
   begin

      Result :=
        (Number => Number,
         Value  =>
           new RFLX.RFLX_Types.Bytes'
             (RFLX.RFLX_Builtin_Types.Index'Base (Value'First) ..
                  RFLX.RFLX_Builtin_Types.Index'Base (Value'Last) =>
                0));

      for I in Result.Value.all'Range loop

         RFLX.RFLX_Types.Operations.Insert
           (Val    =>
              RFLX.RFLX_Types.Base_Integer
                (Character'Pos (Value (Natural (I)))),
            Buffer => Result.Value.all, First => I, Last => I, Off => 0,
            Size   => Character'Size, BO => RFLX.RFLX_Types.High_Order_First);
      end loop;

   end New_String_Option;

   procedure New_UInt_Option
     (Number : RFLX.CoAP.Option_Numbers;
      Value  : Interfaces.Unsigned_32;
      Result : out Option)
   is
      use type Interfaces.Unsigned_32;

      subtype Possible_Sizes_In_Bytes is
        RFLX.RFLX_Builtin_Types.Index range 1 ..
            Interfaces.Unsigned_32'Max_Size_In_Storage_Elements;
      subtype Bytes_Subtype is RFLX.RFLX_Types.Bytes (Possible_Sizes_In_Bytes);
      Bytes_Value   : Bytes_Subtype                 := (others => 0);
      Size_In_Bytes : RFLX.RFLX_Builtin_Types.Index := 1;
   begin

      -- Quote from RFC7252:
      --  An option definition may specify a range of permissible
      --  numbers of bytes; if it has a choice, a sender SHOULD
      --  represent the integer with as few bytes as possible, i.e.,
      --  without leading zero bytes. For example, the number 0 is
      --  represented with an empty option value (a zero-length
      --  sequence of bytes) and the number 1 by a single byte with
      --  the numerical value of 1 (bit combination 00000001 in most
      --  significant bit first notation).

      if Value /= 0 then

         for I in Possible_Sizes_In_Bytes loop

            if I = Possible_Sizes_In_Bytes'Last
              or else Value < 2**(Natural (I) * 8)
            then
               Size_In_Bytes := I;
               exit;
            end if;
         end loop;

         RFLX.RFLX_Types.Operations.Insert
           (Val   => RFLX.RFLX_Arithmetic.U64 (Value), Buffer => Bytes_Value,
            First => Bytes_Value'First, Last => Size_In_Bytes, Off => 0,
            Size  => Positive (Size_In_Bytes) * 8,
            BO    => RFLX.RFLX_Types.High_Order_First);
      end if;

      Result :=
        (Number => Number,
         Value  =>
           new RFLX.RFLX_Types.Bytes'
             (Bytes_Value (Bytes_Value'First .. Size_In_Bytes)));

   end New_UInt_Option;

   procedure New_Opaque_Option
     (Number : RFLX.CoAP.Option_Numbers; Value : RFLX.RFLX_Types.Bytes;
      Result : out Option)
   is
   begin
      Result :=
        (Number => Number,
         Value  => new RFLX.RFLX_Types.Bytes'(Value));
   end New_Opaque_Option;

   procedure New_Empty_Option
     (Number : RFLX.CoAP.Option_Numbers; Result : out Option)
   is
   begin
      Result := (Number => Number,
                 Value  => new RFLX.RFLX_Types.Bytes'(1 .. 0 => 0));
   end New_Empty_Option;

   procedure Take_Buffer (Opt : in out Option;
                          Value : out RFLX.RFLX_Types.Bytes_Ptr)
   is
   begin
      Value := Opt.Value;
      Opt.Value := null;
   end Take_Buffer;

   procedure Free (Opt : in out Option)
   is
   begin
      RFLX.RFLX_Types.Free (Opt.Value);
   end Free;

   procedure Copy (Source : Option; Target : out Option)
   is
   begin
      Target :=
        (Number => Source.Number,
         Value  => new RFLX.RFLX_Types.Bytes'(Source.Value.all));
   end Copy;


   function Image
     (Format : Option_Format; Value : RFLX.RFLX_Types.Bytes) return String
   is
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
            return To_UInt (Value)'Image;

         when Opaque | Unknown =>
            declare
               Result :
                 String (1 .. Value'Length * RFLX.RFLX_Types.Byte'Width) :=
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
   function To_UInt (Value : UInt_Bytes) return Interfaces.Unsigned_32 is
      use type Interfaces.Unsigned_32;
      Result : Interfaces.Unsigned_32 := 0;
   begin
      for I in Value'Range loop
         Result :=
           Result * 2**RFLX.RFLX_Types.Byte'Size +
           Interfaces.Unsigned_32 (Value (I));
      end loop;
      return Result;
   end To_UInt;

end CoAP_SPARK.Options;