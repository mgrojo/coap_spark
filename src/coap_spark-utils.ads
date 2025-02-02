with RFLX.RFLX_Types;
with Interfaces;

package CoAP_SPARK.Utils
  with SPARK_Mode
is

   -- Returns a string representation of Source, padded with Pad to Count
   -- characters.
   function Padded_Image
     (Source : Integer; Count : Natural; Pad : Character := '0') return String
   with
     Post =>
       Padded_Image'Result'First = 1
       and then Padded_Image'Result'Length = Count;

   procedure Copy_String (Source : String; Target : out RFLX.RFLX_Types.Bytes)
   with Pre => Target'Length = Source'Length;

   -- Wrapper around 'Value attribute, which cannot be proved by SPARK.
   function Value (Number : String) return Interfaces.Unsigned_16
   with
     Pre =>
       Number'Length <= Interfaces.Unsigned_16'Width
       and then (for all C of Number => C in '0' .. '9');

   -- Returns the number of occurrences of Char in Source.
   -- This wraps Ada.Strings.Fixed.Count, which doesn't provide a Postcondition,
   -- but by definition, the result cannot be greater than the length of Source.
   function Count (Source : String; Char : Character) return Natural
   with
      Post   => Count'Result in 0 .. Source'Length,
      Global => null;

end CoAP_SPARK.Utils;