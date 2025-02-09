with Ada.Strings.Equal_Case_Insensitive;
with RFLX.CoAP;
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

   generic
      -- For positive integers and modular types (no negative nor enumeration).
      type Numeric_Type is (<>);

   package Valid_Values
   is

      function Is_Valid_As_Number (Number : String) return Boolean
      is (Number'Length <= Numeric_Type'Width
          and then (for all C of Number => C in '0' .. '9'));

      -- Wrapper around 'Value attribute, which cannot be proved by SPARK.
      function Value (Number : String) return Numeric_Type
      with Pre => Is_Valid_As_Number (Number);

      function Value (Number : String) return Numeric_Type
      is (Numeric_Type'Value (Number))
      with SPARK_Mode => Off;

   end Valid_Values;

   package Valid_Unsigned_16_Values is new
     Valid_Values (Interfaces.Unsigned_16);
   package Valid_Natural_Values is new Valid_Values (Natural);

   function Value (Number : String) return Interfaces.Unsigned_16
   renames Valid_Unsigned_16_Values.Value;
   function Value (Number : String) return Natural
   renames Valid_Natural_Values.Value;

   function Is_Equal (Source : String; Target : String) return Boolean
     renames Ada.Strings.Equal_Case_Insensitive;

   -- Returns True if Method is a valid CoAP method.
   function Is_Valid_As_Method (Method : String) return Boolean
   is (Is_Equal (Method, "GET") or else
       Is_Equal (Method, "POST") or else
       Is_Equal (Method, "PUT") or else
       Is_Equal (Method, "DELETE") or else
       Is_Equal (Method, "FETCH") or else
       Is_Equal (Method, "PATCH") or else
       Is_Equal (Method, "IPATCH"));

   -- Wrapper around 'Value attribute, which cannot be proved by SPARK.
   function Value (Method : String) return RFLX.CoAP.Method_Code
      with Pre => Is_Valid_As_Method (Method);

   -- Returns the number of occurrences of Char in Source.
   -- This wraps Ada.Strings.Fixed.Count, which doesn't provide a Postcondition,
   -- but by definition, the result cannot be greater than the length of Source.
   function Count (Source : String; Char : Character) return Natural
   with Post => Count'Result in 0 .. Source'Length, Global => null;

end CoAP_SPARK.Utils;