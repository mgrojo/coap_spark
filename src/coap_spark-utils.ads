with RFLX.RFLX_Types;

package CoAP_SPARK.Utils
  with SPARK_Mode
is

   -- Returns a string representation of Source, padded with Pad to Count
   -- characters.
   function Padded_Image
     (Source : Integer; Count : Natural; Pad : Character := '0') return String
     with Post => Padded_Image'Result'Length = Count;

   procedure Copy_String
     (Source : String; Target : out RFLX.RFLX_Types.Bytes)
   with Pre => Target'Length = Source'Length;

end CoAP_SPARK.Utils;