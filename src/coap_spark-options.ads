with Interfaces;
with RFLX.CoAP;
with RFLX.RFLX_Types;

package CoAP_SPARK.Options
  with SPARK_Mode
is

   -- uint value as bytes.
   -- According to Section 5.10. Option Definitions "Table 4: Options"
   -- the maximum size for uint options is 4 bytes.
   subtype UInt_Bytes is RFLX.RFLX_Types.Bytes (1 .. 4);

   type Option_Format is (Unknown, Empty, Opaque, UInt, UTF8_String);

   type Option_Properties is record
      Format         : Option_Format;
      Repeatable     : Boolean;
      Maximum_Length : Natural;
   end record;

   type Option_Table is array (RFLX.CoAP.Option_Numbers) of Option_Properties;

   Max_Option_Value_Length : constant := 1034;

   -- See RFC7252, Section 5.10. Option Definitions "Table 4: Options"
   Option_Properties_Table : constant Option_Table :=
     (RFLX.CoAP.If_Match       =>
        (Format => Opaque, Repeatable => True, Maximum_Length => 8),
      RFLX.CoAP.Uri_Host       =>
        (Format => UTF8_String, Repeatable => False, Maximum_Length => 255),
      RFLX.CoAP.ETag           =>
        (Format => Opaque, Repeatable => True, Maximum_Length => 8),
      RFLX.CoAP.If_None_Match  =>
        (Format => Empty, Repeatable => False, Maximum_Length => 0),
      RFLX.CoAP.Uri_Port       =>
        (Format => UInt, Repeatable => False, Maximum_Length => 2),
      RFLX.CoAP.Location_Path  =>
        (Format => UTF8_String, Repeatable => True, Maximum_Length => 255),
      RFLX.CoAP.Uri_Path       =>
        (Format => UTF8_String, Repeatable => True, Maximum_Length => 255),
      RFLX.CoAP.Content_Format =>
        (Format => UInt, Repeatable => False, Maximum_Length => 2),
      RFLX.CoAP.Max_Age        =>
        (Format => UInt, Repeatable => False, Maximum_Length => 4),
      RFLX.CoAP.Uri_Query      =>
        (Format => UTF8_String, Repeatable => True, Maximum_Length => 255),
      RFLX.CoAP.Accept_17      =>
        (Format => UInt, Repeatable => True, Maximum_Length => 2),
      RFLX.CoAP.Location_Query =>
        (Format => UTF8_String, Repeatable => True, Maximum_Length => 255),
      RFLX.CoAP.Proxy_Uri      =>
        (Format         => UTF8_String,
         Repeatable     => False,
         Maximum_Length => Max_Option_Value_Length),
      RFLX.CoAP.Proxy_Scheme   =>
        (Format => UTF8_String, Repeatable => False, Maximum_Length => 255),
      RFLX.CoAP.Size1          =>
        (Format => UInt, Repeatable => False, Maximum_Length => 4),
      others                   =>
        (Format         => Unknown,
         Repeatable     => True,
         Maximum_Length => Natural'Last));

   function Image
     (Format : Option_Format; Value : RFLX.RFLX_Types.Bytes) return String
   with Pre => Value'Length in 1 .. Max_Option_Value_Length;

   function To_UInt (Value : UInt_Bytes) return Interfaces.Unsigned_32;

end CoAP_SPARK.Options;