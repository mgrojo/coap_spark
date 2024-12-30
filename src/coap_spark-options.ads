with Interfaces;
with RFLX.CoAP;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;

package CoAP_SPARK.Options
  with SPARK_Mode
is

   use type RFLX.CoAP.Option_Numbers;
   use type RFLX.RFLX_Builtin_Types.Bytes_Ptr;

   -- According to Section 5.10. Option Definitions "Table 4: Options"
   -- the maximum size for uint options is 4 bytes.
   -- The other registered options by IANA are also not larger than 4 bytes.
   -- Maximum size in general is 1034 bytes.
   Max_Uint_Length : constant := 4;
   Max_Option_Value_Length : constant := Max_URI_Length;

   subtype UInt_Bytes is RFLX.RFLX_Types.Bytes
      with Dynamic_Predicate => UInt_Bytes'Length <= Max_Uint_Length;

   subtype Option_Value_Length is
     Natural range 0 .. CoAP_SPARK.Options.Max_Option_Value_Length;

   subtype Option_Index is Positive range 1 .. Max_Number_Of_Options;

   type Option_Format is (Unknown, Empty, Opaque, UInt, UTF8_String);

   type Option_Properties is record
      Format         : Option_Format;
      Repeatable     : Boolean;
      Maximum_Length : Option_Value_Length;
   end record;

   type Option_Table is array (RFLX.CoAP.Option_Numbers) of Option_Properties;

   -- See RFC7252, Section 5.10. Option Definitions "Table 4: Options"
   -- except otherwise noted by another reference.
   Option_Properties_Table : constant Option_Table :=
     [RFLX.CoAP.If_Match                          =>
        (Format => Opaque, Repeatable => True, Maximum_Length => 8),
      RFLX.CoAP.Uri_Host                          =>
        (Format => UTF8_String, Repeatable => False, Maximum_Length => 255),
      RFLX.CoAP.ETag                              =>
        (Format => Opaque, Repeatable => True, Maximum_Length => 8),
      RFLX.CoAP.If_None_Match                     =>
        (Format => Empty, Repeatable => False, Maximum_Length => 0),

      -- rfc7641:
      RFLX.CoAP.Observe                           =>
        (Format => UInt, Repeatable => False, Maximum_Length => 3),

      -- rfc8613:
      RFLX.CoAP.OSCORE                            =>
        (Format => Opaque, Repeatable => False, Maximum_Length => 255),

      RFLX.CoAP.Uri_Port                          =>
        (Format => UInt, Repeatable => False, Maximum_Length => 2),

      -- rfc8768:
      RFLX.CoAP.Hop_Limit                         =>
        (Format => UInt, Repeatable => False, Maximum_Length => 1),

      RFLX.CoAP.Location_Path                     =>
        (Format => UTF8_String, Repeatable => True, Maximum_Length => 255),
      RFLX.CoAP.Uri_Path                          =>
        (Format => UTF8_String, Repeatable => True, Maximum_Length => 255),
      RFLX.CoAP.Content_Format                    =>
        (Format => UInt, Repeatable => False, Maximum_Length => 2),
      RFLX.CoAP.Max_Age                           =>
        (Format => UInt, Repeatable => False, Maximum_Length => 4),
      RFLX.CoAP.Uri_Query                         =>
        (Format => UTF8_String, Repeatable => True, Maximum_Length => 255),
      RFLX.CoAP.Accept_17                         =>
        (Format => UInt, Repeatable => True, Maximum_Length => 2),

      -- rfc9177:
      RFLX.CoAP.Q_Block1                          =>
        (Format => UInt, Repeatable => False, Maximum_Length => 3),

      RFLX.CoAP.Location_Query                    =>
        (Format => UTF8_String, Repeatable => True, Maximum_Length => 255),

      -- RFC-ietf-core-oscore-edhoc-11
      RFLX.CoAP.EDHOC                             =>
        (Format => Empty, Repeatable => False, Maximum_Length => 0),

      -- rfc7959
      RFLX.CoAP.Block2                            =>
        (Format => UInt, Repeatable => False, Maximum_Length => 3),
      RFLX.CoAP.Block1                            =>
        (Format => UInt, Repeatable => False, Maximum_Length => 3),
      RFLX.CoAP.Size2                             =>
        (Format => UInt, Repeatable => False, Maximum_Length => 4),

      -- rfc9177:
      RFLX.CoAP.Q_Block2                          =>
        (Format => UInt, Repeatable => True, Maximum_Length => 3),

      RFLX.CoAP.Proxy_Uri                         =>
        (Format         => UTF8_String,
         Repeatable     => False,
         Maximum_Length => Max_Option_Value_Length),
      RFLX.CoAP.Proxy_Scheme                      =>
        (Format => UTF8_String, Repeatable => False, Maximum_Length => 255),
      RFLX.CoAP.Size1                             =>
        (Format => UInt, Repeatable => False, Maximum_Length => 4),

      -- rfc9175:
      RFLX.CoAP.Echo                              =>
        (Format => Opaque, Repeatable => False, Maximum_Length => 40),

      -- rfc7967:
      RFLX.CoAP.No_Response                       =>
        (Format => UInt, Repeatable => False, Maximum_Length => 1),

      -- rfc9175:
      RFLX.CoAP.Request_Tag                       =>
        (Format => Opaque, Repeatable => True, Maximum_Length => 8),

      -- https://datatracker.ietf.org/meeting/interim-2022-core-10/materials
      -- /slides-interim-2022-core-10-sessa-pramtrzd-pramtrzd-content-format
      -- -for-coap-03-00.pdf
      RFLX.CoAP.OCF_Accept_Content_Format_Version =>
        (Format => UInt, Repeatable => False, Maximum_Length => 2),
      RFLX.CoAP.OCF_Content_Format_Version        =>
        (Format => UInt, Repeatable => False, Maximum_Length => 2),

      -- No reference found:
      RFLX.CoAP.SCP82_Params                      =>
        (Format         => Unknown,
         Repeatable     => True,
         Maximum_Length => Max_Option_Value_Length)];

   type Option is private;

   function "<" (Left, Right : Option) return Boolean;
   overriding function "=" (Left, Right : Option) return Boolean;

   function Get_Number (Opt : Option) return RFLX.CoAP.Option_Numbers;

   function Get_Length (Opt : Option) return Option_Value_Length;

   function Has_Buffer (Opt : Option) return Boolean;

   procedure New_String_Option
     (Number      : RFLX.CoAP.Option_Numbers;
      Value       : String;
      Order_Index : Option_Index := 1;
      Result      : out Option)
   with
      Pre  =>
         Value'Length <= Option_Properties_Table (Number).Maximum_Length
         and then Option_Properties_Table (Number).Format = UTF8_String
         and then (if Option_Properties_Table (Number).Repeatable
                   then Order_Index >= 1
                   else Order_Index = 1),
      Post =>
         Has_Buffer (Result)
         and then Get_Number (Result) = Number
         and then Get_Length (Result) = Value'Length;

   procedure New_UInt_Option
     (Number : RFLX.CoAP.Option_Numbers;
      Value  : Interfaces.Unsigned_32;
      Order_Index : Option_Index := 1;
      Result : out Option)
   with
      Pre  => Option_Properties_Table (Number).Format = UInt
         and then (if Option_Properties_Table (Number).Repeatable
                   then Order_Index >= 1
                   else Order_Index = 1),
      Post =>
         Has_Buffer (Result)
         and then Get_Number (Result) = Number
         and then Get_Length (Result) <= Max_Uint_Length;

   procedure New_Opaque_Option
     (Number : RFLX.CoAP.Option_Numbers;
      Value  : RFLX.RFLX_Types.Bytes;
      Order_Index : Option_Index := 1;
      Result : out Option)
   with
     Pre  =>
      Value'Length <= Option_Properties_Table (Number).Maximum_Length
      and then Option_Properties_Table (Number).Format = Opaque
      and then (if Option_Properties_Table (Number).Repeatable
                  then Order_Index >= 1
                  else Order_Index = 1),
     Post =>
      Has_Buffer (Result)
      and then Get_Number (Result) = Number
      and then Get_Length (Result) = Value'Length;

   procedure New_Empty_Option
     (Number : RFLX.CoAP.Option_Numbers;
      Result : out Option) with
     Pre  =>
         Option_Properties_Table (Number).Format = Empty,
     Post =>
         Has_Buffer (Result)
         and then Get_Number (Result) = Number
         and then Get_Length (Result) = 0;

   procedure Take_Buffer (Opt   : in out Option;
                          Value : out RFLX.RFLX_Types.Bytes_Ptr)
   with
      Pre  => Has_Buffer (Opt),
      Post => not Has_Buffer (Opt) and then Value /= null;

   -- Make a deep copy of the Source option.
   procedure Copy (Source : Option; Target : out Option) with
     Pre  => Has_Buffer (Source),
     Post =>
      Has_Buffer (Target) and then Get_Number (Target) = Get_Number (Source)
      and then Get_Length (Target) = Get_Length (Source);

   procedure Free (Opt : in out Option)
   with Post => not Has_Buffer (Opt);

   function Image
     (Format : Option_Format; Value : RFLX.RFLX_Types.Bytes)
      return String
   with
     Pre  =>
      (case Format is when UInt => Value'Length in 0 .. Max_Uint_Length,
         when UTF8_String | Opaque =>
           Value'Length in 0 .. Max_Option_Value_Length,
         when Empty => Value'Length = 0,
         when Unknown => Value'Length in 0 .. Max_Option_Value_Length),
     Post =>
     Image'Result'First = 1 and then
     (case Format is
         when UInt => Image'Result'Length <= Interfaces.Unsigned_32'Width,
         when UTF8_String => Image'Result'Length = Value'Length,
         when Empty => Image'Result'Length = 0,
         when Opaque | Unknown =>
           Image'Result'Length = Value'Length * RFLX.RFLX_Types.Byte'Width);

   function To_UInt (Value : UInt_Bytes) return Interfaces.Unsigned_32;

private

   use type RFLX.RFLX_Types.Bytes;

   type Option is record
      Number      : RFLX.CoAP.Option_Numbers;
      Value       : RFLX.RFLX_Types.Bytes_Ptr;
      Order_Index : Option_Index;
   end record;

   function "<" (Left, Right : Option) return Boolean is
    (if Left.Number = Right.Number
      then Left.Order_Index < Right.Order_Index
      else Left.Number < Right.Number);

   -- Compare option on values, not on the default (pointer values)
   overriding function "=" (Left, Right : Option) return Boolean is
      (Left.Number = Right.Number and then
      ((Left.Value = null and then Right.Value = null) or else
         (Left.Value /= null and then Right.Value /= null and then
         Left.Value.all = Right.Value.all)));

   function Get_Number (Opt : Option) return RFLX.CoAP.Option_Numbers
   is (Opt.Number);

   function Has_Buffer (Opt : Option) return Boolean
   is (Opt.Value /= null);

   function Get_Length (Opt : Option) return Option_Value_Length is
     (if Opt.Value = null then 0 else Opt.Value.all'Length);

end CoAP_SPARK.Options;