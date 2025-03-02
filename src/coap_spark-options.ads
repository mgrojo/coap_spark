with Interfaces;
with RFLX.CoAP;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;

package CoAP_SPARK.Options
  with SPARK_Mode
is

   use type RFLX.CoAP.Option_Numbers;
   use type RFLX.RFLX_Builtin_Types.Bytes_Ptr;
   use type RFLX.RFLX_Types.Index;

   -- According to Section 5.10. Option Definitions "Table 4: Options"
   -- the maximum size for uint options is 4 bytes.
   -- The other registered options by IANA are also not larger than 4 bytes.
   -- Maximum size in general is 1034 bytes.
   Max_Uint_Length         : constant := 4;
   Max_Option_Value_Length : constant := Max_URI_Length;

   subtype UInt_Bytes is RFLX.RFLX_Types.Bytes
   with Dynamic_Predicate => UInt_Bytes'Length <= Max_Uint_Length;

   subtype Option_Value_Length is
     RFLX.RFLX_Types.Index'Base range 0 .. CoAP_SPARK.Options.Max_Option_Value_Length;

   subtype Option_Value_Ptr is RFLX.RFLX_Types.Bytes_Ptr
   with
     Dynamic_Predicate =>
       (if Option_Value_Ptr /= null
        then Option_Value_Ptr.all'Length <= Max_Option_Value_Length);

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

   -- Return whether the option is critical or not given its option number, as
   -- defined in the RFC-7252.
   -- Received critical options can't be ignored. If they are unkwown, the
   -- receiver must reject the message.
   function Is_Critical (Number : RFLX.RFLX_Types.Base_Integer) return Boolean;

   type Option is private
   with
     Dynamic_Predicate =>
       Has_Valid_Length
         (Option_Properties_Table (Option.Number).Format,
          (if Option.Value = null then 0 else Option.Value.all'Length));

   type Indefinite_Option (Value_Length : Option_Value_Length) is private
   with
     Dynamic_Predicate =>
       Has_Valid_Length
         (Option_Properties_Table (Indefinite_Option.Number).Format,
          Natural (Value_Length));

   function To_Option (From : Indefinite_Option) return Option
   with
     Post =>
       To_Indefinite (To_Option'Result) = From
       and then Has_Buffer (To_Option'Result);

   function To_Indefinite (Opt : Option) return Indefinite_Option;

   function "<" (Left, Right : Option) return Boolean;
   function "<" (Left, Right : Indefinite_Option) return Boolean;

   overriding
   function "=" (Left, Right : Option) return Boolean;

   function Get_Number (Opt : Option) return RFLX.CoAP.Option_Numbers;

   function Get_Length (Opt : Option) return Option_Value_Length;


   function Get_Number (Opt : Indefinite_Option) return RFLX.CoAP.Option_Numbers;

   function Get_Length (Opt : Indefinite_Option) return Option_Value_Length;

   function Get_Value (Opt : Indefinite_Option) return RFLX.RFLX_Types.Bytes;

   function Has_Buffer (Opt : Option) return Boolean;

   procedure New_String_Option
     (Number      : RFLX.CoAP.Option_Numbers;
      Value       : String;
      Order_Index : Option_Index := 1;
      Result      : out Option)
   with
     Pre =>
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
     (Number      : RFLX.CoAP.Option_Numbers;
      Value       : Interfaces.Unsigned_32;
      Order_Index : Option_Index := 1;
      Result      : out Option)
   with
     Pre =>
       Option_Properties_Table (Number).Format = UInt
       and then (if Option_Properties_Table (Number).Repeatable
                 then Order_Index >= 1
                 else Order_Index = 1),
     Post =>
       Has_Buffer (Result)
       and then Get_Number (Result) = Number
       and then Get_Length (Result) <= Max_Uint_Length;

   procedure New_Opaque_Option
     (Number      : RFLX.CoAP.Option_Numbers;
      Value       : RFLX.RFLX_Types.Bytes;
      Order_Index : Option_Index := 1;
      Result      : out Option)
   with
     Pre =>
       Value'Length <= Option_Properties_Table (Number).Maximum_Length
       and then Option_Properties_Table (Number).Format = Opaque
       and then (if Option_Properties_Table (Number).Repeatable
                 then Order_Index >= 1
                 else Order_Index = 1),
     Post =>
       Has_Buffer (Result)
       and then Get_Number (Result) = Number
       and then Get_Length (Result) = Value'Length;

   -- For options where we already have the encoded value, like in responses.
   procedure New_Encoded_Option
     (Number : RFLX.CoAP.Option_Numbers;
      Value  : in out Option_Value_Ptr;
      Result : out Option)
   with
     Pre =>
       Value /= null
       and then Value'Length
                <= Option_Properties_Table (Number).Maximum_Length,
     Post =>
       Has_Buffer (Result)
       and then Value = null
       and then Get_Number (Result) = Number;

   procedure New_Empty_Option
     (Number : RFLX.CoAP.Option_Numbers; Result : out Option)
   with
     Pre => Option_Properties_Table (Number).Format = Empty,
     Post =>
       Has_Buffer (Result)
       and then Get_Number (Result) = Number
       and then Get_Length (Result) = 0;

   procedure Take_Buffer (Opt : in out Option; Value : out Option_Value_Ptr)
   with
     Pre => Has_Buffer (Opt),
     Post => not Has_Buffer (Opt) and then Value /= null;

   -- Make a deep copy of the Source option.
   procedure Copy (Source : Option; Target : out Option)
   with
     Pre => Has_Buffer (Source),
     Post =>
       Has_Buffer (Target)
       and then To_Indefinite (Source) = To_Indefinite (Target);

   procedure Free (Opt : in out Option)
   with Post => not Has_Buffer (Opt);

   function Has_Valid_Length
     (Format : Option_Format; Length : Natural) return Boolean
   is (case Format is
         when UInt => Length in 0 .. Max_Uint_Length,
         when UTF8_String | Opaque => Length in 0 .. Max_Option_Value_Length,
         when Empty => Length = 0,
         when Unknown => Length in 0 .. Max_Option_Value_Length);

   function Is_Valid_Image_Length
     (Format : Option_Format; Length : Natural) return Boolean
   is (case Format is
         when UInt => Length <= Interfaces.Unsigned_32'Width + 1,
         when UTF8_String => Length <= Max_Option_Value_Length,
         when Empty => Length = 0,
         when Opaque | Unknown =>
           Length <= Max_Option_Value_Length * RFLX.RFLX_Types.Byte'Width);

   function Image
     (Format : Option_Format; Value : RFLX.RFLX_Types.Bytes) return String
   with
     Pre => Has_Valid_Length (Format, Value'Length),
     Post =>
       Image'Result'First = 1
       and then Is_Valid_Image_Length (Format, Image'Result'Length);

   function Value_Image (Opt : Option) return String
   with
     Pre =>
       Has_Buffer (Opt)
       and then Has_Valid_Length
                  (Option_Properties_Table (Get_Number ((Opt))).Format,
                   Natural (Get_Length (Opt))),
     Post =>
       Value_Image'Result'First = 1
       and then Is_Valid_Image_Length
                  (Option_Properties_Table (Get_Number ((Opt))).Format,
                   Value_Image'Result'Length);


   function Value_Image (Opt : Indefinite_Option) return String
   with
     Pre =>
         Has_Valid_Length
                  (Option_Properties_Table (Get_Number ((Opt))).Format,
                   Natural (Get_Length (Opt))),
     Post =>
       Value_Image'Result'First = 1
       and then Is_Valid_Image_Length
                  (Option_Properties_Table (Get_Number ((Opt))).Format,
                   Value_Image'Result'Length);

   function To_UInt (Value : UInt_Bytes) return Interfaces.Unsigned_32;

private

   use type RFLX.RFLX_Types.Bytes;

   type Option is record
      Number      : RFLX.CoAP.Option_Numbers;
      Value       : Option_Value_Ptr;
      Order_Index : Option_Index;
   end record;

   type Indefinite_Option (Value_Length : Option_Value_Length) is record
      Number      : RFLX.CoAP.Option_Numbers;
      Value       : RFLX.RFLX_Types.Bytes (1 ..  Value_Length);
      Order_Index : Option_Index;
   end record;

   function To_Indefinite (Opt : Option) return Indefinite_Option
   is ((Value_Length => RFLX.RFLX_Types.Index'Base (Get_Length (Opt)),
        Number       => Get_Number (Opt),
        Value        => (if Get_Length (Opt) = 0 then [] else Opt.Value.all),
        Order_Index  => Opt.Order_Index));

   function "<" (Left, Right : Option) return Boolean is
    (if Left.Number = Right.Number
      then Left.Order_Index < Right.Order_Index
      else Left.Number < Right.Number);

   function "<" (Left, Right : Indefinite_Option) return Boolean is
    (if Left.Number = Right.Number
      then Left.Order_Index < Right.Order_Index
      else Left.Number < Right.Number);

   -- Compare option on values, not on the default (pointer values)
   overriding function "=" (Left, Right : Option) return Boolean is
      (Left.Number = Right.Number and then
       Left.Order_Index = Right.Order_Index and then
      ((Left.Value = null and then Right.Value = null) or else
         (Left.Value /= null and then Right.Value /= null and then
         Left.Value.all = Right.Value.all)));

   function Get_Number (Opt : Option) return RFLX.CoAP.Option_Numbers
   is (Opt.Number);

   function Has_Buffer (Opt : Option) return Boolean
   is (Opt.Value /= null);

   function Get_Length (Opt : Option) return Option_Value_Length is
     (if Opt.Value = null then 0 else Opt.Value.all'Length);

   function Get_Number (Opt : Indefinite_Option) return RFLX.CoAP.Option_Numbers
   is (Opt.Number);

   function Get_Length (Opt : Indefinite_Option) return Option_Value_Length is
     (Opt.Value'Length);

   function Get_Value (Opt : Indefinite_Option) return RFLX.RFLX_Types.Bytes is
     (Opt.Value);

end CoAP_SPARK.Options;