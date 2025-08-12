with RFLX.RFLX_Types;

package CoAP_SPARK.Messages.Encoding
   with SPARK_Mode
is

   use type RFLX.RFLX_Types.Index;

   procedure Encode_Options_And_Payload
     (Options_And_Payload : Content;
      Status              : out CoAP_SPARK.Status_Type;
      Encoded_Data        : out RFLX.RFLX_Types.Bytes;
      Encoded_Length      : out RFLX.RFLX_Types.Length)
   with Pre => Encoded_Data'First = RFLX.RFLX_Types.Index'First;

   procedure Decode_Options_And_Payload
     (Data            : RFLX.RFLX_Types.Bytes;
      Status          : out CoAP_SPARK.Status_Type;
      Decoded_Content : out Content);

end CoAP_SPARK.Messages.Encoding;
