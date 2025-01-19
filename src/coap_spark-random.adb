with Ada.Numerics.Discrete_Random;

-- The random number generators cannot be proved by SPARK.
package body CoAP_SPARK.Random
  with SPARK_Mode => Off
is
   package Bytes is new Ada.Numerics.Discrete_Random (RFLX.RFLX_Types.Byte);
   Byte_Generator : Bytes.Generator;

   package ID is new Ada.Numerics.Discrete_Random (RFLX.CoAP.Message_ID_Type);
   ID_Generator : ID.Generator;

   procedure Get_Random_Byte (Result : out RFLX.RFLX_Types.Byte)
   is
   begin
      Result := Bytes.Random (Byte_Generator);
   end Get_Random_Byte;

   procedure Get_Random_Message_ID (Result : out RFLX.CoAP.Message_ID_Type)
   is
   begin
      Result := ID.Random (ID_Generator);
   end Get_Random_Message_ID;

begin
   Bytes.Reset (Byte_Generator);
   ID.Reset (ID_Generator);
end CoAP_SPARK.Random;
