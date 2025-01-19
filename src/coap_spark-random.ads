with RFLX.RFLX_Types;
with RFLX.CoAP;

package CoAP_SPARK.Random
  with SPARK_Mode,
      Abstract_State => Generator,
      Initializes => Generator
is
   procedure Get_Random_Byte
     (Result : out RFLX.RFLX_Types.Byte)
      with Global => (In_Out => Generator),
      Always_Terminates;

   procedure Get_Random_Message_ID
     (Result : out RFLX.CoAP.Message_ID_Type)
      with Global => (In_Out => Generator),
      Always_Terminates;

end CoAP_SPARK.Random;
