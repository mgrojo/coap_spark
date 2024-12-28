with Interfaces;
with RFLX.CoAP;
with CoAP_SPARK.Options.Lists;

package RFLX.CoAP_Client.Session_Environment with
   SPARK_Mode
is

   type Status_Type is (OK, Capacity_Error, Malformed_Message);

   type State is record
      Method : RFLX.CoAP.Method_Code := RFLX.CoAP.Get;
      Request_Options : CoAP_SPARK.Options.Lists.Vector
                          (CoAP_SPARK.Max_Number_Of_Options);
      Current_Status : Status_Type := OK;
      Is_First_Message : Boolean := True;
      Current_Message_ID : RFLX.CoAP.Message_ID_Type := 0;
      Content_Format : Interfaces.Unsigned_32 := 0;
      Payload : RFLX.RFLX_Types.Bytes_Ptr;
   end record;

   --  Initialize State.
   --
   --  This procedure must be called before the start of the state machine to
   --  ensure that State is initialized.
   --
   procedure Initialize
     (Method        : RFLX.CoAP.Method_Code;
      Server        : String;
      Port          : Interfaces.Unsigned_16;
      Path          : String;
      Query         : String;
      Payload       : RFLX.RFLX_Types.Bytes_Ptr;
      Session_State : out State)
   with Always_Terminates;

end RFLX.CoAP_Client.Session_Environment;