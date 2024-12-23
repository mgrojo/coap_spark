with Interfaces;
with RFLX.CoAP;

package RFLX.CoAP_Client.Session_Environment with
   SPARK_Mode
is

   type Status_Type is (OK, Capacity_Error, Malformed_Message);

   type State is record
      Method : RFLX.CoAP.Method_Code := RFLX.CoAP.Get;
      Current_Status : Status_Type := OK;
      Is_First_Message : Boolean := True;
      Current_Message_ID : RFLX.CoAP.Message_ID_Type := 0;
      Content_Format : Interfaces.Unsigned_32 := 0;
   end record;

end RFLX.CoAP_Client.Session_Environment;