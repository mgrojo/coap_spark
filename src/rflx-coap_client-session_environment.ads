with RFLX.CoAP;

package RFLX.CoAP_Client.Session_Environment with
   SPARK_Mode
is

   type State is record
      Is_First_Message : Boolean := True;
      Current_Message_ID : RFLX.CoAP.Message_ID_Type := 0;
   end record;
end RFLX.CoAP_Client.Session_Environment;