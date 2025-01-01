with CoAP_SPARK.Content_Formats;
with CoAP_SPARK.Options.Lists;
with Interfaces;
with RFLX.CoAP;

package RFLX.CoAP_Client.Session_Environment with
   SPARK_Mode
is

   type Status_Type is (OK, Capacity_Error, Invalid_Request, Malformed_Message);

   type Content is record
      Options : CoAP_SPARK.Options.Lists.Vector
                          (CoAP_SPARK.Max_Number_Of_Options);
      Format : Interfaces.Unsigned_32 := 0;
      Payload : RFLX.RFLX_Types.Bytes_Ptr;
   end record;

   type State is record
      Method : RFLX.CoAP.Method_Code := RFLX.CoAP.Get;
      Current_Status : Status_Type := OK;
      Is_First_Message : Boolean := True;
      Current_Message_ID : RFLX.CoAP.Message_ID_Type := 0;
      Request_Content : Content;
      Response_Content : Content;
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
      Format        : Interfaces.Unsigned_32 :=
        CoAP_SPARK.Content_Formats.text.plain_charset_utf_8;
      Payload       : RFLX.RFLX_Types.Bytes_Ptr := null;
      Session_State : out State)
   with Always_Terminates;

   procedure Print_Content (Item : Content);

end RFLX.CoAP_Client.Session_Environment;