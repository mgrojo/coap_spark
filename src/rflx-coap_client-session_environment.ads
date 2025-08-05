with CoAP_SPARK.Content_Formats;
with CoAP_SPARK.Messages;
with CoAP_SPARK.Options;

with Interfaces;

with RFLX.CoAP;

package RFLX.CoAP_Client.Session_Environment with
   SPARK_Mode
is

   type Status_Type is new CoAP_SPARK.Status_Type;

   type State is record
      Method : RFLX.CoAP.Method_Code := RFLX.CoAP.Get;
      Current_Status : Status_Type := OK;
      Is_First_Message : Boolean := True;
      Current_Message_ID : RFLX.CoAP.Message_ID_Type := 0;
      Request_Content : CoAP_SPARK.Messages.Content;
      Response_Codes : CoAP_SPARK.Messages.Response_Kind;
      Response_Content : CoAP_SPARK.Messages.Content;
   end record;

   --  Initialize State.
   --
   --  This procedure must be called before the start of the state machine to
   --  ensure that State is initialized.
   --
   procedure Initialize
     (Method        : RFLX.CoAP.Method_Code;
      Server        : CoAP_SPARK.Options.Hostname;
      Port          : Interfaces.Unsigned_16;
      Path          : String;
      Query         : String;
      Format        : CoAP_SPARK.Content_Formats.Content_Type :=
        CoAP_SPARK.Content_Formats.text.plain_charset_utf_8;
      Payload       : in out CoAP_SPARK.Messages.Payload_Ptr;
      Session_State : out State)
   with Always_Terminates,
      Pre =>
          Path'Length <= CoAP_SPARK.Max_URI_Length and then
          Query'Length <= CoAP_SPARK.Max_URI_Length,
      Post => Payload in null;

   function Is_Finalized (Session_State : State) return Boolean
     is (CoAP_SPARK.Messages.Is_Empty (Session_State.Request_Content)
         and then CoAP_SPARK.Messages.Is_Empty (Session_State.Response_Content));

   procedure Finalize (Session_State : in out State)
   with Post => Is_Finalized (Session_State);

end RFLX.CoAP_Client.Session_Environment;