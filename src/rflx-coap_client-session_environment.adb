package body RFLX.CoAP_Client.Session_Environment with
  SPARK_Mode
is

   procedure Initialize (Method : RFLX.CoAP.Method_Code;
                         Server : String;
                         Port   : Interfaces.Unsigned_16;
                         Path   : String;
                         Session_State  : out State)
   is
   begin
      Session_State.Method := Method;
      Session_State.Current_Status := OK;
      Session_State.Is_First_Message := True;
      Session_State.Current_Message_ID := 0;
      Session_State.Content_Format := 0;

      declare
         Option : CoAP_SPARK.Options.Option;
      begin
         CoAP_SPARK.Options.New_String_Option
           (Number => RFLX.CoAP.Uri_Host,
            Value  => Server,
            Result => Option);
         CoAP_SPARK.Options.Lists.Append (Session_State.Request_Options,
                                                   Option);

         CoAP_SPARK.Options.New_UInt_Option
           (Number => RFLX.CoAP.Uri_Port,
            Value  => Interfaces.Unsigned_32 (Port),
            Result => Option);
         CoAP_SPARK.Options.Lists.Append (Session_State.Request_Options,
                                                   Option);

         CoAP_SPARK.Options.New_String_Option
           (Number => RFLX.CoAP.Uri_Path,
            Value  => Path,
            Result => Option);
         CoAP_SPARK.Options.Lists.Append (Session_State.Request_Options,
                                                   Option);
      end;

   end Initialize;

end RFLX.CoAP_Client.Session_Environment;