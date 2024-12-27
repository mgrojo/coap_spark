with Ada.Strings.Fixed;
with Ada.Strings.Maps;

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

         -- RFC7252: each Uri-Path Option specifies one segment of the absolute
         --  path to the resource.
         declare
            Segment_First : Natural := Path'First;
            Segment_Last : Natural := Path'First - 1;
            Order_Index : CoAP_SPARK.Options.Option_Index := 1;
         begin

            while Segment_Last + 1 in Path'Range loop

               Ada.Strings.Fixed.Find_Token
                 (Source => Path,
                  Set => Ada.Strings.Maps.To_Set("/"),
                  From => Segment_Last + 1,
                  Test => Ada.Strings.Outside,
                  First => Segment_First,
                  Last => Segment_Last);

               exit when Segment_Last = 0;

               CoAP_SPARK.Options.New_String_Option
                 (Number      => RFLX.CoAP.Uri_Path,
                  Value       => Path (Segment_First .. Segment_Last),
                  Order_Index => Order_Index,
                  Result      => Option);

               CoAP_SPARK.Options.Lists.Append (Session_State.Request_Options,
                                                         Option);
               Order_Index := Order_Index + 1;
            end loop;
         end;

         -- TODO Query part
      end;

   end Initialize;

end RFLX.CoAP_Client.Session_Environment;