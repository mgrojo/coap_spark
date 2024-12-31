with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body RFLX.CoAP_Client.Session_Environment with
  SPARK_Mode
is

   procedure Split_String_In_Repeatable_Options
     (Source        : String;
      Separator     : Character;
      Number        : RFLX.CoAP.Option_Numbers;
      Option_List   : in out CoAP_SPARK.Options.Lists.Vector)
   with Pre => CoAP_SPARK.Options.Option_Properties_Table (Number).Repeatable,
        Post =>
           Natural (CoAP_SPARK.Options.Lists.Length (Option_List)) -
             Natural (CoAP_SPARK.Options.Lists.Length (Option_List'Old)) in
             0 .. Ada.Strings.Fixed.Count
                    (Source => Source,
                     Set => Ada.Strings.Maps.To_Set (Separator)) + 1
   is
      Segment_First : Natural := Source'First;
      Segment_Last  : Natural := Source'First - 1;
      Order_Index   : CoAP_SPARK.Options.Option_Index := 1;
      Option        : CoAP_SPARK.Options.Option;
   begin

      while Segment_Last + 1 in Source'Range loop

         Ada.Strings.Fixed.Find_Token
           (Source => Source,
            Set    => Ada.Strings.Maps.To_Set (Separator),
            From   => Segment_Last + 1,
            Test   => Ada.Strings.Outside,
            First  => Segment_First,
            Last   => Segment_Last);

         exit when Segment_Last = 0;

         CoAP_SPARK.Options.New_String_Option
           (Number      => Number,
            Value       => Source (Segment_First .. Segment_Last),
            Order_Index => Order_Index,
            Result      => Option);

         CoAP_SPARK.Options.Lists.Append (Option_List, Option);
         Order_Index := Order_Index + 1;
      end loop;
   end Split_String_In_Repeatable_Options;

   procedure Initialize
     (Method        : RFLX.CoAP.Method_Code;
      Server        : String;
      Port          : Interfaces.Unsigned_16;
      Path          : String;
      Query         : String;
      Payload       : RFLX.RFLX_Types.Bytes_Ptr;
      Session_State : out State)
   is
   begin
      Session_State.Method := Method;
      Session_State.Current_Status := OK;
      Session_State.Is_First_Message := True;
      Session_State.Current_Message_ID := 0;
      Session_State.Content_Format := 0;
      Session_State.Payload := Payload;
      declare
         Option : CoAP_SPARK.Options.Option;
      begin
         CoAP_SPARK.Options.New_String_Option
           (Number => RFLX.CoAP.Uri_Host,
            Value  => Server,
            Result => Option);

         CoAP_SPARK.Options.Lists.Append
           (Session_State.Request_Options, Option);

         CoAP_SPARK.Options.New_UInt_Option
           (Number => RFLX.CoAP.Uri_Port,
            Value  => Interfaces.Unsigned_32 (Port),
            Result => Option);

         CoAP_SPARK.Options.Lists.Append
           (Session_State.Request_Options, Option);

         -- RFC7252: each Uri-Path Option specifies one segment of the absolute
         --  path to the resource.
         Split_String_In_Repeatable_Options
           (Source      => Path,
            Separator   => '/',
            Number      => RFLX.CoAP.Uri_Path,
            Option_List => Session_State.Request_Options);

         -- RFC7252: each Uri-Query Option specifies one argument parameterizing the
         -- resource.
         Split_String_In_Repeatable_Options
           (Source      => Query,
            Separator   => '&',
            Number      => RFLX.CoAP.Uri_Query,
            Option_List => Session_State.Request_Options);
      end;

   end Initialize;

end RFLX.CoAP_Client.Session_Environment;