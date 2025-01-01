with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with CoAP_SPARK.Options.Text_IO;

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
      Format        : Interfaces.Unsigned_32 :=
        CoAP_SPARK.Content_Formats.text.plain_charset_utf_8;
      Payload       : RFLX.RFLX_Types.Bytes_Ptr := null;
      Session_State : out State)
   is
   begin
      Session_State.Method := Method;
      Session_State.Current_Status := OK;
      Session_State.Is_First_Message := True;
      Session_State.Current_Message_ID := 0;
      Session_State.Request_Content.Format := Format;
      Session_State.Request_Content.Payload := Payload;
      declare
         Option : CoAP_SPARK.Options.Option;
      begin
         CoAP_SPARK.Options.New_String_Option
           (Number => RFLX.CoAP.Uri_Host,
            Value  => Server,
            Result => Option);

         CoAP_SPARK.Options.Lists.Append
           (Session_State.Request_Content.Options, Option);

         CoAP_SPARK.Options.New_UInt_Option
           (Number => RFLX.CoAP.Uri_Port,
            Value  => Interfaces.Unsigned_32 (Port),
            Result => Option);

         CoAP_SPARK.Options.Lists.Append
           (Session_State.Request_Content.Options, Option);

         -- RFC7252: each Uri-Path Option specifies one segment of the absolute
         --  path to the resource.
         Split_String_In_Repeatable_Options
           (Source      => Path,
            Separator   => '/',
            Number      => RFLX.CoAP.Uri_Path,
            Option_List => Session_State.Request_Content.Options);

         -- RFC7252: each Uri-Query Option specifies one argument parameterizing the
         -- resource.
         Split_String_In_Repeatable_Options
           (Source      => Query,
            Separator   => '&',
            Number      => RFLX.CoAP.Uri_Query,
            Option_List => Session_State.Request_Content.Options);

         if Payload not in null then
            CoAP_SPARK.Options.New_UInt_Option
            (Number => RFLX.CoAP.Content_Format,
               Value  => Session_State.Request_Content.Format,
               Result => Option);

            CoAP_SPARK.Options.Lists.Append
            (Session_State.Request_Content.Options, Option);
         end if;
      end;

   end Initialize;

   procedure Print_Content (Item : Content)
   is
      Payload_Format : constant CoAP_SPARK.Options.Option_Format :=
        (if CoAP_SPARK.Content_Formats.Is_Text (Item.Format)
         then CoAP_SPARK.Options.UTF8_String
         else CoAP_SPARK.Options.Opaque);
   begin
      for Option of Item.Options loop
         CoAP_SPARK.Options.Text_IO.Print (Option);
      end loop;

      if Item.Payload not in null then
         Ada.Text_IO.Put ("Content-Format: ");
         Ada.Text_IO.Put_Line
           (CoAP_SPARK.Content_Formats.To_String (Item.Format));

         Ada.Text_IO.Put ("Payload: ");
         Ada.Text_IO.Put_Line
           (CoAP_SPARK.Options.Image
              (Format => Payload_Format, Value => Item.Payload.all));
      end if;
   end Print_Content;

end RFLX.CoAP_Client.Session_Environment;