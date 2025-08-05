with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Containers;

with CoAP_SPARK.Options.Lists;

with RFLX.RFLX_Types;

package body RFLX.CoAP_Client.Session_Environment with
  SPARK_Mode
is

   use type Ada.Containers.Count_Type;
   use type CoAP_SPARK.Options.Option_Format;

   procedure Split_String_In_Repeatable_Options
     (Source        : String;
      Separator     : Character;
      Number        : RFLX.CoAP.Option_Numbers;
      Option_List   : in out CoAP_SPARK.Options.Lists.Vector;
      Status        : in out Status_Type) with
       Always_Terminates,
       Pre => CoAP_SPARK.Options.Option_Properties_Table (Number).Repeatable
         and then CoAP_SPARK.Options.Option_Properties_Table (Number).Format =
                   CoAP_SPARK.Options.UTF8_String
         and then Source'Length <= CoAP_SPARK.Options.Max_Option_Value_Length
   is
      Separator_Set : constant Ada.Strings.Maps.Character_Set :=
        Ada.Strings.Maps.To_Set (Separator);
      Segment_First : Natural;
      Segment_Last  : Natural;
      Order_Index   : CoAP_SPARK.Options.Option_Index := 1;
      Option        : CoAP_SPARK.Options.Option;
   begin

      if Source = "" then
         return;
      end if;

      Segment_Last := Source'First;

      while Segment_Last in Source'Range loop

         Ada.Strings.Fixed.Find_Token
           (Source => Source,
            Set    => Separator_Set,
            From   => Segment_Last,
            Test   => Ada.Strings.Outside,
            First  => Segment_First,
            Last   => Segment_Last);

         exit when
            Segment_First not in Source'Range or else
            Segment_Last not in Source'Range;

         if Segment_Last - Segment_First + 1 >
            Natural (CoAP_SPARK.Options.Option_Properties_Table (Number).Maximum_Length)
            or else CoAP_SPARK.Options.Lists.Length (Option_List) =
            CoAP_SPARK.Max_Number_Of_Options
         then
            Status := Capacity_Error;
            return;
         end if;

         CoAP_SPARK.Options.New_String_Option
           (Number      => Number,
            Value       => Source (Segment_First .. Segment_Last),
            Order_Index => Order_Index,
            Result      => Option);

         CoAP_SPARK.Options.Lists.Append (Option_List,
                                          CoAP_SPARK.Options.To_Indefinite (Option));
         CoAP_SPARK.Options.Free (Option);

         exit when Segment_Last >= Source'Last;

         if Order_Index = CoAP_SPARK.Options.Option_Index'Last then
            Status := Capacity_Error;
            return;
         end if;

         pragma Loop_Invariant (not CoAP_SPARK.Options.Has_Buffer (Option));
         pragma Loop_Invariant (Segment_Last in Source'Range);
         pragma Loop_Invariant (Segment_Last < Natural'Last);
         pragma Loop_Invariant (Order_Index < CoAP_SPARK.Options.Option_Index'Last);
         pragma Loop_Invariant
            (CoAP_SPARK.Options.Lists.Length (Option_List) =
             CoAP_SPARK.Options.Lists.Length (Option_List'Loop_Entry) +
             CoAP_SPARK.Options.Lists.Capacity_Range (Order_Index));
         pragma Loop_Variant (Increases => Segment_Last);

         Segment_Last := @ + 1;
         Order_Index := @ + 1;

      end loop;
   end Split_String_In_Repeatable_Options;

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
   is
      use type RFLX.RFLX_Types.Bytes_Ptr;
   begin
      Session_State.Method := Method;
      Session_State.Current_Status := OK;
      Session_State.Is_First_Message := True;
      Session_State.Current_Message_ID := 0;
      Session_State.Request_Content :=
        (Options => CoAP_SPARK.Options.Lists.Empty_Vector,
         Format  => Format,
         Payload => Payload);

      -- Payload was moved.
      Payload := null;

      declare
         Option : CoAP_SPARK.Options.Option;
      begin
         CoAP_SPARK.Options.New_String_Option
           (Number => RFLX.CoAP.Uri_Host,
            Value  => Server,
            Result => Option);

         CoAP_SPARK.Options.Lists.Append
           (Session_State.Request_Content.Options,
            CoAP_SPARK.Options.To_Indefinite (Option));
         CoAP_SPARK.Options.Free (Option);
         pragma Assert (not CoAP_SPARK.Options.Has_Buffer (Option));

         CoAP_SPARK.Options.New_UInt_Option
           (Number => RFLX.CoAP.Uri_Port,
            Value  => Interfaces.Unsigned_32 (Port),
            Result => Option);

         CoAP_SPARK.Options.Lists.Append
           (Session_State.Request_Content.Options,
            CoAP_SPARK.Options.To_Indefinite (Option));
         CoAP_SPARK.Options.Free (Option);
         pragma Assert (not CoAP_SPARK.Options.Has_Buffer (Option));

         -- RFC7252: each Uri-Path Option specifies one segment of the absolute
         --  path to the resource.
         Split_String_In_Repeatable_Options
           (Source      => Path,
            Separator   => '/',
            Number      => RFLX.CoAP.Uri_Path,
            Option_List => Session_State.Request_Content.Options,
            Status      => Session_State.Current_Status);

         if Session_State.Current_Status = OK then
            -- RFC7252: each Uri-Query Option specifies one argument parameterizing the
            -- resource.
            Split_String_In_Repeatable_Options
              (Source      => Query,
               Separator   => '&',
               Number      => RFLX.CoAP.Uri_Query,
               Option_List => Session_State.Request_Content.Options,
               Status      => Session_State.Current_Status);

            if Session_State.Current_Status = OK
               and then
               Session_State.Request_Content.Payload /= null
            then
               if CoAP_SPARK.Options.Lists.Length
                    (Session_State.Request_Content.Options)
                 = CoAP_SPARK.Max_Number_Of_Options
               then
                  Session_State.Current_Status := Capacity_Error;
               else
                  CoAP_SPARK.Options.New_UInt_Option
                    (Number => RFLX.CoAP.Content_Format,
                     Value  => Interfaces.Unsigned_32
                        (Session_State.Request_Content.Format),
                     Result => Option);

                  CoAP_SPARK.Options.Lists.Append
                    (Session_State.Request_Content.Options,
                       CoAP_SPARK.Options.To_Indefinite (Option));
                  CoAP_SPARK.Options.Free (Option);
                  pragma Assert (not CoAP_SPARK.Options.Has_Buffer (Option));

               end if;
            end if;
         end if;
      end;

      Session_State.Response_Content :=
        (Options => CoAP_SPARK.Options.Lists.Empty_Vector,
         Format  => 0,
         Payload => null);

      Session_State.Response_Codes :=
        (Code_Class => RFLX.CoAP.Success,
         Success_Code => RFLX.CoAP.Success_Response'First);

   end Initialize;

   procedure Finalize (Session_State : in out State)
   is
   begin
      CoAP_SPARK.Messages.Finalize (Session_State.Request_Content);
      CoAP_SPARK.Messages.Finalize (Session_State.Response_Content);
   end Finalize;

end RFLX.CoAP_Client.Session_Environment;