with Ada.Unchecked_Conversion;

with CoAP_SPARK.Options.Text_IO;
with CoAP_SPARK.Utils;

package body CoAP_SPARK.Messages
  with SPARK_Mode
is

   -------------------
   -- Print_Content --
   -------------------
   procedure Print_Content
     (Item              : Content;
      General_Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug;
      Log_Level_Payload : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Info)
   is
      Payload_Format : constant CoAP_SPARK.Options.Option_Format :=
        (if CoAP_SPARK.Content_Formats.Is_Text (Item.Format)
         then CoAP_SPARK.Options.UTF8_String
         else CoAP_SPARK.Options.Opaque);
   begin
      CoAP_SPARK.Options.Text_IO.Print (Item.Options, General_Log_Level);

      if Item.Payload not in null then
         CoAP_SPARK.Log.Put ("Content-Format: ", Level => General_Log_Level);
         CoAP_SPARK.Log.Put_Line
           (CoAP_SPARK.Content_Formats.To_String (Item.Format),
            Level => General_Log_Level);

         CoAP_SPARK.Log.Put ("Payload: ", Level => General_Log_Level);
         CoAP_SPARK.Log.Put_Line
           (CoAP_SPARK.Options.Image
              (Format => Payload_Format, Value => Item.Payload.all),
            Level => Log_Level_Payload);
      end if;
   end Print_Content;

   procedure Print_Response_Kind
     (Item              : Response_Kind;
      General_Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug;
      Log_Level_Errors  : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Info)
   is
   begin

      case Item.Code_Class is
         when RFLX.CoAP.Success =>

            CoAP_SPARK.Log.Put_Line
              (Item => "Server answered with success:",
               Level => General_Log_Level);
            CoAP_SPARK.Log.Put_Line
              (Item => Image (Item),
               Level => Log_Level_Errors);

         when RFLX.CoAP.Client_Error =>

            CoAP_SPARK.Log.Put
              (Item => "Server answered with client error: ",
               Level => General_Log_Level);
            CoAP_SPARK.Log.Put_Line
              (Item => Image (Item),
               Level => General_Log_Level);

            CoAP_SPARK.Log.Put
              (Item => Image (Item, Long => False),
               Level => Log_Level_Errors);
            CoAP_SPARK.Log.Put (" ", Log_Level_Errors);

         when RFLX.CoAP.Server_Error =>

            CoAP_SPARK.Log.Put
              (Item => "Server answered with server error: ",
               Level => General_Log_Level);
            CoAP_SPARK.Log.Put_Line
              (Item => Image (Item),
               Level => General_Log_Level);

            CoAP_SPARK.Log.Put
              (Item => Image (Item, Long => False),
               Level => Log_Level_Errors);
            CoAP_SPARK.Log.Put (" ", Log_Level_Errors);
      end case;
   end Print_Response_Kind;

   function Image (Item : Response_Kind; Long : Boolean := True) return String is
      Detail_Number : constant Integer :=
        (case Item.Code_Class is
           when RFLX.CoAP.Success => 0,
           when RFLX.CoAP.Client_Error =>
             RFLX.CoAP.Client_Error_Response'Enum_Rep (Item.Client_Error_Code),
           when RFLX.CoAP.Server_Error =>
             RFLX.CoAP.Server_Error_Response'Enum_Rep
               (Item.Server_Error_Code));

      Detail_Image : constant String :=
        (if Long
         then
           " ("
           & (case Item.Code_Class is
                when RFLX.CoAP.Success => Item.Success_Code'Image,
                when RFLX.CoAP.Client_Error =>
                  RFLX.CoAP.Client_Error_Response'Image
                    (Item.Client_Error_Code),
                when RFLX.CoAP.Server_Error =>
                  RFLX.CoAP.Server_Error_Response'Image
                    (Item.Server_Error_Code))
           & ")"
         else "");
   begin
      return
        CoAP_SPARK.Utils.Padded_Image
          (Source => Item.Code_Class'Enum_Rep, Count => 1)
        & "."
        & CoAP_SPARK.Utils.Padded_Image (Source => Detail_Number, Count => 2)
        & Detail_Image;
   end Image;

   procedure Initialize_With_Text_Payload
     (Text : String;
      Item : out Content)
   is
      use type RFLX.RFLX_Types.Index;
      function To_Byte
        is new Ada.Unchecked_Conversion (Character, RFLX.RFLX_Types.Byte);
   begin
      Item.Options := CoAP_SPARK.Options.Lists.Empty_Vector;
      Item.Format := CoAP_SPARK.Content_Formats.text.plain_charset_utf_8;

      Item.Payload :=
         new RFLX.RFLX_Types.Bytes'([1 .. Text'Length => 0]);

      for I in Item.Payload.all'Range loop
         Item.Payload (I) :=
           To_Byte (Text (Text'First - 1 + Integer (I)));
      end loop;
   end Initialize_With_Text_Payload;

   procedure Finalize (Item : in out Content)
   is
   begin
      for I in
         CoAP_SPARK.Options.Lists.First_Index (Item.Options) ..
         CoAP_SPARK.Options.Lists.Last_Index (Item.Options)
      loop
         declare
            Option_Copy : CoAP_SPARK.Options.Option :=
              CoAP_SPARK.Options.To_Option
                (CoAP_SPARK.Options.Lists.Element (Item.Options, I));
         begin
            -- Make a swallow copy of the option to be able to use a variable
            -- to free the stored Option.
            --
            CoAP_SPARK.Options.Free (Option_Copy);
            pragma Assert (not CoAP_SPARK.Options.Has_Buffer (Option_Copy));
         end;
      end loop;
      CoAP_SPARK.Options.Lists.Clear (Item.Options);
      if Item.Payload not in null then
         RFLX.RFLX_Types.Free (Item.Payload);
      end if;
   end Finalize;

end CoAP_SPARK.Messages;

