pragma SPARK_Mode;

with Ada.Text_IO;
with Coap_Client_Config;

with CoAP_Secure;

with CoAP_SPARK.Channel;
with CoAP_SPARK.Client_Session;
with CoAP_SPARK.Log;
with CoAP_SPARK.Messages;
with CoAP_SPARK.Random;
with CoAP_SPARK.URI;
with CoAP_SPARK.Utils;

with Interfaces;

with RFLX.CoAP_Client.Session_Environment;
with RFLX.CoAP_Client.Session.FSM;
with RFLX.CoAP;
with RFLX.RFLX_Types;

-- The following package is used, instead of Ada.Command_Line, for the reasons
-- explained in the package itself (related to the use of SPARK).
with SPARK_Terminal;

with Workarounds;

procedure CoAP_Client is
   package FSM renames RFLX.CoAP_Client.Session.FSM;
   package Session_Environment renames RFLX.CoAP_Client.Session_Environment;
   package Types renames RFLX.RFLX_Types;
   package Channel renames CoAP_SPARK.Channel;

   use type CoAP_SPARK.Status_Type;
   use type Types.Index;
   use type Types.Bytes_Ptr;

   procedure Usage (Is_Failure : Boolean := True) is
      procedure Put (Text : String;
                     Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Info)
         renames CoAP_SPARK.Log.Put;
      procedure Put_Line (Text : String;
                          Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Info)
         renames CoAP_SPARK.Log.Put_Line;
   begin
      Put ("Usage: coap_client [-m METHOD] [-e <Payload>] ");
      Put ("[-k <PSK>] [-u <Identity>] [-v <verbosity>] ");
      Put_Line ("<URI>");
      Put ("  METHOD:");
      for I in RFLX.CoAP.Method_Code'Range loop
         Put (" ");
         Put (RFLX.CoAP.Method_Code'Image (I));
      end loop;
      CoAP_SPARK.Log.New_Line (Level => CoAP_SPARK.Log.Info);

      SPARK_Terminal.Set_Exit_Status
        (Status =>
           (if Is_Failure
            then SPARK_Terminal.Exit_Status_Failure
            else SPARK_Terminal.Exit_Status_Success));
   end Usage;

   procedure Run_Session
     (Method : RFLX.CoAP.Method_Code;
      URI_String : String;
      Payload : in out CoAP_SPARK.Messages.Payload_Ptr)
   with
      Pre =>
        URI_String'Length <= CoAP_SPARK.Max_URI_Length,
      Global =>
         (Input  => CoAP_SPARK.Log.Active_Level,
          In_Out => (Ada.Text_IO.File_System, CoAP_SPARK.Random.Generator))

   is
      URI        : constant CoAP_SPARK.URI.URI :=
        CoAP_SPARK.URI.Create (URI_String);
      Ctx        : FSM.Context;
      Skt        :
        CoAP_SPARK.Channel.Socket_Type
          (Is_Secure =>
             CoAP_SPARK.URI.Scheme (URI) = CoAP_SPARK.Secure_Scheme);
      Valid_URI : Boolean := True;
   begin

      if URI_String = "" or else URI_String (URI_String'First) = '-' then
         CoAP_SPARK.Log.Put ("Unrecognized option: ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line (URI_String, CoAP_SPARK.Log.Error);
         Valid_URI := False;
      elsif not CoAP_SPARK.URI.Is_Valid (URI)
        or else not CoAP_SPARK.URI.Has_Valid_Lengths (URI)
      then
         CoAP_SPARK.Log.Put ("Invalid URI: ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line (URI_String, CoAP_SPARK.Log.Error);
         Valid_URI := False;
      end if;

      if not Valid_URI then
         Usage;
         return;
      end if;

      CoAP_SPARK.Log.Put ("Method: ");
      CoAP_SPARK.Log.Put_Line (RFLX.CoAP.Method_Code'Image (Method));
      CoAP_SPARK.Log.Put ("Scheme: ");
      CoAP_SPARK.Log.Put_Line (CoAP_SPARK.URI.Scheme (URI));
      CoAP_SPARK.Log.Put ("Host: ");
      CoAP_SPARK.Log.Put_Line (CoAP_SPARK.URI.Host (URI));
      CoAP_SPARK.Log.Put ("Port:");
      CoAP_SPARK.Log.Put_Line
        (Interfaces.Unsigned_16'Image (CoAP_SPARK.URI.Port (URI)));
      CoAP_SPARK.Log.Put ("Path: ");
      CoAP_SPARK.Log.Put_Line (CoAP_SPARK.URI.Path (URI));
      CoAP_SPARK.Log.Put ("Query: ");
      CoAP_SPARK.Log.Put_Line (CoAP_SPARK.URI.Query (URI));

      CoAP_Secure.Initialize (Socket => Skt);
      if not CoAP_SPARK.Channel.Is_Valid (Skt) then
         CoAP_SPARK.Log.Put_Line
           ("Communication problems.", CoAP_SPARK.Log.Error);
         RFLX.RFLX_Types.Free (Payload);
         return;
      end if;

      Session_Environment.Initialize
        (Method        => Method,
         Server        => CoAP_SPARK.URI.Host (URI),
         Port          => CoAP_SPARK.URI.Port (URI),
         Path          => CoAP_SPARK.URI.Path (URI),
         Query         => CoAP_SPARK.URI.Query (URI),
         Payload       => Payload,
         Session_State => Ctx.E);

      if Ctx.E.Current_Status /= CoAP_SPARK.OK then
         CoAP_SPARK.Log.Put_Line
           (Ctx.E.Current_Status'Image, CoAP_SPARK.Log.Error);
         RFLX.RFLX_Types.Free (Payload);
         Session_Environment.Finalize (Ctx.E);
         pragma Assert (Session_Environment.Is_Finalized (Ctx.E));
         return;
      end if;
      pragma Assert (FSM.Uninitialized (Ctx));

      FSM.Initialize (Ctx);
      Channel.Connect
        (Socket => Skt,
         Server => CoAP_SPARK.URI.Host (URI),
         Port   => CoAP_SPARK.Channel.Port_Type (CoAP_SPARK.URI.Port (URI)));

      if not CoAP_SPARK.Channel.Is_Valid (Skt) then
         CoAP_SPARK.Log.Put_Line
           ("Connection problems.", CoAP_SPARK.Log.Error);
         RFLX.RFLX_Types.Free (Payload);
         FSM.Finalize (Ctx);
         pragma Assert (FSM.Uninitialized (Ctx));
         Session_Environment.Finalize (Ctx.E);
         pragma Assert (Session_Environment.Is_Finalized (Ctx.E));
         return;
      end if;

      CoAP_SPARK.Log.Put_Line ("REQUEST: ");
      CoAP_SPARK.Messages.Print_Content
        (Item => Ctx.E.Request_Content,
         Log_Level_Payload => CoAP_SPARK.Log.Debug);

      CoAP_SPARK.Client_Session.Run_Session_Loop (Ctx, Skt);

      CoAP_SPARK.Channel.Finalize (Skt);
      pragma Assert (not CoAP_SPARK.Channel.Is_Valid (Skt));

      CoAP_SPARK.Log.New_Line;
      if Ctx.E.Current_Status in CoAP_SPARK.OK then
         CoAP_SPARK.Log.Put_Line ("RESPONSE: ");

         case Ctx.E.Response_Codes.Code_Class is
            when RFLX.CoAP.Success =>

               CoAP_SPARK.Log.Put_Line ("Server answered with success.");

            when RFLX.CoAP.Client_Error =>

               CoAP_SPARK.Log.Put ("Server answered with client error: ");
               CoAP_SPARK.Log.Put_Line
               (CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes));

               CoAP_SPARK.Log.Put
               (CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes, Long => False),
                  CoAP_SPARK.Log.Info);
               CoAP_SPARK.Log.Put (" ", CoAP_SPARK.Log.Info);

            when RFLX.CoAP.Server_Error =>

               CoAP_SPARK.Log.Put ("Server answered with server error: ");
               CoAP_SPARK.Log.Put_Line
               (CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes));

               CoAP_SPARK.Log.Put
               (CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes, Long => False),
                  CoAP_SPARK.Log.Info);
               CoAP_SPARK.Log.Put (" ", CoAP_SPARK.Log.Info);
         end case;

         CoAP_SPARK.Messages.Print_Content (Ctx.E.Response_Content);
      else
         CoAP_SPARK.Log.Put ("Aborted with error: ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line
         (Ctx.E.Current_Status'Image, CoAP_SPARK.Log.Error);
         SPARK_Terminal.Set_Exit_Status (SPARK_Terminal.Exit_Status_Failure);
      end if;

      FSM.Finalize (Ctx);
      pragma Assert (FSM.Uninitialized (Ctx));

      Session_Environment.Finalize (Ctx.E);
      pragma Assert (Session_Environment.Is_Finalized (Ctx.E));
   end Run_Session;

   Method : RFLX.CoAP.Method_Code := RFLX.CoAP.Get;
   Payload : CoAP_SPARK.Messages.Payload_Ptr := null;

   Argument_Index : Natural := 1;

   Valid_Command_Line : Boolean := True;
begin

   if SPARK_Terminal.Argument_Count = 0
     or else SPARK_Terminal.Argument (1) = "-h"
     or else SPARK_Terminal.Argument (1) = "--help"
   then
      Usage (Is_Failure => False);
      return;
   end if;

   if SPARK_Terminal.Argument_Count = 1
     and then (SPARK_Terminal.Argument (1) = "-V"
               or else SPARK_Terminal.Argument (1) = "--version")
   then
      CoAP_SPARK.Log.Put (Coap_Client_Config.Crate_Name, CoAP_SPARK.Log.Info);
      CoAP_SPARK.Log.Put (" v", CoAP_SPARK.Log.Info);
      CoAP_SPARK.Log.Put_Line
        (Coap_Client_Config.Crate_Version, CoAP_SPARK.Log.Info);
      return;
   end if;

   while Argument_Index < SPARK_Terminal.Argument_Count loop

      if SPARK_Terminal.Argument (Argument_Index) = "-m" then
         Argument_Index := @ + 1;
         if CoAP_SPARK.Utils.Is_Valid_As_Method
              (SPARK_Terminal.Argument (Argument_Index))
         then
            Method := CoAP_SPARK.Utils.Value
                (SPARK_Terminal.Argument (Argument_Index));
         else
            CoAP_SPARK.Log.Put_Line ("Invalid method", CoAP_SPARK.Log.Error);
            Valid_Command_Line := False;
         end if;

      elsif SPARK_Terminal.Argument (Argument_Index) = "-e" then
         Argument_Index := @ + 1;
         declare
            Payload_Length : constant Natural :=
              SPARK_Terminal.Argument (Argument_Index)'Length;
         begin
            if Payload_Length = 0
              or else Payload_Length > CoAP_SPARK.Max_Payload_Length
            then
               CoAP_SPARK.Log.Put_Line
                 ("Payload too long", CoAP_SPARK.Log.Error);
               Valid_Command_Line := False;
            elsif Payload /= null then
               CoAP_SPARK.Log.Put_Line
                 ("Payload already provided", CoAP_SPARK.Log.Error);
               Valid_Command_Line := False;
            else
               Payload :=
                 new Types.Bytes'(1 .. Types.Index (Payload_Length) => 0);
               CoAP_SPARK.Utils.Copy_String
                 (Source => SPARK_Terminal.Argument (Argument_Index),
                  Target => Payload.all);
            end if;
         end;

      elsif SPARK_Terminal.Argument (Argument_Index) = "-v" then
         Argument_Index := @ + 1;
         if CoAP_SPARK.Utils.Valid_Natural_Values.Is_Valid_As_Number
              (SPARK_Terminal.Argument (Argument_Index))
         then
            declare
               Verbosity_Number : constant Natural :=
                 CoAP_SPARK.Utils.Value
                   (SPARK_Terminal.Argument (Argument_Index));
            begin
               if Verbosity_Number
                 > CoAP_SPARK.Log.Level_Type'Pos
                     (CoAP_SPARK.Log.Level_Type'Last)
               then
                  CoAP_SPARK.Log.Put_Line
                    ("Verbosity level too high", CoAP_SPARK.Log.Error);
                  Valid_Command_Line := False;
               else
                  CoAP_SPARK.Log.Set_Level
                    (CoAP_SPARK.Log.Level_Type'Val
                       (CoAP_SPARK.Log.Level_Type'Pos
                          (CoAP_SPARK.Log.Level_Type'Last)
                        - Verbosity_Number));
               end if;
            end;
         else
            CoAP_SPARK.Log.Put_Line
              ("Invalid verbosity level", CoAP_SPARK.Log.Error);
            Valid_Command_Line := False;
         end if;

      elsif SPARK_Terminal.Argument (Argument_Index) = "-B" then
         -- This is allowed but not implemented. For compatibility to
         -- libcoap's coap-client, which uses this switch for specifying a
         -- timeout.
         Argument_Index := @ + 1;

      elsif SPARK_Terminal.Argument (Argument_Index) = "-k"
        or else SPARK_Terminal.Argument (Argument_Index) = "-u"
      then
         Argument_Index := @ + 1;

         if Argument_Index = SPARK_Terminal.Argument_Count then
            CoAP_SPARK.Log.Put ("Missing argument for ", CoAP_SPARK.Log.Error);
            CoAP_SPARK.Log.Put_Line
              (SPARK_Terminal.Argument (Argument_Index - 1),
               CoAP_SPARK.Log.Error);
            Valid_Command_Line := False;
         end if;
         -- We will handle the PSK and Identity later, in the PSK_Callback
      else
         CoAP_SPARK.Log.Put ("Invalid option: ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line
           (SPARK_Terminal.Argument (Argument_Index), CoAP_SPARK.Log.Error);
         Valid_Command_Line := False;
      end if;

      if Valid_Command_Line and Argument_Index = SPARK_Terminal.Argument_Count
      then
         CoAP_SPARK.Log.Put_Line ("URI is missing", CoAP_SPARK.Log.Error);
         Valid_Command_Line := False;
      end if;

      exit when not Valid_Command_Line;

      pragma Loop_Invariant (Argument_Index < SPARK_Terminal.Argument_Count);
      Argument_Index := @ + 1;
   end loop;

   if SPARK_Terminal.Argument (Argument_Index)'Length
     > CoAP_SPARK.Max_URI_Length
   then
      CoAP_SPARK.Log.Put_Line ("URI too long", CoAP_SPARK.Log.Error);
      Valid_Command_Line := False;
   end if;

   if not Valid_Command_Line then
      RFLX.RFLX_Types.Free (Payload);
      Usage;
      return;
   end if;

   Run_Session
     (URI_String => SPARK_Terminal.Argument (Argument_Index),
      Method     => Method,
      Payload    => Payload);

   RFLX.RFLX_Types.Free (Payload);

   -- This has no effect, but it is needed to avoid a linking error with
   -- SPARKLib in the validation profile.
   Workarounds.Check_Or_Fail;
end CoAP_Client;
