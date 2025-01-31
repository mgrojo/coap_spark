pragma SPARK_Mode;

with Ada.Command_Line;

with Coap_Client_Config;
with CoAP_SPARK.Channel;
with CoAP_SPARK.Log;
with CoAP_SPARK.Messages;
with CoAP_SPARK.URI;
with CoAP_SPARK.Utils;

with CoAP_Secure;

with Interfaces;

with RFLX.CoAP;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with RFLX.CoAP_Client.Session.FSM;
with RFLX.CoAP_Client.Session_Environment;

with Workarounds;

procedure CoAP_Client is
   package FSM renames RFLX.CoAP_Client.Session.FSM;
   package Session_Environment renames RFLX.CoAP_Client.Session_Environment;
   package Types renames RFLX.RFLX_Types;
   package Channel renames CoAP_SPARK.Channel;

   use type RFLX.CoAP_Client.Session_Environment.Status_Type;

   procedure Read (Ctx : FSM.Context;
                   Skt : in out CoAP_SPARK.Channel.Socket_Type) with
      Pre =>
         FSM.Initialized (Ctx)
         and then FSM.Has_Data (Ctx, FSM.C_Transport),
      Post =>
         FSM.Initialized (Ctx)
   is
      use type Types.Index;
      use type Types.Length;
      Buffer : Types.Bytes (Types.Index'First .. Types.Index'First + 4095)
         := [others => 0];
      Size : constant Types.Length := FSM.Read_Buffer_Size (Ctx, FSM.C_Transport);
   begin
      if Size = 0 then
         CoAP_SPARK.Log.Put_Line ("Read buffer size is 0", CoAP_SPARK.Log.Error);
         return;
      end if;
      if Buffer'Length < Size then
         CoAP_SPARK.Log.Put_Line ("Buffer too small", CoAP_SPARK.Log.Error);
         return;
      end if;
      FSM.Read
         (Ctx,
          FSM.C_Transport,
          Buffer (Buffer'First .. Buffer'First - 2 + Types.Index (Size + 1)));
      Channel.Send
         (Skt,
          Buffer (Buffer'First .. Buffer'First - 2 + Types.Index (Size + 1)));
   end Read;

   procedure Write (Ctx : in out FSM.Context;
                    Skt : in out CoAP_SPARK.Channel.Socket_Type) with
      Pre =>
         FSM.Initialized (Ctx)
         and then FSM.Needs_Data (Ctx, FSM.C_Transport),
      Post =>
         FSM.Initialized (Ctx)
   is
      use type Types.Index;
      use type Types.Length;
      Buffer : Types.Bytes (Types.Index'First .. Types.Index'First + 4095);
      Length : RFLX.RFLX_Builtin_Types.Length;
   begin
      Channel.Receive (Skt, Buffer, Length);
      if
         Length > 0
         and Length <= FSM.Write_Buffer_Size (Ctx, FSM.C_Transport)
      then
         FSM.Write
            (Ctx,
             FSM.C_Transport,
             Buffer (Buffer'First .. Buffer'First + RFLX.RFLX_Builtin_Types.Index (Length) - 1));
      end if;
   end Write;

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
         Put (" " & RFLX.CoAP.Method_Code'Image (I));
      end loop;
      CoAP_SPARK.Log.New_Line (Level => CoAP_SPARK.Log.Info);

      if Is_Failure then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Usage;

   Ctx : FSM.Context;

   Method : RFLX.CoAP.Method_Code := RFLX.CoAP.Get;
   Payload : RFLX.RFLX_Types.Bytes_Ptr := null;

   Argument_Index : Natural := 0;
begin

   if Ada.Command_Line.Argument_Count = 0
     or else Ada.Command_Line.Argument (1) = "-h"
     or else Ada.Command_Line.Argument (1) = "--help"
   then
      Usage (Is_Failure => False);
      return;
   end if;

   if Ada.Command_Line.Argument_Count = 1
     and then (Ada.Command_Line.Argument (1) = "-V"
      or else Ada.Command_Line.Argument (1) = "--version")
   then
      CoAP_SPARK.Log.Put (Coap_Client_Config.Crate_Name, CoAP_SPARK.Log.Info);
      CoAP_SPARK.Log.Put (" v", CoAP_SPARK.Log.Info);
      CoAP_SPARK.Log.Put_Line (Coap_Client_Config.Crate_Version, CoAP_SPARK.Log.Info);
      return;
   end if;

   while Argument_Index < Ada.Command_Line.Argument_Count loop
      Argument_Index := @ + 1;
      if Ada.Command_Line.Argument (Argument_Index) = "-m" then
         Argument_Index := @ + 1;
         begin
            Method :=
              RFLX.CoAP.Method_Code'Value
                (Ada.Command_Line.Argument (Argument_Index));
         exception
            when Constraint_Error =>
               CoAP_SPARK.Log.Put_Line ("Invalid method", CoAP_SPARK.Log.Error);
               Usage;
               return;
         end;
      elsif Ada.Command_Line.Argument (Argument_Index) = "-e" then
         Argument_Index := @ + 1;
         declare
            Payload_String : constant String :=
              Ada.Command_Line.Argument (Argument_Index);
         begin
            Payload :=
              new RFLX.RFLX_Types.Bytes'(1 .. Payload_String'Length => 0);
            CoAP_SPARK.Utils.Copy_String
              (Source => Payload_String, Target => Payload.all);
         exception
            when Constraint_Error =>
               CoAP_SPARK.Log.Put_Line ("Invalid payload", CoAP_SPARK.Log.Error);
               Usage;
               return;
         end;
      elsif Ada.Command_Line.Argument (Argument_Index) = "-v" then
         Argument_Index := @ + 1;
         begin
            CoAP_SPARK.Log.Set_Level
              (CoAP_SPARK.Log.Level_Type'Val
                 (CoAP_SPARK.Log.Level_Type'Pos (CoAP_SPARK.Log.Level_Type'Last) -
                    Integer'Value (Ada.Command_Line.Argument (Argument_Index))));
         exception
            when Constraint_Error =>
               CoAP_SPARK.Log.Put_Line ("Invalid verbosity level", CoAP_SPARK.Log.Error);
               Usage;
               return;
         end;
      elsif Ada.Command_Line.Argument (Argument_Index) = "-B" then
         -- This is allowed but not implemented. For compatibility to
         -- libcoap's coap-client, which uses this switch for specifying a
         -- timeout.
         Argument_Index := @ + 1;
      elsif Ada.Command_Line.Argument (Argument_Index) = "-k" or else
         Ada.Command_Line.Argument (Argument_Index) = "-u"
      then
         Argument_Index := @ + 1;

         if Argument_Index = Ada.Command_Line.Argument_Count  then
            CoAP_SPARK.Log.Put ("Missing argument for ", CoAP_SPARK.Log.Error);
            CoAP_SPARK.Log.Put_Line (Ada.Command_Line.Argument (Argument_Index - 1),
                                     CoAP_SPARK.Log.Error);
            Usage;
            return;
         end if;
         -- We will handle the PSK and Identity later, in the PSK_Callback
      elsif Argument_Index = Ada.Command_Line.Argument_Count then
         -- We will handle the URI later
         null;
      else
         CoAP_SPARK.Log.Put ("Invalid option: ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line (Ada.Command_Line.Argument (Argument_Index),
                                  CoAP_SPARK.Log.Error);
         Usage;
         return;
      end if;
   end loop;

   if Argument_Index > Ada.Command_Line.Argument_Count then
      CoAP_SPARK.Log.Put_Line ("URI is missing", CoAP_SPARK.Log.Error);
      Usage;
      return;
   end if;

   if Ada.Command_Line.Argument (Argument_Index) = "" or else
      Ada.Command_Line.Argument (Argument_Index)(1) = '-'
   then
      CoAP_SPARK.Log.Put ("Unrecognized option: ", CoAP_SPARK.Log.Error);
      CoAP_SPARK.Log.Put_Line (Ada.Command_Line.Argument (Argument_Index),
                               CoAP_SPARK.Log.Error);
      Usage;
      return;
   end if;

   declare
      URI_String : constant String :=
        Ada.Command_Line.Argument (Argument_Index);
      URI        : constant CoAP_SPARK.URI.URI :=
        CoAP_SPARK.URI.Create (URI_String);
      Skt : CoAP_SPARK.Channel.Socket_Type
            (Is_Secure => CoAP_SPARK.URI.Scheme (URI) = CoAP_SPARK.Secure_Scheme);
   begin
      if not CoAP_SPARK.URI.Is_Valid (URI) then
         CoAP_SPARK.Log.Put ("Invalid URI: ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line (URI_String, CoAP_SPARK.Log.Error);
         Usage;
         return;
      end if;

      CoAP_SPARK.Log.Put_Line ("Method: " & RFLX.CoAP.Method_Code'Image (Method));
      CoAP_SPARK.Log.Put_Line ("Scheme: " & CoAP_SPARK.URI.Scheme (URI));
      CoAP_SPARK.Log.Put_Line ("Host: " & CoAP_SPARK.URI.Host (URI));
      CoAP_SPARK.Log.Put_Line
        ("Port:" & Interfaces.Unsigned_16'Image (CoAP_SPARK.URI.Port (URI)));
      CoAP_SPARK.Log.Put_Line ("Path: " & CoAP_SPARK.URI.Path (URI));
      CoAP_SPARK.Log.Put_Line ("Query: " & CoAP_SPARK.URI.Query (URI));

      Channel.Initialize
        (Socket       => Skt,
         PSK_Callback => CoAP_Secure.PSK_Client_Callback'Access);

      Session_Environment.Initialize
        (Method        => Method,
         Server        => CoAP_SPARK.URI.Host (URI),
         Port          => CoAP_SPARK.URI.Port (URI),
         Path          => CoAP_SPARK.URI.Path (URI),
         Query         => CoAP_SPARK.URI.Query (URI),
         Payload       => Payload,
         Session_State => Ctx.E);

      if Ctx.E.Current_Status /= Session_Environment.OK then
         CoAP_SPARK.Log.Put_Line (Ctx.E.Current_Status'Image, CoAP_SPARK.Log.Error);
         return;
      end if;

      FSM.Initialize (Ctx);
      Channel.Connect
        (Socket => Skt,
         Server => CoAP_SPARK.URI.Host (URI),
         Port   => CoAP_SPARK.Channel.Port_Type (CoAP_SPARK.URI.Port (URI)));

      CoAP_SPARK.Log.Put_Line ("REQUEST: ");
      CoAP_SPARK.Messages.Print_Content (Ctx.E.Request_Content);

      while FSM.Active (Ctx) loop
         pragma Loop_Invariant (FSM.Initialized (Ctx));
         for C in FSM.Channel'Range loop
            pragma Loop_Invariant (FSM.Initialized (Ctx));
            if FSM.Has_Data (Ctx, C) then
               Read (Ctx, Skt);
            end if;
            if FSM.Needs_Data (Ctx, C) then
               Write (Ctx, Skt);
            end if;
         end loop;
         FSM.Run (Ctx);
      end loop;

      if not CoAP_SPARK.Channel.Is_Valid (Skt) then
         CoAP_SPARK.Log.Put_Line ("Communication problems.", CoAP_SPARK.Log.Error);
      end if;
      CoAP_SPARK.Channel.Finalize (Skt);
   end;

   CoAP_SPARK.Log.New_Line;
   if Ctx.E.Current_Status in RFLX.CoAP_Client.Session_Environment.OK then
      CoAP_SPARK.Log.Put_Line ("RESPONSE: ");

      case Ctx.E.Response_Codes.Code_Class is
         when RFLX.CoAP.Success =>
            CoAP_SPARK.Log.Put_Line
              ("Server answered with success.");
         when RFLX.CoAP.Client_Error =>
            CoAP_SPARK.Log.Put_Line
              ("Server answered with client error: "
               & CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes));
           CoAP_SPARK.Log.Put
              (CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes, Long => False) & " ",
               CoAP_SPARK.Log.Info);
         when RFLX.CoAP.Server_Error =>
            CoAP_SPARK.Log.Put_Line
              ("Server answered with server error: "
               & CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes));
           CoAP_SPARK.Log.Put
              (CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes, Long => False) & " ",
               CoAP_SPARK.Log.Info);
      end case;

      CoAP_SPARK.Messages.Print_Content (Ctx.E.Response_Content);
   else
      CoAP_SPARK.Log.Put ("Aborted with error: ", CoAP_SPARK.Log.Error);
      CoAP_SPARK.Log.Put_Line (Ctx.E.Current_Status'Image, CoAP_SPARK.Log.Error);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   pragma Warnings (Off, "statement has no effect");
   pragma
     Warnings
       (Off, """Ctx"" is set by ""Finalize"" but not used after the call");
   FSM.Finalize (Ctx);
   pragma Warnings (On, "statement has no effect");
   pragma
     Warnings
       (On, """Ctx"" is set by ""Finalize"" but not used after the call");

   RFLX.CoAP_Client.Session_Environment.Finalize (Ctx.E);

   -- This has no effect, but it is needed to avoid a linking error with
   -- SPARKLib in the validation profile.
   Workarounds.Check_Or_Fail;
end CoAP_Client;
