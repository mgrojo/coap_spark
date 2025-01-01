pragma SPARK_Mode;

with Ada.Command_Line;
with Ada.Text_IO;

with CoAP_SPARK.Channel;
with CoAP_SPARK.URI;
with CoAP_SPARK.Messages;
with CoAP_SPARK.Utils;

with GNAT.Sockets;

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

   procedure Read (Ctx : FSM.Context;
                   Skt : in out GNAT.Sockets.Socket_Type) with
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
         Ada.Text_IO.Put_Line ("Error: read buffer size is 0");
         return;
      end if;
      if Buffer'Length < Size then
         Ada.Text_IO.Put_Line ("Error: buffer too small");
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
                    Skt : in out GNAT.Sockets.Socket_Type) with
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
             Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Builtin_Types.Index (Length) - 1));
      end if;
   end Write;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Usage: coap_client [-m METHOD] [-e Payload] <URI>");
      Ada.Text_IO.Put ("  METHOD:");
      for I in RFLX.CoAP.Method_Code'Range loop
         Ada.Text_IO.Put (" " & RFLX.CoAP.Method_Code'Image (I));
      end loop;
      Ada.Text_IO.New_Line;
   end Usage;

   Skt : GNAT.Sockets.Socket_Type;
   Ctx : FSM.Context;

   Method : RFLX.CoAP.Method_Code := RFLX.CoAP.Get;
   Payload : RFLX.RFLX_Types.Bytes_Ptr := null;

   Argument_Index : Natural := 0;
begin

   if Ada.Command_Line.Argument_Count = 0
     or else Ada.Command_Line.Argument (1) = "-h"
   then
      Usage;
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
               Ada.Text_IO.Put_Line ("Error: Invalid method");
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
               Ada.Text_IO.Put_Line ("Error: Invalid payload");
               Usage;
               return;
         end;
      elsif Argument_Index = Ada.Command_Line.Argument_Count then
         -- We will handle the URI later
         null;
      else
         Ada.Text_IO.Put ("Error: Invalid option: ");
         Ada.Text_IO.Put_Line (Ada.Command_Line.Argument (Argument_Index));
         Usage;
         return;
      end if;
   end loop;

   if Argument_Index > Ada.Command_Line.Argument_Count then
      Ada.Text_IO.Put_Line ("Error: URI is missing");
      Usage;
      return;
   end if;

   declare
      URI_String : constant String :=
        Ada.Command_Line.Argument (Argument_Index);
      URI        : constant CoAP_SPARK.URI.URI :=
        CoAP_SPARK.URI.Create (URI_String);
   begin
      Ada.Text_IO.Put_Line ("Method: " & RFLX.CoAP.Method_Code'Image (Method));
      Ada.Text_IO.Put_Line ("Scheme: " & CoAP_SPARK.URI.Scheme (URI));
      Ada.Text_IO.Put_Line ("Host: " & CoAP_SPARK.URI.Host (URI));
      Ada.Text_IO.Put_Line
        ("Port:" & Interfaces.Unsigned_16'Image (CoAP_SPARK.URI.Port (URI)));
      Ada.Text_IO.Put_Line ("Path: " & CoAP_SPARK.URI.Path (URI));
      Ada.Text_IO.Put_Line ("Query: " & CoAP_SPARK.URI.Query (URI));

      Channel.Initialize (Skt);
      Session_Environment.Initialize
        (Method        => Method,
         Server        => CoAP_SPARK.URI.Host (URI),
         Port          => CoAP_SPARK.URI.Port (URI),
         Path          => CoAP_SPARK.URI.Path (URI),
         Query         => CoAP_SPARK.URI.Query (URI),
         Payload       => Payload,
         Session_State => Ctx.E);

      FSM.Initialize (Ctx);
      Channel.Connect
        (Socket => Skt,
         Server => CoAP_SPARK.URI.Host (URI),
         Port   => GNAT.Sockets.Port_Type (CoAP_SPARK.URI.Port (URI)));
   end;

   Ada.Text_IO.Put_Line ("REQUEST: ");
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

   Ada.Text_IO.New_Line;
   if Ctx.E.Current_Status in RFLX.CoAP_Client.Session_Environment.OK then
      Ada.Text_IO.Put_Line ("RESPONSE: ");

      case Ctx.E.Response_Codes.Code_Class is
         when RFLX.CoAP.Success =>
            Ada.Text_IO.Put_Line
              ("Server answered with success.");
         when RFLX.CoAP.Client_Error =>
            Ada.Text_IO.Put_Line
              ("Server answered with client error: "
               & CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes));
         when RFLX.CoAP.Server_Error =>
            Ada.Text_IO.Put_Line
              ("Server answered with server error: "
               & CoAP_SPARK.Messages.Image (Ctx.E.Response_Codes));
      end case;

      CoAP_SPARK.Messages.Print_Content (Ctx.E.Response_Content);
   else
      Ada.Text_IO.Put ("Error: ");
      Ada.Text_IO.Put_Line (Ctx.E.Current_Status'Image);
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

   RFLX.RFLX_Types.Free (Payload);

   -- This has no effect, but it is needed to avoid a linking error in the
   -- validation profile.
   Workarounds.Check_Or_Fail;
end CoAP_Client;
