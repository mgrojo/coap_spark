pragma SPARK_Mode;

with Ada.Command_Line;
with Ada.Text_IO;

with CoAP_SPARK.Channel;

with CoAP_SPARK.URI;

with GNAT.Sockets;

with Interfaces;

with RFLX.CoAP;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with RFLX.CoAP_Client.Session.FSM;
with RFLX.CoAP_Client.Session_Environment;

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
         := (others => 0);
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

   Skt : GNAT.Sockets.Socket_Type;
   Ctx : FSM.Context;

   Default_URI : constant String := "coap://coap.me/test";

begin

   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: coap_client <URI>");
      -- return; TODO
   end if;

   declare
      URI_String : constant String :=
       (if Ada.Command_Line.Argument_Count = 1
         then Ada.Command_Line.Argument (1)
         else Default_URI); -- TODO for debugging
      URI : constant CoAP_SPARK.URI.URI := CoAP_SPARK.URI.Create (URI_String);
   begin
      Ada.Text_IO.Put_Line ("Scheme: " & CoAP_SPARK.URI.Scheme (URI));
      Ada.Text_IO.Put_Line ("Host: " & CoAP_SPARK.URI.Host (URI));
      Ada.Text_IO.Put_Line ("Port:" & Interfaces.Unsigned_16'Image
                            (CoAP_SPARK.URI.Port (URI)));
      Ada.Text_IO.Put_Line ("Path: " & CoAP_SPARK.URI.Path (URI));
      Ada.Text_IO.Put_Line ("Query: " & CoAP_SPARK.URI.Query (URI));

      Channel.Initialize (Skt);
      Session_Environment.Initialize
        (Method        => RFLX.CoAP.Get,
         Server        => CoAP_SPARK.URI.Host (URI),
         Port          => CoAP_SPARK.URI.Port (URI),
         Path          => CoAP_SPARK.URI.Path (URI),
         Query         => CoAP_SPARK.URI.Query (URI),
         Session_State => Ctx.E);

      FSM.Initialize (Ctx);
      Channel.Connect (Socket => Skt,
                       Server => CoAP_SPARK.URI.Host (URI),
                       Port   => GNAT.Sockets.Port_Type
                                   (CoAP_SPARK.URI.Port (URI)));
   end;

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
   pragma Warnings (Off, "statement has no effect");
   pragma Warnings
      (Off, """Ctx"" is set by ""Finalize"" but not used after the call");
   FSM.Finalize (Ctx);
   pragma Warnings (On, "statement has no effect");
   pragma Warnings
      (On, """Ctx"" is set by ""Finalize"" but not used after the call");
end CoAP_Client;
