with Ada.Streams;
with Ada.Unchecked_Conversion;
with Interfaces.C;

package body CoAP_SPARK.Channel
  with SPARK_Mode
is

   use type RFLX.RFLX_Builtin_Types.Index;
   use type Ada.Streams.Stream_Element_Offset;
   use type Interfaces.C.size_t;

   -- Ada.Streams.Stream_Element_Array is not yet supported as buffer type and
   -- thus a conversion is needed.

   function To_Ada_Stream
     (Buffer : RFLX.RFLX_Builtin_Types.Bytes)
      return Ada.Streams.Stream_Element_Array
   with
     Pre =>
       Buffer'First = 1
       and then Buffer'Length <= Ada.Streams.Stream_Element_Offset'Last
   is
      Result : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
   begin
      for I in Result'Range loop
         Result (I) :=
           Ada.Streams.Stream_Element
             (Buffer (RFLX.RFLX_Builtin_Types.Index (I)));
      end loop;
      return Result;
   end To_Ada_Stream;

   function To_Char is new Ada.Unchecked_Conversion
     (Source => RFLX.RFLX_Builtin_Types.Byte,
      Target => Interfaces.C.char);

   function To_RFLX_Byte is new Ada.Unchecked_Conversion
     (Source => Interfaces.C.char,
      Target => RFLX.RFLX_Builtin_Types.Byte);

   function To_C
     (Buffer : RFLX.RFLX_Builtin_Types.Bytes)
      return WolfSSL.Byte_Array
   with
     Pre =>
       Buffer'First = 1
       and then Buffer'Length <= Interfaces.C.size_t'Last
   is
      Result : WolfSSL.Byte_Array (1 .. Buffer'Length);
   begin
      for I in Result'Range loop
         Result (I) :=
           To_Char
             (Buffer (RFLX.RFLX_Builtin_Types.Index (I)));
      end loop;
      return Result;
   end To_C;

   function To_RFLX_Bytes
     (Buffer : Ada.Streams.Stream_Element_Array)
      return RFLX.RFLX_Builtin_Types.Bytes
   with
     Pre =>
       Buffer'First = 1
       and then Buffer'Length
                <= Ada.Streams.Stream_Element_Offset
                     (RFLX.RFLX_Builtin_Types.Index'Last)
   is
      Result : RFLX.RFLX_Builtin_Types.Bytes (1 .. Buffer'Length);
   begin
      for I in Result'Range loop
         Result (I) :=
           RFLX.RFLX_Builtin_Types.Byte
             (Buffer (Ada.Streams.Stream_Element_Offset (I)));
      end loop;
      return Result;
   end To_RFLX_Bytes;

   function To_RFLX_Bytes
     (Buffer : WolfSSL.Byte_Array)
      return RFLX.RFLX_Builtin_Types.Bytes
   with
     Pre =>
       Buffer'First = 1
       and then Buffer'Length
                <= Natural
                     (RFLX.RFLX_Builtin_Types.Index'Last)
   is
      Result : RFLX.RFLX_Builtin_Types.Bytes (1 .. Buffer'Length);
   begin
      for I in Result'Range loop
         Result (I) :=
           To_RFLX_Byte
             (Buffer (Interfaces.C.size_t (I)));
      end loop;
      return Result;
   end To_RFLX_Bytes;

   procedure Initialize
     (Socket       : out Socket_Type;
      Port         : Port_Type := Default_Port;
      PSK_Callback : WolfSSL.PSK_Client_Callback := null;
      Server       : Boolean := False)
   with SPARK_Mode => Off
   is
   begin

      GNAT.Sockets.Create_Socket
        (Socket => Socket.Attached_Socket,
         Family => GNAT.Sockets.Family_Inet,
         Mode   => GNAT.Sockets.Socket_Datagram);

      if Server then
         GNAT.Sockets.Bind_Socket
           (Socket  => Socket.Attached_Socket,
            Address =>
              (Family => GNAT.Sockets.Family_Inet,
               Addr   => GNAT.Sockets.Any_Inet_Addr,
               Port   => GNAT.Sockets.Port_Type (Port)));
      end if;

      if Socket.Is_Secure then
         Socket.Result := WolfSSL.Initialize;

         if Socket.Result /= WolfSSL.Success then
            return;
         end if;

         --  Create and initialize WOLFSSL_CTX.
         WolfSSL.Create_Context
           (Method => WolfSSL.DTLSv1_2_Client_Method, Context => Socket.Ctx);

         if WolfSSL.Is_Valid (Socket.Ctx) then

            WolfSSL.Create_WolfSSL (Context => Socket.Ctx, Ssl => Socket.Ssl);

            if WolfSSL.Is_Valid (Socket.Ssl) then
               -- Use PSK for authentication.
               WolfSSL.Set_PSK_Client_Callback
                 (Ssl      => Socket.Ssl,
                  Callback => PSK_Callback);
            end if;
         end if;
      end if;
   end Initialize;

   procedure Connect
     (Socket : in out Socket_Type;
      Server : String;
      Port   : Port_Type := Default_Port)
   with SPARK_Mode => Off
   is
      Address : constant GNAT.Sockets.Sock_Addr_Type :=
        (Family => GNAT.Sockets.Family_Inet,
         Addr   => GNAT.Sockets.Addresses
                      (GNAT.Sockets.Get_Host_By_Name (Server)),
         Port   => GNAT.Sockets.Port_Type (Port));
   begin
      GNAT.Sockets.Connect_Socket
        (Socket => Socket.Attached_Socket,
         Server => Address);

      if Socket.Is_Secure then
         Socket.Result := WolfSSL.DTLS_Set_Peer
                         (Ssl     => Socket.Ssl,
                          Address => Address);

         --  Attach wolfSSL to the socket.
         Socket.Result := WolfSSL.Attach
                      (Ssl    => Socket.Ssl,
                       Socket => GNAT.Sockets.To_C (Socket.Attached_Socket));
      end if;
   end Connect;

   procedure Send
     (Socket : in out Socket_Type;
      Buffer : RFLX.RFLX_Builtin_Types.Bytes)
   with SPARK_Mode => Off
   is
   begin
      if Socket.Is_Secure then
         declare
            Data : constant WolfSSL.Byte_Array (1 .. Buffer'Length) :=
              To_C (Buffer);
            Output : WolfSSL.Write_Result;
         begin
            Output := WolfSSL.Write (Ssl  => Socket.Ssl,
                                     Data => Data);
            if not Output.Success then
               Socket.Result := Output.Code;
            elsif Output.Bytes_Written /= Buffer'Length then
               Socket.Result := WolfSSL.Failure;
            end if;
         end;
      else
         declare
            Data        :
              constant Ada.Streams.Stream_Element_Array (1 .. Buffer'Length) :=
                To_Ada_Stream (Buffer);
            Unused_Last : Ada.Streams.Stream_Element_Offset;
         begin
            GNAT.Sockets.Send_Socket
              (Socket => Socket.Attached_Socket,
               Item   => Data,
               Last   => Unused_Last);
         end;
      end if;
   end Send;

   procedure Receive
     (Socket : in out Socket_Type;
      Buffer : out RFLX.RFLX_Builtin_Types.Bytes;
      Length : out RFLX.RFLX_Builtin_Types.Length)
   with SPARK_Mode => Off
   is

   begin
      if Socket.Is_Secure then
         declare
            Input : WolfSSL.Read_Result;
         begin
            Input := WolfSSL.Read (Socket.Ssl);
            if Input.Success and then
               Input.Buffer'Length <= Buffer'Length
            then
               Length := RFLX.RFLX_Builtin_Types.Length (Input.Last);
               Buffer (1 .. RFLX.RFLX_Builtin_Types.Index (Length)) :=
                 To_RFLX_Bytes (Input.Buffer);
            else
               Socket.Result :=
                 (if not Input.Success then Input.Code else WolfSSL.Failure);
               Length := 0;
            end if;
         end;
      else
         declare
            Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
            Last : Ada.Streams.Stream_Element_Offset;
         begin

            GNAT.Sockets.Receive_Socket
              (Socket => Socket.Attached_Socket, Item => Data, Last => Last);

            Buffer := To_RFLX_Bytes (Data);
            Length := RFLX.RFLX_Builtin_Types.Length (Last);
         end;
      end if;
   end Receive;

   procedure Finalize
     (Socket : in out Socket_Type)
   is
   begin
      GNAT.Sockets.Close_Socket (Socket => Socket.Attached_Socket);
      if Socket.Is_Secure then
         WolfSSL.Free (Ssl => Socket.Ssl);
         WolfSSL.Free (Context => Socket.Ctx);
      end if;
   end Finalize;

end CoAP_SPARK.Channel;
