with Ada.Streams;
with Ada.Unchecked_Conversion;

with GNAT.Sockets;

with Interfaces.C;

package body CoAP_SPARK.Channel
  with SPARK_Mode
is

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
       and then Buffer'Last
                <= Ada.Streams.Stream_Element_Offset
                     (RFLX.RFLX_Builtin_Types.Index'Last),
      Post =>
        To_RFLX_Bytes'Result'Length = Buffer'Length
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
       and then Buffer'Last
                <= Interfaces.C.size_t (RFLX.RFLX_Builtin_Types.Index'Last),
     Post =>
       To_RFLX_Bytes'Result'Length = Buffer'Length
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
   is
      Result : SPARK_Sockets.Subprogram_Result;
   begin

      Socket.Attached_Socket := (Exists => False);

      SPARK_Sockets.Create_Datagram_Socket
        (Socket => Socket.Attached_Socket);

      if not Socket.Attached_Socket.Exists then
         return;
      end if;

      if Server then
         Result := SPARK_Sockets.Bind_Socket
           (Socket  => Socket.Attached_Socket.Socket,
            Address =>
              (Family => SPARK_Sockets.Family_Inet,
               Addr   => SPARK_Sockets.Any_Inet_Addr,
               Port   => SPARK_Sockets.Port_Type (Port)));

         if Result /= SPARK_Sockets.Success then
            return;
         end if;

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

   use type SPARK_Sockets.Family_Type;

   function Inet_Address (Host_Name : String) return SPARK_Sockets.Optional_Inet_Addr;

   function Inet_Address (Host_Name : String) return SPARK_Sockets.Optional_Inet_Addr
   with SPARK_Mode => Off
   is
   begin
      return (Exists => True,
              Addr   => GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Host_Name)));
   exception
      when others =>
         return (Exists => False);
   end Inet_Address;

   procedure Connect
     (Socket : in out Socket_Type;
      Server : String;
      Port   : Port_Type := Default_Port)
   is
      Inet_Addr : SPARK_Sockets.Optional_Inet_Addr;
      Address : GNAT.Sockets.Sock_Addr_Type;
      Result : SPARK_Sockets.Subprogram_Result;
   begin

      Inet_Addr := Inet_Address (Server);
      if not Inet_Addr.Exists or else
         (Inet_Addr.Exists and then Inet_Addr.Addr.Family /= SPARK_Sockets.Family_Inet)
      then
         return;
      end if;

      Address := (Family => SPARK_Sockets.Family_Inet,
                  Addr   => Inet_Addr.Addr,
                  Port   => SPARK_Sockets.Port_Type (Port));

      Result := SPARK_Sockets.Connect_Socket
        (Socket => Socket.Attached_Socket.Socket,
         Server => Address);

      if Result /= SPARK_Sockets.Success then
         return;
      end if;

      if Socket.Is_Secure then
         Socket.Result := WolfSSL.DTLS_Set_Peer
                         (Ssl     => Socket.Ssl,
                          Address => Address);

         if Socket.Result /= SPARK_Sockets.Success then
            return;
         end if;

         --  Attach wolfSSL to the socket.
         Socket.Result := WolfSSL.Attach
                      (Ssl    => Socket.Ssl,
                       Socket => SPARK_Sockets.To_C (Socket.Attached_Socket.Socket));
      end if;
   end Connect;

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array)
      with Pre => Is_Valid (Socket);

   procedure Send_Socket
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   with SPARK_Mode => Off
   is
      Unused_Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket
        (Socket => Socket.Attached_Socket.Socket, Item => Item, Last => Unused_Last);
   end Send_Socket;

   procedure Send
     (Socket : in out Socket_Type;
      Buffer : RFLX.RFLX_Builtin_Types.Bytes)
   is
   begin
      if Socket.Is_Secure then
         declare
            Data : constant WolfSSL.Byte_Array :=
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
              constant Ada.Streams.Stream_Element_Array :=
                To_Ada_Stream (Buffer);
         begin
            Send_Socket
              (Socket => Socket,
               Item   => Data);
         end;
      end if;
   end Send;

   procedure Receive_Socket
     (Socket : Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   with
      Relaxed_Initialization => Item,
      Pre => Is_Valid (Socket),
      Post => Last in Item'First - 1 .. Item'Last and then
              Item (Item'First .. Last)'Initialized;

   procedure Receive_Socket
     (Socket : Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   with SPARK_Mode => Off
   is
   begin
      GNAT.Sockets.Receive_Socket
        (Socket => Socket.Attached_Socket.Socket, Item => Item, Last => Last);
   end Receive_Socket;

   procedure Receive
     (Socket : in out Socket_Type;
      Buffer : out RFLX.RFLX_Builtin_Types.Bytes;
      Length : out RFLX.RFLX_Builtin_Types.Length)
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
            Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length)
               with Relaxed_Initialization;
            Last : Ada.Streams.Stream_Element_Offset;
         begin

            Receive_Socket
              (Socket => Socket, Item => Data, Last => Last);

            pragma Assert (Data'Length = Buffer'Length);
            Buffer (Buffer'First .. RFLX.RFLX_Builtin_Types.Index'Base (Last)) :=
               To_RFLX_Bytes (Data (Data'First .. Last));
            Length := RFLX.RFLX_Builtin_Types.Length (Last);
         end;
      end if;
   end Receive;

   procedure Finalize
     (Socket : in out Socket_Type)
   is
   begin
      if Has_Attached_Socket (Socket) then
         SPARK_Sockets.Close_Socket (Socket => Socket.Attached_Socket);
      end if;

      if Socket.Is_Secure then
         if WolfSSL.Is_Valid (Socket.Ssl) then
            WolfSSL.Free (Ssl => Socket.Ssl);
         end if;
         if WolfSSL.Is_Valid (Socket.Ctx) then
            WolfSSL.Free (Context => Socket.Ctx);
         end if;
      end if;
   end Finalize;

end CoAP_SPARK.Channel;
