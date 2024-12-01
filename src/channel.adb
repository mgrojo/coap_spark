with Ada.Streams;
with GNAT.Sockets.Thin;
with Interfaces.C;

package body Channel with
   SPARK_Mode
is

   use type RFLX.RFLX_Builtin_Types.Index;
   use type Ada.Streams.Stream_Element_Offset;

   -- Eng/RecordFlux/RecordFlux#482
   -- Ada.Streams.Stream_Element_Array is not yet supported as buffer type and thus a conversion is needed.

   function To_Ada_Stream (Buffer : RFLX.RFLX_Builtin_Types.Bytes) return Ada.Streams.Stream_Element_Array with
      Pre =>
         Buffer'First = 1
         and then Buffer'Length <= Ada.Streams.Stream_Element_Offset'Last
   is
      Result : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
   begin
      for I in Result'Range loop
         Result (I) := Ada.Streams.Stream_Element (Buffer (RFLX.RFLX_Builtin_Types.Index (I)));
      end loop;
      return Result;
   end To_Ada_Stream;

   function To_RFLX_Bytes (Buffer : Ada.Streams.Stream_Element_Array) return RFLX.RFLX_Builtin_Types.Bytes with
      Pre =>
         Buffer'First = 1
         and then Buffer'Length <= Ada.Streams.Stream_Element_Offset (RFLX.RFLX_Builtin_Types.Index'Last)
   is
      Result : RFLX.RFLX_Builtin_Types.Bytes (1 .. Buffer'Length);
   begin
      for I in Result'Range loop
         Result (I) := RFLX.RFLX_Builtin_Types.Byte (Buffer (Ada.Streams.Stream_Element_Offset (I)));
      end loop;
      return Result;
   end To_RFLX_Bytes;

   procedure Initialize (Socket : out GNAT.Sockets.Socket_Type;
                         Port : GNAT.Sockets.Port_Type := Default_Port;
                         Server : Boolean := False) with
      SPARK_Mode => Off
   is
      use Interfaces;
      TTL_Option : aliased C.int := 64;
      IP_TTL : C.int := 2;
      Result : C.int;
      IPPROTO_IP : constant C.int := 0;
   begin
      GNAT.Sockets.Create_Socket (Socket => Socket,
                                  Family => GNAT.Sockets.Family_Inet,
                                  Mode => GNAT.Sockets.Socket_Datagram);

      Result := GNAT.Sockets.Thin.C_Setsockopt
         (S => C.int (GNAT.Sockets.To_C (Socket)),
          Level => IPPROTO_IP,
          Optname => IP_TTL,
          Optval => TTL_Option'Address,
          Optlen => C.int (TTL_Option'Size / 8));

      if Server then
         GNAT.Sockets.Bind_Socket
           (Socket  => Socket,
            Address => (Family => GNAT.Sockets.Family_Inet,
                        Addr => GNAT.Sockets.Any_Inet_Addr,
                        Port => Port));
      end if;
   end Initialize;

   procedure Connect (Socket : in out GNAT.Sockets.Socket_Type;
                      Server : String;
                      Port : GNAT.Sockets.Port_Type := Default_Port) with
   SPARK_Mode => Off
   is
   begin
      GNAT.Sockets.Connect_Socket
         (Socket => Socket,
          Server => (Family => GNAT.Sockets.Family_Inet,
                     Addr => GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Server)),
                     Port => Port));
   end Connect;

   procedure Send (Socket : in out GNAT.Sockets.Socket_Type;
                   Buffer :        RFLX.RFLX_Builtin_Types.Bytes) with
      SPARK_Mode => Off
   is
      Data : constant Ada.Streams.Stream_Element_Array (1 .. Buffer'Length) := To_Ada_Stream (Buffer);
      Unused_Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket (Socket => Socket,
                                Item => Data,
                                Last => Unused_Last);
   end Send;

   procedure Receive (Socket : in out GNAT.Sockets.Socket_Type;
                      Buffer :    out RFLX.RFLX_Builtin_Types.Bytes;
                      Length :    out RFLX.RFLX_Builtin_Types.Length) with
      SPARK_Mode => Off
   is
      Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Receive_Socket (Socket => Socket,
                                   Item => Data,
                                   Last => Last);
      Buffer := To_RFLX_Bytes (Data);
      Length := RFLX.RFLX_Builtin_Types.Length (Last);
   end Receive;

end Channel;
