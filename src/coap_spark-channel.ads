private with SPARK_Sockets;

with WolfSSL;

with RFLX.RFLX_Builtin_Types;

package CoAP_SPARK.Channel with
   SPARK_Mode
is

   -- Declare the discriminant Is_Secure to True if you want to use DTLS.
   type Socket_Type (Is_Secure : Boolean) is limited private;
   type Address_Type is private;

   type Port_Type is mod 2 ** 16;

   procedure Initialize
     (Socket              : out Socket_Type;
      Port                : Port_Type := Default_Port;
      PSK_Client_Callback : WolfSSL.PSK_Client_Callback := null;
      PSK_Server_Callback : WolfSSL.PSK_Server_Callback := null;
      Server              : Boolean := False)
   with
     Pre                    =>
       (if Socket.Is_Secure
        then
          (not Server and then PSK_Client_Callback not in null)
          xor (Server and then PSK_Server_Callback not in null)
        else
          (PSK_Client_Callback in null and then PSK_Server_Callback in null)),
     Relaxed_Initialization => Socket,
     Global                 => null;

   function Is_Valid (Socket : Socket_Type) return Boolean with
      Global =>
         null;

   procedure Connect (Socket : in out Socket_Type;
                      Server : String;
                      Port : Port_Type := Default_Port) with
      Pre => Is_Valid (Socket),
      Global =>
      null;

   use type RFLX.RFLX_Builtin_Types.Index;

   procedure Send (Socket : in out Socket_Type;
                   Buffer :        RFLX.RFLX_Builtin_Types.Bytes) with
      Pre => Is_Valid (Socket) and then Buffer'First = 1,
      Global =>
         null;

   procedure Send_To (Socket : in out Socket_Type;
                      Buffer : RFLX.RFLX_Builtin_Types.Bytes;
                      To     : Address_Type) with
      Pre => Is_Valid (Socket) and then Buffer'First = 1 and then not Socket.Is_Secure,
      Global =>
         null;

   use type RFLX.RFLX_Builtin_Types.Length;

   procedure Receive (Socket : in out Socket_Type;
                      Buffer :    out RFLX.RFLX_Builtin_Types.Bytes;
                      Length :    out RFLX.RFLX_Builtin_Types.Length) with
      Relaxed_Initialization => (Buffer),
      Pre => Is_Valid (Socket) and then Buffer'First = 1,
      Post =>
         Length <= Buffer'Length and then
         Buffer (Buffer'First .. RFLX.RFLX_Builtin_Types.Index'Base (Length))'Initialized,
      Global =>
         null;

   procedure Receive (Socket : in out Socket_Type;
                      Buffer :    out RFLX.RFLX_Builtin_Types.Bytes;
                      Length :    out RFLX.RFLX_Builtin_Types.Length;
                      From   :    out Address_Type) with
      Relaxed_Initialization => (Buffer),
      Pre => Is_Valid (Socket) and then Buffer'First = 1 and then not Socket.Is_Secure,
      Post =>
         Length <= Buffer'Length and then
         Buffer (Buffer'First .. RFLX.RFLX_Builtin_Types.Index'Base (Length))'Initialized,
      Global =>
         null;

   function Has_Attached_Socket (Socket : Socket_Type) return Boolean;

   procedure Finalize
     (Socket : in out Socket_Type) with
        Global => null,
        Post => not Is_Valid (Socket);
private

   type Socket_Type (Is_Secure : Boolean) is record
      Attached_Socket : SPARK_Sockets.Optional_Socket;
      case Is_Secure is
         when True =>
            Ssl    : WolfSSL.WolfSSL_Type;
            Ctx    : WolfSSL.Context_Type;
            Result : WolfSSL.Subprogram_Result;
         when False =>
            null;
      end case;
   end record;

   type Address_Type is
   record
      Sock_Addr : SPARK_Sockets.Sock_Addr_Type;
   end record;

   use type WolfSSL.Subprogram_Result;

   function Has_Attached_Socket (Socket : Socket_Type) return Boolean is
      (Socket.Attached_Socket.Exists);

   function Is_Valid (Socket : Socket_Type) return Boolean is
      (Has_Attached_Socket (Socket)
       and then
       (if Socket.Is_Secure then Socket.Result = WolfSSL.Success and then
          WolfSSL.Is_Valid (Socket.Ssl) and then
          WolfSSL.Is_Valid (Socket.Ctx)));

end CoAP_SPARK.Channel;
