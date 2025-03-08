private with SPARK_Sockets;

with WolfSSL;

with RFLX.RFLX_Builtin_Types;

package CoAP_SPARK.Channel with
   SPARK_Mode
is

   -- Declare the discriminant Is_Secure to True if you want to use DTLS.
   type Socket_Type (Is_Secure : Boolean) is limited private;

   type Port_Type is mod 2 ** 16;

   procedure Initialize (Socket : out Socket_Type;
                         Port   : Port_Type := Default_Port;
                         PSK_Callback : WolfSSL.PSK_Client_Callback := null;
                         Server : Boolean := False) with
      Pre => (if Socket.Is_Secure then PSK_Callback not in null else True),
      Relaxed_Initialization => Socket,
      Global =>
         null;

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

   use type WolfSSL.Subprogram_Result;

   function Has_Attached_Socket (Socket : Socket_Type) return Boolean is
      (Socket.Attached_Socket.Exists);

   function Is_Valid (Socket : Socket_Type) return Boolean is
      (Socket.Attached_Socket.Exists
       and then
       (if Socket.Is_Secure then Socket.Result = WolfSSL.Success and then
          WolfSSL.Is_Valid (Socket.Ssl) and then
          WolfSSL.Is_Valid (Socket.Ctx)));

end CoAP_SPARK.Channel;
