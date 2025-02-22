with CoAP_SPARK.Log;

with RFLX.CoAP_Client.Session_Environment;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;

package body CoAP_SPARK.Client_Session
   with SPARK_Mode
is

   package Session_Environment renames RFLX.CoAP_Client.Session_Environment;
   package Types renames RFLX.RFLX_Types;
   package Channel renames CoAP_SPARK.Channel;

   use type RFLX.CoAP_Client.Session_Environment.Status_Type;
   use type Types.Index;

   procedure Read (Ctx : FSM.Context;
                   Skt : in out CoAP_SPARK.Channel.Socket_Type) with
      Pre =>
         FSM.Initialized (Ctx)
         and then FSM.Has_Data (Ctx, FSM.C_Transport)
         and then CoAP_SPARK.Channel.Is_Valid (Skt),
      Post =>
         FSM.Initialized (Ctx)
   is
      use type Types.Length;
      Buffer_Length : constant := 4095;
      Buffer : Types.Bytes (Types.Index'First .. Types.Index'First + Buffer_Length)
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
         and then FSM.Needs_Data (Ctx, FSM.C_Transport)
         and then CoAP_SPARK.Channel.Is_Valid (Skt),
      Post =>
         FSM.Initialized (Ctx)
      is
      use type Types.Length;
      Buffer : Types.Bytes (Types.Index'First .. Types.Index'First + 4095);
      Length : RFLX.RFLX_Builtin_Types.Length;
   begin
      Channel.Receive (Skt, Buffer, Length);
      if
         Length > 0
         and then Length <= FSM.Write_Buffer_Size (Ctx, FSM.C_Transport)
      then
         FSM.Write
            (Ctx,
             FSM.C_Transport,
             Buffer (Buffer'First .. Buffer'First + RFLX.RFLX_Builtin_Types.Index (Length) - 1));
      end if;
   end Write;

   procedure Run_Session_Loop
      (Ctx : in out FSM.Context;
       Skt : in out CoAP_SPARK.Channel.Socket_Type)
   is
   begin

      while FSM.Active (Ctx) loop
         pragma Loop_Invariant (FSM.Initialized (Ctx));
         pragma Loop_Invariant (CoAP_SPARK.Channel.Is_Valid (Skt));
         for C in FSM.Channel'Range loop
            pragma Loop_Invariant (FSM.Initialized (Ctx));
            exit when not CoAP_SPARK.Channel.Is_Valid (Skt);
            if FSM.Has_Data (Ctx, C) then
               Read (Ctx, Skt);
            end if;
            exit when not CoAP_SPARK.Channel.Is_Valid (Skt);
            if FSM.Needs_Data (Ctx, C) then
               Write (Ctx, Skt);
            end if;
         end loop;
         exit when not CoAP_SPARK.Channel.Is_Valid (Skt);
         FSM.Run (Ctx);
      end loop;

      if not CoAP_SPARK.Channel.Is_Valid (Skt) then
         CoAP_SPARK.Log.Put_Line ("Communication problems.", CoAP_SPARK.Log.Error);
         Ctx.E.Current_Status :=
           Session_Environment.Communication_Problems;
      end if;

   end Run_Session_Loop;

end CoAP_SPARK.Client_Session;