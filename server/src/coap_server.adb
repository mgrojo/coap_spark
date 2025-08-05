pragma SPARK_Mode;

with Coap_Server_Config;

with Secure_Server;

with CoAP_SPARK.Channel;
with CoAP_SPARK.Server_Session;
with CoAP_SPARK.Log;
with CoAP_SPARK.Messages;
with CoAP_SPARK.URI;
with CoAP_SPARK.Utils;

with Interfaces;

with RFLX.CoAP_Server.Main_Loop_Environment;
with RFLX.CoAP_Server.Main_Loop.FSM;
with RFLX.CoAP;
with RFLX.RFLX_Types;

-- The following package is used, instead of Ada.Command_Line, for the reasons
-- explained in the package itself (related to the use of SPARK).
with Ada.Command_Line;

with Server_Handling;

-- with Workarounds;

procedure CoAP_Server is
   package FSM renames RFLX.CoAP_Server.Main_Loop.FSM;
   package Main_Loop_Environment renames RFLX.CoAP_Server.Main_Loop_Environment;
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
      Put ("Usage: coap_server [-p PORT] [-A <Address>] ");
      Put_Line ("[-k <PSK>] [-u <Identity>] [-v <verbosity>] ");
      
      Ada.Command_Line.Set_Exit_Status
        (Code =>
           (if Is_Failure
            then Ada.Command_Line.Failure
            else Ada.Command_Line.Success));
   end Usage;

   procedure Run_Session
     (Method : RFLX.CoAP.Method_Code;
      URI_String : String;
      Payload : in out CoAP_SPARK.Messages.Payload_Ptr)
   with
      Pre =>
        URI_String'Length <= CoAP_SPARK.Max_URI_Length
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

      Secure_Server.Initialize (Socket => Skt);
      if not CoAP_SPARK.Channel.Is_Valid (Skt) then
         CoAP_SPARK.Log.Put_Line
           ("Communication problems.", CoAP_SPARK.Log.Error);
         RFLX.RFLX_Types.Free (Payload);
         return;
      end if;

      Main_Loop_Environment.Initialize
        (Request_Handler => Server_Handling.Handle_Request'Access,
         Session_State   => Ctx.E);

      if Ctx.E.Current_Status /= CoAP_SPARK.OK then
         CoAP_SPARK.Log.Put_Line
           (Ctx.E.Current_Status'Image, CoAP_SPARK.Log.Error);
         RFLX.RFLX_Types.Free (Payload);
         --  Main_Loop_Environment.Finalize (Ctx.E);
         --  pragma Assert (Main_Loop_Environment.Is_Finalized (Ctx.E));
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
         --  Main_Loop_Environment.Finalize (Ctx.E);
         --  pragma Assert (Main_Loop_Environment.Is_Finalized (Ctx.E));
         return;
      end if;

      CoAP_SPARK.Server_Session.Run_Session_Loop (Ctx, Skt);

      CoAP_SPARK.Channel.Finalize (Skt);
      pragma Assert (not CoAP_SPARK.Channel.Is_Valid (Skt));

      FSM.Finalize (Ctx);
      pragma Assert (FSM.Uninitialized (Ctx));

      --  Main_Loop_Environment.Finalize (Ctx.E);
      --  pragma Assert (Main_Loop_Environment.Is_Finalized (Ctx.E));
   end Run_Session;

   Method : RFLX.CoAP.Method_Code := RFLX.CoAP.Get;
   Payload : CoAP_SPARK.Messages.Payload_Ptr := null;

   Argument_Index : Natural := 1;

   Valid_Command_Line : Boolean := True;
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
      CoAP_SPARK.Log.Put (Coap_Server_Config.Crate_Name, CoAP_SPARK.Log.Info);
      CoAP_SPARK.Log.Put (" v", CoAP_SPARK.Log.Info);
      CoAP_SPARK.Log.Put_Line
        (Coap_Server_Config.Crate_Version, CoAP_SPARK.Log.Info);
      return;
   end if;

   while Argument_Index < Ada.Command_Line.Argument_Count loop

      if Ada.Command_Line.Argument (Argument_Index) = "-m" then
         Argument_Index := @ + 1;
         if CoAP_SPARK.Utils.Is_Valid_As_Method
              (Ada.Command_Line.Argument (Argument_Index))
         then
            Method := CoAP_SPARK.Utils.Value
                (Ada.Command_Line.Argument (Argument_Index));
         else
            CoAP_SPARK.Log.Put_Line ("Invalid method", CoAP_SPARK.Log.Error);
            Valid_Command_Line := False;
         end if;

      elsif Ada.Command_Line.Argument (Argument_Index) = "-e" then
         Argument_Index := @ + 1;
         declare
            Payload_Length : constant Natural :=
              Ada.Command_Line.Argument (Argument_Index)'Length;
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
                 (Source => Ada.Command_Line.Argument (Argument_Index),
                  Target => Payload.all);
            end if;
         end;

      elsif Ada.Command_Line.Argument (Argument_Index) = "-v" then
         Argument_Index := @ + 1;
         if CoAP_SPARK.Utils.Valid_Natural_Values.Is_Valid_As_Number
              (Ada.Command_Line.Argument (Argument_Index))
         then
            declare
               Verbosity_Number : constant Natural :=
                 CoAP_SPARK.Utils.Value
                   (Ada.Command_Line.Argument (Argument_Index));
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

      elsif Ada.Command_Line.Argument (Argument_Index) = "-B" then
         -- This is allowed but not implemented. For compatibility to
         -- libcoap's coap-server, which uses this switch for specifying a
         -- timeout.
         Argument_Index := @ + 1;

      elsif Ada.Command_Line.Argument (Argument_Index) = "-k"
        or else Ada.Command_Line.Argument (Argument_Index) = "-u"
      then
         Argument_Index := @ + 1;

         if Argument_Index = Ada.Command_Line.Argument_Count then
            CoAP_SPARK.Log.Put ("Missing argument for ", CoAP_SPARK.Log.Error);
            CoAP_SPARK.Log.Put_Line
              (Ada.Command_Line.Argument (Argument_Index - 1),
               CoAP_SPARK.Log.Error);
            Valid_Command_Line := False;
         end if;
         -- We will handle the PSK and Identity later, in the PSK_Callback
      else
         CoAP_SPARK.Log.Put ("Invalid option: ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line
           (Ada.Command_Line.Argument (Argument_Index), CoAP_SPARK.Log.Error);
         Valid_Command_Line := False;
      end if;

      if Valid_Command_Line and Argument_Index = Ada.Command_Line.Argument_Count
      then
         CoAP_SPARK.Log.Put_Line ("URI is missing", CoAP_SPARK.Log.Error);
         Valid_Command_Line := False;
      end if;

      exit when not Valid_Command_Line;

      pragma Loop_Invariant (Argument_Index < Ada.Command_Line.Argument_Count);
      Argument_Index := @ + 1;
   end loop;

   if Ada.Command_Line.Argument (Argument_Index)'Length
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
     (URI_String => Ada.Command_Line.Argument (Argument_Index),
      Method     => Method,
      Payload    => Payload);

   RFLX.RFLX_Types.Free (Payload);

   -- This has no effect, but it is needed to avoid a linking error with
   -- SPARKLib in the validation profile.
--    Workarounds.Check_Or_Fail;
end CoAP_Server;
