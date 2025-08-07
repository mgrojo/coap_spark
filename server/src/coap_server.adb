pragma SPARK_Mode;

with Coap_Server_Config;

with Secure_Server;

with CoAP_SPARK.Channel;
with CoAP_SPARK.Server_Session;
with CoAP_SPARK.Log;
with CoAP_SPARK.Utils;

with RFLX.CoAP_Server.Main_Loop_Environment;
with RFLX.CoAP_Server.Main_Loop.FSM;

-- The following package is used, instead of Ada.Command_Line, for the reasons
-- explained in the package itself (related to the use of SPARK).
with SPARK_Terminal;

with Server_Handling;

-- with Workarounds;

procedure CoAP_Server is
   package FSM renames RFLX.CoAP_Server.Main_Loop.FSM;
   package Main_Loop_Environment renames RFLX.CoAP_Server.Main_Loop_Environment;

   use type CoAP_SPARK.Status_Type;

   procedure Usage (Is_Failure : Boolean := True) is
      procedure Put (Text : String;
                     Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Info)
         renames CoAP_SPARK.Log.Put;
      procedure Put_Line (Text : String;
                          Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Info)
         renames CoAP_SPARK.Log.Put_Line;
   begin
      Put ("Usage: coap_server [-p PORT] [-A <Address>] "); -- TODO add Address
      Put_Line ("[-k <PSK>] [-u <Identity>] [-v <verbosity>] ");
      
      SPARK_Terminal.Set_Exit_Status
        (Status =>
           (if Is_Failure
            then SPARK_Terminal.Exit_Status_Failure
            else SPARK_Terminal.Exit_Status_Success));
   end Usage;

   procedure Run_Session
     (Port      : CoAP_SPARK.Channel.Port_Type;
      Is_Secure : Boolean)
   is

      Ctx : FSM.Context;
      Skt : CoAP_SPARK.Channel.Socket_Type (Is_Secure);
   begin

      Secure_Server.Initialize
        (Socket => Skt,
         Port   => Port);
      if not CoAP_SPARK.Channel.Is_Valid (Skt) then
         CoAP_SPARK.Log.Put_Line
           ("Communication problems.", CoAP_SPARK.Log.Error);
         return;
      end if;

      Main_Loop_Environment.Initialize
        (Request_Handler => Server_Handling.Handle_Request'Access,
         Session_State   => Ctx.E);

      if Ctx.E.Current_Status /= CoAP_SPARK.OK then
         CoAP_SPARK.Log.Put_Line
           (Ctx.E.Current_Status'Image, CoAP_SPARK.Log.Error);
         --  Main_Loop_Environment.Finalize (Ctx.E);
         --  pragma Assert (Main_Loop_Environment.Is_Finalized (Ctx.E));
         return;
      end if;
      pragma Assert (FSM.Uninitialized (Ctx));

      FSM.Initialize (Ctx);

      CoAP_SPARK.Server_Session.Run_Session_Loop (Ctx, Skt);

      CoAP_SPARK.Channel.Finalize (Skt);
      pragma Assert (not CoAP_SPARK.Channel.Is_Valid (Skt));

      FSM.Finalize (Ctx);
      pragma Assert (FSM.Uninitialized (Ctx));

      --  Main_Loop_Environment.Finalize (Ctx.E);
      --  pragma Assert (Main_Loop_Environment.Is_Finalized (Ctx.E));
   end Run_Session;

   Port : CoAP_SPARK.Channel.Port_Type := CoAP_SPARK.Default_Port;
   Argument_Index : Natural := 1;

   Valid_Command_Line : Boolean := True;
   Is_Secure          : Boolean := False;
begin

   if SPARK_Terminal.Argument_Count = 1
     and then (SPARK_Terminal.Argument (1) = "-h"
               or else SPARK_Terminal.Argument (1) = "--help")
   then
      Usage (Is_Failure => False);
      return;
   end if;

   if SPARK_Terminal.Argument_Count = 1
     and then (SPARK_Terminal.Argument (1) = "-V"
               or else SPARK_Terminal.Argument (1) = "--version")
   then
      CoAP_SPARK.Log.Put (Coap_Server_Config.Crate_Name, CoAP_SPARK.Log.Info);
      CoAP_SPARK.Log.Put (" v", CoAP_SPARK.Log.Info);
      CoAP_SPARK.Log.Put_Line
        (Coap_Server_Config.Crate_Version, CoAP_SPARK.Log.Info);
      return;
   end if;

   -- Any other single command line argument is an error, since all remaining
   --  ones are an option and a value.
   if SPARK_Terminal.Argument_Count = 1 then
      Usage (Is_Failure => True);
      return;
   end if;

   while Argument_Index < SPARK_Terminal.Argument_Count loop

      if SPARK_Terminal.Argument (Argument_Index) = "-p" then
         Argument_Index := @ + 1;
         if CoAP_SPARK.Utils.Valid_Unsigned_16_Values.Is_Valid_As_Number
              (SPARK_Terminal.Argument (Argument_Index))
         then
            Port :=
              CoAP_SPARK.Channel.Port_Type
                (CoAP_SPARK.Utils.Valid_Unsigned_16_Values.Value
                   (SPARK_Terminal.Argument (Argument_Index)));
         else
            CoAP_SPARK.Log.Put_Line ("Invalid specified port", CoAP_SPARK.Log.Error);
            Valid_Command_Line := False;
         end if;

      elsif SPARK_Terminal.Argument (Argument_Index) = "-v" then
         Argument_Index := @ + 1;
         if CoAP_SPARK.Utils.Valid_Natural_Values.Is_Valid_As_Number
              (SPARK_Terminal.Argument (Argument_Index))
         then
            declare
               Verbosity_Number : constant Natural :=
                 CoAP_SPARK.Utils.Value
                   (SPARK_Terminal.Argument (Argument_Index));
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

      elsif SPARK_Terminal.Argument (Argument_Index) = "-k"
        or else SPARK_Terminal.Argument (Argument_Index) = "-u"
      then
         Argument_Index := @ + 1;
         Is_Secure := True;

         if Argument_Index = SPARK_Terminal.Argument_Count then
            CoAP_SPARK.Log.Put ("Missing argument for ", CoAP_SPARK.Log.Error);
            CoAP_SPARK.Log.Put_Line
              (SPARK_Terminal.Argument (Argument_Index - 1),
               CoAP_SPARK.Log.Error);
            Valid_Command_Line := False;
         end if;
         -- We will handle the PSK and Identity later, in the PSK_Callback
      else
         CoAP_SPARK.Log.Put ("Invalid option: ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line
           (SPARK_Terminal.Argument (Argument_Index), CoAP_SPARK.Log.Error);
         Valid_Command_Line := False;
      end if;

      exit when not Valid_Command_Line or else
         Argument_Index = SPARK_Terminal.Argument_Count;

      pragma Loop_Invariant (Argument_Index < SPARK_Terminal.Argument_Count);
      Argument_Index := @ + 1;
   end loop;

   if not Valid_Command_Line then
      Usage;
      return;
   end if;

   Run_Session
     (Port      => Port,
      Is_Secure => Is_Secure);

   -- This has no effect, but it is needed to avoid a linking error with
   -- SPARKLib in the validation profile.
--    Workarounds.Check_Or_Fail;
end CoAP_Server;
