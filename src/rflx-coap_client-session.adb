with Ada.Containers;
with Ada.Numerics.Discrete_Random;
with CoAP_SPARK.Log;
with CoAP_SPARK.Messages;
with CoAP_SPARK.Options.Lists;
with RFLX.CoAP.Option_Sequence;
with RFLX.CoAP.Option_Type;
with RFLX.RFLX_Builtin_Types;

package body RFLX.CoAP_Client.Session
   with SPARK_Mode
is

   use type RFLX.CoAP.Option_Extended16_Type;
   use type RFLX.RFLX_Builtin_Types.Index;
   use type RFLX.RFLX_Builtin_Types.Length;
   use type RFLX.RFLX_Builtin_Types.Bit_Length;
   use type RFLX.RFLX_Types.Bytes_Ptr;
   use type CoAP_SPARK.Options.Option_Format;
   use type RFLX.CoAP.Option_Numbers;
   use type Ada.Containers.Count_Type;

   package Option_Sorting is new CoAP_SPARK.Options.Lists.Generic_Sorting
                                   ("<" => CoAP_SPARK.Options."<");

   -- The random number generators cannot be proved by SPARK.
   package Random
     with SPARK_Mode => Off
   is
      package Bytes is new Ada.Numerics.Discrete_Random (RFLX_Types.Byte);
      Byte_Generator : Bytes.Generator;

      package ID is new
        Ada.Numerics.Discrete_Random (RFLX.CoAP.Message_ID_Type);
      ID_Generator : ID.Generator;
   end Random;

   package body Random
     with SPARK_Mode => Off
   is
   begin
      Bytes.Reset (Byte_Generator);
      ID.Reset (ID_Generator);
   end Random;

   subtype Base_Delta_Type is RFLX.CoAP.Option_Extended16_Type range 0 .. 12;
   subtype Extended8_Delta_Type is
     RFLX.CoAP.Option_Extended16_Type range 13 .. 268;
   subtype Extended16_Delta_Type is
     RFLX.CoAP.Option_Extended16_Type
       range 269 .. RFLX.CoAP.Option_Extended16_Type'Last;

   -- Maximum length of the entire option in bytes
   Max_Option_Length : constant :=
     Extended16_Delta_Type'First + RFLX.CoAP.Option_Extended16_Type'Last;

   subtype Option_Value_Length is CoAP_SPARK.Options.Option_Value_Length;

   subtype Base_Length_Type is Option_Value_Length range 0 .. 12;
   subtype Extended8_Length_Type is Option_Value_Length range 13 .. 268;
   subtype Extended16_Length_Type is
     Option_Value_Length range 269 .. Option_Value_Length'Last;

   procedure Get_Method
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Method_Code) is
   begin

      RFLX_Result := State.Method;
      State.Current_Status := RFLX.CoAP_Client.Session_Environment.OK;

   end Get_Method;

   procedure Get_New_Message_ID
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP.Message_ID_Type)
   with SPARK_Mode => Off
   is
      use type RFLX.CoAP.Message_ID_Type;
   begin
      if State.Is_First_Message then
         -- Quoting the RFC:
         --  It is strongly recommended that the initial
         --  value of the variable (e.g., on startup) be randomized, in order
         --  to make successful off-path attacks on the protocol less likely.
         State.Is_First_Message := False;
         State.Current_Message_ID := Random.ID.Random (Random.ID_Generator);
      elsif State.Current_Message_ID = RFLX.CoAP.Message_ID_Type'Last then
         State.Current_Message_ID := RFLX.CoAP.Message_ID_Type'First;
      else
         State.Current_Message_ID := State.Current_Message_ID + 1;
      end if;
      RFLX_Result := State.Current_Message_ID;
   end Get_New_Message_ID;

   procedure Get_Random_Token
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Token_Data.Structure)
   with SPARK_Mode => Off
   is
      pragma Unreferenced (State);
   begin
      for I in RFLX_Result.Token'Range loop
         RFLX_Result.Token (I) := Random.Bytes.Random (Random.Byte_Generator);
      end loop;

      RFLX_Result :=
        (Length => RFLX_Result.Token'Length,
         Unused => RFLX.CoAP_Client.Unused_4'First,
         Token  => RFLX_Result.Token);
   end Get_Random_Token;

   function To_Option_Extended16_Type
     (Option : RFLX.CoAP.Option_Numbers)
      return RFLX.CoAP.Option_Extended16_Type
   is (RFLX.CoAP.Option_Extended16_Type (RFLX.CoAP.To_Base_Integer (Option)));

   function To_Option_Base
     (Value : RFLX.CoAP.Option_Extended16_Type)
      return RFLX.CoAP.Option_Base_Type
   is (case Value is
         when Base_Delta_Type => RFLX.CoAP.Option_Base_Type (Value),
         when Extended8_Delta_Type => 13,
         when Extended16_Delta_Type => 14);

   function To_Option_Base
     (Value : Option_Value_Length) return RFLX.CoAP.Option_Base_Type
   is (case Value is
         when Base_Length_Type => RFLX.CoAP.Option_Base_Type (Value),
         when Extended8_Length_Type => 13,
         when Extended16_Length_Type => 14);

   -- See figure 8 in RFC7252
   function Option_Byte_Size
     (Option : RFLX.CoAP.Option_Numbers; Option_Length : Option_Value_Length)
      return RFLX.RFLX_Builtin_Types.Length
   is (1
       + (case To_Option_Extended16_Type (Option) is
            when Base_Delta_Type => 0,
            when Extended8_Delta_Type => 1,
            when Extended16_Delta_Type => 2)
       + (case Option_Length is
            when Base_Length_Type => 0,
            when Extended8_Length_Type => 1,
            when Extended16_Length_Type => 2)
       + RFLX.RFLX_Builtin_Types.Length (Option_Length));

   function To_Bit_Size
     (Value : RFLX.RFLX_Builtin_Types.Length) return RFLX.RFLX_Types.Bit_Length
   is (RFLX.RFLX_Types.Bit_Length (Value) * 8);

   procedure Add_Option
    (Opt                 : in out CoAP_SPARK.Options.Option;
     Current_Delta       : in out RFLX.CoAP.Option_Extended16_Type;
     Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context) with
   Pre  =>
      CoAP_SPARK.Options.Has_Buffer (Opt)
      and then RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt)
      and then RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
      and then To_Option_Extended16_Type
         (CoAP_SPARK.Options.Get_Number (Opt)) >= Current_Delta
      and then
         RFLX.CoAP.Option_Sequence.Available_Space (Option_Sequence_Cxt) >=
         To_Bit_Size (Option_Byte_Size
                       (Option        => CoAP_SPARK.Options.Get_Number (Opt),
                        Option_Length => CoAP_SPARK.Options.Get_Length (Opt))),
   Post =>
      not CoAP_SPARK.Options.Has_Buffer (Opt)
      and then To_Option_Extended16_Type
         (CoAP_SPARK.Options.Get_Number (Opt)) = Current_Delta
      and then RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt)
      and then RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt)
   is
      Option_Number : constant RFLX.CoAP.Option_Numbers :=
         CoAP_SPARK.Options.Get_Number (Opt);
      Option_Length : constant Option_Value_Length :=
         CoAP_SPARK.Options.Get_Length (Opt);

      Option_Delta : constant RFLX.CoAP.Option_Extended16_Type :=
         To_Option_Extended16_Type (Option_Number) -
            Current_Delta;

      Option_Cxt    : RFLX.CoAP.Option_Type.Context;
      Option_Buffer : RFLX.RFLX_Types.Bytes_Ptr :=
         new RFLX.RFLX_Types.Bytes'
         (1 ..
             RFLX.RFLX_Builtin_Types.Index
            (Option_Byte_Size (Option_Number, Option_Length)) => 0);
      Value_Bytes : RFLX.RFLX_Builtin_Types.Bytes_Ptr;
   begin

      RFLX.CoAP.Option_Type.Initialize
         (Ctx => Option_Cxt, Buffer => Option_Buffer);

      RFLX.CoAP.Option_Type.Set_Option_Delta
         (Ctx => Option_Cxt, Val => To_Option_Base (Option_Delta));

      RFLX.CoAP.Option_Type.Set_Option_Length
         (Ctx => Option_Cxt, Val => To_Option_Base (Option_Length));

      case RFLX.CoAP.Option_Type.Get_Option_Delta (Option_Cxt) is
         when 13 =>
            RFLX.CoAP.Option_Type.Set_Option_Delta_Extended8
               (Ctx => Option_Cxt,
                Val => RFLX.CoAP.Option_Extended8_Type (Option_Delta - 13));

         when 14 =>
            RFLX.CoAP.Option_Type.Set_Option_Delta_Extended16
               (Ctx => Option_Cxt,
                Val => RFLX.CoAP.Option_Extended16_Type (Option_Delta - 269));

         when others =>
            null;
      end case;

      case RFLX.CoAP.Option_Type.Get_Option_Length (Option_Cxt) is
         when 13 =>
            RFLX.CoAP.Option_Type.Set_Option_Length_Extended8
              (Ctx => Option_Cxt,
               Val => RFLX.CoAP.Option_Extended8_Type (Option_Length - 13));

         when 14 =>
            RFLX.CoAP.Option_Type.Set_Option_Length_Extended16
              (Ctx => Option_Cxt,
               Val => RFLX.CoAP.Option_Extended16_Type (Option_Length - 269));

         when others =>
            null;
      end case;

      CoAP_SPARK.Options.Take_Buffer (Opt, Value_Bytes);

      RFLX.CoAP.Option_Type.Set_Option_Value
         (Ctx => Option_Cxt, Data => Value_Bytes.all);

      RFLX.RFLX_Types.Free (Value_Bytes);

      RFLX.CoAP.Option_Sequence.Append_Element
         (Ctx => Option_Sequence_Cxt, Element_Ctx => Option_Cxt);

      Current_Delta :=
        To_Option_Extended16_Type (CoAP_SPARK.Options.Get_Number (Opt));

      RFLX.CoAP.Option_Type.Take_Buffer
         (Ctx => Option_Cxt, Buffer => Option_Buffer);
      pragma Unreferenced (Option_Cxt);

      RFLX.RFLX_Types.Free (Option_Buffer);

   end Add_Option;

   procedure Get_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      RFLX_Result : out RFLX.CoAP_Client.Options_And_Payload_Data.Structure)
   is
      use type RFLX.CoAP_Client.Session_Environment.Status_Type;
      use type RFLX.CoAP.Length_16;

      Option_Sequence_Cxt    : RFLX.CoAP.Option_Sequence.Context;
      Option_Sequence_Buffer : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'
          [1 .. RFLX.RFLX_Builtin_Types.Index (Max_Option_Length) => 0];

      Current_Delta : RFLX.CoAP.Option_Extended16_Type := 0;
   begin

      RFLX_Result := (Length => 0, Options_And_Payload => [others => 0]);

      RFLX.CoAP.Option_Sequence.Initialize
        (Ctx => Option_Sequence_Cxt, Buffer => Option_Sequence_Buffer);

      -- The options must be sorted by option number before they are encoded
      Option_Sorting.Sort (State.Request_Content.Options);

      for Option of State.Request_Content.Options loop
         declare
            Option_Copy : CoAP_SPARK.Options.Option;
         begin
            -- Make a copy of the option to comply with the SPARK requirements
            -- on pointers.
            --
            CoAP_SPARK.Options.Copy (Source => Option, Target => Option_Copy);

            Add_Option
              (Opt                 => Option_Copy,
               Current_Delta       => Current_Delta,
               Option_Sequence_Cxt => Option_Sequence_Cxt);
            pragma Unreferenced (Option_Copy);
         end;
      end loop;
      pragma Unreferenced (Current_Delta);

      declare
         Last : constant RFLX.RFLX_Builtin_Types.Index'Base :=
           RFLX.RFLX_Builtin_Types.Index'Base
             (RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt));
      begin
         if Last in RFLX_Result.Options_And_Payload'Range then
            RFLX.CoAP.Option_Sequence.Copy
              (Ctx    => Option_Sequence_Cxt,
               Buffer => RFLX_Result.Options_And_Payload (1 .. Last));
            RFLX_Result.Length := RFLX.CoAP.Length_16 (Last);

            State.Current_Status := RFLX.CoAP_Client.Session_Environment.OK;
         else
            State.Current_Status :=
              RFLX.CoAP_Client.Session_Environment.Capacity_Error;
         end if;
      end;

      RFLX.CoAP.Option_Sequence.Take_Buffer
        (Ctx => Option_Sequence_Cxt, Buffer => Option_Sequence_Buffer);
      pragma Unreferenced (Option_Sequence_Cxt);

      RFLX.RFLX_Types.Free (Option_Sequence_Buffer);

      if State.Current_Status = RFLX.CoAP_Client.Session_Environment.OK
         and then
         State.Request_Content.Payload /= null and then
         State.Request_Content.Payload.all'Length in 1 .. RFLX.RFLX_Builtin_Types.Index'Last - 1
      then

         -- Is there space for the payload and the payload marker (1 byte)?
         if State.Request_Content.Payload.all'Length + 1 <=
           RFLX_Result.Options_And_Payload'Length - RFLX_Result.Length
         then
            RFLX_Result.Length := @ + 1;

            declare
               Marker_Index : constant RFLX.RFLX_Builtin_Types.Index :=
                  RFLX.RFLX_Builtin_Types.Index (RFLX_Result.Length);
               Payload_Last : constant RFLX.RFLX_Builtin_Types.Index :=
                 Marker_Index +
                    RFLX.RFLX_Builtin_Types.Index (State.Request_Content.Payload.all'Length);
            begin
               -- Add payload marker
               RFLX_Result.Options_And_Payload (Marker_Index) := 16#FF#;

               -- Add payload
               RFLX_Result.Options_And_Payload
                 (Marker_Index + 1 .. Payload_Last) := State.Request_Content.Payload.all;
               RFLX_Result.Length := @ + State.Request_Content.Payload.all'Length;
            end;
         else
            State.Current_Status :=
              RFLX.CoAP_Client.Session_Environment.Capacity_Error;
         end if;
      end if;

   end Get_Options_And_Payload;

   procedure Put_Option
     (State          : in out RFLX.CoAP_Client.Session_Environment.State;
      Option_Cxt     : RFLX.CoAP.Option_Type.Context;
      Option_Delta   : in out RFLX.RFLX_Types.Base_Integer;
      End_Of_Options : out Boolean)
   with
     Pre =>
       CoAP_SPARK.Options.Lists.Length (State.Response_Content.Options) <
         CoAP_SPARK.Options.Lists.Capacity (State.Response_Content.Options)
       and then RFLX.CoAP.Option_Type.Has_Buffer (Option_Cxt)
       and then RFLX.CoAP.Option_Type.Well_Formed_Message (Option_Cxt)
       and then Option_Delta <
         RFLX.CoAP.Option_Numbers'Enum_Rep (RFLX.CoAP.Option_Numbers'Last)
   is
      Option_Length : Option_Value_Length;
      Encoded_Option_Delta : RFLX.RFLX_Types.Base_Integer;

   begin
      End_Of_Options := False;

      Encoded_Option_Delta :=
        RFLX.CoAP.To_Base_Integer
          (RFLX.CoAP.Option_Type.Get_Option_Delta (Option_Cxt));

      Option_Length :=
        Option_Value_Length
          (RFLX.CoAP.Option_Type.Get_Option_Length (Option_Cxt));

      case Encoded_Option_Delta is
         when 13 =>
            Option_Delta := Option_Delta + 13
              + RFLX.RFLX_Types.Base_Integer
                  (RFLX.CoAP.Option_Type.Get_Option_Delta_Extended8
                     (Option_Cxt));

         when 14 =>
            Option_Delta := Option_Delta + 269
              + RFLX.RFLX_Types.Base_Integer
                  (RFLX.CoAP.Option_Type.Get_Option_Delta_Extended16
                     (Option_Cxt));

         when 15 =>
            if Option_Length = 15 then
               End_Of_Options := True;
               return;
            else
               State.Current_Status :=
                 CoAP_Client.Session_Environment.Malformed_Message;
               return;
            end if;

         when others =>
            Option_Delta := Option_Delta + Encoded_Option_Delta;
      end case;

      case Option_Length is
         when 13 =>
            Option_Length := 13
              + Option_Value_Length
                  (RFLX.CoAP.Option_Type.Get_Option_Length_Extended8
                     (Option_Cxt));

         when 14 =>
            declare
               Length16 : constant RFLX.CoAP.Option_Extended16_Type :=
                 RFLX.CoAP.Option_Type.Get_Option_Length_Extended16
                   (Option_Cxt);
            begin
               if Option_Value_Length'Base (Length16) + 269
                 not in Option_Value_Length
               then
                  State.Current_Status :=
                    CoAP_Client.Session_Environment.Malformed_Message;
                  return;
               end if;
               Option_Length := 269 + Option_Value_Length (Length16);
            end;
         when 15 =>
            State.Current_Status :=
              CoAP_Client.Session_Environment.Malformed_Message;
            return;

         when others =>
            null;
      end case;

      if not RFLX.CoAP.Valid_Option_Numbers (Option_Delta) or else
         Option_Length >
         CoAP_SPARK.Options.Option_Properties_Table
            (RFLX.CoAP.To_Actual (Option_Delta)).Maximum_Length
      then
         State.Current_Status :=
           CoAP_Client.Session_Environment.Malformed_Message;
         return;
      elsif Option_Length > 0 then
         declare
            Option_Value  : RFLX.RFLX_Types.Bytes_Ptr :=
              new RFLX.RFLX_Types.Bytes'
                (1 .. RFLX_Builtin_Types.Index (Option_Length) => 0);
            Option_Number : constant RFLX.CoAP.Option_Numbers :=
              RFLX.CoAP.To_Actual (Option_Delta);
            Option : CoAP_SPARK.Options.Option;
         begin
            RFLX.CoAP.Option_Type.Get_Option_Value
              (Ctx => Option_Cxt, Data => Option_Value.all);

            if Option_Value.all'Length >
             CoAP_SPARK.Options.Option_Properties_Table
              (Option_Number).Maximum_Length
            then
               CoAP_SPARK.Log.Put
                  ("Option value is too long for option");
               CoAP_SPARK.Log.Put_Line (Option_Number'Image);

               State.Current_Status :=
                  RFLX.CoAP_Client.Session_Environment.Malformed_Message;
               return;
            end if;

            if Option_Number = CoAP.Content_Format then
               State.Response_Content.Format :=
                  CoAP_SPARK.Options.To_UInt (Value => Option_Value.all);
            end if;

            CoAP_SPARK.Options.New_Encoded_Option
              (Number => Option_Number,
               Value  => Option_Value,
               Result => Option);
            pragma Assert (Option_Value = null);

            CoAP_SPARK.Options.Lists.Append
              (Container => State.Response_Content.Options,
               New_Item => Option);
         end;
      else
         declare
            Empty_Value : RFLX.RFLX_Types.Bytes_Ptr :=
               new RFLX.RFLX_Types.Bytes'([]);
            Option_Number : constant RFLX.CoAP.Option_Numbers :=
               RFLX.CoAP.To_Actual (Option_Delta);
            Option : CoAP_SPARK.Options.Option;
         begin
            CoAP_SPARK.Options.New_Encoded_Option
              (Number => Option_Number,
               Value  => Empty_Value,
               Result => Option);
            pragma Assert (Empty_Value = null);

            CoAP_SPARK.Options.Lists.Append
              (Container => State.Response_Content.Options,
               New_Item => Option);
         end;

         if CoAP.To_Actual (Option_Delta) = CoAP.Content_Format then
            State.Response_Content.Format := 0;
         end if;
      end if;

   end Put_Option;

   procedure Put_Options_And_Payload
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Data        : RFLX_Types.Bytes;
      RFLX_Result : out Boolean)
   is
      use type CoAP_Client.Session_Environment.Status_Type;
   begin

      -- Make sure the Response_Content is freed before overwriting it
      CoAP_SPARK.Messages.Finalize (State.Response_Content);

      if Data'Length = 0 then

         CoAP_SPARK.Log.Put_Line ("Options and payload are empty");

      elsif Data'Last = RFLX_Types.Index'Last then

         CoAP_SPARK.Log.Put_Line ("Data is too long");
         State.Current_Status :=
           RFLX.CoAP_Client.Session_Environment.Capacity_Error;

      else
         declare
            Option_Sequence_Cxt : RFLX.CoAP.Option_Sequence.Context;
            Buffer              : RFLX.RFLX_Types.Bytes_Ptr :=
              new RFLX.RFLX_Types.Bytes'(Data);
            Option_Delta        : RFLX.RFLX_Types.Base_Integer := 0;
            Option_Cxt          : RFLX.CoAP.Option_Type.Context;
            End_Of_Options      : Boolean;
         begin

            RFLX.CoAP.Option_Sequence.Initialize
              (Ctx => Option_Sequence_Cxt, Buffer => Buffer);

            Read_Options :
            loop
               pragma Loop_Invariant (RFLX.CoAP.Option_Sequence.Has_Buffer
                                    (Option_Sequence_Cxt));
               RFLX.CoAP.Option_Sequence.Switch
                 (Ctx => Option_Sequence_Cxt, Element_Ctx => Option_Cxt);

               RFLX.CoAP.Option_Type.Verify_Message (Ctx => Option_Cxt);

               if not RFLX.CoAP.Option_Type.Well_Formed_Message
                        (Ctx => Option_Cxt)
               then
                  -- This usually happens when the payload marker is the next
                  -- pair of bytes, since this cannot be longer decoded as an
                  -- option. The existence of the payload marker is checked
                  -- later.
                  exit Read_Options;
               end if;
               Put_Option (State, Option_Cxt, Option_Delta, End_Of_Options);

               RFLX.CoAP.Option_Sequence.Update
                 (Ctx => Option_Sequence_Cxt, Element_Ctx => Option_Cxt);

               exit Read_Options when
                 End_Of_Options
                 or else not RFLX.CoAP.Option_Sequence.Has_Element
                               (Option_Sequence_Cxt);
            end loop Read_Options;

            if CoAP.Option_Sequence.Sequence_Last (Option_Sequence_Cxt)
              >= Option_Sequence_Cxt.Last
            then
               RFLX.CoAP.Option_Type.Take_Buffer
                 (Ctx => Option_Cxt, Buffer => Buffer);
               pragma Assert (not RFLX.CoAP.Option_Type.Has_Buffer
                                    (Option_Cxt));
               RFLX.RFLX_Types.Free (Buffer);
            else
               -- When there is something left to be read, it is the payload
               Print_Payload :
               declare
                  use type RFLX.RFLX_Builtin_Types.Byte;
                  Payload_Marker : constant RFLX.RFLX_Builtin_Types.Byte :=
                     16#FF#;
                  First          :
                    constant RFLX.RFLX_Builtin_Types.Index :=
                      RFLX.RFLX_Builtin_Types.Index
                        (CoAP.Option_Sequence.Sequence_Last
                           (Option_Sequence_Cxt) / 8 + 1);
                  Last                  :
                    constant RFLX.RFLX_Builtin_Types.Index :=
                      RFLX.RFLX_Builtin_Types.Index (Option_Cxt.Last / 8);
               begin

                  RFLX.CoAP.Option_Type.Take_Buffer
                    (Ctx => Option_Cxt, Buffer => Buffer);
                  pragma Assert (not RFLX.CoAP.Option_Type.Has_Buffer
                                    (Option_Cxt));

                  if First >= Last or else
                     Buffer (First) /= Payload_Marker
                  then
                     CoAP_SPARK.Log.Put_Line
                       ("Error: Payload marker not found before the payload");
                     CoAP_SPARK.Log.Put_Line
                       ("Remaining bytes: ");
                     CoAP_SPARK.Log.Put_Line (CoAP_SPARK.Options.Image
                         (Format => CoAP_SPARK.Options.Opaque,
                          Value  => Buffer (First .. Last)));

                     RFLX.RFLX_Types.Free (Buffer);

                     State.Current_Status :=
                       RFLX.CoAP_Client.Session_Environment.Malformed_Message;
                     RFLX_Result := False;
                     return;
                  end if;

                  State.Response_Content.Payload :=
                    new RFLX.RFLX_Types.Bytes'
                      (Buffer (First + 1 .. Last));

                  RFLX.RFLX_Types.Free (Buffer);
               end Print_Payload;
            end if;
         end;
      end if;

      RFLX_Result :=
        State.Current_Status = RFLX.CoAP_Client.Session_Environment.OK;

   end Put_Options_And_Payload;

   procedure Put_Client_Error
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Error_Code  : RFLX.CoAP.Client_Error_Response;
      RFLX_Result : out Boolean)
   is
   begin
      State.Response_Codes :=
        (Code_Class => RFLX.CoAP.Client_Error,
         Client_Error_Code => Error_Code);

      RFLX_Result := True;
   end Put_Client_Error;

   procedure Put_Server_Error
     (State       : in out RFLX.CoAP_Client.Session_Environment.State;
      Error_Code  : RFLX.CoAP.Server_Error_Response;
      RFLX_Result : out Boolean)
   is
   begin
      State.Response_Codes :=
        (Code_Class => RFLX.CoAP.Server_Error,
         Server_Error_Code => Error_Code);

      RFLX_Result := True;
   end Put_Server_Error;

end RFLX.CoAP_Client.Session;
