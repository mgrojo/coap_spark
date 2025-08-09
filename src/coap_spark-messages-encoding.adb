with Ada.Containers;

with CoAP_SPARK.Options.List_Sorting;
with CoAP_SPARK.Options.Lists;

with RFLX.CoAP;
with RFLX.CoAP.Option_Sequence;
with RFLX.CoAP.Option_Type;
with RFLX.RFLX_Arithmetic;
with RFLX.RFLX_Builtin_Types;

package body CoAP_SPARK.Messages.Encoding
   with SPARK_Mode
is
   use type Ada.Containers.Count_Type;
   use type RFLX.CoAP.Option_Extended16_Type;
   use type RFLX.CoAP.Option_Numbers;
   use type RFLX.RFLX_Arithmetic.Base_Integer;
   use type RFLX.RFLX_Builtin_Types.Bit_Length;
   use type RFLX.RFLX_Builtin_Types.Length;

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

   -- This is the maximum option value that can be decoded.
   Max_Allowed_Option_Delta : constant RFLX.RFLX_Types.Base_Integer :=
      RFLX.RFLX_Types.Base_Integer'Last
      - RFLX.RFLX_Types.Base_Integer (Extended16_Length_Type'First)
      - RFLX.RFLX_Types.Base_Integer (RFLX.CoAP.Option_Extended16_Type'Last);


   function To_Option_Extended16_Type
     (Option : RFLX.CoAP.Option_Numbers)
      return RFLX.CoAP.Option_Extended16_Type
   is (RFLX.CoAP.Option_Extended16_Type (RFLX.CoAP.To_Base_Integer (Option)))
   with Post => To_Option_Extended16_Type'Result >= 1;

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

   procedure Add_Encoded_Option
    (Opt                 : CoAP_SPARK.Options.Indefinite_Option;
     Current_Delta       : in out RFLX.CoAP.Option_Extended16_Type;
     Option_Sequence_Cxt : in out RFLX.CoAP.Option_Sequence.Context) with
   Pre  =>
      CoAP_SPARK.Options.Get_Length (Opt) <=
        CoAP_SPARK.Options.Max_Option_Value_Length
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
      To_Option_Extended16_Type
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

      RFLX.CoAP.Option_Type.Set_Option_Value
         (Ctx => Option_Cxt, Data => CoAP_SPARK.Options.Get_Value (Opt));

      RFLX.CoAP.Option_Sequence.Append_Element
         (Ctx => Option_Sequence_Cxt, Element_Ctx => Option_Cxt);

      Current_Delta :=
        To_Option_Extended16_Type (CoAP_SPARK.Options.Get_Number (Opt));

      RFLX.CoAP.Option_Type.Take_Buffer
         (Ctx => Option_Cxt, Buffer => Option_Buffer);
      pragma Unreferenced (Option_Cxt);

      RFLX.RFLX_Types.Free (Option_Buffer);

   end Add_Encoded_Option;

   procedure Encode_Options_And_Payload
     (Options_And_Payload : Content;
      Status              : out CoAP_SPARK.Status_Type;
      Encoded_Data        : out RFLX.RFLX_Types.Bytes;
      Encoded_Length      : out RFLX.CoAP.Length_16)
   is
      use type RFLX.CoAP.Length_16;

      Option_Sequence_Cxt    : RFLX.CoAP.Option_Sequence.Context;
      Option_Sequence_Buffer : RFLX.RFLX_Types.Bytes_Ptr :=
        new RFLX.RFLX_Types.Bytes'
          [1 .. RFLX.RFLX_Builtin_Types.Index (Max_Option_Length) => 0];

      Current_Delta : RFLX.CoAP.Option_Extended16_Type := 0;

      Sorted_Options : CoAP_SPARK.Options.Lists.Vector :=
        Options_And_Payload.Options;
   begin

      Status := OK;
      Encoded_Length := 0;
      Encoded_Data := [others => 0];

      RFLX.CoAP.Option_Sequence.Initialize
        (Ctx => Option_Sequence_Cxt, Buffer => Option_Sequence_Buffer);

      -- The options must be sorted by option number before they are encoded
      CoAP_SPARK.Options.List_Sorting.Instance.Sort (Sorted_Options);

      for Option of Sorted_Options loop
         pragma Loop_Invariant (RFLX.CoAP.Option_Sequence.Valid (Option_Sequence_Cxt));
         pragma Loop_Invariant (RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt));
         pragma Loop_Invariant (CoAP_SPARK.Options.List_Sorting.Instance.Is_Sorted
                                   (Sorted_Options));

         if To_Option_Extended16_Type (CoAP_SPARK.Options.Get_Number (Option))
            < Current_Delta
         then
            CoAP_SPARK.Log.Put ("Skipped option number (not in ascending order): ",
                                       CoAP_SPARK.Log.Error);
            CoAP_SPARK.Log.Put_Line (CoAP_SPARK.Options.Get_Number (Option)'Image,
                                       CoAP_SPARK.Log.Error);
            Status := Unexpected_Case;
            exit;
         elsif RFLX.CoAP.Option_Sequence.Available_Space (Option_Sequence_Cxt) <
               To_Bit_Size (Option_Byte_Size
                     (Option        => CoAP_SPARK.Options.Get_Number (Option),
                     Option_Length => CoAP_SPARK.Options.Get_Length (Option)))
         then
            CoAP_SPARK.Log.Put_Line ("Not enough space for option",
                                       CoAP_SPARK.Log.Error);
            Status := Capacity_Error;
            exit;
         else
            Add_Encoded_Option
              (Opt                 => Option,
               Current_Delta       => Current_Delta,
               Option_Sequence_Cxt => Option_Sequence_Cxt);
         end if;
      end loop;
      pragma Unreferenced (Current_Delta);

      if Status = OK then
         declare
            Last : constant RFLX.RFLX_Builtin_Types.Index'Base :=
              RFLX.RFLX_Builtin_Types.Index'Base
                (RFLX.CoAP.Option_Sequence.Byte_Size (Option_Sequence_Cxt));
         begin
            if Last in Encoded_Data'Range then

               RFLX.CoAP.Option_Sequence.Copy
                 (Ctx    => Option_Sequence_Cxt,
                  Buffer => Encoded_Data (1 .. Last));
               Encoded_Length := RFLX.CoAP.Length_16 (Last);

               Status := OK;
            else
               Status := Capacity_Error;
            end if;
         end;
      end if;

      RFLX.CoAP.Option_Sequence.Take_Buffer
        (Ctx => Option_Sequence_Cxt, Buffer => Option_Sequence_Buffer);
      pragma Unreferenced (Option_Sequence_Cxt);

      RFLX.RFLX_Types.Free (Option_Sequence_Buffer);

      if Status = OK
         and then
         Options_And_Payload.Payload /= null and then
         Options_And_Payload.Payload.all'Length in 1 .. RFLX.RFLX_Builtin_Types.Index'Last - 1
      then

         -- Is there space for the payload and the payload marker (1 byte)?
         if Options_And_Payload.Payload.all'Length + 1 <=
           Encoded_Data'Length - Encoded_Length
         then
            Encoded_Length := @ + 1;

            declare
               Marker_Index : constant RFLX.RFLX_Builtin_Types.Index :=
                  RFLX.RFLX_Builtin_Types.Index (Encoded_Length);
               Payload_Last : constant RFLX.RFLX_Builtin_Types.Index :=
                 Marker_Index +
                    RFLX.RFLX_Builtin_Types.Index (Options_And_Payload.Payload.all'Length);
            begin
               -- Add payload marker
               Encoded_Data (Marker_Index) := 16#FF#;

               -- Add payload
               Encoded_Data
                 (Marker_Index + 1 .. Payload_Last) := Options_And_Payload.Payload.all;
               Encoded_Length := @ + Options_And_Payload.Payload.all'Length;
            end;
         else
            Status := Capacity_Error;
         end if;
      end if;

   end Encode_Options_And_Payload;

   procedure Add_Decode_Option
     (Option_Cxt      : RFLX.CoAP.Option_Type.Context;
      Decoded_Content : in out Content;
      Option_Delta    : in out RFLX.RFLX_Types.Base_Integer;
      Status          : in out CoAP_SPARK.Status_Type;
      End_Of_Options  : out Boolean)
   with
     Pre =>
       CoAP_SPARK.Options.Lists.Length (Decoded_Content.Options) <
         CoAP_SPARK.Max_Number_Of_Options
       and then RFLX.CoAP.Option_Type.Has_Buffer (Option_Cxt)
       and then RFLX.CoAP.Option_Type.Well_Formed_Message (Option_Cxt)
       and then Option_Delta < Max_Allowed_Option_Delta
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
               Status := Malformed_Message;
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
                  Status := Malformed_Message;
                  return;
               end if;
               Option_Length := 269 + Option_Value_Length (Length16);
            end;
         when 15 =>
            Status := Malformed_Message;
            return;

         when others =>
            null;
      end case;

      if not RFLX.CoAP.Valid_Option_Numbers (Option_Delta) then

         if CoAP_SPARK.Options.Is_Critical (Number => Option_Delta) then
            CoAP_SPARK.Log.Put
               ("Unknown critical option:", CoAP_SPARK.Log.Error);
            CoAP_SPARK.Log.Put_Line
                (Option_Delta'Image, CoAP_SPARK.Log.Error);
            Status := Unknown_Critical_Option;
            return;
         else
            -- RFC 7252, section 5.4.1
            --  > Upon reception, unrecognized options of class "elective" MUST be
            --  > silently ignored.
            -- So we only log a debug message here.
            CoAP_SPARK.Log.Put
               ("Ignoring unknown elective option:", CoAP_SPARK.Log.Debug);
            CoAP_SPARK.Log.Put_Line
                (Option_Delta'Image, CoAP_SPARK.Log.Debug);
         end if;

      elsif Option_Length >
         CoAP_SPARK.Options.Option_Properties_Table
            (RFLX.CoAP.To_Actual (Option_Delta)).Maximum_Length
      then
         CoAP_SPARK.Log.Put
            ("Option value is too long for option ", CoAP_SPARK.Log.Error);
         CoAP_SPARK.Log.Put_Line
           (RFLX.CoAP.Option_Numbers'Image (RFLX.CoAP.To_Actual (Option_Delta)),
            CoAP_SPARK.Log.Error);
         Status := Malformed_Message;
         return;

      elsif Option_Length > 0 then
         declare
            Option_Value  : RFLX.RFLX_Types.Bytes_Ptr :=
              new RFLX.RFLX_Types.Bytes'
                (1 .. RFLX.RFLX_Builtin_Types.Index (Option_Length) => 0);
            Option_Number : constant RFLX.CoAP.Option_Numbers :=
              RFLX.CoAP.To_Actual (Option_Delta);
            Option : CoAP_SPARK.Options.Option;
         begin
            RFLX.CoAP.Option_Type.Get_Option_Value
              (Ctx => Option_Cxt, Data => Option_Value.all);

            if Option_Number = RFLX.CoAP.Content_Format then
               Decoded_Content.Format := CoAP_SPARK.Content_Formats.Content_Type
                  (CoAP_SPARK.Options.To_UInt (Value => Option_Value.all));
            end if;

            CoAP_SPARK.Options.New_Encoded_Option
              (Number => Option_Number,
               Value  => Option_Value,
               Result => Option);
            pragma Assert (Option_Value = null);

            CoAP_SPARK.Options.Lists.Append
              (Container => Decoded_Content.Options,
               New_Item => CoAP_SPARK.Options.To_Indefinite (Option));
            CoAP_SPARK.Options.Free (Option);
            pragma Assert (not CoAP_SPARK.Options.Has_Buffer (Option));
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
              (Container => Decoded_Content.Options,
               New_Item => CoAP_SPARK.Options.To_Indefinite (Option));
            CoAP_SPARK.Options.Free (Option);
            pragma Assert (not CoAP_SPARK.Options.Has_Buffer (Option));
         end;

         if RFLX.CoAP.To_Actual (Option_Delta) = RFLX.CoAP.Content_Format then
            Decoded_Content.Format := 0;
         end if;
      end if;

   end Add_Decode_Option;

   procedure Decode_Options_And_Payload
     (Data            : RFLX.RFLX_Types.Bytes;
      Status          : out CoAP_SPARK.Status_Type;
      Decoded_Content : out Content)
   is
   begin

      Status := OK;
      Decoded_Content :=
        (Options => CoAP_SPARK.Options.Lists.Empty_Vector,
         Format  => 0,
         Payload => null);

      if Data'Length = 0 then

         CoAP_SPARK.Log.Put_Line ("Options and payload are empty");

      elsif Data'Last = RFLX.RFLX_Types.Index'Last then

         CoAP_SPARK.Log.Put_Line ("Data is too long");
         Status := Capacity_Error;

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
            while RFLX.CoAP.Option_Sequence.Has_Element (Option_Sequence_Cxt)
            loop
               pragma Loop_Invariant (RFLX.CoAP.Option_Sequence.Has_Buffer
                                    (Option_Sequence_Cxt));
               pragma Loop_Invariant (RFLX.CoAP.Option_Sequence.Valid
                                         (Option_Sequence_Cxt));
               pragma Loop_Invariant
                  (CoAP_SPARK.Options.Lists.Length (Decoded_Content.Options) <
                     CoAP_SPARK.Max_Number_Of_Options);
               pragma Loop_Invariant (Option_Delta < Max_Allowed_Option_Delta);

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

               Add_Decode_Option
                 (Option_Cxt,
                  Decoded_Content,
                  Option_Delta,
                  Status,
                  End_Of_Options);

               RFLX.CoAP.Option_Sequence.Update
                 (Ctx => Option_Sequence_Cxt, Element_Ctx => Option_Cxt);
               pragma Assert (not RFLX.CoAP.Option_Type.Has_Buffer (Option_Cxt));

               if CoAP_SPARK.Options.Lists.Length (Decoded_Content.Options) =
                  CoAP_SPARK.Max_Number_Of_Options
                  and then not End_Of_Options
               then
                  CoAP_SPARK.Log.Put_Line
                    ("Too many options", CoAP_SPARK.Log.Error);
                  Status := Capacity_Error;
                  exit Read_Options;
               end if;

               if Option_Delta >= Max_Allowed_Option_Delta
                  and then not End_Of_Options
               then
                  CoAP_SPARK.Log.Put_Line
                    ("Max allowed option number value reached", CoAP_SPARK.Log.Error);
                  Status := Capacity_Error;
                  exit Read_Options;
               end if;

               exit Read_Options when End_Of_Options;
            end loop Read_Options;

            if Status = OK
              and then RFLX.CoAP.Option_Sequence.Sequence_Last (Option_Sequence_Cxt)
                       < Option_Sequence_Cxt.Last
              and then RFLX.CoAP.Option_Type.Has_Buffer (Option_Cxt)
            then
               -- When there is something left to be read, it is the payload
               Save_Payload :
               declare
                  use type RFLX.RFLX_Builtin_Types.Byte;
                  Payload_Marker : constant RFLX.RFLX_Builtin_Types.Byte :=
                    16#FF#;
                  First          : constant RFLX.RFLX_Builtin_Types.Index :=
                    RFLX.RFLX_Builtin_Types.Index
                      (RFLX.CoAP.Option_Sequence.Sequence_Last (Option_Sequence_Cxt)
                       / 8
                       + 1);
                  Last           :
                    constant RFLX.RFLX_Builtin_Types.Index'Base :=
                      RFLX.RFLX_Builtin_Types.Index'Base (Option_Cxt.Last / 8);
               begin

                  RFLX.CoAP.Option_Type.Take_Buffer
                    (Ctx => Option_Cxt, Buffer => Buffer);
                  pragma
                    Assert (not RFLX.CoAP.Option_Type.Has_Buffer (Option_Cxt));

                  if First >= Last
                    or else First not in Buffer'Range
                    or else Buffer (First) /= Payload_Marker
                  then
                     CoAP_SPARK.Log.Put_Line
                       ("Error: Payload marker not found before the payload",
                        CoAP_SPARK.Log.Error);

                     if First in Buffer'Range then
                        CoAP_SPARK.Log.Put_Line
                          ("Remaining bytes: ", CoAP_SPARK.Log.Error);
                        CoAP_SPARK.Log.Put_Line
                          (Item  =>
                             CoAP_SPARK.Options.Image
                               (Format => CoAP_SPARK.Options.Opaque,
                                Value  => Buffer (First ..
                                    RFLX.RFLX_Types.Index'Min (Last,
                                                           CoAP_SPARK.Options.
                                                             Max_Option_Value_Length
                                                             - First + 1))),
                           Level => CoAP_SPARK.Log.Error);
                     else
                        CoAP_SPARK.Log.Put_Line
                          ("Unexpected case for payload",
                           CoAP_SPARK.Log.Error);
                     end if;
                     RFLX.RFLX_Types.Free (Buffer);

                     Status := Malformed_Message;
                  else
                     -- Make sure the Response_Content payload is freed before overwriting it
                     if Decoded_Content.Payload /= null then
                        RFLX.RFLX_Types.Free (Decoded_Content.Payload);
                     end if;

                     if Last - First > CoAP_SPARK.Max_Payload_Length then
                        CoAP_SPARK.Log.Put_Line
                          ("Payload is too long", CoAP_SPARK.Log.Error);

                        Status := Capacity_Error;
                     else
                        Decoded_Content.Payload :=
                          new RFLX.RFLX_Types.Bytes'
                            (Buffer (First + 1 .. Last));
                     end if;
                     RFLX.RFLX_Types.Free (Buffer);
                  end if;
               end Save_Payload;
            end if;
            if RFLX.CoAP.Option_Type.Has_Buffer (Option_Cxt) then
               RFLX.CoAP.Option_Type.Take_Buffer
               (Ctx => Option_Cxt, Buffer => Buffer);
               pragma Assert (not RFLX.CoAP.Option_Type.Has_Buffer
                                    (Option_Cxt));
               RFLX.RFLX_Types.Free (Buffer);
            end if;
            if RFLX.CoAP.Option_Sequence.Has_Buffer (Option_Sequence_Cxt) then
               RFLX.CoAP.Option_Sequence.Take_Buffer
               (Ctx => Option_Sequence_Cxt, Buffer => Buffer);
               pragma Assert (not RFLX.CoAP.Option_Sequence.Has_Buffer
                                    (Option_Sequence_Cxt));
               RFLX.RFLX_Types.Free (Buffer);
            end if;
         end;
      end if;

   end Decode_Options_And_Payload;

end CoAP_SPARK.Messages.Encoding;