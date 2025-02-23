------------------------------------------------------------------------------
--                                                                          --
--               Generated by RecordFlux 0.25.0 on 2025-02-23               --
--                                                                          --
--                     Copyright (C) 2018-2025 AdaCore                      --
--                                                                          --
--         SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;
pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Types.Operations;

package body RFLX.CoAP.Option_Type with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; Written_Last : RFLX_Types.Bit_Length := 0) is
   begin
      Initialize (Ctx, Buffer, RFLX_Types.To_First_Bit_Index (Buffer'First), RFLX_Types.To_Last_Bit_Index (Buffer'Last), Written_Last);
   end Initialize;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX_Types.Bytes_Ptr; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length; Written_Last : RFLX_Types.Bit_Length := 0) is
      Buffer_First : constant RFLX_Types.Index := Buffer'First;
      Buffer_Last : constant RFLX_Types.Index := Buffer'Last;
   begin
      Ctx := (Buffer_First, Buffer_Last, First, Last, First - 1, (if Written_Last = 0 then First - 1 else Written_Last), Buffer, (F_Option_Delta => (State => S_Invalid, others => <>), others => <>));
      Buffer := null;
   end Initialize;

   procedure Reset (Ctx : in out Context) is
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer'First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer'Last));
   end Reset;

   procedure Reset (Ctx : in out Context; First : RFLX_Types.Bit_Index; Last : RFLX_Types.Bit_Length) is
   begin
      Ctx := (Ctx.Buffer_First, Ctx.Buffer_Last, First, Last, First - 1, First - 1, Ctx.Buffer, (F_Option_Delta => (State => S_Invalid, others => <>), others => <>));
   end Reset;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX_Types.Bytes_Ptr) is
   begin
      Buffer := Ctx.Buffer;
      Ctx.Buffer := null;
   end Take_Buffer;

   procedure Copy (Ctx : Context; Buffer : out RFLX_Types.Bytes) is
   begin
      if Buffer'Length > 0 then
         Buffer := Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last));
      else
         Buffer := Ctx.Buffer.all (1 .. 0);
      end if;
   end Copy;

   procedure Generic_Read (Ctx : Context) is
   begin
      Read (Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last)));
   end Generic_Read;

   procedure Generic_Write (Ctx : in out Context; Offset : RFLX_Types.Length := 0) is
      Length : RFLX_Types.Length;
   begin
      Reset (Ctx, RFLX_Types.To_First_Bit_Index (Ctx.Buffer_First), RFLX_Types.To_Last_Bit_Index (Ctx.Buffer_Last));
      Write (Ctx.Buffer.all (Ctx.Buffer'First + RFLX_Types.Index (Offset + 1) - 1 .. Ctx.Buffer'Last), Length, Ctx.Buffer'Length, Offset);
      pragma Assert (Length <= Ctx.Buffer.all'Length, "Length <= Buffer'Length is not ensured by postcondition of ""Write""");
      Ctx.Written_Last := RFLX_Types.Bit_Index'Max (Ctx.Written_Last, RFLX_Types.To_Last_Bit_Index (RFLX_Types.Length (Ctx.Buffer_First) + Offset + Length - 1));
   end Generic_Write;

   procedure Data (Ctx : Context; Data : out RFLX_Types.Bytes) is
   begin
      Data := Ctx.Buffer.all (RFLX_Types.To_Index (Ctx.First) .. RFLX_Types.To_Index (Ctx.Verified_Last));
   end Data;

   function Invalid_Successor (Ctx : Context; Fld : Field) return Boolean is
     ((case Fld is
          when F_Option_Delta =>
             Invalid (Ctx.Cursors (F_Option_Length)),
          when F_Option_Length =>
             Invalid (Ctx.Cursors (F_Option_Delta_Extended16))
             and Invalid (Ctx.Cursors (F_Option_Delta_Extended8))
             and Invalid (Ctx.Cursors (F_Option_Length_Extended16))
             and Invalid (Ctx.Cursors (F_Option_Length_Extended8))
             and Invalid (Ctx.Cursors (F_Option_Value)),
          when F_Option_Delta_Extended16 | F_Option_Delta_Extended8 =>
             Invalid (Ctx.Cursors (F_Option_Length_Extended16))
             and Invalid (Ctx.Cursors (F_Option_Length_Extended8))
             and Invalid (Ctx.Cursors (F_Option_Value)),
          when F_Option_Length_Extended16 | F_Option_Length_Extended8 =>
             Invalid (Ctx.Cursors (F_Option_Value)),
          when F_Option_Value =>
             True));

   function Sufficient_Buffer_Length (Ctx : Context; Fld : Field) return Boolean is
     (Ctx.Buffer /= null
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) < RFLX_Types.Bit_Length'Last
      and Ctx.First <= Field_First (Ctx, Fld)
      and Field_First (Ctx, Fld) + Field_Size (Ctx, Fld) - 1 <= Ctx.Written_Last)
    with
     Pre =>
       RFLX.CoAP.Option_Type.Has_Buffer (Ctx)
       and RFLX.CoAP.Option_Type.Valid_Next (Ctx, Fld);

   function Equal (Ctx : Context; Fld : Field; Data : RFLX_Types.Bytes) return Boolean is
     (Sufficient_Buffer_Length (Ctx, Fld)
      and then (case Fld is
                   when F_Option_Value =>
                      Data'Length = RFLX_Types.To_Index (Field_Last (Ctx, Fld)) - RFLX_Types.To_Index (Field_First (Ctx, Fld)) + 1
                      and then (for all I in RFLX_Types.Index range RFLX_Types.To_Index (Field_First (Ctx, Fld)) .. RFLX_Types.To_Index (Field_Last (Ctx, Fld)) =>
                                   Ctx.Buffer.all (I) = Data (Data'First + (I - RFLX_Types.To_Index (Field_First (Ctx, Fld))))),
                   when others =>
                      False));

   procedure Reset_Dependent_Fields (Ctx : in out Context; Fld : Field) with
     Pre =>
       RFLX.CoAP.Option_Type.Valid_Next (Ctx, Fld),
     Post =>
       Valid_Next (Ctx, Fld)
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and Field_Size (Ctx, Fld) = Field_Size (Ctx, Fld)'Old
       and (for all F in Field =>
               (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F) else Invalid (Ctx, F)))
   is
   begin
      for Fld_Loop in reverse Fld .. Field'Last loop
         pragma Loop_Invariant (Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Loop_Entry
                                and Field_Size (Ctx, Fld) = Field_Size (Ctx, Fld)'Loop_Entry);
         pragma Loop_Invariant ((for all F in Field =>
                                    (if F <= Fld_Loop then Ctx.Cursors (F) = Ctx.Cursors'Loop_Entry (F) else Invalid (Ctx, F))));
         Ctx.Cursors (Fld_Loop) := (State => S_Invalid, others => <>);
      end loop;
   end Reset_Dependent_Fields;

   function Composite_Field (Fld : Field) return Boolean is
     (Fld in F_Option_Value);

   function Get (Ctx : Context; Fld : Field) return RFLX_Types.Base_Integer with
     Pre =>
       RFLX.CoAP.Option_Type.Has_Buffer (Ctx)
       and then RFLX.CoAP.Option_Type.Valid_Next (Ctx, Fld)
       and then RFLX.CoAP.Option_Type.Sufficient_Buffer_Length (Ctx, Fld)
       and then not RFLX.CoAP.Option_Type.Composite_Field (Fld)
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, Fld);
      Last : constant RFLX_Types.Bit_Index := Field_Last (Ctx, Fld);
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (First);
      Buffer_Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Last);
      Offset : constant RFLX_Types.Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Size : constant Positive := (case Fld is
          when F_Option_Delta | F_Option_Length =>
             4,
          when F_Option_Delta_Extended16 =>
             16,
          when F_Option_Delta_Extended8 =>
             8,
          when F_Option_Length_Extended16 =>
             16,
          when F_Option_Length_Extended8 =>
             8,
          when others =>
             Positive'Last);
      Byte_Order : constant RFLX_Types.Byte_Order := RFLX_Types.High_Order_First;
   begin
      return RFLX_Types.Operations.Extract (Ctx.Buffer.all, Buffer_First, Buffer_Last, Offset, Size, Byte_Order);
   end Get;

   procedure Verify (Ctx : in out Context; Fld : Field) is
      Value : RFLX_Types.Base_Integer;
   begin
      if
         Invalid (Ctx.Cursors (Fld))
         and then Valid_Next (Ctx, Fld)
      then
         if Sufficient_Buffer_Length (Ctx, Fld) then
            Value := (if Composite_Field (Fld) then 0 else Get (Ctx, Fld));
            if
               Valid_Value (Fld, Value)
               and then Field_Condition (Ctx, Fld, Value)
            then
               pragma Assert ((if Fld = F_Option_Value then Field_Last (Ctx, Fld) mod RFLX_Types.Byte'Size = 0));
               pragma Assert ((((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
               Ctx.Verified_Last := ((Field_Last (Ctx, Fld) + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size;
               pragma Assert (Field_Last (Ctx, Fld) <= Ctx.Verified_Last);
               if Composite_Field (Fld) then
                  Ctx.Cursors (Fld) := (State => S_Well_Formed, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value);
               else
                  Ctx.Cursors (Fld) := (State => S_Valid, First => Field_First (Ctx, Fld), Last => Field_Last (Ctx, Fld), Value => Value);
               end if;
            else
               Ctx.Cursors (Fld) := (others => <>);
            end if;
         else
            Ctx.Cursors (Fld) := (State => S_Incomplete, others => <>);
         end if;
      end if;
   end Verify;

   procedure Verify_Message (Ctx : in out Context) is
   begin
      for F in Field loop
         pragma Loop_Invariant (Has_Buffer (Ctx)
                                and Ctx.Buffer_First = Ctx.Buffer_First'Loop_Entry
                                and Ctx.Buffer_Last = Ctx.Buffer_Last'Loop_Entry
                                and Ctx.First = Ctx.First'Loop_Entry
                                and Ctx.Last = Ctx.Last'Loop_Entry);
         Verify (Ctx, F);
      end loop;
   end Verify_Message;

   function Get_Option_Value (Ctx : Context) return RFLX_Types.Bytes is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).Last);
   begin
      return Ctx.Buffer.all (First .. Last);
   end Get_Option_Value;

   procedure Get_Option_Value (Ctx : Context; Data : out RFLX_Types.Bytes) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).Last);
   begin
      Data := (others => RFLX_Types.Byte'First);
      Data (Data'First .. Data'First + (Last - First)) := Ctx.Buffer.all (First .. Last);
   end Get_Option_Value;

   procedure Generic_Get_Option_Value (Ctx : Context) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).First);
      Last : constant RFLX_Types.Index := RFLX_Types.To_Index (Ctx.Cursors (F_Option_Value).Last);
   begin
      Process_Option_Value (Ctx.Buffer.all (First .. Last));
   end Generic_Get_Option_Value;

   procedure Set (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer; Size : RFLX_Types.Bit_Length; State_Valid : Boolean; Buffer_First : out RFLX_Types.Index; Buffer_Last : out RFLX_Types.Index; Offset : out RFLX_Types.Offset) with
     Pre =>
       RFLX.CoAP.Option_Type.Has_Buffer (Ctx)
       and then RFLX.CoAP.Option_Type.Valid_Next (Ctx, Fld)
       and then RFLX.CoAP.Option_Type.Valid_Value (Fld, Val)
       and then RFLX.CoAP.Option_Type.Valid_Size (Ctx, Fld, Size)
       and then Size <= RFLX.CoAP.Option_Type.Available_Space (Ctx, Fld)
       and then (if
                    RFLX.CoAP.Option_Type.Composite_Field (Fld)
                 then
                    Size mod RFLX_Types.Byte'Size = 0
                 else
                    State_Valid),
     Post =>
       Valid_Next (Ctx, Fld)
       and then Invalid_Successor (Ctx, Fld)
       and then Buffer_First = RFLX_Types.To_Index (Field_First (Ctx, Fld))
       and then Buffer_Last = RFLX_Types.To_Index (Field_First (Ctx, Fld) + Size - 1)
       and then Offset = RFLX_Types.Offset ((RFLX_Types.Byte'Size - (Field_First (Ctx, Fld) + Size - 1) mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size)
       and then Ctx.Buffer_First = Ctx.Buffer_First'Old
       and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and then Ctx.First = Ctx.First'Old
       and then Ctx.Last = Ctx.Last'Old
       and then Ctx.Buffer_First = Ctx.Buffer_First'Old
       and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and then Ctx.First = Ctx.First'Old
       and then Ctx.Last = Ctx.Last'Old
       and then Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and then Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
       and then Sufficient_Space (Ctx, Fld)
       and then (if State_Valid and Size > 0 then Valid (Ctx, Fld) else Well_Formed (Ctx, Fld))
       and then (Ctx.Cursors (Fld).Value = Val
                 and then (if
                              Fld in F_Option_Value
                              and then Well_Formed_Message (Ctx)
                           then
                              Message_Last (Ctx) = Field_Last (Ctx, Fld)))
       and then (for all F in Field =>
                    (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F)))
   is
      First : RFLX_Types.Bit_Index;
      Last : RFLX_Types.Bit_Length;
   begin
      Reset_Dependent_Fields (Ctx, Fld);
      First := Field_First (Ctx, Fld);
      Last := Field_First (Ctx, Fld) + Size - 1;
      Offset := RFLX_Types.Offset ((RFLX_Types.Byte'Size - Last mod RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size);
      Buffer_First := RFLX_Types.To_Index (First);
      Buffer_Last := RFLX_Types.To_Index (Last);
      pragma Assert ((((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size) mod RFLX_Types.Byte'Size = 0);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => ((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size, Written_Last => ((Last + RFLX_Types.Byte'Size - 1) / RFLX_Types.Byte'Size) * RFLX_Types.Byte'Size);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      if State_Valid then
         Ctx.Cursors (Fld) := (State => S_Valid, First => First, Last => Last, Value => Val);
      else
         Ctx.Cursors (Fld) := (State => S_Well_Formed, First => First, Last => Last, Value => Val);
      end if;
      pragma Assert (Last = (Field_First (Ctx, Fld) + Size) - 1);
   end Set;

   procedure Set_Scalar (Ctx : in out Context; Fld : Field; Val : RFLX_Types.Base_Integer) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.CoAP.Option_Type.Has_Buffer (Ctx)
       and then RFLX.CoAP.Option_Type.Valid_Next (Ctx, Fld)
       and then Fld in F_Option_Delta | F_Option_Length | F_Option_Delta_Extended16 | F_Option_Delta_Extended8 | F_Option_Length_Extended16 | F_Option_Length_Extended8
       and then RFLX.CoAP.Option_Type.Valid_Value (Fld, Val)
       and then RFLX.CoAP.Option_Type.Valid_Size (Ctx, Fld, RFLX.CoAP.Option_Type.Field_Size (Ctx, Fld))
       and then RFLX.CoAP.Option_Type.Available_Space (Ctx, Fld) >= RFLX.CoAP.Option_Type.Field_Size (Ctx, Fld)
       and then RFLX.CoAP.Option_Type.Field_Size (Ctx, Fld) in 1 .. RFLX_Types.Base_Integer'Size
       and then RFLX_Types.Fits_Into (Val, Natural (RFLX.CoAP.Option_Type.Field_Size (Ctx, Fld))),
     Post =>
       Has_Buffer (Ctx)
       and Valid (Ctx, Fld)
       and Invalid_Successor (Ctx, Fld)
       and (Ctx.Cursors (Fld).Value = Val
            and then (if
                         Fld in F_Option_Value
                         and then Well_Formed_Message (Ctx)
                      then
                         Message_Last (Ctx) = Field_Last (Ctx, Fld)))
       and (for all F in Field =>
               (if F < Fld then Ctx.Cursors (F) = Ctx.Cursors'Old (F)))
       and Ctx.Buffer_First = Ctx.Buffer_First'Old
       and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and Ctx.First = Ctx.First'Old
       and Ctx.Last = Ctx.Last'Old
       and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
       and Field_First (Ctx, Fld) = Field_First (Ctx, Fld)'Old
   is
      Buffer_First, Buffer_Last : RFLX_Types.Index;
      Offset : RFLX_Types.Offset;
      Size : constant RFLX_Types.Bit_Length := Field_Size (Ctx, Fld);
   begin
      Set (Ctx, Fld, Val, Size, True, Buffer_First, Buffer_Last, Offset);
      RFLX_Types.Lemma_Size (Val, Positive (Size));
      RFLX_Types.Operations.Insert (Val, Ctx.Buffer.all, Buffer_First, Buffer_Last, Offset, Positive (Size), RFLX_Types.High_Order_First);
   end Set_Scalar;

   procedure Set_Option_Delta (Ctx : in out Context; Val : RFLX.CoAP.Option_Base_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Delta, RFLX.CoAP.To_Base_Integer (Val));
   end Set_Option_Delta;

   procedure Set_Option_Length (Ctx : in out Context; Val : RFLX.CoAP.Option_Base_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Length, RFLX.CoAP.To_Base_Integer (Val));
   end Set_Option_Length;

   procedure Set_Option_Delta_Extended16 (Ctx : in out Context; Val : RFLX.CoAP.Option_Extended16_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Delta_Extended16, RFLX.CoAP.To_Base_Integer (Val));
   end Set_Option_Delta_Extended16;

   procedure Set_Option_Delta_Extended8 (Ctx : in out Context; Val : RFLX.CoAP.Option_Extended8_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Delta_Extended8, RFLX.CoAP.To_Base_Integer (Val));
   end Set_Option_Delta_Extended8;

   procedure Set_Option_Length_Extended16 (Ctx : in out Context; Val : RFLX.CoAP.Option_Extended16_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Length_Extended16, RFLX.CoAP.To_Base_Integer (Val));
   end Set_Option_Length_Extended16;

   procedure Set_Option_Length_Extended8 (Ctx : in out Context; Val : RFLX.CoAP.Option_Extended8_Type) is
   begin
      Set_Scalar (Ctx, F_Option_Length_Extended8, RFLX.CoAP.To_Base_Integer (Val));
   end Set_Option_Length_Extended8;

   procedure Set_Option_Value_Empty (Ctx : in out Context) is
      Unused_Buffer_First, Unused_Buffer_Last : RFLX_Types.Index;
      Unused_Offset : RFLX_Types.Offset;
   begin
      Set (Ctx, F_Option_Value, 0, 0, True, Unused_Buffer_First, Unused_Buffer_Last, Unused_Offset);
   end Set_Option_Value_Empty;

   procedure Initialize_Option_Value_Private (Ctx : in out Context; Length : RFLX_Types.Length) with
     Pre =>
       not Ctx'Constrained
       and then RFLX.CoAP.Option_Type.Has_Buffer (Ctx)
       and then RFLX.CoAP.Option_Type.Valid_Next (Ctx, RFLX.CoAP.Option_Type.F_Option_Value)
       and then RFLX.CoAP.Option_Type.Valid_Length (Ctx, RFLX.CoAP.Option_Type.F_Option_Value, Length)
       and then RFLX_Types.To_Length (RFLX.CoAP.Option_Type.Available_Space (Ctx, RFLX.CoAP.Option_Type.F_Option_Value)) >= Length,
     Post =>
       Has_Buffer (Ctx)
       and then Well_Formed (Ctx, F_Option_Value)
       and then Field_Size (Ctx, F_Option_Value) = RFLX_Types.To_Bit_Length (Length)
       and then Ctx.Verified_Last = Field_Last (Ctx, F_Option_Value)
       and then Ctx.Buffer_First = Ctx.Buffer_First'Old
       and then Ctx.Buffer_Last = Ctx.Buffer_Last'Old
       and then Ctx.First = Ctx.First'Old
       and then Ctx.Last = Ctx.Last'Old
       and then Valid_Next (Ctx, F_Option_Value) = Valid_Next (Ctx, F_Option_Value)'Old
       and then Get_Option_Delta (Ctx) = Get_Option_Delta (Ctx)'Old
       and then Get_Option_Length (Ctx) = Get_Option_Length (Ctx)'Old
       and then Field_First (Ctx, F_Option_Value) = Field_First (Ctx, F_Option_Value)'Old
   is
      First : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Value);
      Last : constant RFLX_Types.Bit_Index := Field_First (Ctx, F_Option_Value) + RFLX_Types.Bit_Length (Length) * RFLX_Types.Byte'Size - 1;
   begin
      pragma Assert (Last mod RFLX_Types.Byte'Size = 0);
      Reset_Dependent_Fields (Ctx, F_Option_Value);
      pragma Warnings (Off, "attribute Update is an obsolescent feature");
      Ctx := Ctx'Update (Verified_Last => Last, Written_Last => Last);
      pragma Warnings (On, "attribute Update is an obsolescent feature");
      Ctx.Cursors (F_Option_Value) := (State => S_Well_Formed, First => First, Last => Last, Value => 0);
   end Initialize_Option_Value_Private;

   procedure Initialize_Option_Value (Ctx : in out Context) is
   begin
      Initialize_Option_Value_Private (Ctx, RFLX_Types.To_Length (Field_Size (Ctx, F_Option_Value)));
   end Initialize_Option_Value;

   procedure Set_Option_Value (Ctx : in out Context; Data : RFLX_Types.Bytes) is
      Buffer_First : constant RFLX_Types.Index := RFLX_Types.To_Index (Field_First (Ctx, F_Option_Value));
      Buffer_Last : constant RFLX_Types.Index := Buffer_First + Data'Length - 1;
   begin
      Initialize_Option_Value_Private (Ctx, Data'Length);
      pragma Assert (Buffer_Last = RFLX_Types.To_Index (Field_Last (Ctx, F_Option_Value)));
      Ctx.Buffer.all (Buffer_First .. Buffer_Last) := Data;
      pragma Assert (Ctx.Buffer.all (RFLX_Types.To_Index (Field_First (Ctx, F_Option_Value)) .. RFLX_Types.To_Index (Field_Last (Ctx, F_Option_Value))) = Data);
   end Set_Option_Value;

   procedure Generic_Set_Option_Value (Ctx : in out Context; Length : RFLX_Types.Length) is
      First : constant RFLX_Types.Index := RFLX_Types.To_Index (Field_First (Ctx, F_Option_Value));
   begin
      if Length > 0 then
         Process_Option_Value (Ctx.Buffer.all (First .. First + RFLX_Types.Index (Length) - 1));
      end if;
      pragma Assert (RFLX.CoAP.Option_Type.Valid_Length (Ctx, RFLX.CoAP.Option_Type.F_Option_Value, Length));
      Initialize_Option_Value_Private (Ctx, Length);
   end Generic_Set_Option_Value;

end RFLX.CoAP.Option_Type;
