with Ada.Strings;
with Ada.Strings.Fixed;

package body CoAP_SPARK.URI
  with SPARK_Mode
is

   ------------
   -- Create --
   ------------

   function Create
     (Scheme : String := Default_Scheme;
      Host   : String;
      Port   : Interfaces.Unsigned_16 := CoAP_SPARK.Default_Port;
      Path   : String := "/";
      Query  : String := "") return URI
   is
      Port_Image : String := Port'Image;
      Result     :
        URI
          (Scheme'Length + Scheme_Suffix'Length + Host'Length
           + Port_Image'Length
           + Path'Length
           + Query'Length);
   begin
      -- Replace the padding space with a colon
      Port_Image (Port_Image'First) := ':';

      Result.URI_String :=
        Scheme
        & Scheme_Suffix
        & Host
        & Host_Suffix
        & Port_Image
        & Path
        & Query;
      Result.Scheme_Last := Scheme'Length;
      Result.Host_Last :=
        Result.Scheme_Last + Scheme_Suffix'Length + Host'Length;
      Result.Port_Last :=
        Result.Host_Last + Host_Suffix'Length + Port'Image'Length;
      Result.Path_Last := Result.Port_Last + Path'Length;

      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (URI_String : String) return URI is
      URI_Object : URI (URI_String'Length);
      Aux_Index  : Natural;
   begin
      URI_Object.URI_String := URI_String;
      Aux_Index :=
        Ada.Strings.Fixed.Index
          (Source  => URI_Object.URI_String,
           Pattern => Scheme_Suffix,
           From    => URI_Object.URI_String'First);

      if Aux_Index = 0 then
         URI_Object.Scheme_Last := 0;

         Aux_Index :=
           Ada.Strings.Fixed.Index
             (Source  => URI_Object.URI_String,
              Pattern => ":",
              From    => URI_Object.URI_String'First);
      else
         URI_Object.Scheme_Last := Aux_Index - 1;

         Aux_Index :=
           Ada.Strings.Fixed.Index
             (Source  => URI_Object.URI_String,
              Pattern => ":",
              From    => URI_Object.Scheme_Last + Scheme_Suffix'Length + 1);
      end if;

      URI_Object.Host_Last := 0;

      if Aux_Index /= 0 then
         URI_Object.Host_Last := Aux_Index - 1;
      end if;

      Aux_Index :=
        Ada.Strings.Fixed.Index
          (Source  => URI_Object.URI_String,
           Pattern => "/",
           From    => URI_Object.Scheme_Last + Scheme_Suffix'Length + 1);

      if Aux_Index = 0 then
         -- No path part
         if URI_Object.Host_Last = 0 then
            URI_Object.Host_Last := URI_Object.URI_String'Last;
         end if;
         URI_Object.Port_Last := URI_Object.URI_String'Last;
         URI_Object.Path_Last := URI_Object.URI_String'Last;
         URI_Object.Port_Last := URI_Object.URI_String'Last;
      else
         -- Path part
         if URI_Object.Host_Last = 0 then
            -- No port part
            URI_Object.Host_Last := Aux_Index - 1;
         end if;
         URI_Object.Port_Last := Aux_Index - 1;

         Aux_Index :=
           Ada.Strings.Fixed.Index
             (Source  => URI_Object.URI_String,
              Pattern => "?",
              From    => URI_Object.Host_Last + Host_Suffix'Length + 1);

         if Aux_Index = 0 then
            -- No query part
            URI_Object.Path_Last := URI_Object.URI_String'Last;
         else
            -- Query part
            URI_Object.Path_Last := Aux_Index - 1;
         end if;
      end if;

      return URI_Object;
   end Create;

   ------------
   -- Scheme --
   ------------

   function Scheme (Self : URI) return String is
   begin
      return Self.URI_String (1 .. Self.Scheme_Last);
   end Scheme;

   ----------
   -- Host --
   ----------

   function Host (Self : URI) return String is
   begin
      return
        Self.URI_String
          (Self.Scheme_Last + Scheme_Suffix'Length + 1 .. Self.Host_Last);
   end Host;

   ----------
   -- Port --
   ----------

   function Port (Self : URI) return Interfaces.Unsigned_16 is
   begin
      if Self.Host_Last = Self.Port_Last then
         return CoAP_SPARK.Default_Port;
      end if;

      return
        Interfaces.Unsigned_16'Value
          (Self.URI_String
             (Self.Host_Last + Host_Suffix'Length + 1 .. Self.Port_Last));
   end Port;

   ----------
   -- Path --
   ----------

   function Path (Self : URI) return String is
   begin
      return Self.URI_String (Self.Port_Last + 1 .. Self.Path_Last);
   end Path;

   -----------
   -- Query --
   -----------

   function Query (Self : URI) return String is
   begin
      return Self.URI_String (Self.Path_Last + 1 .. Self.URI_String'Last);
   end Query;

end CoAP_SPARK.URI;

