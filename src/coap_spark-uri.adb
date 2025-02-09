with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with CoAP_SPARK.Utils;

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
   begin

      if Port_Image /= "" then
         -- Replace the padding space with a colon
         Port_Image (Port_Image'First) := ':';
      end if;

      declare
         Result_String : constant String :=
            Scheme & Scheme_Suffix & Host & Port_Image & Path &
            (if Query = "" then "" else Query_Prefix & Query);
      begin
         return
           (Length      => Result_String'Length,
            Valid       => True,
            URI_String  => Result_String,
            Scheme_Last => Scheme'Length,
            Host_Last   => Scheme'Length + Scheme_Suffix'Length + Host'Length,
            Port_Last   => Scheme'Length + Scheme_Suffix'Length + Host'Length +
              Port'Image'Length,
            Path_Last   => Scheme'Length + Scheme_Suffix'Length + Host'Length +
              Port'Image'Length + Path'Length);
      end;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (URI_String : String) return URI is

      URI_Object : URI :=
        (URI_String  => URI_String,
         Length      => URI_String'Length,
         Valid       => False,
         Scheme_Last => 0,
         Host_Last   => 0,
         Port_Last   => 0,
         Path_Last   => 0);

      Aux_Index, Host_First  : Natural;
   begin

      Aux_Index :=
        Ada.Strings.Fixed.Index
          (Source  => URI_Object.URI_String,
           Pattern => Scheme_Suffix,
           From    => URI_Object.URI_String'First);

      if Aux_Index = 0 then
         URI_Object.Scheme_Last := 0;

         Host_First := URI_Object.URI_String'First;

         Aux_Index :=
           Ada.Strings.Fixed.Index
             (Source  => URI_Object.URI_String,
              Pattern => ":",
              From    => Host_First);

      elsif Aux_Index + Scheme_Suffix'Length in URI_Object.URI_String'Range then

         URI_Object.Scheme_Last := Aux_Index - 1;
         Host_First := URI_Object.Scheme_Last + Scheme_Suffix'Length + 1;

         Aux_Index :=
           Ada.Strings.Fixed.Index
             (Source  => URI_Object.URI_String,
              Pattern => ":",
              From    => Host_First);
      else
         URI_Object.Valid := False;
         return URI_Object;
      end if;

      URI_Object.Host_Last := 0;

      if Aux_Index /= 0 then
         URI_Object.Host_Last := Aux_Index - 1;
      end if;

      Aux_Index :=
        Ada.Strings.Fixed.Index
          (Source  => URI_Object.URI_String,
           Pattern => "/",
           From    => Host_First);

      if Aux_Index = 0 then
         -- No path part
         if URI_Object.Host_Last = 0 then
            URI_Object.Host_Last := URI_Object.URI_String'Last;
         end if;
         URI_Object.Path_Last := URI_Object.URI_String'Last;
         URI_Object.Port_Last := URI_Object.URI_String'Last;
      else
         -- Path part
         if URI_Object.Host_Last = 0 then
            -- No port part
            URI_Object.Host_Last := Aux_Index - 1;
         end if;
         URI_Object.Port_Last := Aux_Index - 1;

         if URI_Object.Host_Last + Host_Suffix'Length + 1
            not in URI_Object.URI_String'Range
         then
            URI_Object.Valid := False;
            return URI_Object;
         end if;

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

      URI_Object.Valid := True;
      return URI_Object;
   end Create;

   ------------
   -- Scheme --
   ------------

   function Scheme (Self : URI) return String is
   begin
      if Self.URI_String'First not in Self.URI_String'Range or else
         Self.Scheme_Last not in Self.URI_String'Range
      then
         return "";
      end if;
      return Self.URI_String (Self.URI_String'First .. Self.Scheme_Last);
   end Scheme;

   ----------
   -- Host --
   ----------

   function Host (Self : URI) return String is
   begin
      if Self.Scheme_Last + Scheme_Suffix'Length + 1 not in
           Self.URI_String'Range
        or else Self.Host_Last not in Self.URI_String'Range
      then
         return "";
      end if;

      return
        Self.URI_String
          (Self.Scheme_Last + Scheme_Suffix'Length + 1 .. Self.Host_Last);
   end Host;

   ----------
   -- Port --
   ----------

   function Port (Self : URI) return Interfaces.Unsigned_16 is
   begin

      -- No port was defined in the URI
      if Self.Host_Last = Self.Port_Last then
         return (if Scheme (Self) = Default_Scheme then
                     CoAP_SPARK.Default_Port
                  elsif Scheme (Self) = Secure_Scheme then
                     CoAP_SPARK.Secure_Port
                  else
                     0);
      end if;

      if Self.Host_Last + Host_Suffix'Length + 1 not in Self.URI_String'Range
        or else Self.Port_Last not in Self.URI_String'Range or else
         (for some C of
           Self.URI_String (Self.Host_Last + Host_Suffix'Length + 1 .. Self.Port_Last) =>
           not Ada.Characters.Handling.Is_Decimal_Digit (C)) or else
           Self.Port_Last - Self.Host_Last + Host_Suffix'Length >
           Interfaces.Unsigned_16'Width
      then
         return 0;
      end if;

      return
        CoAP_SPARK.Utils.Value
          (Self.URI_String
             (Self.Host_Last + Host_Suffix'Length + 1 .. Self.Port_Last));

   end Port;

   ----------
   -- Path --
   ----------

   function Path (Self : URI) return String is
   begin
      if Self.Port_Last + 1 not in Self.URI_String'Range or else
         Self.Path_Last not in Self.URI_String'Range
      then
         return "";
      end if;
      return Self.URI_String (Self.Port_Last + 1 .. Self.Path_Last);
   end Path;

   -----------
   -- Query --
   -----------

   function Query (Self : URI) return String is
   begin
      if Self.Path_Last < Self.URI_String'Last
        and then Self.URI_String (Self.Path_Last + 1) =
          Query_Prefix (Query_Prefix'First)
      then
         return
           Self.URI_String
             (Self.Path_Last + Query_Prefix'Length + 1
              .. Self.URI_String'Last);
      else
         return "";
      end if;
   end Query;

end CoAP_SPARK.URI;

