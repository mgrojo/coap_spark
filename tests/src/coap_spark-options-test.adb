with AUnit.Assertions; use AUnit.Assertions;
with CoAP_SPARK.Options.Text_IO;

package body CoAP_SPARK.Options.Test is

   overriding
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test CoAP Options package");
   end Name;

   overriding
   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      Regular_Option, Final_Option, Copied_Option : Option;
      Value                                       :
        constant RFLX.RFLX_Builtin_Types.Bytes := [0, 1, 2, 3, 4];
      An_Indefinite_Option                        :
        Indefinite_Option (Value'Length);
   begin

      CoAP_SPARK.Options.New_Opaque_Option
        (Number      => RFLX.CoAP.ETag,
         Value       => Value,
         Order_Index => 1,
         Result      => Regular_Option);

      An_Indefinite_Option :=
        CoAP_SPARK.Options.To_Indefinite (Regular_Option);
      Final_Option := CoAP_SPARK.Options.To_Option (An_Indefinite_Option);

      CoAP_SPARK.Options.Text_IO.Print (Final_Option);

      Assert
        (CoAP_SPARK.Options."=" (Final_Option, Regular_Option),
         Message => "Invalid conversion or ""="" operator");

      Assert
        (not (CoAP_SPARK.Options."<" (Final_Option, Regular_Option)),
         Message => "Invalid conversion or ""<"" operator");

      Assert
        (not (CoAP_SPARK.Options."<" (Final_Option, Regular_Option)),
         Message => "Invalid conversion or ""<"" operator");

      Assert
        (CoAP_SPARK.Options.Value_Image (Regular_Option) /= "",
         "Value_Image is empty");

      Assert
        (CoAP_SPARK.Options.Get_Value (An_Indefinite_Option) = Value,
         "Value is not preserved");

      CoAP_SPARK.Options.Free (Regular_Option);
      CoAP_SPARK.Options.Free (Final_Option);

      CoAP_SPARK.Options.New_Empty_Option
        (Number => RFLX.CoAP.If_None_Match, Result => Regular_Option);

      CoAP_SPARK.Options.Copy
        (Source => Regular_Option, Target => Copied_Option);

      Assert
        (CoAP_SPARK.Options."=" (Regular_Option, Copied_Option),
         Message => "Invalid copy");

      CoAP_SPARK.Options.Free (Regular_Option);
      CoAP_SPARK.Options.Free (Copied_Option);

   end Run_Test;

end CoAP_SPARK.Options.Test;
