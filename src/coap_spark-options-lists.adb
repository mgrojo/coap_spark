
package body CoAP_SPARK.Options.Lists
  with SPARK_Mode => Off
is

   function First_Index (Container : Vector) return Natural
   is (Instance.First_Index (Container.C));
   function Last_Index (Container : Vector) return Natural
   is (Instance.Last_Index (Container.C));

   package body Formal_Model is
      function Model (V : Vector) return M.Sequence is
         R : M.Sequence;

      begin
         for I in 1 .. Instance.Last_Index (V.C) loop
            R := M.Add (R, Model (Instance.Element (V.C, I)));
         end loop;

         return R;
      end Model;
   end Formal_Model;

   function Element (V : Vector; P : Extended_Index) return Option is
      Target : Option;
   begin
      Copy (Instance.Element (V.C, P), Target);
      return Target;
   end Element;

   procedure Append (Container : in out Vector; New_Item : Option) is
      Copy_Item : Option;
   begin
      Copy (New_Item, Copy_Item);
      Instance.Append (Container.C, Copy_Item);
   end Append;

   procedure Clear (V : in out Vector) is
   begin
      for I in 1 .. Instance.Last_Index (V.C) loop
         declare
            Opt : Option := Instance.Last_Element (V.C);
         begin
            Instance.Delete_Last (V.C);
            Free (Opt);
         end;
      end loop;
   end Clear;

end CoAP_SPARK.Options.Lists;