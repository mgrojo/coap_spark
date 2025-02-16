
-- Unbounded vector for the actual implementation of the list.
with SPARK.Containers.Formal.Unbounded_Vectors;

-- Functional verctor to be used only on the model side.
-- See "5.11.3. Functional Containers Library" in the SPARK User's Guide.
with SPARK.Containers.Functional.Vectors;
with Ada.Unchecked_Deallocation;

package CoAP_SPARK.Options.Lists
  with SPARK_Mode
is

   subtype Extended_Index is Natural;

   type Vector is private
   with
     Iterable =>
       (First       => Iter_First,
        Next        => Iter_Next,
        Has_Element => Iter_Has_Element,
        Element     => Element),
     Default_Initial_Condition => Last_Index (Vector) = 0,
     Annotate => (GNATprove, Ownership, "Needs_Reclamation");

   function First_Index (Container : Vector) return Natural;

   function Last_Index (Container : Vector) return Natural;

   function Is_Empty (Container : Vector) return Boolean
   is (Last_Index (Container) = 0)
   with Annotate => (GNATprove, Ownership, "Is_Reclaimed");

   function Iter_First (Container : Vector) return Extended_Index
   with Global => null;

   function Iter_Has_Element
     (Container : Vector; Position : Extended_Index) return Boolean
   with
     Global => null,
     Post =>
       Iter_Has_Element'Result
       = (Position in Natural'First .. Last_Index (Container));
   pragma Annotate (GNATprove, Inline_For_Proof, Iter_Has_Element);

   function Iter_Next
     (Container : Vector; Position : Extended_Index) return Extended_Index
   with Global => null, Pre => Iter_Has_Element (Container, Position);

   package Formal_Model
     with Ghost
   is

      package M is new
        SPARK.Containers.Functional.Vectors
          (Index_Type          => Positive,
           Element_Type        => Option_Model,
           "="                 => "=",
           Equivalent_Elements => "=");

      function Model (V : Vector) return M.Sequence
      with Post => M.Last (Model'Result) = Last_Index (V);
   end;
   use type Formal_Model.M.Sequence;

   function Element (V : Vector; P : Extended_Index) return Option
   with
     Global => null,
     Pre => P in 1 .. Last_Index (V),
     Post =>
       Model (Element'Result) = Formal_Model.M.Get (Formal_Model.Model (V), P);

   procedure Append (Container : in out Vector; New_Item : Option)
   with
     Global => null,
     Pre => Last_Index (Container) < Integer'Last,
     Post =>
       Last_Index (Container)
       = Last_Index (Container)'Old
         + 1

           --  Elements of Container are preserved

       and Formal_Model.Model (Container)'Old
           < Formal_Model.Model
               (Container)

               --  Container now has New_Item at the end of Container

       and Model (New_Item)
           = Formal_Model.M.Get
               (Formal_Model.Model (Container),
                Last_Index (Container)'Old + 1);

   procedure Clear (V : in out Vector)
   with Global => null, Post => Last_Index (V) = 0;

private
   pragma SPARK_Mode (Off);
   package Instance is new
     SPARK.Containers.Formal.Unbounded_Vectors
       (Index_Type   => Option_Index,
        Element_Type => Option);

   type Vector is record
      C : Instance.Vector;
   end record;

   function Iter_First (Container : Vector) return Extended_Index is
     (Natural'First);

   function Iter_Next
     (Container : Vector;
      Position  : Extended_Index) return Extended_Index
   is
     (if Position = Extended_Index'Last then
         Extended_Index'First
      else
         Extended_Index'Succ (Position));

   function Iter_Has_Element
     (Container : Vector;
      Position  : Extended_Index) return Boolean
   is
     (Position in Natural'First .. Last_Index (Container));

end CoAP_SPARK.Options.Lists;
