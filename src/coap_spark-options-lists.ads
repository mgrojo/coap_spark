pragma SPARK_Mode (On);

-- Unbounded vector for the actual implementation of the list.
with SPARK.Containers.Formal.Unbounded_Vectors;

-- We store in the list the indefinite version of the option, so we pass
-- the requirements of the SPARK containers.
package CoAP_SPARK.Options.Lists is new SPARK.Containers.Formal.Unbounded_Vectors
   (Index_Type   => Option_Index,
    Element_Type => Indefinite_Option);