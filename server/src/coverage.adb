package body Coverage
   with SPARK_Mode => Off
is

   -- A body is needed to allow having the Always_Terminates aspect in the spec.
   procedure Dump is null;

end Coverage;