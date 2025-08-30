package Coverage
   with SPARK_Mode, Abstract_State => Report
is

   -- Dumps the current coverage data to the gcov report files.
   -- This version requires linking with the gcov library.
   procedure Dump
   with Always_Terminates,
      Global => Report,
      Import,
      Convention => C,
      External_Name => "__gcov_dump";

end Coverage;