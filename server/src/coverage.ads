package Coverage
   with SPARK_Mode
is

   -- Dumps the current coverage data to the gcov report files.
   -- This version does nothing, so the same code can be used without
   -- linking with the gcov library when coverage data is not collected.
   procedure Dump with Always_Terminates;

end Coverage;