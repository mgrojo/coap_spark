package body Coverage
   with SPARK_Mode => Off
is

   -- Dumps the current coverage data to the gcov report files.
   -- This version requires linking with the gcov library.
   procedure Gcov_Dump
   with Always_Terminates,
      Import,
      Convention => C,
      External_Name => "__gcov_dump";

   procedure Dump is
   begin
      Gcov_Dump;
   end Dump;

end Coverage;