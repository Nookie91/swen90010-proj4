with Ada.Text_IO; use Ada.Text_IO;

with FatBatExample;

-- This procedure demonstrates a simple user of AccountManagementSystem.
procedure Example is
begin
   Put_Line("Running FatBatExample...");
   Put_Line("");

   FatBatExample.Run;

   Put_Line("");
   Put_Line("FatBatExample Finished!");
end Example;
