--
with Core; use Core;
with Core.Operations; use Core.Operations;
with Highgui; use Highgui;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;



-- benchmark_long file.avi something else
procedure Benchmark_Long is
   Capture : aliased cv_Capture_Ptr;
   Frame   : Ipl_Image_Ptr;
begin
   Cv_Named_Window ("test");
   if Argument_Count > 0 then
      Capture := Cv_Create_File_Capture (Argument(1));
      Put_Line("18");
   else
      return;
   end if;

   loop
      Frame := Cv_Query_Frame (Capture);
      Cv_Show_Image ("test", Frame);
      exit when Frame = null or Cv_Wait_Key(60/15) = Ascii.Esc;
   end loop;

   Cv_Release_Capture (Capture'Access);
   Cv_Destroy_Window ("test");
end Benchmark_Long;
