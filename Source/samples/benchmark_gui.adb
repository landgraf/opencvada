--
with Core; use Core;
with Core.Operations; use Core.Operations;
with Highgui; use Highgui;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Ada.Unchecked_Conversion;



-- benchmark_big file.image
procedure Benchmark_Gui is
   Image         : aliased ipl_Image_Ptr;
begin
   if Argument_Count > 0 then
      Image := Cv_Load_Image (Argument (1));
   else
      return;
   end if;


   for I in Integer range 1 .. 100 loop
      Cv_Named_Window (I'Img);
   end loop;

   for I in Integer range 1 .. 100 loop
      Cv_Show_Image (I'Img, Image);
   end loop;

   for I in Integer range 1 .. 100 loop
      Cv_Destroy_Window (I'Img);
   end loop;

   Cv_Release_Image (Image'Access);
end Benchmark_Gui;
