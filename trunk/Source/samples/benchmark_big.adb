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
procedure Benchmark_Big is
   Image         : aliased ipl_Image_Ptr;
   Image_Small   : aliased Ipl_Image_Ptr;
begin
   if Argument_Count > 0 then
      Image := Cv_Load_Image (Argument (1));
   else
      return;
   end if;

   Image_Small := Cv_Create_Image (Cv_Create_Size(640, 480), 8, 3);
   Imgproc.Operations.Cv_Resize (Image, Image_Small);

   Cv_Save_Image (Argument (1) & "_small.png", Image_Small);

   Cv_Release_Image (Image'Access);
   Cv_Release_Image (Image_Small'Access);
end Benchmark_Big;
