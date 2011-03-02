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



-- benchmark_long file.avi
procedure Benchmark_Long is
   Capture : aliased cv_Capture_Ptr;
   Frame   : Ipl_Image_Ptr;
   Frames  : Interfaces.Integer_64 := 0;
   Bw,Recolor      : aliased Ipl_Image_Ptr;
   Storage         : aliased Cv_Mem_Storage_Ptr;
   Circles : Cv_Seq_Ptr;
   subtype Lf_Circle is Core.Cv_32f_Array (0 .. 2);
   Circle  : Lf_Circle;

   type Lf_Circle_Ptr is access Lf_Circle;

   function To_Void_Ptr is new Ada.Unchecked_Conversion    (Target => Cv_Void_Ptr,
                                                            Source => Cv_Mem_Storage_Ptr);
   function To_Lf_Circle_Ptr is new Ada.Unchecked_Conversion    (Target => Lf_Circle_Ptr,
                                                                 Source => Cv_Void_Ptr);
begin
   if Argument_Count > 0 then
      Capture := Cv_Create_File_Capture (Argument(1));
   else
      return;
   end if;
--     Frame := Cv_Load_Image ("cirklar.png");
   loop
      Frame := Cv_Query_Frame (Capture);
      --waste a bit of time
      exit when Frame = null;
      Storage := Cv_Create_Mem_Storage(0);
      Bw := Cv_Create_Image (Cv_Get_Size (Frame), 8, 1);
      Recolor := Cv_Create_Image (Cv_Get_Size (Frame), 8, 3);
      Cv_Cvt_Color (Frame, Bw, Cv_Bgr2gray);
      Cv_Canny (Bw, Bw, 10.0, 240.0);
      Circles := Imgproc.Operations.Cv_Hough_Circles (Bw, To_Void_Ptr (Storage), Cv_Hough_Gradient, 1.0, Long_Float (Bw.all.Height / 10), 300.0, 30.0);
      Cv_Cvt_Color (Bw, Recolor, Cv_Gray2bgr);
      for I in Integer range 0 .. Circles.all.Total - 1 loop
         Circle := To_Lf_Circle_Ptr (Cv_Get_Seq_Elem (Circles, I)).all;
         Cv_Draw_Circle (Recolor, Cv_Create_Point (Cv_Round(Circle(0)), Cv_Round(Circle(1))), Cv_Round(Circle(2)), Cv_Rgb (255, 0, 0));
      end loop;
--        Put_Line (Circles.all.Total'Img);
--        Cv_Show_Image("hej",Recolor);
      Frames := Frames + 1;
      Cv_Release_Image (Bw'Access);
      Cv_Release_Image (Recolor'Access);
      Cv_Release_Mem_Storage (Storage'Access);
   end loop;
   Put_Line("Frames: " & Frames'Img);
   Cv_Release_Capture (Capture'Access);
end Benchmark_Long;
