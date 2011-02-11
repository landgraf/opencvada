with Highgui; use Highgui;
with Core; use Core;
with Calib_3D;
use Calib_3D;
with Core.Operations; use Core.Operations;
with Imgproc.Operations;
use Imgproc.Operations;
with Imgproc; use Imgproc;
with Ada.Text_IO; use Ada.Text_IO;

procedure Chess_Demo is
   Capture      : aliased Cv_Capture_Ptr;
   Image        : Ipl_Image_Ptr;
   Bw           : aliased Ipl_Image_Ptr;
   Retval       : Integer;
   Corners      : Cv_Point_2d_32f_Array (1 .. 49);
   Corner_Count : aliased Integer;
begin
   for I in Corners'Range loop
      Corners (I) := (0.0, 0.0);
   end loop;

   Capture := Cv_Create_Camera_Capture (0);
   Retval := Cv_Named_Window ("Chess Demo");

   loop
      Image := Cv_Query_Frame (Capture);

      Bw := Cv_Create_Image ((Image.all.Width,Image.all.Height), Image.all.Depth, 1);

      Cv_Cvt_Color(+Image,+Bw,Cv_Rgb2gray);
      Retval := Calib_3d.Cv_Check_Chessboard (Bw, (6, 6));
      Put_Line(Retval'Img);
      if  Retval = 1 then
         Retval := Cv_Find_Chessboard_Corners (+Image, (7, 7), Corners, Corner_Count'Access);
         Cv_Draw_Chessboard_Corners (+Image, (7, 7), Corners, Corner_Count, Retval);
         Cv_Show_Image ("Chess Demo", +Image);
      else
         Cv_Show_Image ("Chess Demo", +Bw);
      end if;

      exit when Cv_Wait_Key(30) = ASCII.ESC;
   end loop;

   Cv_Release_Image (Bw'Access);
   Cv_Release_Capture (Capture'Access);
   Cv_Destroy_Window ("Chess Demo");

end Chess_Demo;
