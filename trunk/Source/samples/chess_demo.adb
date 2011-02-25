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
   Capture               : aliased Cv_Capture_Ptr;
   Image                 : Ipl_Image_Ptr;
   Corners_Found : Integer;
   Corners               : Cv_Point_2d_32f_Array (1 .. 49) := (others => (0.0,0.0));
   Corner_Count          : aliased Integer;
begin

   Capture := Cv_Create_Camera_Capture (0);
   Cv_Named_Window ("Chess Demo");

   loop
      Image := Cv_Query_Frame (Capture);

      Corners_Found := Cv_Find_Chessboard_Corners (Image, (7, 7), Corners, Corner_Count'Access);
      if Corners_Found = 1 then
         Cv_Draw_Chessboard_Corners (Image, (7, 7), Corners, Corner_Count, Corners_Found);
      end if;
      Cv_Show_Image ("Chess Demo", Image);
      exit when Cv_Wait_Key(30) = ASCII.ESC;
   end loop;

   Cv_Release_Capture (Capture'Access);
   Cv_Destroy_Window ("Chess Demo");

end Chess_Demo;
