with Highgui; use Highgui;
with Core; use Core;
with Calib_3D;
use Calib_3D;

procedure Chess_Demo is
   Capture : aliased Cv_Capture_P;
   Image   : Ipl_Image_P;
   Retval  : Integer;
   Corners : Cv_Point_2D_32F_Array (1 .. 49);
   Corner_Count : aliased Integer;
begin
   for I in Corners'Range loop
      Corners (I) := (0.0, 0.0);
   end loop;

   Capture := CvCreateCameraCapture (0);
   Retval := CvNamedWindow ("Chess Demo");

   loop
      Image := CvQueryFrame (Capture);

      Retval := Cv_Find_Chessboard_Corners (+Image, (7, 7), Corners, Corner_Count'Access);

      Cv_Draw_Chessboard:Corners (+Image, (7, 7), Corners, Corner_Count, Retval);

      Cv_Show_Image ("Chess Demo", +Image);

      exit when Cv_Wait_Key(30) = ASCII.ESC;
   end loop;

   Cv_Release_Capture (Capture'Access);
   Cv_Destroy_Window ("Chess Demo");

end Chess_Demo;
