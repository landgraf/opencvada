with Highgui; use Highgui;
with Core; use Core;
with Calib_3D;
use Calib_3D;

procedure Chess_Demo is
   Capture : Cv_Capture_P;
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
      Retval := CvFindChessboardCorners (+Image, (7, 7), Corners, Corner_Count'Access);

      CvDrawChessboardCorners (+Image, (7, 7), Corners, Corner_Count, Retval);

      CvShowImage ("Chess Demo", +Image);

      exit when CvWaitKey(30) = ASCII.ESC;
   end loop;

end Chess_Demo;
