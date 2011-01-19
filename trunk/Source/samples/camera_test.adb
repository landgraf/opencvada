with Highgui; use Highgui;
with Core; use Core;

procedure Camera_Test is
   Capture : aliased Cv_Capture_P;
   Image   : Ipl_Image_P;
   Retval  : Integer;
begin
   Capture := CvCreateCameraCapture (0);
   --     Retval := CvNamedWindow ("Test");
   for I in Integer range 1 .. 100 loop
      Image := CvQueryFrame (Capture);
   end loop;
--     CvShowImage ("test", +Image);
--     CvDestroyWindow ("test");
   CvReleaseCapture (Capture'Access);

end Camera_Test;
