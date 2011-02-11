with Highgui; use Highgui;
with Core; use Core;

procedure Camera_Test is
   Capture : aliased Cv_Capture_Ptr;
   Image   : Ipl_Image_Ptr;
--     Retval  : Integer;
begin
   Capture := Cv_Create_Camera_Capture (0);
   --     Retval := CvNamedWindow ("Test");
   for I in Integer range 1 .. 100 loop
      Image := Cv_Query_Frame (Capture);
   end loop;
--     CvShowImage ("test", +Image);
--     CvDestroyWindow ("test");
   Cv_Release_Capture (Capture'Access);

end Camera_Test;
