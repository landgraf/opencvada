--
with Ada.Text_IO; use Ada.Text_IO;
with Core; use Core;
with Imgproc;
use Imgproc;
with Imgproc.Operations;
use Imgproc.Operations;
with Core.Operations;
use Core.Operations;
with Highgui;
use Highgui;
with Ada.Command_Line;
with Interfaces; use Interfaces;
--
procedure Polar_Transforms is
   procedure Help is
   begin
      New_Line;
      Put_Line ("This program illustrates Linear-Polar and Log-Polar image transforms");
      Put_Line ("Call:");
      Put_Line ("./polar_transforms [[camera number -- Default 0],[AVI path_filename]]");
      New_Line;
   end Help;

   Capture : aliased Cv_Capture_P := null;
   Log_Polar_Img, Lin_Polar_Img, Recovered_Img : Ipl_Image_P := null;
   Frame : Ipl_Image_P;
   Temp : Integer;
   Center : aliased Cv_Point_2D_32F;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Capture := Highgui.CvCreateCameraCapture (0);
   elsif Integer'Value (Ada.Command_Line.Argument (1)) >= 0 then
      Temp := Integer'Value (Ada.Command_Line.Argument (1));
      if Character'Pos (Ada.Command_Line.Argument (1) (1)) <  58 and Character'Pos (Ada.Command_Line.Argument (1) (1)) > 47  then
         Capture := CvCreateCameraCapture (Cv_Cap (Temp));
      else
         Capture := CvCreateFileCapture (Ada.Command_Line.Argument (1));
      end if;
   end if;

   if Capture = null then
      Put_Line ("Could not initialize capturing...");
      Put_Line ("Usage: %s <CAMERA_NUMBER>    , or \n       %s <VIDEO_FILE");
      return;
   end if;

   Temp := CvNamedWindow ("Linear-Polar", 0);
   Temp := CvNamedWindow ( "Log-Polar", 0 );
   Temp := CvNamedWindow ( "Recovered image", 0 );

   CvMoveWindow ( "Linear-Polar", 20, 20 );
   CvMoveWindow ( "Log-Polar", 700, 20 );
   CvMoveWindow ( "Recovered image", 20, 400 );

   loop
      Frame := CvQueryFrame (Capture);
      exit when Frame = null;

      if Log_Polar_Img = null then
         Log_Polar_Img := CvCreateImage ( CvSize(Frame.all.Width, Frame.all.Height), IPL_DEPTH_8U, 3 );
         Lin_Polar_Img := CvCreateImage ( CvSize(Frame.all.Width, Frame.all.Height), IPL_DEPTH_8U, 3 );
         Recovered_Img := CvCreateImage ( CvSize (Frame.all.Width, Frame.all.Height), IPL_DEPTH_8U,3 );
         Put_Line("Images done");
      end if;
      --        Interfaces.Shift_Right;

      Center.X := Float (Interfaces.Shift_Right (Unsigned_32 (Frame.all.Width), 1));
      Center.Y := Float (Interfaces.Shift_Right (Unsigned_32 (Frame.all.Height), 1));
      CvLogPolar (+Frame, +Log_Polar_Img,
                  Center,
                  70.0,
                 1+ 8);

      CvLinearPolar (+Frame, +Lin_Polar_Img,
                     Center,
                     70.0,
                     1 + 8);

      CvLinearPolar (+Lin_Polar_Img, +Recovered_Img,
                     Center,
                     70.0,
                     16 + 1 + 8);

      CvShowImage ("Log-Polar", +Log_Polar_Img );
      CvShowImage ("Linear-Polar", +Lin_Polar_Img );
      CvShowImage ("Recovered image", +Recovered_Img );
      exit when CvWaitKey (10) = Ascii.Esc;
   end loop;
   CvReleaseCapture ( Capture'Access );
   CvDestroyWindow ("Linear-Polar");
   CvDestroyWindow ("Log-Polar");
   CvDestroyWindow ("Recovered image");
end Polar_Transforms;
