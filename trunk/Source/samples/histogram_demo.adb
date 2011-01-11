
with Core; use Core;
with Highgui;
use Highgui;
with Imgproc;
with Imgproc.Operations;
with Core.Operations;

procedure Histogram_Demo is
   Capture : aliased Cv_Capture_P := Highgui.CvCreateCameraCapture (0);
   Image   : aliased Ipl_Image_P;
   Ret     : Integer;
   pragma Unreferenced (Ret);

   --     Point_Array : Core.Cv_Point_Array (1 .. 3) := ((100, 100), (150, 150), (50, 150));
   --     Point_Array2 : Core.Cv_Point_Array (1 .. 3) := ((100 * 2, 100 * 2), (150 * 2, 150 * 2), (50 * 2, 150 * 2));
   --     C_Pointer_1 : Core.Cv_Point_CPtr := Point_ArraY (1)'Unchecked_Access;
   --     C_Pointer_2 : Core.Cv_Point_CPtr := Point_ArraY2 (1)'Unchecked_Access;
   --     Dubbel_Pek : Cv_Point_Pointer_Array := (C_Pointer_1, C_Pointer_2);

   Arr1    : aliased Cv_32f_2d_Array := ((0.0, 255.0),
                                         (0.0, 255.0),
                                         (0.0, 255.0));

   H_Bins  : constant Integer := 30;
   S_Bins  : constant Integer := 32;
   Scale   : constant Integer := 10;

   Hist    : aliased Cv_Histogram_P;
   Target_Hist : aliased Cv_Histogram_P;

   Max_Value : aliased Float := 0.0;
   Value   : aliased Long_Float := 0.0;
   Intensity : Integer := 0;

   -- hsv images
   Hist_Image, Hist_Image_2 , H, S, V, Hsv : aliased Ipl_Image_P;
   Target  : aliased Ipl_Image_P := Highgui.CvLoadImage ("target.jpg");

   -- float

   function GetHist (Image : Ipl_Image_P) return Cv_Histogram_P is

      Hist    : aliased constant Cv_Histogram_P := Imgproc.Operations.CvCreateHist (3,
                                                                                    (H_Bins, S_Bins, 30),
                                                                                    0,
                                                                                    Core.To_2d_Pointer (Arr1'Access));

      Hsv     : aliased Ipl_Image_P := Core.Operations.CvCreateImage (CvSize(Image.all.Width,
                                                                      Image.all.Height),
                                                                      Image.all.Depth,
                                                                      Image.all.N_Channels);
      H       : aliased Ipl_Image_P := Core.Operations.CvCreateImage (CvSize(Image.all.Width,
                                                                      Image.all.Height),
                                                                      Image.all.Depth,
                                                                      1);
      S       : aliased Ipl_Image_P := Core.Operations.CvCreateImage (CvSize(Image.all.Width,
                                                                      Image.all.Height),
                                                                      Image.all.Depth,
                                                                      1);
      V       : aliased Ipl_Image_P := Core.Operations.CvCreateImage (CvSize(Image.all.Width,
                                                                      Image.all.Height),
                                                                      Image.all.Depth,
                                                                      1);
   begin
      --        Put_Line ("doing histogram{");
      Imgproc.Operations.CvCvtColor (+Image, +Hsv, Imgproc.CV_BGR2HSV);
      Core.Operations.CvSplit (+Hsv, +H, +S, +V, null);
      Imgproc.Operations.CvCalcHist ((+H, +S, +V), Hist);
      --        Put_Line ("}done histogram");
      Core.Operations.CvReleaseImage (Hsv'Access);
      Core.Operations.CvReleaseImage (H'Access);
      Core.Operations.CvReleaseImage (S'Access);
      Core.Operations.CvReleaseImage (V'Access);
      return Hist;
   end GetHist;

   --     Char : Character ;
begin
   Ret := Highgui.CvNamedWindow ("hej");
   Ret := Highgui.CvNamedWindow ("histogram");
   Ret := Highgui.CvNamedWindow ("Target");

--     Highgui.CvShowImage ("Target", +Target);


   --     Char := Highgui.CvWaitKey (0);

   Target_Hist := GetHist (Target);
   --     Put_Line("Entering Mainloop");


   loop

      Ret := Highgui.CvGrabframe (Capture);

      Image := Highgui.CvRetrieveFrame (Capture);

      Hist_Image_2 := Core.Operations.CvCreateImage (CvSize(H_Bins * Scale, S_Bins * Scale), 8, 3);

      Hist_Image := Core.Operations.CvCreateImage (CvSize(Image.all.Width,
                                                   Image.all.Height),
                                                   Image.all.Depth,
                                                   1);

      Hsv := Core.Operations.CvCreateImage (CvSize(Image.all.Width, Image.all.Height), Image.all.Depth, Image.all.N_Channels);
      H := Core.Operations.CvCreateImage (CvSize(Image.all.Width, Image.all.Height), Image.all.Depth, 1);
      S := Core.Operations.CvCreateImage (CvSize(Image.all.Width, Image.all.Height), Image.all.Depth, 1);
      V := Core.Operations.CvCreateImage (CvSize(Image.all.Width, Image.all.Height), Image.all.Depth, 1);

      Imgproc.Operations.CvCvtColor (+Image, +Hsv, Imgproc.CV_BGR2HSV);
      Core.Operations.CvSplit (+Hsv, +H, +S, +V, null);


      Hist := GetHist (Image);

      Imgproc.Operations.CvCalcBackProject ((+H, +S, +V), +Hist_Image, Target_Hist);



      Imgproc.Operations.CvGetMinMaxHistValue (Hist, null, Max_Value'Access);
      for H_I in Integer range 0 .. H_Bins - 1 loop
         for S_I in Integer range 0 .. S_Bins - 1 loop
            Value := Core.Operations.CvGetReal3d (Hist.all.Bins, H_I, S_I,0);
--              Put(Value'Img & Max_Value'Img);
            Intensity := Core.CvRound ((Value * 255.0) / Long_Float (Max_Value));
            Core.Operations.CvRectangle (+Hist_Image_2,
                                         CvPoint (H_I * Scale, S_I * Scale),
                                         Cvpoint (((H_I + 1) * Scale)-1, ((S_I + 1) * Scale)-1),
                                         Core.Operations.Cv_RGB (Intensity, Intensity, Intensity),
                                         Core.Operations.Cv_Filled);
         end loop;
      end loop;

      Highgui.CvShowImage (WindowName => "Target", Image => +Hist_Image);
      Highgui.CvShowImage (WindowName => "histogram", Image => +Hist_Image_2);
      Highgui.CvShowImage (WindowName => "hej", Image => +Image);

      exit when Highgui.CvWaitKey (30) = Ascii.Esc;

      Core.Operations.CvReleaseImage (Hist_Image'Access);
      Core.Operations.CvReleaseImage (Hist_Image_2'Access);
      Core.Operations.CvReleaseImage (Hsv'Access);
      Core.Operations.CvReleaseImage (H'Access);
      Core.Operations.CvReleaseImage (S'Access);
      Core.Operations.CvReleaseImage (V'Access);
      Imgproc.Operations.CvReleaseHist (Hist'Access);
   end loop;

   --     Ret := Highgui.CvSaveImage("target.jpg",+Image);

   Core.Operations.CvReleaseImage (Hist_Image'Access);
   Core.Operations.CvReleaseImage (Target'Access);
   Imgproc.Operations.CvReleaseHist (Target_Hist'Access);
   Imgproc.Operations.CvReleaseHist (Hist'Access);
   Highgui.CvReleaseCapture (Capture'Access);
   Core.Operations.CvReleaseImage (Image'Access);
   Highgui.CvDestroyAllWindows;
end Histogram_Demo;
