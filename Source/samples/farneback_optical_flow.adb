with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Core; use Core;
with Core.Operations; use Core.Operations;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Highgui; use Highgui;
with Video.Tracking; use Video.Tracking;

procedure Farneback_Optical_Flow is

   function From_Void1 is new
     Ada.Unchecked_Conversion (Source => Cv_Void_P,
                               Target => Cv_Point_2d_32f_P);

   procedure Help is
   begin
      Put_Line ("This program demonstrates dense Farneback optical flow");
      Put_Line ("It reads from camera 0 and shows how to use and display dense Farneback optical flow");
      Put_Line ("Call: ./farneback_optical_flow");
   end Help;

   procedure DrawOptFlowMap (Flow       : Cv_Mat_P;
                             C_Flow_Map : Cv_Mat_P;
                             Step       : Integer;
                             Scale      : Long_Float;
                             Color      : Cv_Scalar) is
      X, Y : Integer := 0;
--        Fxy  : Cv_Point_2d_32f;
      Fxy_P : Cv_Point_2d_32f_P;
   begin
      while Y < C_Flow_Map.all.Rows loop
         while X < C_Flow_Map.all.Cols loop
            Fxy_P := From_Void1(CvMatElem(Flow, Cv_Point_2d_32f'Size, Y, X));
--              Fxy := CvMatElem (Flow, Cv_Point_2d_32f'Size, Y, X);
            CvLine (To_Arr(C_Flow_Map), CvPoint (X, Y), CvPoint (CvRound (Float(X) + Fxy_P.all.X), CvRound (Float(Y) + Fxy_P.all.Y)), Color, 1, 8, 0);
            CvCircle (To_Arr(C_Flow_Map), CvPoint (X, Y), 2, Color, -1, 8, 0);
            X := X + Step;
         end loop;
         Y := Y + Step;
         X := 0;
      end loop;
   end DrawOptFlowMap;

   Capture   : aliased Cv_Capture_P := CvCreateCameraCapture (0);
   Prev_Gray : Cv_Mat_P := null;
   Gray      : Cv_Mat_P := null;
   Flow      : Cv_Mat_P := null;
   C_Flow    : Cv_Mat_P := null;
   Temp      : Cv_Mat_P := null;

   First_Frame : Integer := 0;
   Frame       : Ipl_Image_P;
   Ret         : Integer := 0;
begin
   Help;

   if Capture = null then
      return;
   end if;

   Ret := CvNamedWindow ("Flow", 1);

   loop
      First_Frame := 0;
      Frame := CvQueryFrame (Capture);

      if Frame = null then
         exit;
      end if;

      if Gray = null then
         Gray      := CvCreateMat (Frame.all.Height, Frame.all.Width, Cv_Maketype (Cv_8u, 1));
         Prev_Gray := CvCreateMat (Gray.all.Rows, Gray.all.Cols, Gray.all.Mat_Type);
         Flow      := CvCreateMat (Gray.all.Rows, Gray.all.Cols, Cv_Maketype (Cv_32f, 2));
         C_Flow    := CvCreateMat (Gray.all.Rows, Gray.all.Cols, Cv_Maketype (Cv_8u, 3));
      end if;

      CvCvtColor (To_Arr(Frame), To_Arr(Gray), CV_BGR2GRAY);

      if First_Frame = 0 then
         CvCalcOpticalFlowFarneback (To_Arr(Prev_Gray), To_Arr(Gray), To_Arr(Flow), 0.5, 3, 15, 3, 5, 1.2, 0);
         CvCvtColor (To_Arr(Prev_Gray), To_Arr(C_Flow), Cv_Gray2bgr);
         DrawOptFlowMap (Flow, C_Flow, 16, 1.5, Cv_Rgb (0, 255, 0));
         CvShowImage ("Flow", To_Arr (C_Flow));
      end if;

      if CvWaitKey (30) = Ascii.Esc then
         exit;
      end if;

      Temp := Prev_Gray;
      Prev_Gray := Gray;
      Gray := Temp;
   end loop;
   CvReleaseCapture (Capture'Access);
end Farneback_Optical_Flow;
