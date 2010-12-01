with Highgui; use Highgui;
with Core; use Core;
with Core.Operations;
use Core.Operations;
with Imgproc;
use Imgproc;
with Imgproc.Operations;
use Imgproc.Operations;
with Video.Background_Segm;
with Video.Blob_Track;
with Video.Tracking; -- empty
use Video.Tracking;
with Objdetect;
with Features_2d;
with Ff_Opencv;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;
with Ada.Numerics.Generic_Elementary_Functions; use Core.Long_Float_Numerics;

-- tiden funkar nog inte!
procedure Motempl is
-- tracking params (in seconds)
   MHI_Duration   : constant Long_Float := 1.0;
   Max_Time_Delta : constant Long_Float := 0.5;
   Min_Time_Delta : constant Long_Float := 0.05;
   --frame buffer N
   N              : constant Integer := 4;

   -- image buffers
   Buf            : Ipl_Image_P_Array (0 .. N - 1) := (others => null);
   Last           : Integer := 0;

   -- temp images
   Mhi, Orient, Mask, Segmask : aliased Ipl_Image_P := null;
   Storage        : Cv_Mem_Storage_P := null;

   procedure Update_Mhi (Img            : Ipl_Image_P;
                         Dst            : Ipl_Image_P;
                         Diff_Threshold : Long_Float) is
      Local_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Timestamp  : constant Long_Float := Long_Float (Ada.Calendar.Seconds (Local_Time));
      Size       : constant Cv_Size := CvSize (Img.all.Width, Img.all.Height);
      Idx2       : Integer;
      Idx1       : constant Integer := Last;
      Silh       : Ipl_Image_P;
      Seq        : Cv_Seq_P;
      Comp_Rect  : Cv_Rect;
      Count      : Long_Float;
      Angle      : Long_Float;
      Center     : Cv_Point;
      Magnitude  : Long_Float;
      Color      : Cv_Scalar;
      Skip       : Boolean := False;
   begin
--        Put_Line (Timestamp'Img);
      if Mhi = null or else Mhi.all.Width /= Size.Width or else Mhi.all.Height /= Size.Height then
         for I in Integer range Buf'Range loop
            CvReleaseImage (Buf (I)'Access);
            Buf (I) := CvCreateImage (Size.Width, Size.Height, IPL_DEPTH_8U, 1);
            CvZero (+Buf (I));
         end loop;
         CvReleaseImage (Mhi'Access);
         CvReleaseImage (Orient'Access);
         CvReleaseImage (Segmask'Access);
         CvReleaseImage (Mask'Access);

         Mhi := CvCreateImage (Size.Width, Size.Height, IPL_DEPTH_32F, 1);
         CvZero (+Mhi);
         Orient := CvCreateImage (Size.Width, Size.Height, IPL_DEPTH_32F, 1);
         Segmask := CvCreateImage (Size.Width, Size.Height, IPL_DEPTH_32F, 1);
         Mask := CvCreateImage (Size.Width, Size.Height, IPL_DEPTH_8u, 1);
      end if;

      CvCvtColor (+Img, +Buf (Last), CV_Bgr2gray);

      Idx2 := (Last + 1) mod N;
      Last := Idx2;

      Silh := Buf (Idx2);
      CvAbsDiff (+Buf (Idx1), +Buf (Idx2), +Silh);

      CvThreshold (+Silh, +Silh, Diff_Threshold, 1.0, CV_THRESH_Binary);
      CvUpdateMotionHistory (+Silh, +Mhi, Timestamp, Mhi_Duration);

      CvCvtScale (+Mhi, +Mask, 255.0 / Mhi_Duration,
                  (Mhi_Duration - Timestamp) * 255.0 / Mhi_Duration);
      CvZero (+Dst);
      CvMerge (+Mask, null, null, null, +Dst);

      CvCalcMotionGradient (+Mhi, +Mask, +Orient, Max_Time_Delta, Min_Time_Delta, 3);

      if Storage = null then
         Storage := CvCreateMemStorage (0);
      else
         CvClearMemStorage (Storage);
      end if;

      Seq := CvSegmentMotion (+Mhi, +Segmask, Storage, Timestamp, Max_Time_Delta);

      for I in Integer range -1 .. Seq.all.Total - 1
      loop
         if I < 0 then
            Comp_Rect := CvRect (0, 0, Size.Width, Size.Height);
            Color := Cv_Rgb (255, 255, 255);
            Magnitude := 100.0;
         else
            Comp_Rect := From_Void (CvGEtSeqElem (Seq, I)).all.Rect;
            if Comp_Rect.Width + Comp_Rect.Height < 100 then
               Skip := True;
            else
               Color := Cv_Rgb (255, 0, 0);
               Magnitude := 30.0;
            end if;
         end if;
         if Skip then
            Skip := False;
         else

            CvSetImageROI (Silh, Comp_Rect);
            CvSetImageROI (Mhi, Comp_Rect);
            CvSetImageROI (Orient, Comp_Rect);
            CvSetImageROI (Mask, Comp_Rect);

            Angle := CvCalcGlobalOrientation (+Orient, +Mask, +Mhi, Timestamp, Mhi_Duration);
            Angle := 360.0 - Angle;

            Count := CvNorm (+Silh, null, Cv_L1, null);

            CvResetImageROI ( Mhi );
            CvResetImageROI ( Orient );
            CvResetImageROI ( Mask );
            CvResetImageROI ( Silh );

            if Count < Long_Float (Comp_Rect.Width * Comp_Rect.Height) * 0.05 then
               null;
            else
               Center := CvPoint (Comp_Rect.X + Comp_Rect.Width / 2,
                                  Comp_Rect.Y + Comp_Rect.Height / 2);
               CvCircle (+Dst, Center, CvRound (Magnitude * 1.2), Color, 3, Cv_AA, 0);
               CvLine (+Dst, Center, CvPoint (CvRound (Long_Float (Center.X) + Magnitude * Cos (Angle * Long_Float (Cv_Pi) / 180.0)),
                 CvRound (Long_Float (Center.Y) - Magnitude * Sin (Angle * Cv_PI / 180.0))), Color, 3, Cv_AA, 0);
            end if;
         end if;
      end loop;
   end Update_Mhi;
   Motion         : aliased Ipl_Image_P := null;
   Capture        : aliased Cv_Capture_P;
   Image          : Ipl_Image_P := null;

   Ret            : Integer;
begin
   Capture := Highgui.CvCreateCameraCapture (0);

   Ret := CvNamedWindow ("Motion", 1);
   Ret := CvNamedWIndow ("Origin", 1);
   loop
      Image := CvQueryFrame (Capture);
      if Image = null then
         null;
         Put_Line("168: Image = null");
      else
         if Motion = null then
            Motion := CvCreateImage (Image.all.Width, Image.all.Height, 8, 3);
            CvZero (+Motion);
            Motion.all.Origin := Image.all.Origin;
         end if;
         Update_Mhi (Image, Motion, 30.0);
         CvShowImage ("Origin", +Image);
         CvShowImage ("Motion", +Motion);
      end if;

      exit when CvWaitKey (10) = Ascii.Esc;
      CvReleaseImage (Motion'Access);
      Motion := null;
   end loop;

   CvReleaseCapture (Capture'Access);
   Cvdestroyallwindows;
end Motempl;
