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
            Cv_Release_Image (Buf (I)'Access);
            Buf (I) := Cv_Create_Image (CvSize(Size.Width, Size.Height), IPL_DEPTH_8U, 1);
            CvZero (+Buf (I));
         end loop;
         Cv_Release_Image (Mhi'Access);
         Cv_Release_Image (Orient'Access);
         Cv_Release_Image (Segmask'Access);
         Cv_Release_Image (Mask'Access);

         Mhi := Cv_Create_Image (Cv_Create_Size(Size.Width, Size.Height), IPL_DEPTH_32F, 1);
         Cv_Zero (+Mhi);
         Orient := Cv_Create_Image (Cv_Create_Size(Size.Width, Size.Height), IPL_DEPTH_32F, 1);
         Segmask := Cv_Create_Image (Cv_Create_Size(Size.Width, Size.Height), IPL_DEPTH_32F, 1);
         Mask := Cv_Create_Image (Cv_Create_Size(Size.Width, Size.Height), IPL_DEPTH_8u, 1);
      end if;

      Cv_Cvt_Color (+Img, +Buf (Last), CV_Bgr2gray);

      Idx2 := (Last + 1) mod N;
      Last := Idx2;

      Silh := Buf (Idx2);
      Cv_Abs_Diff (+Buf (Idx1), +Buf (Idx2), +Silh);

      Cv_Threshold (+Silh, +Silh, Diff_Threshold, 1.0, CV_THRESH_Binary);
      Cv_Update_Motion_History (+Silh, +Mhi, Timestamp, Mhi_Duration);

      Cv_Cvt_Scale (+Mhi, +Mask, 255.0 / Mhi_Duration,
                  (Mhi_Duration - Timestamp) * 255.0 / Mhi_Duration);
      Cv_Zero (+Dst);
      Cv_Merge (+Mask, null, null, null, +Dst);

      Cv_Calc_Motion_Gradient (+Mhi, +Mask, +Orient, Max_Time_Delta, Min_Time_Delta, 3);

      if Storage = null then
         Storage := Cv_Create_Mem_Storage (0);
      else
         Cv_Clear_Mem_Storage (Storage);
      end if;

      Seq := Cv_Segment_Motion (+Mhi, +Segmask, Storage, Timestamp, Max_Time_Delta);

      for I in Integer range -1 .. Seq.all.Total - 1
      loop
         if I < 0 then
            Comp_Rect := Cv_Create_rect (0, 0, Size.Width, Size.Height);
            Color := Cv_Rgb (255, 255, 255);
            Magnitude := 100.0;
         else
            Comp_Rect := Imgproc.From_Void (Cv_Get_Seq_Elem (Seq, I)).all.Rect;
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

            Cv_Set_Image_Roi (Silh, Comp_Rect);
            Cv_Set_Image_Roi (Mhi, Comp_Rect);
            Cv_Set_Image_Roi (Orient, Comp_Rect);
            Cv_Set_Image_Roi (Mask, Comp_Rect);

            Angle := Cv_Calc_Global_Orientation (+Orient, +Mask, +Mhi, Timestamp, Mhi_Duration);
            Angle := 360.0 - Angle;

            Count := Cv_Norm (+Silh, null, Cv_L1, null);

            Cv_Reset_Image_Roi ( Mhi );
            Cv_Reset_Image_Roi ( Orient );
            Cv_Reset_Image_Roi ( Mask );
            Cv_Reset_Image_Roi ( Silh );

            if Count < Long_Float (Comp_Rect.Width * Comp_Rect.Height) * 0.05 then
               null;
            else
               Center := Cv_Create_Point (Comp_Rect.X + Comp_Rect.Width / 2,
                                  Comp_Rect.Y + Comp_Rect.Height / 2);
               Cv_Circle (+Dst, Center, Cv_Round (Magnitude * 1.2), Color, 3, Cv_AA, 0);
               Cv_Line (+Dst, Center, Cv_Create_Point (Cv_Round (Long_Float (Center.X) + Magnitude * Cos (Angle * Long_Float (Cv_Pi) / 180.0)),
                 Cv_Round (Long_Float (Center.Y) - Magnitude * Sin (Angle * Cv_PI / 180.0))), Color, 3, Cv_AA, 0);
            end if;
         end if;
      end loop;
   end Update_Mhi;
   Motion         : aliased Ipl_Image_P := null;
   Capture        : aliased Cv_Capture_P;
   Image          : Ipl_Image_P := null;

   Ret            : Integer;
begin
   Capture := Highgui.Cv_Create_Camera_Capture (0);

   Ret := Cv_Named_Window ("Motion", 1);
   Ret := Cv_Named_Window ("Origin", 1);
   loop
      Image := Cv_Query_Frame (Capture);
      if Image = null then
         null;
         Put_Line("168: Image = null");
      else
         if Motion = null then
            Motion := Cv_Create_Image (Cv_Create_Size(Image.all.Width, Image.all.Height), 8, 3);
            Cv_Zero (+Motion);
            Motion.all.Origin := Image.all.Origin;
         end if;
         Update_Mhi (Image, Motion, 30.0);
         Cv_Show_Image ("Origin", +Image);
         Cv_Show_Image ("Motion", +Motion);
      end if;

      exit when Cv_Wait_Key (10) = Ascii.Esc;
      Cv_Release_Image (Motion'Access);
      Motion := null;
   end loop;

   Cv_Release_Capture (Capture'Access);
   Cv_Destroy_All_Windows;
end Motempl;
