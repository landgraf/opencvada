---
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Highgui; use Highgui;
with Core; use Core;
with Core.Operations; use Core.Operations;
with Ada.Numerics.Generic_Elementary_Functions; use Core.Long_Float_Numerics;



procedure Contours is
   W        : constant := 500;
   Levels   : aliased Integer := 3;
   Contours : aliased Cv_Seq_P := null;

   procedure On_Trackbar (Position : Integer);
   pragma Convention (C, On_Trackbar);

   procedure On_Trackbar (Position : Integer) is

      Cnt_Img        : aliased Ipl_Image_P := CvCreateImage (W, W, 8, 3);
      Contours_Local : Cv_Seq_P := Contours;
      Levels_Local   : constant Integer := Levels - 3;
   begin
      if Levels_Local <= 0 then
         Contours_Local := Contours_Local.all.H_Next.all.H_Next.all.H_Next;
      end if;
      CvZero (+Cnt_Img);
      CvDrawContours (+Cnt_Img, Contours_Local, Cv_Rgb (255, 0, 0), Cv_Rgb (0, 255, 0), Levels_Local, 3, Cv_Aa);
      CvShowImage("contours",+Cnt_Img);
      CvReleaseImage (Cnt_Img'Access);
   end On_Trackbar;


   Storage  : aliased Cv_Mem_Storage_P := CvCreateMemStorage (0);
   Img      : aliased Ipl_Image_P := CvCreateImage (W, W, 8, 1);

   Dx, Dy   : Long_Float;
   White, Black : Cv_Scalar;
   Angle    : Long_Float;

   Ret      : Integer;
begin
   CvZero (+Img);

   for I in Integer range 0 .. 6 - 1 loop
      Dx := Long_Float(I mod 2) * 250.0 - 30.0;
      Dy := (Long_Float(I) / 2.0) * 150.0;
      White := CvRealScalar (255.0);
      Black := CvRealScalar (0.0);

      if I = 0 then
         for J in Integer range 0 .. 10 loop
            Angle := Long_Float((Long_Float(J) + 5.0) * Cv_Pi / 21.0);
            Cvline (+Img,
              CvPoint (CvRound (Dx + 100.0 + Long_Float(J) * 10.0 - 80.0 * Cos (Angle)),
                CvRound (Dy + 100.0 - 90.0 * Sin (Angle))),
              CvPoint (CvRound (Dx + 100.0 + Long_Float(J) * 10.0 - 30.0 * Cos (Angle)),
                CvRound (Dy + 100.0 - 30.0 * Sin (Angle))),
              White,
              1,
              8,
              0);
         end loop;
      end if;
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 150, Integer (Dy) + 100), CvSize (100, 70), 0.0, 0.0, 360.0, White, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 115, Integer (Dy) + 70), CvSize (30, 20), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 185, Integer (Dy) + 70), CvSize (30, 20), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 115, Integer (Dy) + 70), CvSize (15, 15), 0.0, 0.0, 360.0, White, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 185, Integer (Dy) + 70), CvSize (15, 15), 0.0, 0.0, 360.0, White, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 115, Integer (Dy) + 70), CvSize (5, 5), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 185, Integer (Dy) + 70), CvSize (5, 5), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 150, Integer (Dy) + 100), CvSize (10, 5), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 150, Integer (Dy) + 150), CvSize (40, 10), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 27, Integer (Dy) + 100), CvSize (20, 35), 0.0, 0.0, 360.0, White, -1, 8, 0 );
      CvEllipse ( +Img, CvPoint (Integer (Dx) + 273, Integer (Dy) + 100), CvSize (20, 35), 0.0, 0.0, 360.0, White, -1, 8, 0 );
   end loop;

   Ret := CvNamedWindow ("image", 1);
   CvShowImage ("image", +Img);

   Ret := CvFindContours (+Img, Storage, Contours'Access, Cv_Contour'Size / 8, Cv_Retr_Tree, Cv_Chain_Approx_Simple, Cvpoint (0, 0));

   Contours := CvApproxPoly (To_Void(Contours), Cv_Contour'Size / 8, Storage, Cv_Poly_Approx_Dp, 3.0, 1);

   Ret := CvNamedWindow ("contours", 1);
   Ret := CvCreateTrackbar ("levels+3", "contours", Levels'Access, 7, On_Trackbar'Unrestricted_Access); -- dont do this at home, at least dont blame me for it.

   On_Trackbar (0);

   loop
      exit when CvWaitKey (0) = Ascii.Esc;
   end loop;

   CvReleaseMemStorage (Storage'Access);
   CvReleaseImage (Img'Access);
   CvDestroyAllWindows;
end Contours;
