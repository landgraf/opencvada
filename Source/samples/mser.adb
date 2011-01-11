with Core; use Core;
with Core.Operations; use Core.Operations;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Highgui; use Highgui;

with Features_2d; use Features_2d;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Unchecked_Conversion;

procedure Mser is

   type Cv_Seq_P_P is access Cv_Seq_P;
   type Cv_Contour_P_P is access Cv_Contour_P;

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Contour_P_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Seq_P_P);

   procedure Help is
   begin
      Put_Line ("This program demonstrated the Maximal Extermal Region interest point detector.");
      Put_Line ("It finds the most stable (in size) dark and white regions as a threshold is increased.");
      Put_Line ("Call: ./mser < path_and_image_filename, Default is 'puzzle.png'>");
   end Help;

   Colors : constant array (0 .. 10) of Cv_Scalar := (CvScalar (0.0, 0.0, 244.0),
                                             CvScalar (0.0, 128.0, 255.0),
                                             CvScalar (0.0, 255.0, 255.0),
                                             CvScalar (0.0, 255.0, 0.0),
                                             CvScalar (255.0, 128.0, 0.0),
                                             CvScalar (255.0, 255.0, 0.0),
                                             CvScalar (255.0, 0.0, 0.0),
                                             CvScalar (255.0, 0.0, 255.0),
                                             CvScalar (255.0, 255.0, 255.0),
                                             CvScalar (196.0, 255.0, 255.0),
                                             CvScalar (255.0, 255.0, 196.0));

   B_Colors : constant array (0 .. 8, 0 .. 2) of Unsigned_8 := ((0, 0, 255),
                                                      (0, 128, 255),
                                                      (0, 255, 255),
                                                      (0, 255, 0),
                                                      (255, 128, 0),
                                                      (255, 255, 0),
                                                      (255, 0, 0),
                                                      (255, 0, 255),
                                                       (255, 255, 255));

   Path     : Unbounded_String;
   Img      : aliased Ipl_Image_P;

   Rsp      : aliased Ipl_Image_P;
   Rsp_Array : Cv_8u_Array_P;
   Ellipses : aliased Ipl_Image_P;
   Contours : aliased Cv_Seq_P := new Cv_Seq;
   Storage  : Cv_Mem_Storage_P;
   Hsv      : Ipl_Image_P;
   Params   : Cv_MSER_Params;

   R        : Cv_Seq_P;
   Pt       : Cv_Point_P;

   Contour  : Cv_Contour_P;
   Box      : Cv_Box_2d;

   Iterator : Cv_8u_Pointer;

   Use_Debug : constant Boolean := False;
begin
   Help;
   if Ada.Command_Line.Argument_Count = 0 then
      Path := To_Unbounded_String ("puzzle.png");
   else
      Path := To_Unbounded_String(Ada.Command_Line.Argument (1));
   end if;

   Img := CvLoadImage (To_String(Path), CV_LOAD_IMAGE_GRAYSCALE);

   if Img = null then
      Put_Line ("Could not load " & To_String(Path));
      return;
   end if;

   Storage := CvCreateMemStorage;

   Rsp := CvLoadImage (To_String (Path), CV_LOAD_IMAGE_COLOR);
   Ellipses := CvCloneImage (Rsp);
   Hsv := CvCreateImage (CvGetSize (To_Arr (Rsp)), IPL_DEPTH_8U, 3);
   CvCvtColor (To_Arr (Rsp), To_Arr (Hsv), CV_BGR2YCrCb);
   Params := CvMserParams;

   CvExtractMSER (To_Arr (Hsv), null, Contours'Access, Storage, Params);
   Rsp_Array := new Cv_8u_Array (1 .. (Rsp.all.Width * Rsp.all.Height * 3));
   Rsp_Array.all := Cv_8u_Pointer_Pkg.Value(Rsp.all.Image_Data, Ptrdiff_T(Rsp.all.Width * Rsp.all.Height * 3));

   Iterator := Rsp.all.Image_Data;

   for I in reverse 0 .. Contours.all.Total - 1 loop
      R := From_Void (CvGetSeqElem (Contours, I)).all;
      for J in Integer range 0 .. R.all.Total - 1 loop
         Pt := From_Void (CvGetSeqElem (R, J));

         if Use_Debug then
            Debug_Windows :
            declare
               I_Ret : Integer;
               C_Ret : Character;
               Debug : aliased constant Ipl_Image_P := CvCloneImage (Rsp);
               Iter  : Cv_8u_Pointer := Debug.all.Image_Data;
            begin

               for X in 1 .. Rsp.all.Width * Rsp.all.Height * 3 loop
                  Iter.all := (Rsp_Array.all (X) + 128) mod 255;
                  Cv_8u_Pointer_Pkg.Increment (Iter);
               end loop;

               I_Ret := CvNamedWindow ("original", 0);
               CvShowImage ("original", To_Arr (Img));

               I_Ret := CvNamedWindow ("response", 0);
               CvShowImage ("response", To_Arr (Rsp));

               I_Ret := CvNamedWindow ("debug", 0);
               CvShowImage ("debug", To_Arr (Debug));

               C_Ret := CvWaitKey (0);

               CvDestroyWindow ("original");
               CvDestroyWindow ("response");
               CvDestroyWindow ("debug");
            end Debug_Windows;
         end if;

         Rsp_Array.all (Pt.all.X * 3 + Pt.all.Y * Rsp.all.Width_Step) := B_Colors ((I mod 9), 2);
         Rsp_Array.all (Pt.all.X * 3 + 1 + Pt.all.Y * Rsp.all.Width_Step) := B_Colors ((I mod 9), 1);
         Rsp_Array.all (Pt.all.X * 3 + 2 + Pt.all.Y * Rsp.all.Width_Step) := B_Colors ((I mod 9), 0);
      end loop;
   end loop;

   for I in Integer range 1 .. Contours.all.Total loop
      Contour := From_Void (CvGetSeqElem (Contours, I)).all;
      Box := CvFitEllipse2 (To_Arr (Contour));
      Box.Angle := Float (CV_PI / 2.0 - Box.Angle);

      if Contour.all.Color > 0 then
         CvEllipseBox (To_Arr (Ellipses), Box, Colors (9), 2);
      else
         CvEllipseBox (To_Arr (Ellipses), Box, Colors (2), 2);
      end if;
   end loop;

   Main_Loop :
   declare
      I_Ret : Integer;
      C_Ret : Character;
   begin
      I_Ret := CvSaveImage ("rsp.png", To_Arr (Rsp));

      I_Ret := CvNamedWindow ("original", 0);
      CvShowImage ("original", To_Arr (Img));

      I_Ret := CvNamedWindow ("response", 0);
      CvShowImage ("response", To_Arr (Rsp));

      I_Ret := CvNamedWindow ("ellipses", 0);
      CvShowImage ("ellipses", To_Arr (Ellipses));

      C_Ret := CvWaitKey (0);

      CvDestroyWindow ("original");
      CvDestroyWindow ("response");
      CvDestroyWindow ("ellipses");

      CvReleaseImage (Rsp'Access);
      CvReleaseImage (Img'Access);
      CvReleaseImage (Ellipses'Access);
   end Main_Loop;
end Mser;
