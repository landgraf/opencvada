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

procedure Distrans is
   procedure Help is
   begin
      Put_Line ("Program to demonstrate the use of the distance transform function between edge images.");
      Put_Line ("Usage:");
      Put_Line ("./distrans [image_name -- default image is stuff.jpg]");
      New_Line;
      Put_Line ("Hot keys:");
      Put_Line (Ascii.Ht & "ESC - quite the program");
      Put_Line (Ascii.Ht & "C - use C/Inf metric");
      Put_Line (Ascii.Ht & "L1 - use L1 metric");
      Put_Line (Ascii.Ht & "L2 - use L2 metric");
      Put_Line (Ascii.Ht & "3 - use 3x3 mask");
      Put_Line (Ascii.Ht & "5 - use 5x5 mask");
      Put_Line (Ascii.Ht & "0 - use precise distance transform");
      Put_Line (Ascii.Ht & "SPACE - loop through all the modes");
   end Help;

   Window_Name   : constant String := "Distance transform";
   Trackbar_Name : constant String := "Threshold";

   -- The output images
   Dist     : Ipl_Image_P;
   Dist_8u1 : Ipl_Image_P;
   Dist_8u2 : Ipl_Image_P;
   Dist_8u  : Ipl_Image_P;
   Dist_32s : Ipl_Image_P;

   Gray     : Ipl_Image_P;
   Edge     : Ipl_Image_P;

   procedure On_Trackbar (Position : Integer);
   pragma Convention (C, On_Trackbar);

   procedure On_Trackbar (Position : Integer) is
   begin
      CvThreshold (To_Arr (Gray), To_Arr (Edge), Long_Float (Position), Long_Float (Position), CV_THRESH_BINARY);
      CvDistTransform (To_Arr (Edge), To_Arr (Dist), CV_DIST_L2, CV_DIST_MASK_5);

      CvConvertScale (To_Arr (Dist), To_Arr (Dist), 5000.0);
      CvPow (To_Arr (Dist), To_Arr (Dist), 0.5);

      CvConvertScale (To_Arr (Dist), To_Arr (Dist_32s), 1.0, 0.5);
      CvAndS (To_Arr (Dist_32s), CvScalarAll (255.0), To_Arr (Dist_32s));
      CvConvertScale (To_Arr (Dist_32s), To_Arr (Dist_8u1), 1.0);
      CvConvertScale (To_Arr (Dist_32s), To_Arr (Dist_32s), -1.0);
      CvAddS (To_Arr (Dist_32s), CvScalarAll (255.0), To_Arr (Dist_32s));
      CvConvertScale (To_Arr (Dist_32s), To_Arr (Dist_8u2), 1.0);
      CvMerge (To_Arr (Dist_8u1), To_Arr (Dist_8u2), To_Arr (Dist_8u2), null, To_Arr (Dist_8u));
      CvShowImage (Window_Name, To_Arr (Dist_8u));
   end On_Trackbar;

   Filename    : Unbounded_String := To_Unbounded_String ("stuff.jpg");
   Edge_Thresh : aliased Integer := 100;
   I_Ret       : Integer;
   C_Ret       : Character;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Filename := To_Unbounded_String (Ada.Command_Line.Argument (1));
   end if;

   Gray := CvLoadImage (To_String (Filename), CV_LOAD_IMAGE_GRAYSCALE);

   if Gray = null then
      Put_Line ("Failed to load" & To_String (Filename));
      return;
   end if;

   Dist     := CvCreateImage (CvSize (Gray.all.Width, Gray.all.Height), IPL_DEPTH_32F, 1);
   Dist_8u1 := CvCloneImage (Gray);
   Dist_8u2 := CvCloneImage (Gray);
   Dist_8u  := CvCreateImage (CvSize (Gray.all.Width, Gray.all.Height), IPL_DEPTH_8U, 3);
   Dist_32s := CvCreateImage (CvSize (Gray.all.Width, Gray.all.Height), IPL_DEPTH_32S, 1);

   Edge := CvCloneImage (Gray);

   I_Ret := CvNamedWindow (Window_Name, 1);
   I_Ret := CvCreateTrackbar (Trackbar_Name, Window_Name, Edge_Thresh'Access, 255, On_Trackbar'Unrestricted_Access);

   On_Trackbar (Edge_Thresh);

   C_Ret := CvWaitKey (0);
end Distrans;
