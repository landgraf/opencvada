---

with Core; use Core;
with Highgui; use Highgui;
with Imgproc; use Imgproc;
with Imgproc.Operations;
use Imgproc.Operations;
with Ada.Command_Line; use Ada.Command_Line;
with ADa.Strings.Unbounded; use ADa.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Core.Operations;
use Core.Operations;
with Interfaces; use Interfaces;

procedure Pyramid_Segmentation is

   Image : aliased Ipl_Image_P_Array (0 .. 2);

   Threshold1, Threshold2 : aliased Integer;

   Level : constant Integer := 3; -- possible error try with 3

   Block_Size : constant Integer := 1000;
   Result : Integer;
   Filter : Pyr_Filter := CV_GAUSSIAN_5x5;
   Comp : aliased Cv_Seq_P_Array(0 .. 0);
   Storage : aliased Cv_Mem_Storage_P;

   procedure On_Segment (Position : Integer);
   pragma Convention (C, On_Segment);

   procedure On_Segment (Position : Integer) is
   begin
      -- comp'Access
      CvPyrSegmentation (Image(1), Image(2), Storage, Comp, Level, Long_Float(Threshold1 + 1), Long_Float(Threshold2 + 1));
      CvShowImage ("Segmentation", +Image(2));
   end On_Segment;

   Filename : ADa.Strings.Unbounded.Unbounded_String := To_Unbounded_String("");
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Filename := Filename & Ada.Command_Line.Argument(1);
   else
      Filename := To_Unbounded_String("fruits.jpg");
   end if;

   Image (0) := CvLoadImage (To_String(Filename), 1);
   if Image (0) = null then
      return;
   end if;
   Put_Line(To_String(Filename));

   Result := CvNamedWindow ("Source", 0);
   CvShowImage ("Source", +Image (0));

   Result := CvNamedWindow ("Segmentation", 0);

   Storage := Core.Operations.CvCreateMemStorage (Block_Size);

   Image (0).all.Width := Integer (Unsigned_64 (Image (0).all.Width) and - Shift_Left (1, Level));
   Image (0).all.Height := Integer (Unsigned_64 (Image (0).all.Height) and - Shift_Left (1, Level));

   Image (1) := CvCloneImage (Image (0));
   Image (2) := CvCloneImage (Image (0));

   Threshold1 := 255;
   Threshold2 := 30;

   On_Segment (1);

   Result := CvCreateTrackbar ("Threshold1", "Segmentation", Threshold1'Access, 255, On_Segment'Unrestricted_Access);
   Result := CvCreateTrackbar ("Threshold2", "Segmentation", Threshold2'Access, 255, On_Segment'Unrestricted_Access);

   CvShowImage ("Segmentation", +Image (2));
   loop
      exit when CvWaitKey (0) = Ascii.Esc;
   end loop;

   CvDestroyWindow ("Segmentation");
   CvDestroyWindow ("Source");

   CvReleaseMemStorage (Storage'Access);

   CvReleaseImage (Image (0)'Access);
   CvReleaseImage (Image (1)'Access);
   CvReleaseImage (Image (2)'Access);

end Pyramid_Segmentation;

