--
--
with Core; use Core;
with Highgui; use Highgui;
with Core.Operations; use Core.Operations;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Morphology is
   Src, Dst      : aliased Ipl_Image_P := null;
   Element       : aliased Ipl_Conv_Kernel_P := null;
   Element_Shape : Structuring_Shape := CV_SHAPE_RECT;

   Max_Iters     : constant Integer := 10;
   Open_Close_Pos : aliased Integer := 0;
   Erode_Dilate_Pos : aliased Integer := 0;

   procedure Open_Close (Position : Integer);
   pragma Convention (C, Open_Close);
   procedure Erode_Dilate (Position : Integer);
   pragma Convention (C, Erode_Dilate);

   procedure Open_Close (Position : Integer) is
      N, An : Integer;
   begin
      N := Open_Close_Pos - Max_Iters;
      if N > 0 then
         An := N;
      else
         An := -N;
      end if;
      Element := CvCreateStructuringElementEx (An * 2 + 1, An * 2 + 1, An, An, Element_Shape, null);
      if (N < 0) then
         CvErode (+Src, +Dst, Element, 1);
         CvDilate (+Src, +Dst, Element, 1);
      else
         CvDilate (+Src, +Dst, Element, 1);
         CvErode (+Src, +Dst, Element, 1);
      end if;
      CvReleaseStructuringElement (Element'Access);
      CvShowImage("Open/Close",+Dst);
   end Open_Close;


   procedure Erode_Dilate (Position : Integer) is
      N, An : Integer;
   begin
      N := Erode_Dilate_Pos - Max_Iters;
      if N > 0 then
         An := N;
      else
         An := -N;
      end if;
      Element := CvCreateStructuringElementEx (An * 2 + 1, An * 2 + 1, An, An, Element_Shape, null);
      if (N < 0) then
         CvErode (+Src, +Dst, Element, 1);
      else
         CvDilate (+Src, +Dst, Element, 1);
      end if;
      CvReleaseStructuringElement (Element'Access);
      CvShowImage("Erode/Dilate",+Dst);
   end Erode_Dilate;

   procedure Help is
   begin
      Put_Line ("This program demonstrated the use of the morphology operator, especially open, close, erode, dilate operations");
      Put_Line ("Morphology operators are built on max (close) and min (open) operators as measured by pixels covered by small structuring elements.");
      Put_Line ("These operators are very efficient.");
      Put_Line ("This program also allows you to play with elliptical, rectangluar and cross structure elements");
      Put_Line ("Call:");
      Put_Line ("./morphologyc [image_name -- Default baboon.jpg]");
      Put_Line ("Hot keys:");
      Put_Line (Ascii.Ht & "ESC - quit the program");
      Put_Line (Ascii.Ht & "r - use rectangle structuring element");
      Put_Line (Ascii.Ht & "e - use elliptic structuring element");
      Put_Line (Ascii.Ht & "c - use cross-shaped structuring element");
      Put_Line (Ascii.Ht & "SPACE - loop through all the options");
   end Help;

   Filename      : Unbounded_String;
   Ret           : Integer := 0;
   Char          : Character;
begin

   Help; -- help text

   if Ada.Command_Line.Argument_Count = 1 then
      Filename := To_Unbounded_String (Ada.Command_Line.Argument (1));
   else
      Filename := To_Unbounded_String ("Ada.jpg");
   end if;
   Src := CvLoadImage (To_String (Filename), 1);
   if Src = null then
      return ;
   end if;

   Dst := CvCloneImage (Src);

   Ret := CvNamedWindow ("Open/Close", 0);
   Ret := CvNamedWindow ("Erode/Dilate", 0);

   Open_Close_Pos := Max_Iters;
   Erode_Dilate_Pos := Max_Iters;
   Ret := CvCreateTrackbar ("iterations", "Open/Close", Open_Close_Pos'Access, Max_Iters * 2 + 1, Open_Close'Unrestricted_Access);
   Ret := CvCreateTrackbar ("iterations", "Erode/Dilate", Erode_Dilate_Pos'Access, Max_Iters * 2 + 1, Erode_Dilate'Unrestricted_Access);

   loop

      Open_Close (Open_Close_Pos);
      Erode_Dilate (Erode_Dilate_Pos);

      Char := CvWaitKey (0);
      exit when Char = Ascii.Esc;

      if Char = 'e' then
         Element_Shape := CV_SHAPE_ELLIPSE;
      elsif Char = 'r' then
         Element_Shape := CV_SHAPE_RECT;
      elsif Char = 'c' then
         Element_Shape := Cv_SHAPE_CROSS;
      elsif Char = ' ' then
         Element_Shape := Structuring_Shape'Val((Structuring_Shape'Pos(Element_Shape) + 1) mod 3);
      end if;
   end loop;

   CvDestroyWindow ("Open/Close");
   CvDestroyWindow ("Erode/Dilate");
   CvReleaseImage (Src'Access);
   CvReleaseImage (Dst'Access);
end Morphology;
