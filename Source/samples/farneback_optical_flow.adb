with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Core; use Core;
with Core.Operations; use Core.Operations;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Highgui; use Highgui;
with Video.Tracking; use Video.Tracking;

procedure Farneback_Optical_Flow is

   function To_Point_2d_32f_Ptr is new
     Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                               Target => Cv_Point_2d_32f_Ptr);

   procedure Help is
   begin
      Put_Line ("This program demonstrates dense Farneback optical flow");
      Put_Line ("It reads from camera 0 and shows how to use and display dense Farneback optical flow");
      Put_Line ("Call: ./farneback_optical_flow");
   end Help;

   procedure Draw_Opt_Flow_Map (Flow       : Cv_Mat_Ptr;
                                C_Flow_Map : Cv_Mat_Ptr;
                                Step       : Integer;
                                Scale      : Long_Float;
                                Color      : Cv_Scalar) is
      X, Y : Integer := 0;
--        Fxy  : Cv_Point_2d_32f;
      Fxy_P : Cv_Point_2d_32f_Ptr;
   begin
      while Y < C_Flow_Map.all.Rows loop
         while X < C_Flow_Map.all.Cols loop
            Fxy_P := To_Point_2d_32f_Ptr (Cv_Mat_Elem (Flow, Cv_Point_2d_32f'Size, Y, X));
--              Fxy := CvMatElem (Flow, Cv_Point_2d_32f'Size, Y, X);
            Cv_Line (To_Arr_Ptr (C_Flow_Map), Cv_Create_Point (X, Y), Cv_Create_Point (Cv_Round (Float (X) + Fxy_P.all.X), Cv_Round (Float (Y) + Fxy_P.all.Y)), Color, 1, 8, 0);
            Cv_Circle (To_Arr_Ptr (C_Flow_Map), Cv_Create_Point (X, Y), 2, Color, -1, 8, 0);
            X := X + Step;
         end loop;
         Y := Y + Step;
         X := 0;
      end loop;
   end Draw_Opt_Flow_Map;

   Capture   : aliased Cv_Capture_Ptr := Cv_Create_Camera_Capture (0);
   Prev_Gray : Cv_Mat_Ptr := null;
   Gray      : Cv_Mat_Ptr := null;
   Flow      : Cv_Mat_Ptr := null;
   C_Flow    : Cv_Mat_Ptr := null;
   Temp      : Cv_Mat_Ptr := null;

   First_Frame : Integer := 0;
   Frame       : Ipl_Image_Ptr;
   Ret         : Integer := 0;
begin
   Help;

   if Capture = null then
      return;
   end if;

   Ret := Cv_Named_Window ("Flow", 1);

   loop
      First_Frame := 0;
      Frame := Cv_Query_Frame (Capture);

      if Frame = null then
         exit;
      end if;

      if Gray = null then
         Gray := new Cv_Mat;
         Prev_Gray := new Cv_Mat;
         Flow := new Cv_Mat;
         C_Flow := new Cv_Mat;
         Gray      := Core.Operations.Cv_Create_Mat (Frame.all.Height, Frame.all.Width, Cv_Maketype (Cv_8u, 1));
         Prev_Gray := Core.Operations.Cv_Create_Mat (Gray.all.Rows, Gray.all.Cols, Gray.all.Mat_Type);
         Flow      := Core.Operations.Cv_Create_Mat (Gray.all.Rows, Gray.all.Cols, Cv_Maketype (Cv_32f, 2));
         C_Flow    := Core.Operations.Cv_Create_Mat (Gray.all.Rows, Gray.all.Cols, Cv_Maketype (Cv_8u, 3));
      end if;

      Cv_Cvt_Color (To_Arr_Ptr (Frame), To_Arr_Ptr (Gray), CV_BGR2GRAY);

      if First_Frame = 0 then
         Cv_Calc_Optical_Flow_Farneback (To_Arr_Ptr (Prev_Gray), To_Arr_Ptr (Gray), To_Arr_Ptr (Flow), 0.5, 3, 15, 3, 5, 1.2, 0);
         Cv_Cvt_Color (To_Arr_Ptr (Prev_Gray), To_Arr_Ptr (C_Flow), Cv_Gray2bgr);
         Draw_Opt_Flow_Map (Flow, C_Flow, 16, 1.5, Cv_Rgb (0, 255, 0));
         Cv_Show_Image ("Flow", To_Arr_Ptr (C_Flow));
      end if;

      if Cv_Wait_Key (1) = Ascii.Esc then
         exit;
      end if;

      Temp := Prev_Gray;
      Prev_Gray := Gray;
      Gray := Temp;
   end loop;
   Cv_Release_Capture (Capture'Access);
end Farneback_Optical_Flow;
