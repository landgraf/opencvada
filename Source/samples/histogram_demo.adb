
with Core; use Core;
with Highgui;
use Highgui;
with Imgproc;
with Imgproc.Operations;
with Core.Operations;
with Ada.Text_IO; use Ada.Text_IO;

procedure Histogram_Demo is
   Capture : aliased Cv_Capture_Ptr := Highgui.Cv_Create_Camera_Capture (0);
   Image   : aliased Ipl_Image_Ptr := null;
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
   Scale   : constant Integer := 1;

   Hist    : aliased Cv_Histogram_Ptr;
   Target_Hist : aliased Cv_Histogram_Ptr;

   Max_Value : aliased Float := 1.0;
   Value   : aliased Float := 0.0;
   Intensity : Integer := 0;

   -- hsv images
   Hist_Image, Hist_Image_2 , H, S, V, Hsv : aliased Ipl_Image_Ptr;
   Arr_Hsv : Core.Ipl_Image_Ptr_Array := (H, S, V);
   Target  : aliased Ipl_Image_Ptr := Highgui.Cv_Load_Image ("target.jpg");

   -- float

   function Get_Hist (Image : Ipl_Image_Ptr) return Cv_Histogram_Ptr is

      Hist    : aliased constant Cv_Histogram_Ptr := Imgproc.Operations.Cv_Create_Hist (3,
                                                                                    (H_Bins, S_Bins, 30),
                                                                                    0,
                                                                                    Core.To_2d_Pointer (Arr1'Access));

      Hsv     : aliased Ipl_Image_Ptr := Core.Operations.Cv_Create_Image ( (Image.all.Width,
                                                                        Image.all.Height),
                                                                        Image.all.Depth,
                                                                        Image.all.N_Channels);
      H       : aliased Ipl_Image_Ptr := Core.Operations.Cv_Create_Image ( (Image.all.Width,
                                                                        Image.all.Height),
                                                                        Image.all.Depth,
                                                                        1);
      S       : aliased Ipl_Image_Ptr := Core.Operations.Cv_Create_Image ( (Image.all.Width,
                                                                        Image.all.Height),
                                                                        Image.all.Depth,
                                                                        1);
      V       : aliased Ipl_Image_Ptr := Core.Operations.Cv_Create_Image ((Image.all.Width,
                                                                        Image.all.Height),
                                                                        Image.all.Depth,
        1);
      Arr_Hsv : Core.Ipl_Image_Ptr_Array := (H, S, V);
   begin
              Put_Line ("doing histogram{");
      Imgproc.Operations.Cv_Cvt_Color (+Image, +Hsv, Imgproc.CV_BGR2HSV);
      Core.Operations.Cv_Split (+Hsv, +H, +S, +V, null);
      Imgproc.Operations.Cv_Calc_Hist (Arr_Hsv, Hist);
              Put_Line ("}done histogram");
      Core.Operations.Cv_Release_Image (Hsv'Access);
      Core.Operations.Cv_Release_Image (H'Access);
      Core.Operations.Cv_Release_Image (S'Access);
      Core.Operations.Cv_Release_Image (V'Access);
      return Hist;
   end Get_Hist;

   --     Char : Character ;
   Min_Val : aliased Float;
begin
   Ret := Highgui.Cv_Named_Window ("hej");
   Ret := Highgui.Cv_Named_Window ("histogram");
   Ret := Highgui.Cv_Named_Window ("Target");

   Highgui.Cv_Show_Image ("Target", +Target);


   --     Char := Highgui.CvWaitKey (0);

   Target_Hist := Get_Hist (Target);
   --     Put_Line("Entering Mainloop");

   Image := Highgui.Cv_Query_Frame (Capture);
   loop

--        Ret := Highgui.Cv_Grab_Frame (Capture);


      Image := Highgui.Cv_Query_Frame (Capture);
      Hist_Image_2 := Core.Operations.Cv_Create_Image ((H_Bins * Scale, S_Bins * Scale), 8, 3);

      Hist_Image := Core.Operations.Cv_Create_Image ((Image.all.Width,
                                                     Image.all.Height),
                                                     Image.all.Depth,
                                                     1);

      Hsv := Core.Operations.Cv_Create_Image ((Image.all.Width, Image.all.Height), Image.all.Depth, Image.all.N_Channels);
      H := Core.Operations.Cv_Create_Image ((Image.all.Width, Image.all.Height), Image.all.Depth, 1);
      S := Core.Operations.Cv_Create_Image ((Image.all.Width, Image.all.Height), Image.all.Depth, 1);
      V := Core.Operations.Cv_Create_Image ((Image.all.Width, Image.all.Height), Image.all.Depth, 1);

      Imgproc.Operations.Cv_Cvt_Color (+Image, +Hsv, Imgproc.CV_BGR2HSV);
      Core.Operations.Cv_Split (+Hsv, +H, +S, +V, null);


      Hist := Get_Hist (Image);

      Imgproc.Operations.Cv_Calc_Back_Project (Arr_Hsv, +Hist_Image, Target_Hist);

      if Cv_Is_Hist (Hist) <= 0 then
         return;
      end if;

        Imgproc.Operations.Cv_Get_Min_Max_Hist_Value (Hist, Min_Val'Access, Max_Value'Access);
      Put_Line ("minval: " & Min_Val'Img & ", max:" & Max_Value'Img);

      for H_I in Integer range 0 .. H_Bins - 1 loop
         for S_I in Integer range 0 .. S_Bins - 1 loop
            Value := Float(Core.Operations.Cv_Get_Real_3d (Target_Hist.all.Bins, H_I, S_I,0));
            Put_Line (Value'Img & Max_Value'Img);
            Intensity := Core.Cv_Round ((Value * 255.0) / Float (Max_Value));
            Core.Operations.Cv_Rectangle (+Hist_Image_2,
                                         Cv_Create_Point (H_I * Scale, S_I * Scale),
                                         Cv_Create_point (((H_I + 1) * Scale)-1, ((S_I + 1) * Scale)-1),
                                         Core.Operations.Cv_RGB (Intensity, Intensity, Intensity),
                                         Core.Cv_Filled);
         end loop;
      end loop;

      Highgui.Cv_Show_Image (WindowName => "Target", Image => +Hist_Image);
      Highgui.Cv_Show_Image (WindowName => "histogram", Image => +Hist_Image_2);
      Highgui.Cv_Show_Image (WindowName => "hej", Image => +Image);

      exit when Highgui.Cv_Wait_Key (300) = Ascii.Esc;

      Core.Operations.Cv_Release_Image (Hist_Image'Access);
      Core.Operations.Cv_Release_Image (Hist_Image_2'Access);
      Core.Operations.Cv_Release_Image (Hsv'Access);
      Core.Operations.Cv_Release_Image (H'Access);
      Core.Operations.Cv_Release_Image (S'Access);
      Core.Operations.Cv_Release_Image (V'Access);
      Imgproc.Operations.Cv_Release_Hist (Hist'Access);
   end loop;

   --     Ret := Highgui.CvSaveImage("target.jpg",+Image);

   Core.Operations.Cv_Release_Image (Hist_Image'Access);
   Core.Operations.Cv_Release_Image (Target'Access);
   Imgproc.Operations.Cv_Release_Hist (Target_Hist'Access);
   Imgproc.Operations.Cv_Release_Hist (Hist'Access);
   Highgui.Cv_Release_Capture (Capture'Access);
--     Core.Operations.Cv_Release_Image (Image'Access);
   Highgui.Cv_Destroy_Window ("Target");
   Highgui.Cv_Destroy_Window ("Histogram");
   Highgui.Cv_Destroy_Window ("hej");
end Histogram_Demo;
