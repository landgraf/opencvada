with Highgui; use Highgui;
with Core; use Core;
with Core.Operations; use Core.Operations;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
procedure Demo is


   Aperture_Size : Integer := 1;
   Block_Size : Integer := 1;

   Slider_A_Value : aliased Integer := 1;
   Slider_B_Value : aliased Integer := 1;
   Capture : aliased Cv_Capture_Ptr;
   Image : Ipl_Image_Ptr;
   Corners, Image_Gray : aliased Ipl_Image_Ptr;

   Scale, Shift : Long_Float;
   Min, Max : aliased Long_Float;

   procedure Slider_A (Position : Integer);
   pragma Convention (C, Slider_A);

   procedure Slider_A (Position : Integer) is
   begin
      if Position = 0 or Position mod 2 = 0 then
         Aperture_Size := Position + 1;
      else
         Aperture_Size := Position;
      end if;
   end Slider_A;

   procedure Slider_B (Position : Integer);
   pragma Convention (C, Slider_B);

   procedure Slider_B (Position : Integer) is
   begin
      if Position > 0 then
         Block_Size := Position;
      else
         Block_Size := 1;
      end if;
   end Slider_B;



begin
   Cv_Named_Window ("Original", Highgui.Cv_Window_Autosize);
   Cv_Named_Window ("Corners", Highgui.Cv_Window_Normal);

   Cv_Move_Window ("Corners", 700, 50);
   Cv_Move_Window ("Original", 0, 50);

   Cv_Create_Trackbar ("Aperture", "Corners", Slider_A_Value'Access, 31, Slider_A'Unrestricted_Access);
   Cv_Create_Trackbar ("Block", "Corners", Slider_B_Value'Access, 11, Slider_B'Unrestricted_Access);

   Capture := Cv_Create_Camera_Capture (0);

   loop
      exit when Cv_Wait_Key (100) = Ascii.Esc;

      Image := Cv_Query_Frame (Capture);
      Cv_Show_Image ("Original", Image);

      Corners := Cv_Create_Image (Cv_Get_Size (Image), Ipl_Depth_32f, 1);
      Image_Gray := Cv_Create_Image (Cv_Get_Size (Image), Ipl_Depth_8u, 1);
      Cv_Cvt_Color (Image, Image_Gray, Cv_Rgb2gray);
      Cv_Corner_Harris (Image_Gray, Corners, Block_Size, Aperture_Size);

      Cv_Min_Max_Loc (Corners, Min'Access, Max'Access, null, null, null);
      Scale := (255.0 - 0.0) / (Max - Min);
      Shift := -Min * Scale + 0.0;
      Cv_Convert_Scale (Corners, Image_Gray, Scale, Shift);
      Cv_Show_Image ("Corners", Image_Gray);
      Cv_Release_Image (Image_Gray'Access);
      Cv_Release_Image (Corners'Access);
   end loop;


   Cv_Destroy_Window ("Original");
   Cv_Destroy_Window ("Corners");
   Cv_Release_Capture (Capture'Access);
end Demo;
