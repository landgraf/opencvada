with Highgui; use Highgui;
with Core; use Core;
with Core.Operations; use Core.Operations;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Ada.Unchecked_Conversion;


procedure Camera_Test is

   subtype Line_Array is Cv_Point_Array (0 .. 1);
   type Line_Array_Ptr is access Line_Array;

   function To_Void_Ptr is new Ada.Unchecked_Conversion    (Target => Cv_Void_Ptr,
                                                            Source => Cv_Mem_Storage_Ptr);
   function To_Line_Array_Ptr is new Ada.Unchecked_Conversion    (Target => Line_Array_Ptr,
                                                                  Source => Cv_Void_Ptr);

   Capture                : aliased Cv_Capture_Ptr;
   Image                  : Ipl_Image_Ptr;
   Dst, Color_Dst, Gray   : aliased Ipl_Image_Ptr;
   Storage                : aliased cv_Mem_Storage_Ptr;
   Lines                  : Cv_Seq_Ptr;
   Line                   : Line_Array;

begin
   Capture := Cv_Create_Camera_Capture (0);
   Cv_Named_Window ("Hough");
   loop
      Image := Cv_Query_Frame (Capture);

      Dst := Cv_Create_Image (Cv_Get_Size (Image), 8, 1);
      Gray := Cv_Create_Image (Cv_Get_Size (Image), 8, 1);
      Color_Dst := Cv_Create_Image (Cv_Get_Size (Image), 8, 3);

      Storage := Cv_Create_Mem_Storage (0);
      Cv_Cvt_Color (Image, Gray, Cv_Bgr2gray);

      Cv_Canny (Gray, Dst, 50.0, 200.0, 3);
      Cv_Cvt_Color (Dst, Color_Dst, Cv_Gray2bgr);

      Lines := Cv_Hough_Lines2 (Dst, Storage, Cv_Hough_Probabilistic , 1.0, Cv_Pi / 180.0, 100, 5.0, 10.0);

      for I in Integer range 0 .. Lines.all.Total - 1
      loop
         Line := To_Line_Array_Ptr (Cv_Get_Seq_Elem (Lines, I)).all;
         Cv_Line (Color_Dst, Line (0), Line (1), Cv_Rgb (255, 0, 0), 3, 8);
      end loop;

      Cv_Show_Image ("Hough", Color_Dst);


      Cv_Release_Mem_Storage (Storage'Access);
      Cv_Release_Image (Dst'Access);
      Cv_Release_Image (Gray'Access);
      Cv_Release_Image (Color_Dst'Access);

      exit when Cv_Wait_Key (30) = Ascii.Esc;

   end loop;
   Cv_Destroy_Window ("Hough");
   Cv_Release_Capture (Capture'Access);

end Camera_Test;
