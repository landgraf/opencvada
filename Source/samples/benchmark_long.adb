--
with Core; use Core;
with Core.Operations; use Core.Operations;
with Highgui; use Highgui;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Ada.Unchecked_Conversion;
with Core.Mat;


-- benchmark_long file.avi
procedure Benchmark_Long is
   subtype Lf_Circle is Core.Cv_32f_Array (0 .. 2);
   package Matrix is new Core.Mat (Float);

   Capture : aliased cv_Capture_Ptr;
   Frame   : Ipl_Image_Ptr;
   Frames  : Interfaces.Integer_64 := 0;
   Bw,Recolor      : aliased Ipl_Image_Ptr;
   Storage         : aliased Matrix.Cv_Mat_Ptr;
   Circles : Cv_Seq_Ptr;
   C_Array : Matrix.Element_Array(0 .. (10000*3)-1) := (others => 0.0);

   Circle  : Lf_Circle;

   type Lf_Circle_Ptr is access Lf_Circle;

   function To_Void_Ptr is new Ada.Unchecked_Conversion    (Target => Cv_Void_Ptr,
                                                            Source => Cv_Mem_Storage_Ptr);
   function To_Lf_Circle_Ptr is new Ada.Unchecked_Conversion    (Target => Lf_Circle_Ptr,
                                                                 Source => Cv_Void_Ptr);

   Loop_N : Integer := 0;
begin
   if Argument_Count > 0 then
      Capture := Cv_Create_File_Capture (Argument(1));
   else
      return;
   end if;
--     Frame := Cv_Load_Image ("cirklar.png");
   loop
      Frame := Cv_Query_Frame (Capture);
      --waste a bit of time
      exit when Frame = null;
      Storage := Matrix.Cv_Create_Mat (1, 10000, Cv_32f, 3, C_Array);
      Bw := Cv_Create_Image (Cv_Get_Size (Frame), 8, 1);
      Recolor := Cv_Create_Image (Cv_Get_Size (Frame), 8, 3);
      Cv_Cvt_Color (Frame, Bw, Cv_Bgr2gray);
      Cv_Canny (Bw, Bw, 10.0, 240.0);
      Circles := Imgproc.Operations.Cv_Hough_Circles (Bw, Matrix.To_Void_Ptr (Storage), Cv_Hough_Gradient, 1.0, Long_Float (Bw.all.Height / 10), 300.0, 30.0);
      Cv_Cvt_Color (Bw, Recolor, Cv_Gray2bgr);
--        C_Array := new Matrix.element_Array(0 .. storage.all.Rows - 1);
--        C_Array.all := Matrix.Cv_Get_Mat_Data (Storage);
--        for I in Integer range 0 .. Storage.all.Rows - 1 loop
      while Loop_N < C_Array'Length loop
         Circle := (C_Array (Loop_N), C_Array (Loop_N + 1), C_Array (Loop_N + 2));
         Cv_Draw_Circle (Recolor, Cv_Create_Point (Cv_Round (Circle (0)), Cv_Round (Circle (1))), Cv_Round (Circle (2)), Cv_Rgb (255, 0, 0));
         Loop_N := Loop_N + 3;
      end loop;
--        Put_Line (Circles.all.Total'Img);
--        Cv_Show_Image ("hej", Recolor);
--        Cv_Wait_Key (1);
      Frames := Frames + 1;
      Cv_Release_Image (Bw'Access);
      Cv_Release_Image (Recolor'Access);
      Matrix.Cv_Release_Mat (Storage);
   end loop;
   Put_Line("Frames: " & Frames'Img);
   Cv_Release_Capture (Capture'Access);
end Benchmark_Long;
