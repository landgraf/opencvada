---
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Highgui; use Highgui;
with Core; use Core;
with Core.Operations; use Core.Operations;
with Ada.Numerics.Generic_Elementary_Functions; use Core.Long_Float_Numerics;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;



procedure Contours is
--     W        : constant := 500;
   Size     : constant Cv_Size := Cv_Create_Size (500, 500);
   Levels   : aliased Integer := 3;
   Contours : aliased Cv_Seq_Ptr := null;

   procedure On_Trackbar (Position : Integer);
   pragma Convention (C, On_Trackbar);

   procedure On_Trackbar (Position : Integer) is

      Cnt_Img        : aliased Ipl_Image_Ptr := Cv_Create_Image (Size, 8, 3);
      Contours_Local : Cv_Seq_Ptr := Contours;
      Levels_Local   : constant Integer := Levels - 3;
   begin
      if Levels_Local <= 0 then
         Contours_Local := Contours_Local.all.H_Next.all.H_Next.all.H_Next;
         Put_Line ("3 down ");
      end if;
      Cv_Zero (+Cnt_Img);
      Cv_Draw_Contours (+Cnt_Img, Contours_Local, Cv_Rgb (255, 0, 0), Cv_Rgb (0, 255, 0), Levels_Local, 3, Cv_Aa);
      Cv_Show_Image ("contours", +Cnt_Img);
      Cv_Release_Image (Cnt_Img'Access);
      Put_Line ("On_Trackbar end");
   end On_Trackbar;


   Storage  : aliased Cv_Mem_Storage_Ptr := Cv_Create_Mem_Storage (0);
   Img      : aliased Ipl_Image_Ptr := Cv_Create_Image (Size, 8, 1);

   Dx, Dy   : Long_Float;
   White, Black : Cv_Scalar;
   Angle    : Long_Float;

   Ret      : Integer;

   Attrs    : Cv_String_Array (0 .. 2) := (New_String ("recursive"), New_String ("1"), Null_Ptr);

   function Cv_Load_Seq (Filename  : String_C;
                       Storage   : Cv_Mem_Storage_Ptr := null;
                       Name      : Interfaces.C.Strings.Chars_Ptr := Null_Ptr;
                       Real_Name : Interfaces.C.Strings.Chars_Ptr := Null_Ptr)
                          return Cv_Seq_Ptr;
   pragma Import (C, Cv_Load_Seq, "cvLoad");
begin
   Cv_Zero (+Img);

   for I in Integer range 0 .. 6 - 1 loop
      Dx := Long_Float (I mod 2) * 250.0 - 30.0;
      Dy := Long_Float (I / 2) * 150.0;
      White := Cv_Real_Scalar (255.0);
      Black := Cv_Real_Scalar (0.0);

      if I = 0 then
         for J in Integer range 0 .. 10 loop
            Angle := Long_Float ((Long_Float (J) + 5.0) * Cv_Pi / 21.0);
            Cv_Line (+Img,
              Cv_Create_Point (Cv_Round (Dx + 100.0 + Long_Float (J) * 10.0 - 80.0 * Cos (Angle)),
                Cv_Round (Dy + 100.0 - 90.0 * Sin (Angle))),
              Cv_Create_Point (Cv_Round (Dx + 100.0 + Long_Float (J) * 10.0 - 30.0 * Cos (Angle)),
                Cv_Round (Dy + 100.0 - 30.0 * Sin (Angle))),
              White,
              1,
              8,
              0);
         end loop;
      end if;
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 150, Integer (Dy) + 100), Cv_Create_Size (100, 70), 0.0, 0.0, 360.0, White, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 115, Integer (Dy) + 70), Cv_Create_Size (30, 20), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 185, Integer (Dy) + 70), Cv_Create_Size (30, 20), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 115, Integer (Dy) + 70), Cv_Create_Size (15, 15), 0.0, 0.0, 360.0, White, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 185, Integer (Dy) + 70), Cv_Create_Size (15, 15), 0.0, 0.0, 360.0, White, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 115, Integer (Dy) + 70), Cv_Create_Size (5, 5), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 185, Integer (Dy) + 70), Cv_Create_Size (5, 5), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 150, Integer (Dy) + 100), Cv_Create_Size (10, 5), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 150, Integer (Dy) + 150), Cv_Create_Size (40, 10), 0.0, 0.0, 360.0, Black, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 27, Integer (Dy) + 100), Cv_Create_Size (20, 35), 0.0, 0.0, 360.0, White, -1, 8, 0 );
      Cv_Ellipse ( +Img, Cv_Create_Point (Integer (Dx) + 273, Integer (Dy) + 100), Cv_Create_Size (20, 35), 0.0, 0.0, 360.0, White, -1, 8, 0 );

   end loop;

   Ret := Cv_Named_Window ("image", 1);
   Cv_Show_Image ("image", +Img);

   Ret := Cv_Find_Contours (+Img, Storage, Contours'Access, Cv_Contour'Size / 8, Cv_Retr_Tree, CV_CHAIN_APPROX_SIMPLE, Cv_Create_point (0, 0));

   Cv_Save (New_String ("contours.xml"), To_Void_Ptr (Contours), Null_Ptr, Null_Ptr, Cv_Create_Attr_List (Attrs (0)'Unchecked_Access, null));
   Contours := To_Seq_Ptr(Cv_Load (+"contours.xml", Storage));



   Contours := Cv_Approx_Poly (To_Void_Ptr (Contours), Cv_Contour'Size / 8, Storage, Cv_Poly_Approx_Dp, 3.0, 1);

   Ret := Cv_Named_Window ("contours", 1);
   Cv_Show_Image ("contours", +Img);
   Ret := Cv_Create_Trackbar ("levels+3", "contours", Levels'Access, 7, On_Trackbar'Unrestricted_Access); -- dont do this at home, at least dont blame me for it.

   On_Trackbar (0);

   loop
      exit when Cv_Wait_Key (0) = Ascii.Esc;
   end loop;

   Cv_Release_Mem_Storage (Storage'Access);
   Cv_Release_Image (Img'Access);
   Cv_Destroy_Window ("contours");
   Cv_Destroy_Window ("image");
end Contours;
