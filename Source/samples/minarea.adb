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

with Ada.Numerics.Discrete_Random;

use Core.Cv_8u_Pointer_Pkg;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Minarea is
   package Random_Integer is new Ada.Numerics.Discrete_Random (Integer);
   package Random_Unsigned is new Ada.Numerics.Discrete_Random (Unsigned_8);

   package Float_Trig is new
     Ada.Numerics.Generic_Elementary_Functions (Long_Float);


   type Cv_Rect_Corners is array (0 .. 3) of Cv_Point_2d_32f;
   function  GetCorners (Box : Cv_Box_2d) return Cv_Rect_Corners is
      Corners : Cv_Rect_Corners;
      Angle   :  constant Long_Float := Long_Float(Box.Angle) * CV_PI / 180.0;
      A       :  constant Float := Float(Float_Trig.Sin (Angle) * 0.5);
      B       :  constant Float := Float(Float_Trig.Cos (Angle) * 0.5);
   begin
      Corners (0).X := Box.Center.X - (A * Box.Size.Height) - (B * Box.Size.Width);
      Corners (0).Y := Box.Center.Y + (B * Box.Size.Height) - (A * Box.Size.Width);
      Corners (1).X := Box.Center.X + (A * Box.Size.Height) - (B * Box.Size.Width);
      Corners (1).Y := Box.Center.Y - (B * Box.Size.Height) - (A * Box.Size.Width);
      Corners (2).X := (2.0 * Box.Center.X) - Corners (0).X;
      Corners (2).Y := (2.0 * Box.Center.Y) - Corners (0).Y;
      Corners (3).X := (2.0 * Box.Center.X) - Corners (1).X;
      Corners (3).Y := (2.0 * Box.Center.Y) - Corners (1).Y;

      Put_Line ("Cv_Rect_Corners:");
      Put_Line (Ascii.Ht & "angle:" & Angle'Img);

      return Corners;
   end GetCorners;

   Width     : constant Integer := 500;
   Height    : constant Integer := 500;
   Channels  : constant Integer := 3;
   Max_Count : constant Integer := 100;

   Gen_Integer  : Random_Integer.Generator;
   Gen_Unsigned : Random_Unsigned.Generator;

   Data_Array : Cv_8u_Array_P;

   Points       : Cv_Mat_P;
   Points_Array : Cv_32s_Array_P;

   Img          : Cv_Mat_P;
   Count        : Integer := 0;
   Point        : Cv_Point;
   Index        : Integer := 0;

   Min_Rect     : Cv_Box_2d;

   I_Ret        : Integer;
   C_Ret        : Character;

   Data_Pointer  : Cv_8u_Pointer;
   Point_Pointer : Cv_32s_Pointer;
   Box_Corners   : Cv_Rect_Corners;

   Circle_Center : aliased Cv_Point_2d_32f;
   Circle_Radius : aliased Float;
begin
   Data_Array := new Cv_8u_Array (0 .. Width * Height * Channels - 1);
   Points := new Cv_Mat;
   Img := new Cv_Mat;

   Data_Pointer := Data_Array.all (0)'Access;
   Img.all := Cv_Create_Mat (Height, Width, Cv_Make_Type (Cv_8u, Channels), To_Void(Data_Pointer));

   if Img = null then
      Put_Line ("CvMat: Img is (null)");
   elsif Img.all.Data.Cv_8u = null then
      Put_Line ("CvMat: Img.data is (null)");
   end if;

   I_Ret := Cv_Named_Window ("Awesome", 1);

   Random_Integer.Reset (Gen_Integer);

   loop
      Count := (Random_Integer.Random (Gen_Integer) mod Max_Count) + 1;
      Points_Array := new Cv_32s_Array (0 .. (Count - 1) * 2 + 1);
      for I in Integer range 0 .. Count - 1 loop
         Point.X := Random_Integer.Random (Gen_Integer) mod (Img.all.Cols * 3 / 4 - Img.all.Cols / 4) + Img.all.Cols / 4;
         Point.Y := Random_Integer.Random (Gen_Integer) mod (Img.all.Rows * 3 / 4 - Img.all.Rows / 4) + Img.all.Rows / 4;

         Index := I * 2;
         Points_Array.all (Index) := Point.X;
         Points_Array.all (Index + 1) := Point.Y;
      end loop;
      Point_Pointer := Points_Array.all (0)'Access;
      Points.all := CvMat (Count, 1, Cv_Make_Type (Cv_32s, 2), To_Void (Point_Pointer));

      Min_Rect := Cv_Min_Area_Rect2 (Points  => To_Arr (Points),
                                  Storage => null);

      I_Ret := Cv_Min_Enclosing_Circle (Points => To_Arr(Points),
                                     Center => Circle_Center'Access,
                                     Radius => Circle_Radius'Access);

      Put_Line ("CvMinAreaRect2: done");
      Put_Line (Ascii.Ht & "center:" & Min_Rect.Center.X'Img & Min_Rect.Center.Y'Img & Ascii.Ht & Integer'Image (Cvround (Min_Rect.Center.X)) & Integer'Image (Cvround (Min_Rect.Center.Y)));
      Put_Line (Ascii.Ht & "size:" & Min_Rect.Size.Width'Img & Min_Rect.Center.Y'Img);
      Put_Line (Ascii.Ht & "angle:" & Min_Rect.Angle'Img);

      Cv_Zero (To_Arr (Img));

      for I in Integer range 0 .. Count - 1 loop
         Index := I * 2;
         Cv_Circle (To_Arr (Img), CvPoint (Points_Array.all (Index), Points_Array.all (Index + 1)), 3, CvScalar (0.0, 0.0, 255.0), CV_FILLED, CV_AA);
      end loop;
      New_Line;

      Put_Line ("CvMinEnclosingCircle:");
      Put_Line (Ascii.Ht & "Center:" & Circle_Center.X'Img & Circle_Center.Y'Img);
      Put_Line (Ascii.Ht & "Radius:" & Circle_Radius'Img);

      Box_Corners := GetCorners (Min_Rect);

      for I in Integer range 0 .. 3 loop
         Cv_Line (To_Arr (Img), Cv_Create_Point (Cv_Round (Box_Corners (I).X), Cv_Round (Box_Corners (I).Y)), Cv_Create_Point (Cvround (Box_Corners ((I + 1) mod 4).X), Cv_Round (Box_Corners ((I + 1) mod 4).Y)), Cv_Create_Scalar (0.0, 255.0), 1, Cv_Aa);
      end loop;

      Cv_Circle (To_Arr (Img), Cv_Create_Point (Cv_Round (Circle_Center.X), Cv_Round (Circle_Center.Y)), Cv_Round (Circle_Radius), Cv_Create_Scalar (255.0), 1, CV_AA);

      Cv_Show_Image (WindowName => "Awesome",
                   Image      => To_Arr (Img));

      C_Ret := Cv_Wait_Key (0);

      if C_Ret = Ascii.Esc then
         exit;
      end if;
   end loop;
end Minarea;
