with Core; use Core;
with Core.Operations; use Core.Operations;
with Core.Mat;
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

with Core.Mat_Nd;
with Core.Sparse_Mat;

procedure Minarea is
   package Random_Integer is new Ada.Numerics.Discrete_Random (Integer);
   package Random_Unsigned is new Ada.Numerics.Discrete_Random (Unsigned_8);

   package Cv_Mat_32s is new Core.Mat (Integer);
   use Cv_Mat_32s;
   package Cv_Mat_8u is new Core.Mat (Unsigned_8);

   package Mat_Nd_8u is new Core.Mat_Nd (Unsigned_8);
   package Sparse_Mat_8u is new Core.Sparse_Mat (Unsigned_8);

   use Cv_Mat_32s;

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

      return Corners;
   end GetCorners;

   Width     : constant Integer := 500;
   Height    : constant Integer := 500;
   Channels  : constant Integer := 3;
   Max_Count : constant Integer := 100;

   Gen_Integer  : Random_Integer.Generator;
   Gen_Unsigned : Random_Unsigned.Generator;

   Count        : Integer := 0;
   Point        : Cv_Point;
   Index        : Integer := 0;

   Min_Rect     : Cv_Box_2d;

   I_Ret        : Integer;
   C_Ret        : Character;

   Box_Corners   : Cv_Rect_Corners;

   Circle_Center : aliased Cv_Point_2d_32f;
   Circle_Radius : aliased Float;

   -- Test of new Cv_Mat structure
   Mat_32s : aliased Cv_Mat_32s.Cv_Mat_Ptr;
   Arr_32s : aliased Cv_Mat_32s.Element_Array_Ptr;
   Mat_8u  : Cv_Mat_8u.Cv_Mat_Ptr;
   Arr_8u  : Cv_Mat_8u.Element_Array_Ptr;


   function To_Int is new Ada.Unchecked_Conversion (Source => Cv_Mat_32s.Cv_Mat_Ptr,
                                                    Target => Integer);
begin
   Arr_8u := new Cv_Mat_8u.Element_Array (0 .. Width * Height * Channels - 1);
   Mat_8u := Cv_Mat_8u.Cv_Create_Mat (Height, Width, Cv_8u, Channels, Arr_8u);

   Random_Integer.Reset (Gen_Integer);

   loop
      Count := (Random_Integer.Random (Gen_Integer) mod Max_Count) + 1;
      Arr_32s := new Cv_Mat_32s.Element_Array (0 .. (Count - 1) * 2 + 1);
      for I in Integer range 0 .. Count - 1 loop
         Point.X := Random_Integer.Random (Gen_Integer) mod (Mat_8u.all.Cols * 3 / 4 - Mat_8u.all.Cols / 4) + Mat_8u.all.Cols / 4;
         Point.Y := Random_Integer.Random (Gen_Integer) mod (Mat_8u.all.Rows * 3 / 4 - Mat_8u.all.Rows / 4) + Mat_8u.all.Rows / 4;

         Index := I * 2;
         Arr_32s.all (Index) := Point.X;
         Arr_32s.all (Index + 1) := Point.Y;
      end loop;

--        Mat_32s := Cv_Mat_32s.Cv_Create_Mat (Count, 1, Cv_32s, 2, Arr_32s);

      Mat_32s := Cv_Create_Mat (Count, 1, Cv_32s, 2);
      Cv_Set_Data (To_Arr_Ptr (Mat_32s), To_Void_Ptr (Arr_32s), CV_AUTOSTEP);

      Min_Rect := Cv_Min_Area_Rect2 (Points  => Cv_Mat_32s.To_Arr_Ptr (Mat_32s),
                                     Storage => null);

      I_Ret := Cv_Min_Enclosing_Circle (Points => Cv_Mat_32s.To_Arr_Ptr (Mat_32s),
                                        Center => Circle_Center'Access,
                                        Radius => Circle_Radius'Access);

      Cv_Zero (Cv_Mat_8u.To_Arr_Ptr (Mat_8u));

      for I in Integer range 0 .. Count - 1 loop
         Index := I * 2;
         Cv_Circle (Cv_Mat_8u.To_Arr_Ptr (Mat_8u), Cv_Create_Point (Arr_32s.all (Index), Arr_32s.all (Index + 1)), 3, Cv_Create_Scalar (0.0, 0.0, 255.0), Cv_Filled, Cv_Aa);
      end loop;

      Box_Corners := GetCorners (Min_Rect);

      for I in Integer range 0 .. 3 loop
         Cv_Line (Cv_Mat_8u.To_Arr_Ptr (Mat_8u), Cv_Create_Point (Cv_Round (Box_Corners (I).X), Cv_Round (Box_Corners (I).Y)), Cv_Create_Point (Cv_Round (Box_Corners ((I + 1) mod 4).X), Cv_Round (Box_Corners ((I + 1) mod 4).Y)), Cv_Create_Scalar (0.0, 255.0), 1, Cv_Aa);
      end loop;

      Cv_Circle (Cv_Mat_8u.To_Arr_Ptr (Mat_8u), Cv_Create_Point (Cv_Round (Circle_Center.X), Cv_Round (Circle_Center.Y)), Cv_Round (Circle_Radius), Cv_Create_Scalar (255.0), 1, CV_AA);

      Cv_Show_Image (WindowName => "Red Penguin",
                     Image      => Cv_Mat_8u.To_Arr_Ptr (Mat_8u));

      Cv_Mat_32s.Cv_Release_Mat (Mat_32s, Arr_32s'Access);

      C_Ret := Cv_Wait_Key (1);

      if C_Ret = Ascii.Esc then
         Cv_Destroy_Window ("Red Penguin");
         exit;
      end if;
   end loop;
end Minarea;
