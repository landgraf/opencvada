with Core; use Core;
with Core.Operations; use Core.Operations;
with Core.Mat;
with Imgproc.Operations; use Imgproc.Operations;
with Highgui; use Highgui;
with Interfaces; use Interfaces;

with Ada.Numerics.Discrete_Random;

use Core.Cv_8u_Pointer_Pkg;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;


procedure Minarea is
   package Random_Integer is new Ada.Numerics.Discrete_Random (Integer);
   package Random_Unsigned is new Ada.Numerics.Discrete_Random (Unsigned_8);

   package Cv_Mat_32s is new Core.Mat (Integer);
   use Cv_Mat_32s;
   package Cv_Mat_8u is new Core.Mat (Unsigned_8);

   package Float_Trig is new
     Ada.Numerics.Generic_Elementary_Functions (Long_Float);

   type Cv_Rect_Corners is array (0 .. 3) of Cv_Point_2d_32f;
   function GetCorners (Box : Cv_Box_2d) return Cv_Rect_Corners is
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

   Min_Rect     : Cv_Box_2d;

   I_Ret        : Integer;
   C_Ret        : Character;

   Box_Corners   : Cv_Rect_Corners;

   Circle_Center : aliased Cv_Point_2d_32f;
   Circle_Radius : aliased Float;

   Mat_32s_Channels : constant Integer := 2;

   Mat2_8u  : aliased Cv_Mat_8u.Cv_Mat_Ptr;
   Mat2_32s : aliased Cv_Mat_32s.Cv_Mat_Ptr;

   -- Subprogram access test code... Remove when done with function pointers
   type Func_Acc is access function (A : Integer; B : Integer) return Integer;
   pragma Convention (C, Func_Acc);
   function Func_Test (A : Integer; B : Integer) return Integer;
   pragma Convention (C, Func_Test);

   function Func_Test (A : Integer; B : Integer) return Integer is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Func_Test;

   type Bounded_Array is array (Integer range 0 .. 4) of Integer;
   type Unbounded_Array is array (Integer range <>) of Integer;
   function Bounded_Array_Test (Arr : Unbounded_Array) return Integer;
   pragma Import (C, Bounded_Array_Test, "bounded_array_test");
   function Unbounded_Array_Test (Arr : Unbounded_Array; Length : Integer) return Integer;
   pragma Import (C, Unbounded_Array_Test, "unbounded_array_test");

--     function Acc_Test (Func : access function (A : Integer; B : Integer) return Integer) return Integer;
   function Acc_Test (Func : Func_Acc) return Integer;
   pragma Import (C, Acc_Test, "func_ptr_test");

   Result : Integer;
   UA     : Unbounded_Array (10 .. 14) := (11, 12, 13, 12, 11);
begin
   Mat2_8u := Cv_Mat_8u.Cv_Create_Mat2 (Height, Width, Cv_8u, Channels);

   Random_Integer.Reset (Gen_Integer);

   Put_Line ("sizeof long_integer:" & Long_Integer'Size'Img);
   Put_Line ("sizeof long:" & Interfaces.C.Long'Size'Img);

   Result := Acc_Test (Func_Test'Access);
   Put_Line ("result of acc_test:" & Result'Img);
   Result := Bounded_Array_Test ((10, 20));
   Put_Line ("result of bounded_test:" & Result'Img);
   Result := Unbounded_Array_Test (UA, 5);
   Put_Line ("result of unbounded_test:" & Result'Img);
   for I in UA'Range loop
      Put ("" & UA (I)'Img);
   end loop;
   New_Line;

   loop

      Count := (Random_Integer.Random (Gen_Integer) mod Max_Count) + 10;

      Mat2_32s := Cv_Mat_32s.Cv_Create_Mat2 (Count, 1, Cv_32s, Mat_32s_Channels);
      for I in Integer range 0 .. Count - 1 loop
         Cv_Mat_32s.Cv_Set_Elem (Mat2_32s, 0, I, 0, Random_Integer.Random (Gen_Integer) mod (Mat2_8u.all.Cols * 3 / 4 - Mat2_8u.all.Cols / 4) + Mat2_8u.all.Cols / 4);
         Cv_Mat_32s.Cv_Set_Elem (Mat2_32s, 0, I, 1, Random_Integer.Random (Gen_Integer) mod (Mat2_8u.all.Rows * 3 / 4 - Mat2_8u.all.Rows / 4) + Mat2_8u.all.Rows / 4);
      end loop;

      Min_Rect := Cv_Min_Area_Rect2 (Points => Cv_Mat_32s.To_Arr_Ptr (Mat2_32s),
                                     Storage => null);

      I_Ret := Cv_Min_Enclosing_Circle (Points => Cv_Mat_32s.To_Arr_Ptr (Mat2_32s),
                                        Center => Circle_Center'Access,
                                        Radius => Circle_Radius'Access);

      Cv_Zero (Cv_Mat_8u.To_Arr_Ptr (Mat2_8u));

      for I in Integer range 0 .. Count - 1 loop
         Cv_Circle (Cv_Mat_8u.To_Arr_Ptr (Mat2_8u), Cv_Create_Point (Cv_Get_Elem (Mat2_32s, 0, I, 0), Cv_Get_Elem (Mat2_32s, 0, I, 1)), 3, Cv_Create_Scalar (0.0, 0.0, 255.0), Cv_Filled, Cv_Aa);
      end loop;

      Box_Corners := GetCorners (Min_Rect);

      for I in Integer range 0 .. 3 loop
         Cv_Line (Cv_Mat_8u.To_Arr_Ptr (Mat2_8u), Cv_Create_Point (Cv_Round (Box_Corners (I).X), Cv_Round (Box_Corners (I).Y)), Cv_Create_Point (Cv_Round (Box_Corners ((I + 1) mod 4).X), Cv_Round (Box_Corners ((I + 1) mod 4).Y)), Cv_Create_Scalar (0.0, 255.0), 1, Cv_Aa);
      end loop;

      Cv_Circle (Cv_Mat_8u.To_Arr_Ptr (Mat2_8u), Cv_Create_Point (Cv_Round (Circle_Center.X), Cv_Round (Circle_Center.Y)), Cv_Round (Circle_Radius), Cv_Create_Scalar (255.0), 1, Cv_AA);

      Cv_Show_Image (WindowName => "Blue Platypus",
                     Image      => Cv_Mat_8u.To_Mat_Ptr (Mat2_8u));

      Cv_Mat_32s.Cv_Release (Mat2_32s'Access);

      C_Ret := Cv_Wait_Key (1);

      if C_Ret = Ascii.Esc then
         Cv_Destroy_Window ("Blue Platypus");
         Cv_Mat_8u.Cv_Release (Mat2_8u'Access);
         exit;
      end if;
   end loop;
end Minarea;
