--

with Core; use Core;
with Core.Operations; use Core.Operations;
with Features_2d; use Features_2d;
with Ada.Unchecked_Conversion;
use Core.Cv_Arr_Pointer_Pkg;
with Interfaces.C;
with Ada.Containers.Vectors;
procedure Find_Obj is
   procedure Help is begin null; end Help;


   package Vectors is new Ada.Containers.Vectors (Positive, Integer);

   function From_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_Pointer,
                                   Target => Cv_SURF_Point_P);

   function From_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_Pointer,
                                   Target => Cv_32f_Pointer);

--     Image : Ipl_Image_P;

   function Compare_Surf_Descriptors (D1     : Cv_32f_Array;
                                      D2     : Cv_32f_Array;
                                      Best   : Long_Float;
                                      Length : Integer) return Long_Float is
      Total_Cost : Long_Float := 0.0;

   begin
      pragma Assert ((Length mod 4) = 0, "length mod 4 not 0");
      loop
         Loop_4_Step :
         declare
            I              : Integer := D1'First;
            T0, T1, T2, T3 : Long_Float := 0.0;
         begin
            exit when I < Length;
            T0 := Long_Float (D1 (I) - D2 (I));
            T1 := Long_Float (D1 (I + 1) - D2 (I + 1));
            T2 := Long_Float (D1 (I + 2) - D2 (I + 2));
            T3 := Long_Float (D1 (I + 3) - D2 (I + 3));
            Total_Cost := Total_Cost + T0 ** 2 + T1 ** 2 + T2 ** 2 + T3 ** 2;
            exit when Total_Cost > Best;
            I := I + 4;
         end Loop_4_Step;
      end loop;
      return Total_Cost;
   end Compare_Surf_Descriptors;

   function Naive_Nearest_Neighbor (Vec               : Cv_32f_Array;
                                    Laplacian         : Integer;
                                    Model_Keypoints   : Cv_Seq_P;
                                    Model_Descriptors : Cv_Seq_P) return Integer is
      Length          : constant Integer := Model_Descriptors.all.Elem_Size / (Float'Size / 8);
      Neighbor        : Integer := -1;
      D               : Long_Float;
      Dist1, Dist2    : Long_Float := 1.0e6;
      Reader, Kreader : aliased Cv_Seq_Reader;


   begin
      CvStartReadSeq (Model_Keypoints, Kreader'Unchecked_Access, 0);
      CvStartReadSeq (Model_Descriptors, Reader'Unchecked_Access, 0);
      for I in Integer range 0 .. Model_Descriptors.all.Total - 1
      loop
         Loop_Nnn :
         declare

            Kp   : constant Cv_SURF_Point_P := From_Arr (Kreader.Ptr);
            Mvec : constant Cv_32f_Array := Core.Cv_32f_Pointer_Pkg.Value (From_Arr (Reader.Ptr), Interfaces.C.Ptrdiff_T (Model_Descriptors.all.Total));
         begin
            CvNextSeqElem (Kreader.Seq.all.Elem_Size, Kreader'Unchecked_Access);
            CvNextSeqElem (Reader.Seq.all.Elem_Size, Reader'Unchecked_Access);
            if Laplacian = Kp.all.Laplacian then
               D := Compare_Surf_Descriptors (Vec, Mvec, Dist2, Length);
               if  D < Dist1 then
                  Dist2 := Dist1;
                  Dist1 := D;
                  Neighbor := I;
               elsif D < Dist2 then
                  Dist2 := D;
               end if;
            end if;
         end Loop_NNN;
      end loop;
      if Dist1 < 0.6 * Dist2 then
         return Neighbor;
      else
         return -1;
      end if;
   end Naive_Nearest_Neighbor;

   procedure Find_Pairs (Object_Keypoints : Cv_Seq_P;
                         Object_Descriptors : Cv_Seq_P;
                         Image_Keypoints    : Cv_Seq_P;
                         Image_Descriptors  : Cv_Seq_P;
                         Ptpairs            : in out Vectors.Vector) is
      Reader, Kreader : aliased Cv_Seq_Reader;
   begin
      CvStartReadSeq (Object_Keypoints, Kreader'Unchecked_Access, 0);
      CvStartReadSeq (Object_Descriptors, Reader'Unchecked_Access, 0);
      Vectors.Clear (Ptpairs);
      for I in Integer range 0 .. Object_Descriptors.all.Total-1
      loop
         Loop_Fp :
         declare
            Kp         : constant Cv_SURF_Point_P := From_Arr (Kreader.Ptr);
            Descriptor : constant Cv_32f_Array := Core.Cv_32f_Pointer_Pkg.Value (From_Arr (Reader.Ptr), Interfaces.C.Ptrdiff_T (Object_Descriptors.all.Total));
            Nearest_Neighbor : Integer;
         begin
            CvNextSeqElem (Kreader.Seq.all.Elem_Size, Kreader'Unchecked_Access);
            CvNextSeqElem (Reader.Seq.all.Elem_Size, Reader'Unchecked_Access);
            Nearest_Neighbor := Naive_Nearest_Neighbor (Descriptor, Kp.all.Laplacian, Image_Keypoints, Image_Descriptors);
            if Nearest_Neighbor >= 0 then
               Vectors.Append (Ptpairs, I);
               Vectors.Append (Ptpairs, Nearest_Neighbor);
            end if;
         end Loop_Fp;
      end loop;
   end Find_Pairs;

   function Locate_Planar_Object (Object_Keypoints : Cv_Seq_P;
                                  Object_Descriptors : Cv_Seq_P;
                                  Image_Keypoints    : Cv_Seq_P;
                                  Image_Descriptors  : Cv_Seq_P;
                                  Src_Corners        : Cv_Point_Array ;
                                  Dst_Corners        : Cv_Point_Array ) return Integer is
      H : Cv_64f_Array (0 .. 8);
--        Mat_H : Cv_Mat := CvMat (3, 3, Cv_64f, H);
   begin
      return 1;
   end Locate_Planar_Object;

begin
   null;
end Find_Obj;
