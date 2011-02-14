--
with Core; use Core; with Core.Operations; use Core.Operations;
with Ada.Containers.Vectors;
with Highgui; use Highgui;
with Imgproc; use Imgproc; with Imgproc.Operations; use Imgproc.Operations;
with Features_2d; use Features_2d;
with Ada.Text_Io; use Ada.Text_Io;
with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Conversion;
with Calib_3d; use Calib_3d;
procedure Find_Object is

   Colors : constant Cv_Scalar_Array (0 .. 8) := (Cv_Create_Scalar (0.0, 0.0, 255.0, 0.0),
                                                  Cv_Create_Scalar (0.0, 128.0, 255.0, 0.0),
                                                  Cv_Create_Scalar (0.0, 255.0, 255.0, 0.0),
                                                  Cv_Create_Scalar (0.0, 255.0, 0.0, 0.0),
                                                  Cv_Create_Scalar (255.0, 128.0, 0.0, 0.0),
                                                  Cv_Create_Scalar (255.0, 255.0, 0.0, 0.0),
                                                  Cv_Create_Scalar (255.0, 0.0, 0.0, 0.0),
                                                  Cv_Create_Scalar (255.0, 0.0, 255.0, 0.0),
                                                  Cv_Create_Scalar (255.0, 255.0, 255.0, 0.0));

   package Int_Vector is new Ada.Containers.Vectors (Positive, Integer);

   --    double find_obj_compareSURFDescriptors( const float* d1, const float* d2, double best, int length );
   function Compare_Surf_Descriptors (D1     : Cv_32f_Pointer;
                                      D2     : Cv_32f_Pointer;
                                      Best   : Long_Float;
                                      Length : Integer) return Long_Float is
      Total_Cost     : Long_Float := 0.0;
      T0, T1, T2, T3 : Long_Float;
      Temp_D1        : Cv_32f_Pointer := D1;
      Temp_D2        : Cv_32f_Pointer := D2;
      I              : Integer := 0;
   begin
      loop
         exit when I >= Length;
         T0 := Long_Float (Temp_D1.all - Temp_D2.all);
         Core.Cv_32f_Pointer_Pkg.Increment (Temp_D1);
         Core.Cv_32f_Pointer_Pkg.Increment (Temp_D2);
         T1 := Long_Float (Temp_D1.all - Temp_D2.all);
         Core.Cv_32f_Pointer_Pkg.Increment (Temp_D1);
         Core.Cv_32f_Pointer_Pkg.Increment (Temp_D2);
         T2 := Long_Float (Temp_D1.all - Temp_D2.all);
         Core.Cv_32f_Pointer_Pkg.Increment (Temp_D1);
         Core.Cv_32f_Pointer_Pkg.Increment (Temp_D2);
         T3 := Long_Float (Temp_D1.all - Temp_D2.all);
         Core.Cv_32f_Pointer_Pkg.Increment (Temp_D1);
         Core.Cv_32f_Pointer_Pkg.Increment (Temp_D2);
         Total_Cost := Total_Cost + T0 ** 2 + T1 ** 2 + T2 ** 2 + T3 ** 2;
         I := I + 4;
         exit when Total_Cost > Best;
      end loop;
      return Total_Cost;
   end Compare_Surf_Descriptors;
   --     pragma Import (C, Compare_Surf_Descriptors, "find_obj_compareSURFDescriptors");


   --    int find_obj_naiveNearestNeighbor( const float* vec, int laplacian,
   --  				    const CvSeq* model_keypoints,
   --  				    const CvSeq* model_descriptors );
   function Naive_Nearest_Neighbor (Vec               : Cv_32f_Pointer;
                                    Laplacian         : Integer;
                                    Model_Keypoints   : Cv_Seq_Ptr;
                                    Model_Descriptors : Cv_Seq_Ptr) return Integer is
      Length          : Integer := Model_Descriptors.all.Elem_Size / (Float'Size / 8);
      Neighbor        : Integer := -1;
      Dist1, Dist2    : Long_Float := 1.0e6;
      D               : Long_Float;
      Reader, Kreader : aliased Cv_Seq_Reader;
      Kp              : Cv_Surf_Point_Ptr;
      Mvec            : Cv_32f_Pointer;

      function To_Surf_Point_Ptr is
        new Ada.Unchecked_Conversion (Source => Cv_Arr_Pointer,
                                      Target => Cv_Surf_Point_Ptr);
      function To_32f_Pointer is
        new Ada.Unchecked_Conversion (Source => Cv_Arr_Pointer,
                                      Target => Cv_32f_Pointer);
   begin
      Cv_Start_Read_Seq (Model_Keypoints, Kreader'Unchecked_Access, 0);
      Cv_Start_Read_Seq (Model_Descriptors, Reader'Unchecked_Access, 0);

      for I in Integer range 0 .. Model_Descriptors.all.Total - 1
      loop
         Kp := To_Surf_Point_Ptr (Kreader.Ptr);
         Mvec := To_32f_Pointer (Reader.Ptr);
         Cv_Next_Seq_Elem (Kreader.Seq.all.Elem_Size, Kreader'Unchecked_Access);
         Cv_Next_Seq_Elem (Reader.Seq.all.Elem_Size, Reader'Unchecked_Access);
         if Laplacian = Kp.all.Laplacian then
            D := Compare_Surf_Descriptors (Vec, Mvec, Dist2, Length);
            if D < Dist1 then
               Dist2 := Dist1;
               Dist1 := D;
               Neighbor := I;
            elsif D < Dist2 then
               Dist2 := D;
            end if;
         end if;
      end loop;
      if Dist1 < 0.6 * Dist2 then
         return Neighbor;
      else
         return -1;
      end if;
   end Naive_Nearest_Neighbor;
   --     pragma Import (C, Naive_Nearest_Neighbor, "find_obj_naiveNearestNeighbor");

   --    void find_obj_findPairs( const CvSeq* objectKeypoints, const CvSeq* objectDescriptors,
   --  			  const CvSeq* imageKeypoints, const CvSeq* imageDescriptors, vector<int>& ptpairs );
   procedure Find_Pairs (Object_Keypoints   : Cv_Seq_Ptr;
                         Object_Descriptors : Cv_Seq_Ptr;
                         Image_Keypoints    : Cv_Seq_Ptr;
                         Image_Descriptors  : Cv_Seq_Ptr;
                         Pt_Pairs           : access Core.Cv_32s_Pointer;
                         Length             : access Integer) is

      package Int_Vector is new
        Ada.Containers.Vectors (Natural, Integer);

      Reader, Kreader : aliased Cv_Seq_Reader;
      Kp : Cv_Surf_Point_Ptr;
      Descriptor : Cv_32f_Pointer;

      function To_Surf_Point_Ptr is
        new Ada.Unchecked_Conversion (Source => Cv_Arr_Pointer,
                                      Target => Cv_Surf_Point_Ptr);
      function To_32f_Pointer is
        new Ada.Unchecked_Conversion (Source => Cv_Arr_Pointer,
                                      Target => Cv_32f_Pointer);
      Nearest_Neighbor : Integer;
      Temp_Pairs : Int_Vector.Vector;
      Temp_Length : Integer := 0;
      Temp_Array : Cv_32s_Array_Ptr;
   begin
      Cv_Start_Read_Seq (Object_Keypoints, Kreader'Unchecked_Access);
      Cv_Start_Read_Seq (Object_Descriptors, Reader'Unchecked_Access);
      for I in Integer range 0 .. Object_Descriptors.all.Total - 1
      loop
         Kp := To_Surf_Point_Ptr (Kreader.Ptr);
         Descriptor := To_32f_Pointer (Reader.Ptr);
         Cv_Next_Seq_Elem (Kreader.Seq.all.Elem_Size, Kreader'Unchecked_Access);
         Cv_Next_Seq_Elem (Reader.Seq.all.Elem_Size, Reader'Unchecked_Access);
         Nearest_Neighbor := Naive_Nearest_Neighbor (Descriptor, Kp.all.Laplacian, Image_Keypoints, Image_Descriptors);
         if Nearest_Neighbor >= 0 then
            Int_Vector.Append (Temp_Pairs, I);
            Int_Vector.Append (Temp_Pairs, Nearest_Neighbor);
            Temp_Length := Temp_Length + 2;
         end if;
      end loop;

      Temp_Array := new Cv_32s_Array (0 .. Temp_Length - 1);
      for N in Temp_Array'Range
      loop
         Temp_Array.all (N) := Int_Vector.Element (Temp_Pairs, N);
      end loop;
      Pt_Pairs.all := Temp_Array.all (0)'Access;
      Length.all := Temp_Length;
   end Find_Pairs;
   --     pragma Import (C, Find_Pairs, "find_obj_findPairs");

   --       int find_obj_locatePlanarObject( const CvSeq* objectKeypoints, const CvSeq* objectDescriptors,
   --  				  const CvSeq* imageKeypoints, const CvSeq* imageDescriptors,
   --  				  const CvPoint src_corners[4], CvPoint dst_corners[4] ):
   function Locate_Planar_Object (Object_Keypoints   : Cv_Seq_Ptr;
                                  Object_Descriptors : Cv_Seq_Ptr;
                                  Image_Keypoints    : Cv_Seq_Ptr;
                                  Image_Descriptors  : Cv_Seq_Ptr;
                                  Src_Corners        : Cv_Point_Array;
                                  Dst_Corners        : Cv_Point_Array_Ptr) return Integer is
      H                     : aliased Cv_64f_Array (0 .. 8) := (others => 0.0);
      H_Ptr                 : Cv_64f_Pointer := H (0)'Unchecked_Access;
      H_Mat                 : aliased Cv_Mat := Cv_Create_Mat (3, 3, Cv_Make_Type (Cv_64f, 1), To_Void_Ptr (H_Ptr));
      Pt_Pairs_P            : aliased Cv_32s_Pointer := null;
      Pt_Pairs              : Cv_32s_Array_Ptr;
      Pt1, Pt2              : Cv_Point_2d_32f_Array_Ptr;
      Pt1_Ptr, Pt2_Ptr      : Cv_Point_2d_32f_Pointer;
      Pt1_Mat, Pt2_Mat      : aliased Cv_Mat;
      Pt_Length             : aliased Integer := 0;
      N                     : Integer := 0;
      X, Y, Z               : Long_Float;


      function To_Surf_Point_Ptr is
        new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                      Target => Cv_Surf_Point_Ptr);
      function To_Void_Ptr is
        new Ada.Unchecked_Conversion (Source => Cv_Point_2d_32f_Pointer,
                                      Target => Cv_Void_Ptr);
   begin
      Find_Pairs (Object_Keypoints,
                  Object_Descriptors,
                  Image_Keypoints,
                  Image_Descriptors,
                  Pt_Pairs_P'Access,
                  Pt_Length'Access);
      Pt_Pairs := new Cv_32s_Array (0 .. Pt_Length - 1 );
      if Pt_Pairs = null then
         Put_Line ("pt_pairs = null");
      end if;
      Pt_Pairs.all := Core.Cv_32s_Pointer_Pkg.Value (Pt_Pairs_P, Ptrdiff_T (Pt_Length));
      N := Pt_Length / 2;
      if N < 4 then
         return 0;
      end if;

      Pt1 := new Cv_Point_2d_32f_Array (0 .. N - 1);
      Pt2 := new Cv_Point_2d_32f_Array (0 .. N - 1);

      for I in Integer range 0 .. N - 1
      loop
         Pt1.all (I) := To_Surf_Point_Ptr (Cv_Get_Seq_Elem (Object_Keypoints, Pt_Pairs.all (I * 2))).all.Pt;
         Pt2.all (I) := To_Surf_Point_Ptr (Cv_Get_Seq_Elem (Image_Keypoints, Pt_Pairs.all (I * 2 + 1))).all.Pt;
      end loop;

      Pt1_Ptr := Pt1.all (0)'Access;
      Pt2_Ptr := Pt2.all (0)'Access;

      Pt1_Mat := Cv_Create_Mat (1, N, Cv_Make_Type (Cv_32f, 2), To_Void_Ptr (Pt1_Ptr));
      Pt2_Mat := Cv_Create_Mat (1, N, Cv_Make_Type (Cv_32f, 2), To_Void_Ptr (Pt2_Ptr));

      if Cv_Find_Homography (Pt1_Mat'Unchecked_Access, Pt2_Mat'Unchecked_Access, H_Mat'Unchecked_Access, Cv_Ransac, 5.0) = 0 then
         return 0;
      end if;

      for I in Integer range 0 .. 3
      loop
         Z := 1.0 / (H (6) * Long_Float (Src_Corners (I).X) + H (7) * Long_Float (Src_Corners (I).Y) + H (8));
         X := (H (0) * Long_Float (Src_Corners (I).X) + H (1) * Long_Float (Src_Corners (I).Y) + H (2)) * Z;
         Y := (H (3) * Long_Float (Src_Corners (I).X) + H (4) * Long_Float (Src_Corners (I).Y) + H (5)) * Z;
         Dst_Corners.all (I) := Cv_Create_Point (Cv_Round (X), Cv_Round (Y));
      end loop;

      return 1;
   end Locate_Planar_Object;
   --


   ---
   -- Main
   ---
   Object_Filename : constant String := ("C:\MDH\master_thesis\opencvada\Exec\box.png");
   Scene_Filename : constant String := ("C:\MDH\master_thesis\opencvada\Exec\box_in_scene.png");
   Storage : Cv_Mem_Storage_Ptr := Cv_Create_Mem_Storage (0);
   Object : Ipl_Image_Ptr := Cv_Load_Image (Object_Filename, Cv_Load_Image_Grayscale);
   Image : Ipl_Image_Ptr := Cv_Load_Image (Scene_Filename, Cv_Load_Image_Grayscale);
   Object_Color : Ipl_Image_Ptr := Cv_Create_Image (Cv_Get_Size (+Object), 8, 3);
   Correspond : Ipl_Image_Ptr := Cv_Create_Image (Cv_Create_Size (Image.all.Width, Object.all.Height + Image.all.Height), 8, 1);
   Object_Keypoints, Object_Descriptors, Image_Keypoints, Image_Descriptors  : aliased Cv_Seq_Ptr := null;

   Params : Cv_Surf_Params := Cv_Create_Surf_Params (500.0, 1);
   Src_Corners : Cv_Point_Array (0 .. 3)  := ((0, 0), (Object.all.Width, 0), (Object.all.Width, Object.all.Height), (0, Object.all.Height));
   Dst_Corners : aliased Cv_Point_Array_Ptr := new Cv_Point_Array (0 .. 3);
   Pt_Pairs : aliased Cv_32s_Pointer;
   Pt_Length : aliased Integer := 0;
   N : Integer;
   R1, R2 : Cv_Surf_Point_Ptr;

   function To_Surf_Point_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_Surf_Point_Ptr);
begin
   Cv_Cvt_Color (+Object, +Object_Color, Cv_Gray2bgr);
   Cv_Extract_Surf (+Object, null, Object_Keypoints'Access, Object_Descriptors'Access, Storage, Params);
   Cv_Extract_Surf (+Image, null, Image_Keypoints'Access, Image_Descriptors'Access, Storage, Params);

   Cv_Set_Image_Roi ( Correspond, Cv_Create_Rect ( 0, 0, Object.all.Width, Object.all.Height ) );
   Cv_Copy ( +Object, +Correspond );
   Cv_Set_Image_Roi ( Correspond, Cv_Create_Rect ( 0, Object.all.Height, Correspond.all.Width, Correspond.all.Height ) );
   Cv_Copy ( +Image, +Correspond );
   Cv_Reset_Image_Roi ( Correspond );

   if Locate_Planar_Object (Object_Keypoints,
                            Object_Descriptors,
                            Image_Keypoints,
                            Image_Descriptors,
                            Src_Corners,
                            Dst_Corners) > 0 then
      for I in Integer range 0 .. 3
      loop
         Cv_Line (+Correspond,
                  Cv_Create_Point (Dst_Corners.all (I).X, Dst_Corners.all (I).Y + Object.all.Height),
                  Cv_Create_Point (Dst_Corners.all ((I + 1) mod 4).X, Dst_Corners.all ((I + 1)mod 4).Y + Object.all.Height),
                  Colors (8));
      end loop;
   end if;

   Find_Pairs (Object_Keypoints,
               Object_Descriptors,
               Image_Keypoints,
               Image_Descriptors,
               Pt_Pairs'Access,
               Pt_Length'Access);
   N := 0;
   loop
      exit when N >= Pt_Length;

      R1 := To_Surf_Point_Ptr (Cv_Get_Seq_Elem (Object_Keypoints, Pt_Pairs.all));
      Core.Cv_32s_Pointer_Pkg.Increment (Pt_Pairs);
      R2 := To_Surf_Point_Ptr (Cv_Get_Seq_Elem (Image_Keypoints, Pt_Pairs.all));
      Core.Cv_32s_Pointer_Pkg.Increment (Pt_Pairs);
      Cv_Line (+Correspond,
               Cv_Point_From_32f (R1.all.Pt),
               Cv_Create_Point (Cv_Round (R2.all.Pt.X), Cv_Round (R2.all.Pt.Y + Float(object.all.Height))),
               Colors(8));
      N := N + 2;
   end loop;

   Cv_Show_Image ("correspond", +Correspond);
   for I in Integer range 0 .. Object_Keypoints.all.Total - 1
   loop
      R1 := To_Surf_Point_Ptr (Cv_Get_Seq_Elem (Object_Keypoints, I));
      Cv_Circle (+Object_Color,
                 Cv_Create_Point (Cv_Round (R1.all.Pt.X), Cv_Round (R1.all.Pt.Y)),
                 Cv_Round (Float(r1.all.Size) * 1.2 / 9.0 * 2.0),
                 Colors (0),
                 1, 8, 0);
   end loop;

   Cv_Show_Image ("object", +Object_Color);
   Cv_Wait_Key;
   Cv_Destroy_Window ("correspond");
   Cv_Destroy_Window ("object");
   null;
end Find_Object;
