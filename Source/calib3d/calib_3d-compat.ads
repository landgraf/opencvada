with Interfaces; use Interfaces;
-----------------------------------------------------------------------
-- Ada bindings for OpenCV 2.1.1 (from SVN 3 October 2010, rev. 3703)
-- Developed as a master thesis project at Mälardalens Högskola
-- OpenCV: http://opencv.willowgarage.com/
-- Ada bindings : http://not_available.nope/
-- License @ ./LICENSE (BSD license)
-----------------------------------------------------------------------

--Contact--------------------------------------------------------------
-- Lars Cederholm, Niklas Pettersson
-- Mälardalens Högskola, http://www.mdh.se/
-- [lcm06001,npn06002]@student.mdh.se
-----------------------------------------------------------------------

--File-Info-------------------------------------------------------------
-- calib_3d-compat.ads - compat_c.h
-- Comments, Information, Other
-----------------------------------------------------------------------

package Calib_3d.Compat is
   procedure Cv_Find_Fundamental_Matrix (Points1   : Cv_32s_Array;
                                         Points2   : Cv_32s_Array;
                                         Numpoints : Integer;
                                         Method    : Integer;
                                         Matrix    : Cv_32f_Array);

   function Cv_Find_Chess_Board_Corner_Guesses (Arr          : Cv_Arr_P;
                                                Thresharr    : Cv_Arr_P;
                                                Storage      : Cv_Mem_Storage;
                                                Pattern_Size : Cv_Size;
                                                Corners      : Cv_Point_2d_32f_Array;
                                                Corner_Count : access Integer)
                                                return Integer;

   --     Calibrates camera using multiple views of calibration pattern
   procedure Cv_Calibrate_Camera (Image_Count         : Integer;
                                  Point_Counts        : Cv_32s_Array;
                                  Image_Size          : Cv_Size;
                                  Image_Points        : Cv_Point_2d_32f_Array;
                                  Object_Points       : Cv_Point_3d_32f_Array;
                                  Distortion_Coeffs   : Cv_32f_Array;
                                  Camera_Matrix       : Cv_32f_Array;
                                  Translation_Vectors : Cv_32f_Array;
                                  Rotation_Matrices   : Cv_32f_Array;
                                  Flags               : Unsigned_32);

   procedure Cv_Calibrate_Camera_64d (Image_Count         : Integer;
                                      Point_Counts        : Cv_32s_Array;
                                      Image_Size          : Cv_Size;
                                      Image_Points        : Cv_Point_2d_64f_Array;
                                      Object_Points       : Cv_Point_3d_64f_Array;
                                      Distortion_Coeffs   : Cv_64f_Array;
                                      Camera_Matrix       : Cv_64f_Array;
                                      Translation_Vectors : Cv_64f_Array;
                                      Rotation_Matrices   : Cv_64f_Array;
                                      Flags               : Unsigned_32);

   --     Find 3d position of object given intrinsic camera parameters,
   --     3d model of the object and projection of the object into view plane
   procedure Cv_Find_Extrinsic_Camera_Params (Point_Count        : Integer;
                                              Image_Size         : Cv_Size;
                                              Image_Points       : Cv_Point_3d_32f_Array;
                                              Object_Points      : Cv_Point_3d_32f_Array;
                                              Focal_Length       : Cv_32f_Array;
                                              Principal_Point    : Cv_Point_2d_32f;
                                              Distortion_Coeffs  : Cv_32f_Array;
                                              Rotation_Vector    : Cv_32f_Array;
                                              Translation_Vector : Cv_32f_Array);

   --     Variant of the previous function that takes double-precision parameters
   procedure Cv_Find_Extrinsic_Camera_Params_64d (Point_Count        : Integer;
                                                  Image_Size         : Cv_Size;
                                                  Image_Points       : Cv_Point_2d_64f_Array;
                                                  Object_Points      : Cv_Point_3d_64f_Array;
                                                  Focal_Length       : Cv_64f_Array;
                                                  Principal_Point    : Cv_Point_2d_64f;
                                                  Distortion_Coeffs  : Cv_64f_Array;
                                                  Rotation_Vector    : Cv_64f_Array;
                                                  Translation_Vector : Cv_64f_Array);

   Cv_Rodrigues_M2v : constant := 0;
   Cv_Rodrigues_V2m : constant := 0;

   --     Converts rotation_matrix matrix to rotation_matrix vector or vice versa
   procedure Cv_Rodrigues (Rotation_Matrix : Cv_Mat_P;
                           Rotation_Vector : Cv_Mat_P;
                           Jacobian        : Cv_Mat_P;
                           Conv_Type       : Integer);

   procedure Cv_Project_Points (Point_Count                    : Integer;
                                Object_Points                  : Cv_Point_3d_64f_Array;
                                Rotation_Vector                : Cv_64f_Array;
                                Translation_Vector             : Cv_64f_Array;
                                Focal_Length                   : Cv_64f_Array;
                                Principal_Point                : Cv_Point_2d_64f;
                                Distortion                     : Cv_64f_Array;
                                Image_Points                   : Cv_Point_2d_64f_Array;
                                Deriv_Points_Rotation_Matrix   : Cv_64f_Array;
                                Deriv_Points_Translation_Vect  : Cv_64f_Array;
                                Deriv_Points_Focal             : Cv_64f_Array;
                                Deriv_Points_Principal_Point   : access Long_Float;
                                Deriv_Points_Distortion_Coeffs : Cv_64f_Array);

   --     Simpler version of the previous function
   procedure Cv_Project_Points_Simple (Point_Count        : Integer;
                                       Object_Points      : Cv_Point_3d_64f_Array;
                                       Rotation_Matrix    : Cv_64f_Array;
                                       Translation_Vector : Cv_64f_Array;
                                       Camera_Matrix      : Cv_64f_Array;
                                       Distortion         : Cv_64f_Array;
                                       Image_Points       : Cv_Point_2d_64f_Array);

private

   pragma Import (C, Cv_Find_Fundamental_Matrix, "CvFindFundamentalMatrix");
   pragma Import (C, Cv_Calibrate_Camera, "cvCalibrateCamera");
   pragma Import (C, Cv_Find_Chess_Board_Corner_Guesses, "cvFindChessBoardCornerGuesses");
   pragma Import (C, Cv_Calibrate_Camera_64d, "cvCalibrateCamera_64d");
   pragma Import (C, Cv_Find_Extrinsic_Camera_Params, "cvFindExtrinsicCameraParams");
   pragma Import (C, Cv_Find_Extrinsic_Camera_Params_64d, "cvFindExtrinsicCameraParams_64d");
   pragma Import (C, Cv_Rodrigues, "cvRodrigues");
   pragma Import (C, Cv_Project_Points, "cvProjectPoints");
   pragma Import (C, Cv_Project_Points_Simple, "cvProjectPointsSimple");
end Calib_3d.Compat;
