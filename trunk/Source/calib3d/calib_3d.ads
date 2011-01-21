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
-- calib_3d.ads - Camera Calibration, Pose Estimation and Stereo
-- Comments, Information, Other
-----------------------------------------------------------------------
with Core; use Core;

package Calib_3d is
-- Camera Calibration, Pose Estimation and Stereo ---------------------------
-----------------------------------------------------------------------------
   type Cv_Posit_Object is record
      N        : Integer;
      Inv_Matr : access Cv_32f_Array;
      Obj_Vecs : access Cv_32f_Array;
      Img_Vecs : access Cv_32f_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Posit_Object);
   type Cv_Posit_Object_P is access all Cv_Posit_Object;

   --     Allocates and initializes CvPOSITObject structure before doing cvPOSIT
   function Cv_Create_Posit_Object (Points      : Cv_Point_3d_32f_Array;
                                    Point_Count : Integer)
                                    return Cv_Posit_Object_P;

   --     Runs POSIT (POSe from ITeration) algorithm for determining 3d position
   --     of an object given its model and projection in a weak-perspective case
   procedure Cv_Posit (Posit_Object       : Cv_Posit_Object_P;
                       Image_Points       : Cv_Point_2d_32f;
                       Focal_Length       : Long_Float;
                       Criteria           : Cv_Term_Criteria;
                       Rotation_Matrix    : Cv_32f_Array;
                       Translation_Vector : Cv_32f_Array);

   --     Releases CvPOSITObject structure
   procedure Cv_Release_Posit_Object (Posit_Object : access Cv_Posit_Object_P);

   --     updates the number of RANSAC iterations
   function Cv_Ransac_Update_Num_Iters (P            : Long_Float;
                                        Err_Prob     : Long_Float;
                                        Model_Points : Integer;
                                        Max_Iters    : Integer)
                                        return Integer;

   --     Convert points to/from homogeneous coordinates.
   procedure Cv_Convert_Points_Homogeneous (Src : Cv_Mat_P;
                                            Dst : Cv_Mat_P);

   Cv_Lmeds          : constant := 4; -- Shouldn't be here
   Cv_Ransac         : constant := 8; -- Shouldn't be here

   Cv_Fm_7point      : constant := 1;
   Cv_Fm_8point      : constant := 2;
   Cv_Fm_Lmeds       : constant := Cv_Lmeds;
   Cv_Fm_Ransac      : constant := Cv_Ransac;
   Cv_Fm_Lmeds_Only  : constant := Cv_Lmeds;
   Cv_Fm_Ransac_Only : constant := Cv_Ransac;

   --     Calculates the fundamental matrix from the corresponding points in
   --     two images.
   function Cv_Find_Fundamental_Mat (Points1           : Cv_Mat_P;
                                     Points2           : Cv_Mat_P;
                                     Fundamentalmatrix : Cv_Mat_P;
                                     Method            : Integer := Cv_Fm_Ransac;
                                     Param1            : Integer := 1;
                                     Param2            : Long_Float := 0.99;
                                     Status            : Cv_Mat_P := null)
                                     return Integer;

   --     for points in one image of a stereo pair, computes the corresponding
   --     epilines in the other image.
   procedure Cv_Compute_Correspond_Epilines (Points     : Cv_Mat_P;
                                             Whichimage : Integer;
                                             F          : Cv_Mat_P;
                                             Lines      : Cv_Mat_P);

   --     Triangulation functions
   procedure Cv_Triangulate_Points (Proj_Matr1   : Cv_Mat_P;
                                    Proj_Matr2   : Cv_Mat_P;
                                    Proj_Points1 : Cv_Mat_P;
                                    Proj_Points2 : Cv_Mat_P;
                                    Points_4d    : Cv_Mat_P);

   procedure Cv_Correct_Matches (F           : Cv_Mat_P;
                                 Points1     : Cv_Mat_P;
                                 Points2     : Cv_Mat_P;
                                 New_Points1 : Cv_Mat_P;
                                 New_Points2 : Cv_Mat_P);

   --     Returns the new camera matrix based on the free scaling parameter
   procedure Cv_Get_Optimal_New_Camera_Matrix (Cameramatrix    : Cv_Mat_P;
                                               Distcoeffs      : Cv_Mat_P;
                                               Imagesize       : Cv_Size;
                                               Alpha           : Long_Float;
                                               Newcameramatrix : Cv_Mat_P;
                                               Newimagesize    : Cv_Size := Cv_Create_Size (0, 0);
                                               Validpixroi     : access Cv_Rect := null);

   --     Converts a rotation matrix to a rotation vector or vice versa.
   function Cv_Rodrigues2 (Src      : Cv_Mat_P;
                           Dst      : Cv_Mat_P;
                           Jacobian : Cv_Mat_P := null)
                           return Integer;

   --     Finds the perspective transformation between two planes.
   procedure Cv_Find_Homography (Srcpoints             : Cv_Mat_P;
                                 Dstpoints             : Cv_Mat_P;
                                 H                     : Cv_Mat_P;
                                 Method                : Integer := 0;
                                 Ransacreprojthreshold : Integer := 3;
                                 Status                : Cv_Mat_P := null);

   --     Computes the RQ decomposition of 3x3 matrices.
   procedure Cv_Rq_Decomp_3x3 (M           : Cv_Mat_P;
                               R           : Cv_Mat_P;
                               Q           : Cv_Mat_P;
                               Qx          : Cv_Mat_P := null;
                               Qy          : Cv_Mat_P := null;
                               Qz          : Cv_Mat_P := null;
                               Eulerangles : Cv_Point_3d_64f_Array := Cv_Point_3d_64f_Array_Null);

   --     Decomposes the projection matrix into a rotation matrix and a camera
   --     matrix.
   procedure Cv_Decompose_Projection_Matrix (Projmatrix   : Cv_Mat_P;
                                             Cameramatrix : Cv_Mat_P;
                                             Rotmatrix    : Cv_Mat_P;
                                             Transvect    : Cv_Mat_P;
                                             Rotmatrx     : Cv_Mat_P := null;
                                             Rotmatry     : Cv_Mat_P := null;
                                             Rotmatrz     : Cv_Mat_P := null;
                                             Eulerangles  : Cv_Point_3d_64f_Array := Cv_Point_3d_64f_Array_Null);

   --     Computes d(AB)/dA and d(AB)/dB
   procedure Cv_Calc_Mat_Mul_Deriv (A     : Cv_Mat_P;
                                    B     : Cv_Mat_P;
                                    Dabda : Cv_Mat_P;
                                    Dabdb : Cv_Mat_P);

   --     Computes r3 = rodrigues(rodrigues(r2)*rodrigues(r1)),
   --     t3 = rodrigues(r2)*t1 + t2 and the respective derivatives
   procedure Cv_Compose_Rt (Rvec1  : Cv_Mat_P;
                            Tvec1  : Cv_Mat_P;
                            Rvec2  : Cv_Mat_P;
                            Tvec2  : Cv_Mat_P;
                            Rvec3  : Cv_Mat_P;
                            Tvec3  : Cv_Mat_P;
                            Dr3dr1 : Cv_Mat_P := null;
                            Dr3dt1 : Cv_Mat_P := null;
                            Dr3dr2 : Cv_Mat_P := null;
                            Dr3dt2 : Cv_Mat_P := null;
                            Dt3dr1 : Cv_Mat_P := null;
                            Dt3dt1 : Cv_Mat_P := null;
                            Dt3dr2 : Cv_Mat_P := null;
                            Dt3dt2 : Cv_Mat_P := null);

   --     Project 3D points on to an image plane.
   procedure Cv_Project_Points2 (Objectpoints : Cv_Mat_P;
                                 Rvec         : Cv_Mat_P;
                                 Tvec         : Cv_Mat_P;
                                 Cameramatrix : Cv_Mat_P;
                                 Distcoeffs   : Cv_Mat_P;
                                 Imagepoints  : Cv_Mat_P;
                                 Dpdrot       : Cv_Mat_P := null;
                                 Dpdt         : Cv_Mat_P := null;
                                 Dpdf         : Cv_Mat_P := null;
                                 Dpdc         : Cv_Mat_P := null;
                                 Dpddist      : Cv_Mat_P := null);

   --     Finds the object pose from the 3D-2D point correspondences
   procedure Cv_Find_Extrinsic_Camera_Params2 (Objectpoints      : Cv_Mat_P;
                                               Imagepoints       : Cv_Mat_P;
                                               Cameramatrix      : Cv_Mat_P;
                                               Distcoeffs        : Cv_Mat_P;
                                               Rvec              : Cv_Mat_P;
                                               Tvec              : Cv_Mat_P;
                                               Useextrinsicguess : Integer := 0);

   --     Finds the initial camera matrix from the 3D-2D point correspondences
   procedure Cv_Init_Intrinsic_Params_2d (Objectpoints : Cv_Mat_P;
                                          Imagepoints  : Cv_Mat_P;
                                          Npoints      : Cv_Mat_P;
                                          Imagesize    : Cv_Size;
                                          Cameramatrix : Cv_Mat_P;
                                          Aspectratio  : Long_Float := 1.0);

   Cv_Calib_Cb_Adaptive_Thresh : constant := 1;
   Cv_Calib_Cb_Normalize_Image : constant := 2;
   Cv_Calib_Cb_Filter_Quads    : constant := 4;
   Cv_Calib_Cb_Fast_Check      : constant := 8;

   --     Performs a fast check if a chessboard is in the input image. This is a
   --     workaround to a problem of cvFindChessboardCorners being slow on
   --     images with no chessboard
   --     - src: input image
   --     - size: chessboard size
   --     Returns 1 if a chessboard can be in this image and
   --     findChessboardCorners should be called, 0 if there is no chessboard,
   --     -1 in case of error
   function Cv_Check_Chessboard (Src  : Ipl_Image_P;
                                 Size : Cv_Size)
                                 return Integer;

   --     Finds the positions of the internal corners of the chessboard.
   function Cv_Find_Chessboard_Corners (Image       : Cv_Arr_P; -- doesn't correspond to C but makes more sense.
                                        Patternsize : Cv_Size;
                                        Corners     : Cv_Point_2d_32f_Array;
                                        Cornercount : access Integer;
                                        Flags       : Integer := Cv_Calib_Cb_Adaptive_Thresh) return Integer;

   --     Renders the detected chessboard corners.
   procedure Cv_Draw_Chessboard_Corners (Image           : Cv_Arr_P;
                                         Patternsize     : Cv_Size;
                                         Corners         : Cv_Point_2d_32f_Array;
                                         Count           : Integer;
                                         Patternwasfound : Integer);

   Cv_Calib_Use_Intrinsic_Guess : constant := 1;
   Cv_Calib_Fix_Aspect_Ratio    : constant := 2;
   Cv_Calib_Fix_Principal_Point : constant := 4;
   Cv_Calib_Zero_Tangent_Dist   : constant := 8;
   Cv_Calib_Fix_Focal_Length    : constant := 16;
   Cv_Calib_Fix_K1              : constant := 32;
   Cv_Calib_Fix_K2              : constant := 64;
   Cv_Calib_Fix_K3              : constant := 128;

   --     Finds the camera intrinsic and extrinsic parameters from several
   --     views of a calibration pattern.
   function Cv_Calibrate_Camera2 (Objectpoints : Cv_Mat_P;
                                  Imagepoints  : Cv_Mat_P;
                                  Pointcounts  : Cv_Mat_P;
                                  Imagesize    : Cv_Size;
                                  Cameramatrix : Cv_Mat_P;
                                  Distcoeffs   : Cv_Mat_P;
                                  Rvecs        : Cv_Mat_P := null;
                                  Tvecs        : Cv_Mat_P := null;
                                  Flags        : Integer := 0) return Long_Float;

   --     Computes various useful characteristics of the camera from the data
   --     computed by cvCalibrateCamera2
   procedure Cv_Calibration_Matrix_Values (Camera_Matrix      : Cv_Mat_P;
                                           Image_Size         : Cv_Size;
                                           Aperture_Width     : Long_Float := 0.0;
                                           Aperture_Height    : Long_Float := 0.0;
                                           Fovx               : access Long_Float := null;
                                           Fovy               : access Long_Float := null;
                                           Focal_Length       : access Long_Float := null;
                                           Principal_Point    : access Cv_Point_2d_64f := null;
                                           Pixel_Aspect_Ratio : access Long_Float := null);

   Cv_Calib_Fix_Intrinsic     : constant := 256;
   Cv_Calib_Same_Focal_Length : constant := 512;

   --     Calibrates stereo camera.
   function Cv_Stereo_Calibrate (Objectpoints  : Cv_Mat_P;
                                 Imagepoints1  : Cv_Mat_P;
                                 Imagepoints2  : Cv_Mat_P;
                                 Pointcounts   : Cv_Mat_P;
                                 Cameramatrix1 : Cv_Mat_P;
                                 Distcoeffs1   : Cv_Mat_P;
                                 Cameramatrix2 : Cv_Mat_P;
                                 Distcoeffs2   : Cv_Mat_P;
                                 Imagesize     : Cv_Size;
                                 R             : Cv_Mat_P;
                                 T             : Cv_Mat_P;
                                 E             : Cv_Mat_P := null;
                                 F             : Cv_Mat_P := null;
                                 Termcrit      : Cv_Term_Criteria := Cv_Create_Term_Criteria (Cv_Termcrit_Iter + Cv_Termcrit_Eps, 30, 1.0e-6);
                                 Flags         : Integer := Cv_Calib_Fix_Intrinsic) return Long_Float;

   Cv_Calib_Zero_Disparity : constant := 1024;

   --     Computes rectification transforms for each head of a calibrated
   --     stereo camera.
   procedure Cv_Stereo_Rectify (Cameramatrix1  : Cv_Mat_P;
                                Cameramatrix2  : Cv_Mat_P;
                                Distcoeffs1    : Cv_Mat_P;
                                Distcoeffs2    : Cv_Mat_P;
                                Imagesize      : Cv_Size;
                                R              : Cv_Mat_P;
                                T              : Cv_Mat_P;
                                R1             : Cv_Mat_P;
                                R2             : Cv_Mat_P;
                                P1             : Cv_Mat_P;
                                P2             : Cv_Mat_P;
                                Q              : Cv_Mat_P := null;
                                Flags          : Integer := Cv_Calib_Zero_Disparity;
                                Alpha          : Long_Float := 1.0;
                                Newimagesize   : Cv_Size := Cv_Create_Size (0, 0);
                                Roi1           : access Cv_Rect := null; -- might need to be Cv_Rect_P
                                Roi2           : access Cv_Rect := null); -- might need to be Cv_Rect_P

   --     Computes rectification transform for uncalibrated stereo camera.
   procedure Cv_Stereo_Rectify_Uncalibrated (Points1   : Cv_Mat_P;
                                             Points2   : Cv_Mat_P;
                                             F         : Cv_Mat_P;
                                             Imagesize : Cv_Size;
                                             H1        : Cv_Mat_P;
                                             H2        : Cv_Mat_P;
                                             Threshold : Long_Float := 5.0);

   Cv_Stereo_Bm_Normalized_Response : constant := 0;
   Cv_Stereo_Bm_Xsobel              : constant := 1;

   -- The structure for block matching stereo correspondence algorithm.
   type Cv_Stereo_Bm_State is record
      Prefiltertype       : Integer;
      Prefiltersize       : Integer;
      Prefiltercap        : Integer;
      Sadwindowsize       : Integer;
      Mindisparity        : Integer;
      Numberofdisparities : Integer;
      Texturethreshold    : Integer;
      Uniqenessratio      : Integer;
      Specklewindowsize   : Integer;
      Specklerange        : Integer;
      Trysmallerwindows   : Integer;
      Roi1                : Cv_Rect;
      Roi2                : Cv_Rect;
      Displ2maxdiff       : Integer;
      Prefilteredimg0     : Cv_Mat_P;
      Prefilteredimg1     : Cv_Mat_P;
      Slidingsumbuf       : Cv_Mat_P;
      Cost                : Cv_Mat_P;
      Disp                : Cv_Mat_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Stereo_Bm_State);
   type Cv_Stereo_Bm_State_P is access Cv_Stereo_Bm_State;

   Cv_Stereo_Bm_Basic               : constant := 0;
   Cv_Stereo_Bm_Fish_Eye            : constant := 1;
   Cv_Stereo_Bm_Narrow              : constant := 2;

   --     Creates block matching stereo correspondence structure.
   function Cv_Create_Stereo_Bm_State (Preset              : Integer;
                                       Numberofdisparities : Integer)
                                       return Cv_Stereo_Bm_State_P;

   --     Releases block matching stereo correspondence structure
   procedure Cv_Release_Stereo_Bm_State (State : access Cv_Stereo_Bm_State_P);

   --     Computes the disparity map using block matching algorithm.
   procedure Cv_Find_Stereo_Correspondence_Bm (Left      : Cv_Arr_P;
                                               Right     : Cv_Arr_P;
                                               Disparity : Cv_Arr_P;
                                               State     : Cv_Stereo_Bm_State_P);

   function Cv_Get_Valid_Disparity_Roi (Roi1                  : Cv_Rect;
                                        Roi2                  : Cv_Rect;
                                        Min_Disparity         : Integer;
                                        Number_Of_Disparities : Integer;
                                        Sad_Window_Size       : Integer)
                                        return Cv_Rect;

   procedure Cv_Validate_Disparity (Disparity             : Cv_Arr_P;
                                    Cost                  : Cv_Arr_P;
                                    Min_Disparity         : Integer;
                                    Number_Of_Disparities : Integer;
                                    Disp_12_Max_Diff      : Integer := 1);

   Cv_Stereo_Gc_Occluded : constant := 32767; -- SHRT_MAX from limits.h

   -- The structure for graph cuts-based stereo correspondence algorithm
   type Cv_Stereo_Gc_State is record
      Ithreshold                  : Integer;
      Interactionradius           : Integer;
      K, Lambda, Lambda1, Lambda2 : Float;
      Occlusioncost               : Integer;
      Mindisparity                : Integer;
      Numberofdisparities         : Integer;
      Maxiters                    : Integer;
      Left                        : Cv_Mat_P;
      Right                       : Cv_Mat_P;
      Displeft                    : Cv_Mat_P;
      Dispright                   : Cv_Mat_P;
      Ptrleft                     : Cv_Mat_P;
      Ptrright                    : Cv_Mat_P;
      Vtxbuf                      : Cv_Mat_P;
      Edgebuf                     : Cv_Mat_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Stereo_Gc_State);
   type Cv_Stereo_Gc_State_P is access Cv_Stereo_Gc_State;

   --     Creates the state of graph cut-based stereo correspondence algorithm.
   function Cv_Create_Stereo_Gc_State (Numberofdisparities : Integer;
                                       Maxiters            : Integer)
                                       return Cv_Stereo_Gc_State_P;

   --     Releases the state structure of the graph cut-based stereo
   --     correspondence algorithm.
   procedure Cv_Release_Stereo_Gc_State (State : access Cv_Stereo_Gc_State_P);

   --     Computes the disparity map using graph cut-based algorithm.
   procedure Cv_Find_Stereo_Correspondence_Gc (Left              : Cv_Arr_P;
                                               Right             : Cv_Arr_P;
                                               Displeft          : Cv_Arr_P;
                                               Dispright         : Cv_Arr_P;
                                               State             : Cv_Stereo_Gc_State_P;
                                               Usedisparityguess : Integer := 0);

   --     Reprojects disparity image to 3D space.
   procedure Cv_Reproject_Image_To_3d (Disparity           : Cv_Arr_P;
                                       Image3d             : Cv_Arr_P;
                                       Q                   : Cv_Mat_P;
                                       Handlemissingvalues : Integer := 0);

private
   pragma Import (C, Cv_Create_Posit_Object, "cvCreatePOSITObject");
   pragma Import (C, Cv_Posit, "cvPOSIT");
   pragma Import (C, Cv_Release_Posit_Object, "cvReleasePOSITObject");
   pragma Import (C, Cv_Ransac_Update_Num_Iters, "cvRANSACUpdateNumIters");
   pragma Import (C, Cv_Convert_Points_Homogeneous, "cvConvertPointsHomogeneous");
   pragma Import (C, Cv_Find_Fundamental_Mat, "cvFindFundamentalMat");
   pragma Import (C, Cv_Compute_Correspond_Epilines, "cvComputeCorrespondEpilines");
   pragma Import (C, Cv_Triangulate_Points, "CvTriangulatePoints");
   pragma Import (C, Cv_Correct_Matches, "cvCorrectMatches");
   pragma Import (C, Cv_Get_Optimal_New_Camera_Matrix, "cvGetOptimalNewCameraMatrix");
   pragma Import (C, Cv_Rodrigues2, "cvRodrigues2");
   pragma Import (C, Cv_Find_Homography, "cvFindHomography");
   pragma Import (C, Cv_Rq_Decomp_3x3, "cvRQDecomp3x3");
   pragma Import (C, Cv_Decompose_Projection_Matrix, "cvDecomposeProjectionMatrix");
   pragma Import (C, Cv_Calc_Mat_Mul_Deriv, "cvCalcMatMulDeriv");
   pragma Import (C, Cv_Compose_Rt, "cvComposeRT");
   pragma Import (C, Cv_Project_Points2, "cvProjectPoints2");
   pragma Import (C, Cv_Find_Extrinsic_Camera_Params2, "cvFindExtrinsicCameraParams2");
   pragma Import (C, Cv_Init_Intrinsic_Params_2d, "cvInitIntrinsicParams2D");
   pragma Import (C, Cv_Check_Chessboard, "cvCheckChessboard");
   pragma Import (C, Cv_Find_Chessboard_Corners, "cvFindChessboardCorners");
   pragma Import (C, Cv_Draw_Chessboard_Corners, "cvDrawChessboardCorners");
   pragma Import (C, Cv_Calibrate_Camera2, "cvCalibrateCamera2");
   pragma Import (C, Cv_Calibration_Matrix_Values, "cvCalibrationMatrixValues");
   pragma Import (C, Cv_Stereo_Calibrate, "CvStereoCalibrate");
   pragma Import (C, Cv_Stereo_Rectify, "cvStereoRectify");
   pragma Import (C, Cv_Stereo_Rectify_Uncalibrated, "cvStereoRectifyUncalibrated");
   pragma Import (C, Cv_Create_Stereo_Bm_State, "cvCreateStereoBMState");
   pragma Import (C, Cv_Release_Stereo_Bm_State, "cvReleaseStereoBMState");
   pragma Import (C, Cv_Find_Stereo_Correspondence_Bm, "cvFindStereoCorrespondenceBM");
   pragma Import (C, Cv_Get_Valid_Disparity_Roi, "cvGetValidDisparityROI");
   pragma Import (C, Cv_Validate_Disparity, "cvValidateDisparity");
   pragma Import (C, Cv_Create_Stereo_Gc_State, "cvCreateStereoGCState");
   pragma Import (C, Cv_Release_Stereo_Gc_State, "cvReleaseStereoGCState");
   pragma Import (C, Cv_Find_Stereo_Correspondence_Gc, "cvFindStereoCorrespondenceGC");
   pragma Import (C, Cv_Reproject_Image_To_3d, "cvReprojectImageTo3D");
end Calib_3d;
