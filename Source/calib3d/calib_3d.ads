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
with Core_Types_C; use Core_Types_C;
with Core; use Core;

package Calib_3D is
   -- Camera Calibration, Pose Estimation and Stereo ---------------------------
   -----------------------------------------------------------------------------
   type Cv_POSIT_Object is record
      N : Integer;
      Inv_Matr : access Cv_32f_Array;
      Obj_Vecs : access Cv_32f_Array;
      Img_Vecs : access Cv_32f_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_POSIT_Object);
   type Cv_POSIT_Object_P is access all Cv_POSIT_Object;

   --     Allocates and initializes CvPOSITObject structure before doing cvPOSIT
   function CvCreatePOSITObject (Points      : Cv_Point_3d_32f_Array;
                                 Point_Count : Integer)
                                 return Cv_POSIT_Object_P;

   pragma Import (C, CvCreatePOSITObject, "cvCreatePOSITObject");

   --     Runs POSIT (POSe from ITeration) algorithm for determining 3d position
   --     of an object given its model and projection in a weak-perspective case
   procedure CvPOSIT (Posit_Object       : Cv_POSIT_Object_P;
                      Image_Points       : Cv_Point_2d_32f;
                      Focal_Length       : Long_Float;
                      Criteria           : Cv_Term_Criteria;
                      Rotation_Matrix    : Cv_32f_Array;
                      Translation_Vector : Cv_32f_Array);
   pragma Import (C, CvPOSIT, "cvPOSIT");

   --     Releases CvPOSITObject structure
   procedure CvReleasePOSITObject (Posit_Object : access Cv_POSIT_Object_P);
   pragma Import (C, CvReleasePOSITObject, "cvReleasePOSITObject");

   --     updates the number of RANSAC iterations
   function CvRANSACUpdateNumIters (P            : Long_Float;
                                    Err_Prob     : Long_Float;
                                    Model_Points : Integer;
                                    Max_Iters    : Integer)
                                    return Integer;
   pragma Import (C, CvRANSACUpdateNumIters, "cvRANSACUpdateNumIters");

   procedure CvConvertPointsHomogeneous (Src : Cv_Mat_P;
                                         Dst : Cv_Mat_P);
   pragma Import (C, CvConvertPointsHomogeneous, "cvConvertPointsHomogeneous");

   CV_LMEDS          : constant := 4; -- Shouldn't be here
   CV_RANSAC         : constant := 8; -- Shouldn't be here

   CV_FM_7POINT      : constant := 1;
   CV_FM_8POINT      : constant := 2;
   CV_FM_LMEDS       : constant := CV_LMEDS;
   CV_FM_RANSAC      : constant := CV_RANSAC;
   CV_FM_LMEDS_ONLY  : constant := CV_LMEDS;
   CV_FM_RANSAC_ONLY : constant := CV_RANSAC;

   --     Calculates the fundamental matrix from the corresponding points in
   --     two images.
   function CvFindFundamentalMat (Points1           : Cv_Mat_P;
                                  Points2           : Cv_Mat_P;
                                  FundamentalMatrix : Cv_Mat_P;
                                  Method            : Integer := CV_FM_RANSAC;
                                  Param1            : Integer := 1;
                                  Param2            : Long_Float := 0.99;
                                  Status            : Cv_Mat_P := null)
                                  return Integer;
   pragma Import (C, CvFindFundamentalMat, "cvFindFundamentalMat");

   --     for points in one image of a stereo pair, computes the corresponding
   --     epilines in the other image.
   procedure CvComputeCorrespondEpilines (Points     : Cv_Mat_P;
                                          WhichImage : Integer;
                                          F          : Cv_Mat_P;
                                          Lines      : Cv_Mat_P);
   pragma Import (C, CvComputeCorrespondEpilines, "cvComputeCorrespondEpilines");

   --     Triangulation functions
   procedure CvTriangulatePoints (Proj_Matr1   : Cv_Mat_P;
                                  Proj_Matr2   : Cv_Mat_P;
                                  Proj_Points1 : Cv_Mat_P;
                                  Proj_Points2 : Cv_Mat_P;
                                  Points_4d    : Cv_Mat_P);
   pragma Import (C, CvTriangulatePoints, "CvTriangulatePoints");

   procedure CvCorrectMatches (F           : Cv_Mat_P;
                               Points1     : Cv_Mat_P;
                               Points2     : Cv_Mat_P;
                               New_Points1 : Cv_Mat_P;
                               New_Points2 : Cv_Mat_P);
   pragma Import (C, CvCorrectMatches, "cvCorrectMatches");

   --     Returns the new camera matrix based on the free scaling parameter
   procedure CvGetOptimalNewCameraMatrix (CameraMatrix    : Cv_Mat_P;
                                          DistCoeffs      : Cv_Mat_P;
                                          ImageSize       : Cv_Size;
                                          Alpha           : Long_Float;
                                          NewCameraMatrix : Cv_Mat_P;
                                          NewImageSize    : Cv_Size := CvSize (0, 0);
                                          ValidPixROI     : access Cv_Rect := null);
   pragma Import (C, CvGetOptimalNewCameraMatrix, "cvGetOptimalNewCameraMatrix");

   --     Converts a rotation matrix to a rotation vector or vice versa.
   function CvRodrigues2 (Src      : Cv_Mat_P;
                          Dst      : Cv_Mat_P;
                          Jacobian : Cv_Mat_P := null)
                          return Integer;
   pragma Import (C, CvRodrigues2, "cvRodrigues2");

   --     Finds the perspective transformation between two planes.
   procedure CvFindHomography (SrcPoints             : Cv_Mat_P;
                               DstPoints             : Cv_Mat_P;
                               H                     : Cv_Mat_P;
                               Method                : Integer := 0;
                               RansacReprojThreshold : Integer := 3;
                               Status                : Cv_Mat_P := null);
   pragma Import (C, CvFindHomography, "cvFindHomography");

   --     Computes the RQ decomposition of 3x3 matrices.
   procedure CvRQDecomp3x3 (M           : Cv_Mat_P;
                            R           : Cv_Mat_P;
                            Q           : Cv_Mat_P;
                            Qx          : Cv_Mat_P := null;
                            Qy          : Cv_Mat_P := null;
                            Qz          : Cv_Mat_P := null;
                            EulerAngles : Cv_Point_3D_64F_Array := Cv_Point_3D_64F_Array_Null);
   pragma Import (C, CvRQDecomp3x3, "cvRQDecomp3x3");

   --     Decomposes the projection matrix into a rotation matrix and a camera
   --     matrix.
   procedure CvDecomposeProjectionMatrix (ProjMatrix   : Cv_Mat_P;
                                          CameraMatrix : Cv_Mat_P;
                                          RotMatrix    : Cv_Mat_P;
                                          TransVect    : Cv_Mat_P;
                                          RotMatrX     : Cv_Mat_P := null;
                                          RotMatrY     : Cv_Mat_P := null;
                                          RotMatrZ     : Cv_Mat_P := null;
                                          EulerAngles  : Cv_Point_3D_64F_Array := Cv_Point_3D_64F_Array_Null);
   pragma Import (C, CvDecomposeProjectionMatrix, "cvDecomposeProjectionMatrix");

   --     Computes d(AB)/dA and d(AB)/dB
   procedure CvCalcMatMulDeriv (A     : Cv_Mat_P;
                                B     : Cv_Mat_P;
                                DABdA : Cv_Mat_P;
                                DABdB : Cv_Mat_P);
   pragma Import (C, CvCalcMatMulDeriv, "cvCalcMatMulDeriv");

   --     Computes r3 = rodrigues(rodrigues(r2)*rodrigues(r1)),
   --     t3 = rodrigues(r2)*t1 + t2 and the respective derivatives
   procedure CvComposeRT (Rvec1  : Cv_Mat_P;
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
   pragma Import (C, CvComposeRT, "cvComposeRT");

   --     Project 3D points on to an image plane.
   procedure CvProjectPoints2 (ObjectPoints : Cv_Mat_P;
                               Rvec         : Cv_Mat_P;
                               Tvec         : Cv_Mat_P;
                               CameraMatrix : Cv_Mat_P;
                               DistCoeffs   : Cv_Mat_P;
                               ImagePoints  : Cv_Mat_P;
                               Dpdrot       : Cv_Mat_P := null;
                               Dpdt         : Cv_Mat_P := null;
                               Dpdf         : Cv_Mat_P := null;
                               Dpdc         : Cv_Mat_P := null;
                               Dpddist      : Cv_Mat_P := null);
   pragma Import (C, CvProjectPoints2, "cvProjectPoints2");

   --     Finds the object pose from the 3D-2D point correspondences
   procedure CvFindExtrinsicCameraParams2 (ObjectPoints      : Cv_Mat_P;
                                           ImagePoints       : Cv_Mat_P;
                                           CameraMatrix      : Cv_Mat_P;
                                           DistCoeffs        : Cv_Mat_P;
                                           Rvec              : Cv_Mat_P;
                                           Tvec              : Cv_Mat_P;
                                           UseExtrinsicGuess : Integer := 0);
   pragma Import (C, CvFindExtrinsicCameraParams2, "cvFindExtrinsicCameraParams2");

   --     Finds the initial camera matrix from the 3D-2D point correspondences
   procedure CvInitIntrinsicParams2D (ObjectPoints : Cv_Mat_P;
                                      ImagePoints  : Cv_Mat_P;
                                      NPoints      : Cv_Mat_P;
                                      ImageSize    : Cv_Size;
                                      CameraMatrix : Cv_Mat_P;
                                      AspectRatio  : Long_Float := 1.0);
   pragma Import (C, CvInitIntrinsicParams2D, "cvInitIntrinsicParams2D");

   CV_CALIB_CB_ADAPTIVE_THRESH : constant := 1;
   CV_CALIB_CB_NORMALIZE_IMAGE : constant := 2;
   CV_CALIB_CB_FILTER_QUADS    : constant := 4;
   CV_CALIB_CB_FAST_CHECK      : constant := 8;

   --     Performs a fast check if a chessboard is in the input image. This is a
   --     workaround to a problem of cvFindChessboardCorners being slow on
   --     images with no chessboard
   --     - src: input image
   --     - size: chessboard size
   --     Returns 1 if a chessboard can be in this image and
   --     findChessboardCorners should be called, 0 if there is no chessboard,
   --     -1 in case of error
   function CvCheckChessboard (Src  : Ipl_Image_P;
                               Size : Cv_Size)
                               return Integer;
   pragma Import (C, CvCheckChessboard, "cvCheckChessboard");

   --     Finds the positions of the internal corners of the chessboard.
   function CvFindChessboardCorners (Image       : Cv_Arr_P; -- doesn't correspond to C but makes more sense.
                                     PatternSize : Cv_Size;
                                     Corners     : Cv_Point_2D_32F_Array;
                                     CornerCount : access Integer;
                                     Flags       : Integer := CV_CALIB_CB_ADAPTIVE_THRESH) return Integer;
   pragma Import (C, CvFindChessboardCorners, "cvFindChessboardCorners");

   --     Renders the detected chessboard corners.
   procedure CvDrawChessboardCorners (Image           : Cv_Arr_P;
                                      PatternSize     : Cv_Size;
                                      Corners         : Cv_Point_2D_32F_Array;
                                      Count           : Integer;
                                      PatternWasFound : Integer);
   pragma Import (C, CvDrawChessboardCorners, "cvDrawChessboardCorners");

   CV_CALIB_USE_INTRINSIC_GUESS : constant := 1;
   CV_CALIB_FIX_ASPECT_RATIO    : constant := 2;
   CV_CALIB_FIX_PRINCIPAL_POINT : constant := 4;
   CV_CALIB_ZERO_TANGENT_DIST   : constant := 8;
   CV_CALIB_FIX_FOCAL_LENGTH    : constant := 16;
   CV_CALIB_FIX_K1              : constant := 32;
   CV_CALIB_FIX_K2              : constant := 64;
   CV_CALIB_FIX_K3              : constant := 128;

   --     Finds the camera intrinsic and extrinsic parameters from several
   --     views of a calibration pattern.
   function CvCalibrateCamera2 (ObjectPoints : Cv_Mat_P;
                                ImagePoints  : Cv_Mat_P;
                                PointCounts  : Cv_Mat_P;
                                ImageSize    : Cv_Size;
                                CameraMatrix : Cv_Mat_P;
                                DistCoeffs   : Cv_Mat_P;
                                Rvecs        : Cv_Mat_P := null;
                                Tvecs        : Cv_Mat_P := null;
                                Flags        : Integer := 0) return Long_Float;
   pragma Import (C, CvCalibrateCamera2, "cvCalibrateCamera2");

   --     Computes various useful characteristics of the camera from the data
   --     computed by cvCalibrateCamera2
   procedure CvCalibrationMatrixValues (Camera_Matrix      : Cv_Mat_P;
                                        Image_Size         : Cv_Size;
                                        Aperture_Width     : Long_Float := 0.0;
                                        Aperture_Height    : Long_Float := 0.0;
                                        Fovx               : access Long_Float := null;
                                        Fovy               : access Long_Float := null;
                                        Focal_Length       : access Long_Float := null;
                                        Principal_Point    : access Cv_Point_2d_64f := null;
                                        Pixel_Aspect_Ratio : access Long_Float := null);
   pragma Import (C, CvCalibrationMatrixValues, "cvCalibrationMatrixValues");

   CV_CALIB_FIX_INTRINSIC     : constant := 256;
   CV_CALIB_SAME_FOCAL_LENGTH : constant := 512;

   --     Calibrates stereo camera.
   function CvStereoCalibrate (ObjectPoints  : Cv_Mat_P;
                               ImagePoints1  : Cv_Mat_P;
                               ImagePoints2  : Cv_Mat_P;
                               PointCounts   : Cv_Mat_P;
                               CameraMatrix1 : Cv_Mat_P;
                               DistCoeffs1   : Cv_Mat_P;
                               CameraMatrix2 : Cv_Mat_P;
                               DistCoeffs2   : Cv_Mat_P;
                               ImageSize     : Cv_Size;
                               R             : Cv_Mat_P;
                               T             : Cv_Mat_P;
                               E             : Cv_Mat_P := null;
                               F             : Cv_Mat_P := null;
                               TermCrit      : Cv_Term_Criteria := CvTermCriteria (CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 30, 1.0e-6);
                               Flags         : Integer := CV_CALIB_FIX_INTRINSIC) return Long_Float;
   pragma Import (C, CvStereoCalibrate, "CvStereoCalibrate");

   CV_CALIB_ZERO_DISPARITY : constant := 1024;

   --     Computes rectification transforms for each head of a calibrated
   --     stereo camera.
   procedure CvStereoRectify (CameraMatrix1  : Cv_Mat_P;
                              CameraMatrix2  : Cv_Mat_P;
                              DistCoeffs1    : Cv_Mat_P;
                              DistCoeffs2    : Cv_Mat_P;
                              ImageSize      : Cv_Size;
                              R              : Cv_Mat_P;
                              T              : Cv_Mat_P;
                              R1             : Cv_Mat_P;
                              R2             : Cv_Mat_P;
                              P1             : Cv_Mat_P;
                              P2             : Cv_Mat_P;
                              Q              : Cv_Mat_P := null;
                              Flags          : Integer := CV_CALIB_ZERO_DISPARITY;
                              Alpha          : Long_Float := 1.0;
                              NewImageSize   : Cv_Size := CvSize (0, 0);
                              Roi1           : access Cv_Rect := null; -- might need to be Cv_Rect_P
                              Roi2           : access Cv_RecT := null); -- might need to be Cv_Rect_P
   pragma Import (C, CvStereoRectify, "cvStereoRectify");

   --     Computes rectification transform for uncalibrated stereo camera.
   procedure CvStereoRectifyUncalibrated (Points1   : Cv_Mat_P;
                                          Points2   : Cv_Mat_P;
                                          F         : Cv_Mat_P;
                                          ImageSize : Cv_Size;
                                          H1        : Cv_Mat_P;
                                          H2        : Cv_Mat_P;
                                          Threshold : Long_Float := 5.0);
   pragma Import (C, CvStereoRectifyUncalibrated, "cvStereoRectifyUncalibrated");

   CV_STEREO_BM_NORMALIZED_RESPONSE : constant := 0;
   CV_STEREO_BM_XSOBEL              : constant := 1;

   -- The structure for block matching stereo correspondence algorithm.
   type Cv_Stereo_BM_State is record
      PreFilterType       : Integer;
      PreFilterSize       : Integer;
      PreFIlterCap        : Integer;
      SADWindowSize       : Integer;
      MinDisparity        : Integer;
      NumberofDisparities : Integer;
      TextureThreshold    : Integer;
      UniqenessRatio      : Integer;
      SpeckleWindowSize   : Integer;
      SpeckleRange        : Integer;
      TrySmallerWindows   : Integer;
      Roi1                : Cv_Rect;
      Roi2                : Cv_Rect;
      Displ2MaxDiff       : Integer;
      PreFilteredImg0     : Cv_Mat_P;
      PreFilteredImg1     : Cv_Mat_P;
      SlidingSumBuf       : Cv_Mat_P;
      Cost                : Cv_Mat_P;
      Disp                : Cv_Mat_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Stereo_BM_State);
   type Cv_Stereo_BM_State_P is access Cv_Stereo_BM_State;

   CV_STEREO_BM_BASIC               : constant := 0;
   CV_STEREO_BM_FISH_EYE            : constant := 1;
   CV_STEREO_BM_NARROW              : constant := 2;

   --     Creates block matching stereo correspondence structure.
   function CvCreateStereoBMState (Preset              : Integer;
                                   NumberOfDisparities : Integer)
                                   return Cv_Stereo_BM_State_P;
   pragma Import (C, CvCreateStereoBMState, "cvCreateStereoBMState");

   --     Releases block matching stereo correspondence structure
   procedure CvReleaseStereoBMState (State : access Cv_Stereo_BM_State_P);
   pragma Import (C, CvReleaseStereoBMState, "cvReleaseStereoBMState");

   --     Computes the disparity map using block matching algorithm.
   procedure CvFindStereoCorrespondenceBM (Left      : Cv_Arr_P;
                                           Right     : Cv_Arr_P;
                                           Disparity : Cv_Arr_P;
                                           State     : Cv_Stereo_BM_State_P);
   pragma Import (C, CvFindStereoCorrespondenceBM, "cvFindStereoCorrespondenceBM");

   function CvGetValidDisparityROI (Roi1                  : Cv_Rect;
                                    Roi2                  : Cv_Rect;
                                    Min_Disparity         : Integer;
                                    Number_Of_Disparities : Integer;
                                    SAD_Window_Size       : Integer)
                                    return Cv_Rect;
   pragma Import (C, CvGetValidDisparityROI, "cvGetValidDisparityROI");

   procedure CvValidateDisparity (Disparity             : Cv_Arr_P;
                                  Cost                  : Cv_Arr_P;
                                  Min_Disparity         : Integer;
                                  Number_Of_Disparities : Integer;
                                  Disp_12_Max_Diff      : Integer := 1);
   pragma Import (C, CvValidateDisparity, "cvValidateDisparity");

   CV_STEREO_GC_OCCLUDED : constant := 32767; -- SHRT_MAX from limits.h

   -- The structure for graph cuts-based stereo correspondence algorithm
   type Cv_Stereo_GC_State is record
      IThreshold                  : Integer;
      InteractionRadius           : Integer;
      K, Lambda, Lambda1, Lambda2 : Float;
      OcclusionCost               : Integer;
      MinDisparity                : Integer;
      NumberOfDisparities         : Integer;
      MaxIters                    : Integer;
      Left                        : Cv_MAt_P;
      Right                       : Cv_Mat_P;
      DispLeft                    : Cv_MAt_P;
      DispRight                   : Cv_Mat_P;
      PtrLeft                     : Cv_Mat_P;
      PtrRight                    : Cv_Mat_P;
      VtxBuf                      : Cv_Mat_P;
      EdgeBuf                     : Cv_Mat_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Stereo_GC_State);
   type Cv_Stereo_Gc_State_P is access Cv_Stereo_GC_State;

   --     Creates the state of graph cut-based stereo correspondence algorithm.
   function CvCreateStereoGCState (NumberOfDisparities : Integer;
                                   MaxIters            : Integer)
                                   return Cv_Stereo_GC_State_P;
   pragma Import (C, CvCreateStereoGCState, "cvCreateStereoGCState");

   --     Releases the state structure of the graph cut-based stereo
   --     correspondence algorithm.
   procedure CvReleaseStereoGCState (State : access Cv_Stereo_GC_State_P);
   pragma Import (C, CvReleaseStereoGCState, "cvReleaseStereoGCState");

   --     Computes the disparity map using graph cut-based algorithm.
   procedure CvFindStereoCorrespondenceGC (Left              : Cv_Arr_P;
                                           Right             : Cv_Arr_P;
                                           DispLeft          : Cv_Arr_P;
                                           DispRight         : Cv_Arr_P;
                                           State             : Cv_Stereo_GC_State_P;
                                           UseDisparityGuess : Integer := 0);
   pragma Import (C, CvFindStereoCorrespondenceGC, "cvFindStereoCorrespondenceGC");

   --     Reprojects disparity image to 3D space.
   procedure CvReprojectImageTo3D (Disparity           : Cv_Arr_P;
                                   Image3D             : Cv_Arr_P;
                                   Q                   : Cv_Mat_P;
                                   HandleMissingValues : Integer := 0);
   pragma Import (C, CvReprojectImageTo3D, "cvReprojectImageTo3D");

private
   procedure Nulled;
end Calib_3D;
