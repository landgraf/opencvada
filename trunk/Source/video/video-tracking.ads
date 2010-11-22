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
-- video-tracking.ads - video-tracking.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

with Core; use Core;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces; use Interfaces;
with Imgproc;

package Video.Tracking is
--


   -----------------------------------------------------------------------------
   -- Motion analysis
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Optical flow
   -----------------------------------------------------------------------------
   -- Calculates optical flow for 2 images using classical Lucas & Kanade algorithm
   procedure CvCalcOpticalFlowLK (Prev    : Cv_Arr_P;
                                  Curr    : Cv_Arr_P;
                                  WinSize : Cv_Size;
                                  Velx    : Cv_Arr_P;
                                  Vely    : Cv_Arr_P);

   -- Calculates the optical flow for two images by using the block matching method.
   procedure CvCalcOpticalFlowBM (Prev        : Cv_Arr_P;
                                  Curr        : Cv_Arr_P;
                                  BlockSize   : Cv_Size;
                                  ShiftSize   :  Cv_Size;
                                  MaxRange    : Cv_Size;
                                  UsePrevious : Integer;
                                  Velx        : Cv_Arr_P;
                                  Vely        : Cv_Arr_P);

   -- Calculates Optical flow for 2 images using Horn & Schunck algorithm
   procedure CvCalcOpticalFlowHS (Prev        : Cv_Arr_P;
                                  Curr        : Cv_Arr_P;
                                  UsePrevious : Integer;
                                  Velx        : Cv_Arr_P;
                                  Vely        : Cv_Arr_P;
                                  Lambda      : Long_Float;
                                  Criteria    : Cv_Term_Criteria);

   CV_LKFLOW_PYR_A_READY : constant := 1;
   CV_LKFLOW_PYR_B_READY : constant := 2;
   CV_LKFLOW_INITIAL_GUESSES : constant := 4;
   CV_LKFLOW_GET_MIN_EIGENVALS : constant := 8;

   -- It is Lucas & Kanade method, modified to use pyramids.
   -- Also it does several iterations to get optical flow for
   -- every point at every pyramid level.
   -- Calculates optical flow between two images for certain set of points (i.e.
   -- it is a "sparse" optical flow, which is opposite to the previous 3 methods)
   procedure CvCalcOpticalFlowPyrLK (Prev         : Cv_Arr_P;
                                     Curr         : Cv_Arr_P;
                                     PrevPyr      : Cv_Arr_P;
                                     CurrPyr      : Cv_Arr_P;
                                     PrevFeatures : Cv_Point_2D_32F_Array;
                                     CurrFeatures : Cv_Point_2D_32F_Array;
                                     Count        : Integer;
                                     WinSize      : Cv_Size;
                                     Level        : Integer;
                                     Status       : Cv_8u_Array;
                                     TrackError   : Cv_32F_Array;
                                     Citeria      : Cv_Term_Criteria;
                                     Flags        : Integer);

   -- Modification of a previous sparse optical flow algorithm to calculate
   -- affine flow
   procedure CvCalcAffineFlowPyrLK (Prev          : Cv_Arr_P;
                                    Curr          : Cv_Arr_P;
                                    Prev_Pyr      : Cv_Arr_P;
                                    Curr_Pyr      : Cv_Arr_P;
                                    Prev_Features : Cv_Point_2d_32f_Array;
                                    Curr_Features : Cv_Point_2d_32f_Array;
                                    Matrices      : Cv_32f_Array;
                                    Count         : Integer;
                                    Win_Size      : Cv_Size;
                                    Level         : Integer;
                                    Status        : Chars_Ptr;
                                    Track_Error   : Cv_32f_Array;
                                    Criteria      : Cv_Term_Criteria;
                                    Flags         : Unsigned_32);

   -- Estimate rigid transformation between 2 images or 2 point sets
   function CvEstimateRigidTransform (A           : Cv_Arr_P;
                                      B           : Cv_Arr_P;
                                      M           : Cv_Mat_P;
                                      Full_Affine : Integer)
                                      return Integer;

   -- Estimate optical flow for each pixel using the two-frame G. Farneback algorithm
   procedure CvCalcOpticalFlowFarneback (Prev       : Cv_Arr_P;
                                         Next       : Cv_Arr_P;
                                         Flow       : Cv_Arr_P;
                                         Pyr_Scale  : Long_Float;
                                         Levels     : Integer;
                                         Winsize    : Integer;
                                         Iterations : Integer;
                                         Poly_N     : Integer;
                                         Poly_Sigma : Long_Float;
                                         Flags      : Unsigned_32);

   -----------------------------------------------------------------------------
   -- motion templates
   -----------------------------------------------------------------------------
   -- All the motion template functions work only with single channel images.
   -- Silhouette image must have depth IPL_DEPTH_8U or IPL_DEPTH_8S
   -- Motion history image must have depth IPL_DEPTH_32F,
   -- Gradient mask - IPL_DEPTH_8U or IPL_DEPTH_8S,
   -- Motion orientation image - IPL_DEPTH_32F
   -- Segmentation mask - IPL_DEPTH_32F
   -- all the angles are in degrees, all the times are in milliseconds

   -- Updates the motion history image by a moving silhouette.
   procedure CvUpdateMotionHistory (Silhouette : Cv_Arr_P;
                                    Mhi        : Cv_Arr_P;
                                    Timestamp  : Long_Float;
                                    Duration   : Long_Float);

   -- Calculates gradient of the motion history image and fills
   -- a mask indicating where the gradient is valid
   procedure CvCalcMotionGradient (Mhi          : Cv_Arr_P;
                                   Mask         : Cv_Arr_P;
                                   Orientation  : Cv_Arr_P;
                                   Delta1       : Long_Float;
                                   Delta2       : Long_Float;
                                   ApertureSize : Integer := 3);

   -- Calculates average motion direction within a selected motion region
   -- (region can be selected by setting ROIs and/or by composing a valid gradient mask
   -- with the region mask)
   function CvCalcGlobalOrientation (Orientation : Cv_Arr_P;
                                     Mask        : Cv_Arr_P;
                                     Mhi         : Cv_Arr_P;
                                     Timemstamp  : Long_Float;
                                     Duration    : Long_Float) return Long_Float;

   -- Splits a motion history image into a few parts corresponding to separate independent motions
   -- (e.g. left hand, right hand)
   function CvSegmentMotion (Mhi       : Cv_Arr_P;
                             SegMask   : Cv_Arr_P;
                             Storage   : Cv_Mem_Storage_P;
                             Timestamp : Long_Float;
                             SegThresh : Long_Float) return Cv_Seq_P;

   -----------------------------------------------------------------------------
   -- Tracking
   -----------------------------------------------------------------------------

   -- Finds the object center, size, and orientation.
   function CvCamShift (ProbImage : Cv_Arr_P;
                        Window    : Cv_Rect;
                        Criteria  : Cv_Term_Criteria;
                        Comp      : Imgproc.Cv_Connected_Comp_P;
                        Box       : access Cv_Box_2D := null) return Integer;

   -- Finds the object center on back projection.
   function CvMeanShift (ProbImage : Cv_Arr_P;
                         Window    : Cv_Rect;
                         Criteria  : Cv_Term_Criteria;
                         Comp      : Imgproc.Cv_Connected_Comp_P) return Integer;

   -- Standard Kalman filter (in G. Welch' and G. Bishop's notation):
   -- x(k)=A*x(k-1)+B*u(k)+w(k)  p(w)~N(0,Q)
   -- z(k)=H*x(k)+v(k),   p(v)~N(0,R)
   type Cv_Kalman is
      record
         MP                     : Integer;
         DP                     : Integer;
         CP                     : Integer;
         --/* backward compatibility fields
         PosterState            : Cv_32F_Array_P;
         PriorState             : Cv_32F_Array_P;
         DynamMatr              : Cv_32F_Array_P;
         MeasurementMatr        : Cv_32F_Array_P;
         MNCovariance           : Cv_32F_Array_P;
         PNCovariance           : Cv_32F_Array_P;
         KalmGainMatr           : Cv_32F_Array_P;
         PriorErrorCovariance   : Cv_32F_Array_P;
         PosterErrorCovariance  : Cv_32F_Array_P;
         Temp_1                 : Cv_32F_Array_P;
         Temp_2                 : Cv_32F_Array_P;
         --*/
         StatePre               : Cv_Mat_P;
         StatePost              : Cv_Mat_P;
         TransitionMatrix       : Cv_Mat_P;
         ControlMatrix          : Cv_Mat_P;
         MeasurementMatrix      : Cv_Mat_P;
         ProcessNoiseCov        : Cv_Mat_P;
         ErrorCovPre            : Cv_Mat_P;
         Gain                   : Cv_Mat_P;
         ErrorCovPost           : Cv_Mat_P;

         Temp1                  : Cv_Mat_P;
         Temp2                  : Cv_Mat_P;
         Temp3                  : Cv_Mat_P;
         Temp4                  : Cv_Mat_P;
         Temp5                  : Cv_Mat_P;
      end record;
   type Cv_Kalman_P is access Cv_Kalman;

   -- Creates Kalman filter and sets A, B, Q, R and state to some initial values
   function CvCreateKalman (DynamParams   : Integer;
                            MeasureParams : Integer;
                            ControlParams : Integer := 0) return Cv_Kalman_P;

   -- Releases Kalman filter state
   procedure CvReleaseKalman (Kalman : access Cv_Kalman_P);

   -- Updates Kalman filter by time (predicts future state of the system)
   function CvKalmanPredict (Kalman  : Cv_Kalman_P;
                             Control : Cv_Mat_P := null) return Cv_Mat_P;

   -- Updates Kalman filter by measurement
   -- (corrects state of the system and internal matrices)
   function CvKalmanCorrect (Kalman      : Cv_Kalman_P;
                             Measurement : Cv_Mat_P) return Cv_Mat_P;

   -- Synonym for CvKalmanPredict
   function CvKalmanUpdateByTime (Kalman  : Cv_Kalman_P;
                                  Control : Cv_Mat_P := null) return Cv_Mat_P renames CvKalmanPredict;

   -- Synonym for CvKalmanCorrect
   function CvKalmanUpdateByMeasurement (Kalman      : Cv_Kalman_P;
                                         Measurement : Cv_Mat_P) return Cv_Mat_P renames CvKalmanCorrect;

private
   procedure Nulled;

   pragma Import (C, CvCalcOpticalFlowLK, "cvCalcOpticalFlowLK");
   pragma Import (C, CvCalcOpticalFlowBM, "cvCalcOpticalFlowBM");
   pragma Import (C, CvCalcOpticalFlowHS, "cvCalcOpticalFlowHS");
   pragma Import (C, CvCalcOpticalFlowPyrLK, "cvCalcOpticalFlowPyrLK");
   pragma Import (C, CvCalcAffineFlowPyrLK, "cvCalcAffineFlowPyrLK");
   pragma Import (C, CvEstimateRigidTransform, "cvEstimateRigidTransform");
   pragma Import (C, CvCalcOpticalFlowFarneback, "cvCalcOpticalFlowFarneback");

   pragma Import (C, CvUpdateMotionHistory, "cvUpdateMotionHistory");
   pragma Import (C, CvCalcMotionGradient, "cvCalcMotionGradient");
   pragma Import (C, CvCalcGlobalOrientation, "cvCalcGlobalOrientation");
   pragma Import (C, CvSegmentMotion, "cvSegmentMotion");

   pragma Import (C, CvCamShift, "cvCamShift");
   pragma Import (C, CvMeanShift, "cvMeanShift");
   pragma Import (C, CvCreateKalman, "cvCreateKalman");
   pragma Import (C, CvReleaseKalman, "cvReleaseKalman");
   pragma Import (C, CvKalmanPredict, "cvKalmanPredict");
   pragma Import (C, CvKalmanCorrect, "cvKalmanCorrect");
end Video.Tracking;
