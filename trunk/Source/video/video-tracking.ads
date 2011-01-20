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
   procedure Cv_Calc_Optical_Flow_Lk (Prev    : Cv_Arr_P;
                                      Curr    : Cv_Arr_P;
                                      Winsize : Cv_Size;
                                      Velx    : Cv_Arr_P;
                                      Vely    : Cv_Arr_P);

   -- Calculates the optical flow for two images by using the block matching method.
   procedure Cv_Calc_Optical_Flow_Bm (Prev        : Cv_Arr_P;
                                      Curr        : Cv_Arr_P;
                                      Blocksize   : Cv_Size;
                                      Shiftsize   :  Cv_Size;
                                      Maxrange    : Cv_Size;
                                      Useprevious : Integer;
                                      Velx        : Cv_Arr_P;
                                      Vely        : Cv_Arr_P);

   -- Calculates Optical flow for 2 images using Horn & Schunck algorithm
   procedure Cv_Calc_Optical_Flow_Hs (Prev        : Cv_Arr_P;
                                      Curr        : Cv_Arr_P;
                                      Useprevious : Integer;
                                      Velx        : Cv_Arr_P;
                                      Vely        : Cv_Arr_P;
                                      Lambda      : Long_Float;
                                      Criteria    : Cv_Term_Criteria);

   Cv_Lkflow_Pyr_A_Ready : constant := 1;
   Cv_Lkflow_Pyr_B_Ready : constant := 2;
   Cv_Lkflow_Initial_Guesses : constant := 4;
   Cv_Lkflow_Get_Min_Eigenvals : constant := 8;

   -- It is Lucas & Kanade method, modified to use pyramids.
   -- Also it does several iterations to get optical flow for
   -- every point at every pyramid level.
   -- Calculates optical flow between two images for certain set of points (i.e.
   -- it is a "sparse" optical flow, which is opposite to the previous 3 methods)
   procedure Cv_Calc_Optical_Flow_Pyr_Lk (Prev         : Cv_Arr_P;
                                          Curr         : Cv_Arr_P;
                                          Prevpyr      : Cv_Arr_P;
                                          Currpyr      : Cv_Arr_P;
                                          Prevfeatures : Cv_Point_2d_32f_Array;
                                          Currfeatures : Cv_Point_2d_32f_Array;
                                          Count        : Integer;
                                          Winsize      : Cv_Size;
                                          Level        : Integer;
                                          Status       : Cv_8u_Array;
                                          Trackerror   : Cv_32f_Array;
                                          Citeria      : Cv_Term_Criteria;
                                          Flags        : Integer);

   -- Modification of a previous sparse optical flow algorithm to calculate
   -- affine flow
   procedure Cv_Calc_Affine_Flow_Pyr_Lk (Prev          : Cv_Arr_P;
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
   function Cv_Estimate_Rigid_Transform (A           : Cv_Arr_P;
                                         B           : Cv_Arr_P;
                                         M           : Cv_Mat_P;
                                         Full_Affine : Integer)
                                         return Integer;

   -- Estimate optical flow for each pixel using the two-frame G. Farneback algorithm
   procedure Cv_Calc_Optical_Flow_Farneback (Prev       : Cv_Arr_P;
                                             Next       : Cv_Arr_P;
                                             Flow       : Cv_Arr_P;
                                             Pyr_Scale  : Long_Float;
                                             Levels     : Integer;
                                             Winsize    : Integer;
                                             Iterations : Integer;
                                             Poly_N     : Integer;
                                             Poly_Sigma : Long_Float;
                                             Flags      : Integer);

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
   procedure Cv_Update_Motion_History (Silhouette : Cv_Arr_P;
                                       Mhi        : Cv_Arr_P;
                                       Timestamp  : Long_Float;
                                       Duration   : Long_Float);

   -- Calculates gradient of the motion history image and fills
   -- a mask indicating where the gradient is valid
   procedure Cv_Calc_Motion_Gradient (Mhi          : Cv_Arr_P;
                                      Mask         : Cv_Arr_P;
                                      Orientation  : Cv_Arr_P;
                                      Delta1       : Long_Float;
                                      Delta2       : Long_Float;
                                      Aperturesize : Integer := 3);

   -- Calculates average motion direction within a selected motion region
   -- (region can be selected by setting ROIs and/or by composing a valid gradient mask
   -- with the region mask)
   function Cv_Calc_Global_Orientation (Orientation : Cv_Arr_P;
                                        Mask        : Cv_Arr_P;
                                        Mhi         : Cv_Arr_P;
                                        Timemstamp  : Long_Float;
                                        Duration    : Long_Float) return Long_Float;

   -- Splits a motion history image into a few parts corresponding to separate independent motions
   -- (e.g. left hand, right hand)
   function Cv_Segment_Motion (Mhi       : Cv_Arr_P;
                               Segmask   : Cv_Arr_P;
                               Storage   : Cv_Mem_Storage_P;
                               Timestamp : Long_Float;
                               Segthresh : Long_Float) return Cv_Seq_P;

   -----------------------------------------------------------------------------
   -- Tracking
   -----------------------------------------------------------------------------

   -- Finds the object center, size, and orientation.
   function Cv_Cam_Shift (Probimage : Cv_Arr_P;
                          Window    : Cv_Rect;
                          Criteria  : Cv_Term_Criteria;
                          Comp      : Imgproc.Cv_Connected_Comp_P;
                          Box       : access Cv_Box_2d := null) return Integer;

   -- Finds the object center on back projection.
   function Cv_Mean_Shift (Probimage : Cv_Arr_P;
                           Window    : Cv_Rect;
                           Criteria  : Cv_Term_Criteria;
                           Comp      : Imgproc.Cv_Connected_Comp_P) return Integer;

   -- Standard Kalman filter (in G. Welch' and G. Bishop's notation):
   -- x(k)=A*x(k-1)+B*u(k)+w(k)  p(w)~N(0,Q)
   -- z(k)=H*x(k)+v(k),   p(v)~N(0,R)
   type Cv_Kalman is
      record
         Mp                     : Integer;
         Dp                     : Integer;
         Cp                     : Integer;
         --/* backward compatibility fields
         Posterstate            : Cv_32f_Array_P;
         Priorstate             : Cv_32f_Array_P;
         Dynammatr              : Cv_32f_Array_P;
         Measurementmatr        : Cv_32f_Array_P;
         Mncovariance           : Cv_32f_Array_P;
         Pncovariance           : Cv_32f_Array_P;
         Kalmgainmatr           : Cv_32f_Array_P;
         Priorerrorcovariance   : Cv_32f_Array_P;
         Postererrorcovariance  : Cv_32f_Array_P;
         Temp_1                 : Cv_32f_Array_P;
         Temp_2                 : Cv_32f_Array_P;
         --*/
         Statepre               : Cv_Mat_P;
         Statepost              : Cv_Mat_P;
         Transitionmatrix       : Cv_Mat_P;
         Controlmatrix          : Cv_Mat_P;
         Measurementmatrix      : Cv_Mat_P;
         Processnoisecov        : Cv_Mat_P;
         Errorcovpre            : Cv_Mat_P;
         Gain                   : Cv_Mat_P;
         Errorcovpost           : Cv_Mat_P;

         Temp1                  : Cv_Mat_P;
         Temp2                  : Cv_Mat_P;
         Temp3                  : Cv_Mat_P;
         Temp4                  : Cv_Mat_P;
         Temp5                  : Cv_Mat_P;
      end record;
   type Cv_Kalman_P is access Cv_Kalman;

   -- Creates Kalman filter and sets A, B, Q, R and state to some initial values
   function Cv_Create_Kalman (Dynamparams   : Integer;
                              Measureparams : Integer;
                              Controlparams : Integer := 0) return Cv_Kalman_P;

   -- Releases Kalman filter state
   procedure Cv_Release_Kalman (Kalman : access Cv_Kalman_P);

   -- Updates Kalman filter by time (predicts future state of the system)
   function Cv_Kalman_Predict (Kalman  : Cv_Kalman_P;
                               Control : Cv_Mat_P := null) return Cv_Mat_P;

   -- Updates Kalman filter by measurement
   -- (corrects state of the system and internal matrices)
   function Cv_Kalman_Correct (Kalman      : Cv_Kalman_P;
                               Measurement : Cv_Mat_P) return Cv_Mat_P;

   -- Synonym for CvKalmanPredict
   function Cv_Kalman_Update_By_Time (Kalman  : Cv_Kalman_P;
                                      Control : Cv_Mat_P := null) return Cv_Mat_P renames Cv_Kalman_Predict;

   -- Synonym for CvKalmanCorrect
   function Cv_Kalman_Update_By_Measurement (Kalman      : Cv_Kalman_P;
                                             Measurement : Cv_Mat_P) return Cv_Mat_P renames Cv_Kalman_Correct;

private

   pragma Import (C, Cv_Calc_Optical_Flow_Lk, "cvCalcOpticalFlowLK");
   pragma Import (C, Cv_Calc_Optical_Flow_Bm, "cvCalcOpticalFlowBM");
   pragma Import (C, Cv_Calc_Optical_Flow_Hs, "cvCalcOpticalFlowHS");
   pragma Import (C, Cv_Calc_Optical_Flow_Pyr_Lk, "cvCalcOpticalFlowPyrLK");
   pragma Import (C, Cv_Calc_Affine_Flow_Pyr_Lk, "cvCalcAffineFlowPyrLK");
   pragma Import (C, Cv_Estimate_Rigid_Transform, "cvEstimateRigidTransform");
   pragma Import (C, Cv_Calc_Optical_Flow_Farneback, "cvCalcOpticalFlowFarneback");

   pragma Import (C, Cv_Update_Motion_History, "cvUpdateMotionHistory");
   pragma Import (C, Cv_Calc_Motion_Gradient, "cvCalcMotionGradient");
   pragma Import (C, Cv_Calc_Global_Orientation, "cvCalcGlobalOrientation");
   pragma Import (C, Cv_Segment_Motion, "cvSegmentMotion");

   pragma Import (C, Cv_Cam_Shift, "cvCamShift");
   pragma Import (C, Cv_Mean_Shift, "cvMeanShift");
   pragma Import (C, Cv_Create_Kalman, "cvCreateKalman");
   pragma Import (C, Cv_Release_Kalman, "cvReleaseKalman");
   pragma Import (C, Cv_Kalman_Predict, "cvKalmanPredict");
   pragma Import (C, Cv_Kalman_Correct, "cvKalmanCorrect");
end Video.Tracking;
