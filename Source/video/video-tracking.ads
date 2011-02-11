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
   procedure Cv_Calc_Optical_Flow_Lk (Prev    : Cv_Arr_Ptr;
                                      Curr    : Cv_Arr_Ptr;
                                      Winsize : Cv_Size;
                                      Velx    : Cv_Arr_Ptr;
                                      Vely    : Cv_Arr_Ptr);
   procedure Cv_Calc_Optical_Flow_Lk (Prev    : Cv_Mat_Ptr;
                                      Curr    : Cv_Mat_Ptr;
                                      Winsize : Cv_Size;
                                      Velx    : Cv_Mat_Ptr;
                                      Vely    : Cv_Mat_Ptr);
   procedure Cv_Calc_Optical_Flow_Lk (Prev    : Ipl_Image_Ptr;
                                      Curr    : Ipl_Image_Ptr;
                                      Winsize : Cv_Size;
                                      Velx    : Ipl_Image_Ptr;
                                      Vely    : Ipl_Image_Ptr);

   -- Calculates the optical flow for two images by using the block matching method.
   procedure Cv_Calc_Optical_Flow_Bm (Prev        : Cv_Arr_Ptr;
                                      Curr        : Cv_Arr_Ptr;
                                      Blocksize   : Cv_Size;
                                      Shiftsize   :  Cv_Size;
                                      Maxrange    : Cv_Size;
                                      Useprevious : Integer;
                                      Velx        : Cv_Arr_Ptr;
                                      Vely        : Cv_Arr_Ptr);
   procedure Cv_Calc_Optical_Flow_Bm (Prev        : Cv_Mat_Ptr;
                                      Curr        : Cv_Mat_Ptr;
                                      Blocksize   : Cv_Size;
                                      Shiftsize   :  Cv_Size;
                                      Maxrange    : Cv_Size;
                                      Useprevious : Integer;
                                      Velx        : Cv_Mat_Ptr;
                                      Vely        : Cv_Mat_Ptr);
   procedure Cv_Calc_Optical_Flow_Bm (Prev        : Ipl_Image_Ptr;
                                      Curr        : Ipl_Image_Ptr;
                                      Blocksize   : Cv_Size;
                                      Shiftsize   :  Cv_Size;
                                      Maxrange    : Cv_Size;
                                      Useprevious : Integer;
                                      Velx        : Ipl_Image_Ptr;
                                      Vely        : Ipl_Image_Ptr);


   -- Calculates Optical flow for 2 images using Horn & Schunck algorithm
   procedure Cv_Calc_Optical_Flow_Hs (Prev        : Cv_Arr_Ptr;
                                      Curr        : Cv_Arr_Ptr;
                                      Useprevious : Integer;
                                      Velx        : Cv_Arr_Ptr;
                                      Vely        : Cv_Arr_Ptr;
                                      Lambda      : Long_Float;
                                      Criteria    : Cv_Term_Criteria);
   procedure Cv_Calc_Optical_Flow_Hs (Prev        : Cv_Mat_Ptr;
                                      Curr        : Cv_Mat_Ptr;
                                      Useprevious : Integer;
                                      Velx        : Cv_Mat_Ptr;
                                      Vely        : Cv_Mat_Ptr;
                                      Lambda      : Long_Float;
                                      Criteria    : Cv_Term_Criteria);
   procedure Cv_Calc_Optical_Flow_Hs (Prev        : Ipl_Image_Ptr;
                                      Curr        : Ipl_Image_Ptr;
                                      Useprevious : Integer;
                                      Velx        : Ipl_Image_Ptr;
                                      Vely        : Ipl_Image_Ptr;
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
   procedure Cv_Calc_Optical_Flow_Pyr_Lk (Prev         : Cv_Arr_Ptr;
                                          Curr         : Cv_Arr_Ptr;
                                          Prevpyr      : Cv_Arr_Ptr;
                                          Currpyr      : Cv_Arr_Ptr;
                                          Prevfeatures : Cv_Point_2d_32f_Array;
                                          Currfeatures : Cv_Point_2d_32f_Array;
                                          Count        : Integer;
                                          Winsize      : Cv_Size;
                                          Level        : Integer;
                                          Status       : Cv_8u_Array;
                                          Trackerror   : Cv_32f_Array;
                                          Citeria      : Cv_Term_Criteria;
                                          Flags        : Integer);
   procedure Cv_Calc_Optical_Flow_Pyr_Lk (Prev         : Cv_Mat_Ptr;
                                          Curr         : Cv_Mat_Ptr;
                                          Prevpyr      : Cv_Mat_Ptr;
                                          Currpyr      : Cv_Mat_Ptr;
                                          Prevfeatures : Cv_Point_2d_32f_Array;
                                          Currfeatures : Cv_Point_2d_32f_Array;
                                          Count        : Integer;
                                          Winsize      : Cv_Size;
                                          Level        : Integer;
                                          Status       : Cv_8u_Array;
                                          Trackerror   : Cv_32f_Array;
                                          Citeria      : Cv_Term_Criteria;
                                          Flags        : Integer);
   procedure Cv_Calc_Optical_Flow_Pyr_Lk (Prev         : Ipl_Image_Ptr;
                                          Curr         : Ipl_Image_Ptr;
                                          Prevpyr      : Ipl_Image_Ptr;
                                          Currpyr      : Ipl_Image_Ptr;
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
   procedure Cv_Calc_Affine_Flow_Pyr_Lk (Prev          : Cv_Arr_Ptr;
                                         Curr          : Cv_Arr_Ptr;
                                         Prev_Pyr      : Cv_Arr_Ptr;
                                         Curr_Pyr      : Cv_Arr_Ptr;
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
   procedure Cv_Calc_Affine_Flow_Pyr_Lk (Prev          : Cv_Mat_Ptr;
                                         Curr          : Cv_Mat_Ptr;
                                         Prev_Pyr      : Cv_Mat_Ptr;
                                         Curr_Pyr      : Cv_Mat_Ptr;
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
   procedure Cv_Calc_Affine_Flow_Pyr_Lk (Prev          : Ipl_Image_Ptr;
                                         Curr          : Ipl_Image_Ptr;
                                         Prev_Pyr      : Ipl_Image_Ptr;
                                         Curr_Pyr      : Ipl_Image_Ptr;
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
   function Cv_Estimate_Rigid_Transform (A           : Cv_Arr_Ptr;
                                         B           : Cv_Arr_Ptr;
                                         M           : Cv_Mat_Ptr;
                                         Full_Affine : Integer)
                                         return Integer;
   function Cv_Estimate_Rigid_Transform (A           : Cv_Mat_Ptr;
                                         B           : Cv_Mat_Ptr;
                                         M           : Cv_Mat_Ptr;
                                         Full_Affine : Integer)
                                         return Integer;
   function Cv_Estimate_Rigid_Transform (A           : Ipl_Image_Ptr;
                                         B           : Ipl_Image_Ptr;
                                         M           : Cv_Mat_Ptr;
                                         Full_Affine : Integer)
                                         return Integer;

   -- Estimate optical flow for each pixel using the two-frame G. Farneback algorithm
   procedure Cv_Calc_Optical_Flow_Farneback (Prev       : Cv_Arr_Ptr;
                                             Next       : Cv_Arr_Ptr;
                                             Flow       : Cv_Arr_Ptr;
                                             Pyr_Scale  : Long_Float;
                                             Levels     : Integer;
                                             Winsize    : Integer;
                                             Iterations : Integer;
                                             Poly_N     : Integer;
                                             Poly_Sigma : Long_Float;
                                             Flags      : Integer);
   procedure Cv_Calc_Optical_Flow_Farneback (Prev       : Cv_Mat_Ptr;
                                             Next       : Cv_Mat_Ptr;
                                             Flow       : Cv_Mat_Ptr;
                                             Pyr_Scale  : Long_Float;
                                             Levels     : Integer;
                                             Winsize    : Integer;
                                             Iterations : Integer;
                                             Poly_N     : Integer;
                                             Poly_Sigma : Long_Float;
                                             Flags      : Integer);
   procedure Cv_Calc_Optical_Flow_Farneback (Prev       : Ipl_Image_Ptr;
                                             Next       : Ipl_Image_Ptr;
                                             Flow       : Ipl_Image_Ptr;
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
   procedure Cv_Update_Motion_History (Silhouette : Cv_Arr_Ptr;
                                       Mhi        : Cv_Arr_Ptr;
                                       Timestamp  : Long_Float;
                                       Duration   : Long_Float);
   procedure Cv_Update_Motion_History (Silhouette : Cv_Mat_Ptr;
                                       Mhi        : Cv_Mat_Ptr;
                                       Timestamp  : Long_Float;
                                       Duration   : Long_Float);
   procedure Cv_Update_Motion_History (Silhouette : Ipl_Image_Ptr;
                                       Mhi        : Ipl_Image_Ptr;
                                       Timestamp  : Long_Float;
                                       Duration   : Long_Float);

   -- Calculates gradient of the motion history image and fills
   -- a mask indicating where the gradient is valid
   procedure Cv_Calc_Motion_Gradient (Mhi          : Cv_Arr_Ptr;
                                      Mask         : Cv_Arr_Ptr;
                                      Orientation  : Cv_Arr_Ptr;
                                      Delta1       : Long_Float;
                                      Delta2       : Long_Float;
                                      Aperturesize : Integer := 3);
   procedure Cv_Calc_Motion_Gradient (Mhi          : Cv_Mat_Ptr;
                                      Mask         : Cv_Mat_Ptr;
                                      Orientation  : Cv_Mat_Ptr;
                                      Delta1       : Long_Float;
                                      Delta2       : Long_Float;
                                      Aperturesize : Integer := 3);
   procedure Cv_Calc_Motion_Gradient (Mhi          : Ipl_Image_Ptr;
                                      Mask         : Ipl_Image_Ptr;
                                      Orientation  : Ipl_Image_Ptr;
                                      Delta1       : Long_Float;
                                      Delta2       : Long_Float;
                                      Aperturesize : Integer := 3);

   -- Calculates average motion direction within a selected motion region
   -- (region can be selected by setting ROIs and/or by composing a valid gradient mask
   -- with the region mask)
   function Cv_Calc_Global_Orientation (Orientation : Cv_Arr_Ptr;
                                        Mask        : Cv_Arr_Ptr;
                                        Mhi         : Cv_Arr_Ptr;
                                        Timemstamp  : Long_Float;
                                        Duration    : Long_Float) return Long_Float;
   function Cv_Calc_Global_Orientation (Orientation : Cv_Mat_Ptr;
                                        Mask        : Cv_Mat_Ptr;
                                        Mhi         : Cv_Mat_Ptr;
                                        Timemstamp  : Long_Float;
                                        Duration    : Long_Float) return Long_Float;
   function Cv_Calc_Global_Orientation (Orientation : Ipl_Image_Ptr;
                                        Mask        : Ipl_Image_Ptr;
                                        Mhi         : Ipl_Image_Ptr;
                                        Timemstamp  : Long_Float;
                                        Duration    : Long_Float) return Long_Float;

   -- Splits a motion history image into a few parts corresponding to separate independent motions
   -- (e.g. left hand, right hand)
   function Cv_Segment_Motion (Mhi       : Cv_Arr_Ptr;
                               Segmask   : Cv_Arr_Ptr;
                               Storage   : Cv_Mem_Storage_Ptr;
                               Timestamp : Long_Float;
                               Segthresh : Long_Float) return Cv_Seq_Ptr;
   function Cv_Segment_Motion (Mhi       : Cv_Mat_Ptr;
                               Segmask   : Cv_Mat_Ptr;
                               Storage   : Cv_Mem_Storage_Ptr;
                               Timestamp : Long_Float;
                               Segthresh : Long_Float) return Cv_Seq_Ptr;
   function Cv_Segment_Motion (Mhi       : Ipl_Image_Ptr;
                               Segmask   : Ipl_Image_Ptr;
                               Storage   : Cv_Mem_Storage_Ptr;
                               Timestamp : Long_Float;
                               Segthresh : Long_Float) return Cv_Seq_Ptr;

   -----------------------------------------------------------------------------
   -- Tracking
   -----------------------------------------------------------------------------

   -- Finds the object center, size, and orientation.
   function Cv_Cam_Shift (Probimage : Cv_Arr_Ptr;
                          Window    : Cv_Rect;
                          Criteria  : Cv_Term_Criteria;
                          Comp      : Imgproc.Cv_Connected_Comp_Ptr;
                          Box       : access Cv_Box_2d := null) return Integer;
   function Cv_Cam_Shift (Probimage : Cv_Mat_Ptr;
                          Window    : Cv_Rect;
                          Criteria  : Cv_Term_Criteria;
                          Comp      : Imgproc.Cv_Connected_Comp_Ptr;
                          Box       : access Cv_Box_2d := null) return Integer;
   function Cv_Cam_Shift (Probimage : Ipl_Image_Ptr;
                          Window    : Cv_Rect;
                          Criteria  : Cv_Term_Criteria;
                          Comp      : Imgproc.Cv_Connected_Comp_Ptr;
                          Box       : access Cv_Box_2d := null) return Integer;

   -- Finds the object center on back projection.
   function Cv_Mean_Shift (Probimage : Cv_Arr_Ptr;
                           Window    : Cv_Rect;
                           Criteria  : Cv_Term_Criteria;
                           Comp      : Imgproc.Cv_Connected_Comp_Ptr) return Integer;
   function Cv_Mean_Shift (Probimage : Cv_Mat_Ptr;
                           Window    : Cv_Rect;
                           Criteria  : Cv_Term_Criteria;
                           Comp      : Imgproc.Cv_Connected_Comp_Ptr) return Integer;
   function Cv_Mean_Shift (Probimage : Ipl_Image_Ptr;
                           Window    : Cv_Rect;
                           Criteria  : Cv_Term_Criteria;
                           Comp      : Imgproc.Cv_Connected_Comp_Ptr) return Integer;

   -- Standard Kalman filter (in G. Welch' and G. Bishop's notation):
   -- x(k)=A*x(k-1)+B*u(k)+w(k)  p(w)~N(0,Q)
   -- z(k)=H*x(k)+v(k),   p(v)~N(0,R)
   type Cv_Kalman is
      record
         Mp                     : Integer;
         Dp                     : Integer;
         Cp                     : Integer;
         --/* backward compatibility fields
         Posterstate            : Cv_32f_Array_Ptr;
         Priorstate             : Cv_32f_Array_Ptr;
         Dynammatr              : Cv_32f_Array_Ptr;
         Measurementmatr        : Cv_32f_Array_Ptr;
         Mncovariance           : Cv_32f_Array_Ptr;
         Pncovariance           : Cv_32f_Array_Ptr;
         Kalmgainmatr           : Cv_32f_Array_Ptr;
         Priorerrorcovariance   : Cv_32f_Array_Ptr;
         Postererrorcovariance  : Cv_32f_Array_Ptr;
         Temp_1                 : Cv_32f_Array_Ptr;
         Temp_2                 : Cv_32f_Array_Ptr;
         --*/
         Statepre               : Cv_Mat_Ptr;
         Statepost              : Cv_Mat_Ptr;
         Transitionmatrix       : Cv_Mat_Ptr;
         Controlmatrix          : Cv_Mat_Ptr;
         Measurementmatrix      : Cv_Mat_Ptr;
         Processnoisecov        : Cv_Mat_Ptr;
         Errorcovpre            : Cv_Mat_Ptr;
         Gain                   : Cv_Mat_Ptr;
         Errorcovpost           : Cv_Mat_Ptr;

         Temp1                  : Cv_Mat_Ptr;
         Temp2                  : Cv_Mat_Ptr;
         Temp3                  : Cv_Mat_Ptr;
         Temp4                  : Cv_Mat_Ptr;
         Temp5                  : Cv_Mat_Ptr;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Kalman);
   type Cv_Kalman_Ptr is access all Cv_Kalman;

   -- Creates Kalman filter and sets A, B, Q, R and state to some initial values
   function Cv_Create_Kalman (Dynamparams   : Integer;
                              Measureparams : Integer;
                              Controlparams : Integer := 0) return Cv_Kalman_Ptr;

   -- Releases Kalman filter state
   procedure Cv_Release_Kalman (Kalman : access Cv_Kalman_Ptr);

   -- Updates Kalman filter by time (predicts future state of the system)
   function Cv_Kalman_Predict (Kalman  : Cv_Kalman_Ptr;
                               Control : Cv_Mat_Ptr := null) return Cv_Mat_Ptr;

   -- Updates Kalman filter by measurement
   -- (corrects state of the system and internal matrices)
   function Cv_Kalman_Correct (Kalman      : Cv_Kalman_Ptr;
                               Measurement : Cv_Mat_Ptr) return Cv_Mat_Ptr;

   -- Synonym for CvKalmanPredict
   function Cv_Kalman_Update_By_Time (Kalman  : Cv_Kalman_Ptr;
                                      Control : Cv_Mat_Ptr := null) return Cv_Mat_Ptr renames Cv_Kalman_Predict;

   -- Synonym for CvKalmanCorrect
   function Cv_Kalman_Update_By_Measurement (Kalman      : Cv_Kalman_Ptr;
                                             Measurement : Cv_Mat_Ptr) return Cv_Mat_Ptr renames Cv_Kalman_Correct;

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
