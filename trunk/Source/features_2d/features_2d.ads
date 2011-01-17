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
-- features_2d.ads - features_2d.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------
with Core; use Core;
package Features_2D is
--


   -- Use with CvExtractSURF
   -- This is what the Cv_Seq_P will contain.
   type Cv_SURF_Point is
      record
         Pt        : Cv_Point_2D_32F;
         Laplacian : Integer;
         Size      : Integer;
         Dir       : Float;
         Hessian   : Float;
      end record;
   type Cv_SURF_Point_P is access Cv_SURF_Point;

   function CvSURFPoint (Pt        : Cv_Point_2d_32f;
                         Laplacian : Integer;
                         Size      : Integer;
                         Dir       : Float := 0.0;
                         Hessian   : Float := 0.0)
                         return Cv_SURF_Point;

   --
   type Cv_SURF_Params is
      record
         Extended         : Integer;
         HessianThreshold : Long_Float;
         NOctaves         : Integer;
         NOctaveLayers    : Integer;
      end record;

   --              // returns default parameters
   function CvSURFParams (Threshold : Long_Float;
                          Extended  : Integer := 0) return Cv_SURF_Params;

   -- Extracts Speeded Up Robust Features from an image.
   procedure CvExtractSURF (Image       : Cv_Arr_P;
                            Mask        : Cv_Arr_P;
                            Keypoints   : access Cv_Seq_P;
                            Descriptors : access Cv_Seq_P;
                            Storage     : Cv_Mem_Storage_P;
                            Params      : Cv_SURF_Params);

   type Cv_MSER_Params is record
      Delta_Val      : Integer;
      Max_Area       : Integer;
      Min_Area       : Integer;
      Max_Variation  : Float;
      Min_Diversity  : Float;
      Max_Evolution  : Integer;
      Area_Threshold : Long_Float;
      Min_Margin     : Long_Float;
      Edge_Blur_Size : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_MSER_Params);
   type Cv_MSER_Params_P is access Cv_MSER_Params;

   function CvMSERParams (Delta_Val      : Integer := 5;
                          Min_Area       : Integer := 60;
                          Max_Area       : Integer := 14400;
                          Max_Variation  : Float := 0.25;
                          Min_Diversity  : Float := 0.2;
                          Max_Evolution  : Integer := 200;
                          Area_Threshold : Long_Float := 1.01;
                          Min_Margin     : Long_Float := 0.003;
                          Edge_Blur_Size : Integer := 5)
                          return Cv_MSER_Params;

   -- Extracts the contours of Maximally Stable Extremal Regions
   procedure CvExtractMSER (Img      : Cv_Arr_P;
                            Mask     : Cv_Arr_P;
                            Contours : access Cv_Seq_P;
                            Storage  : Cv_Mem_Storage_P;
                            Params   : Cv_MSER_Params);

   type Cv_Star_Keypoint is record
      Pt       : Cv_Point;
      Size     : Integer;
      Response : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Star_Keypoint);
   type Cv_Star_Keypoint_P is access Cv_Star_Keypoint;

   function CvStarKeypoint (Pt       : Cv_Point;
                            Size     : Integer;
                            Response : Float)
                            return Cv_Star_Keypoint;

   type Cv_Star_Detector_Params is
      record
         MaxSize                : Integer;
         ResponeThreshold       : Integer;
         LineThresholdProjected : Integer;
         LineThresholdBinarized : Integer;
         SuppressNonmaxSize     : Integer;
      end record;

   -- Constructor for Cv_Star_Detector_Params with default values.
   function CvStarDetectorParams (MaxSize                : Integer := 45;
                                  ResponeThreshold       : Integer := 30;
                                  LineThresholdProjected : Integer := 10;
                                  LineThresholdBinarized : Integer := 8;
                                  SuppressNonmaxSize     : Integer := 5) return Cv_Star_Detector_Params;

   --Retrieves keypoints using the StarDetector algorithm.
   function CvGetStarKeypoints (Image   : Cv_Arr_P;
                                Storage : Cv_Mem_Storage_P;
                                Params  : Cv_Star_Detector_Params := CvStarDetectorParams) return Cv_Seq_P;

private
   pragma Import (C, CvExtractSURF, "cvExtractSURF");
   pragma Import (C, CvGetStarKeypoints, "cvGetStarKeypoints");
   pragma Import (C, CvSURFPoint, "cvSURFPoint");
   pragma Import (C, CvMSERParams, "cvMSERParams");
   pragma Import (C, CvExtractMSER, "cvExtractMSER");
   pragma Import (C, CvStarKeypoint, "cvStarKeypoint");
end Features_2D;
