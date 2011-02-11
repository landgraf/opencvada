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
package Features_2d is
--


   -- Use with CvExtractSURF
   -- This is what the Cv_Seq_P will contain.
   type Cv_Surf_Point is
      record
         Pt        : Cv_Point_2d_32f;
         Laplacian : Integer;
         Size      : Integer;
         Dir       : Float;
         Hessian   : Float;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Surf_Point);
   type Cv_Surf_Point_Ptr is access all Cv_Surf_Point;

   function Cv_Create_Surf_Point (Pt        : Cv_Point_2d_32f;
                                  Laplacian : Integer;
                                  Size      : Integer;
                                  Dir       : Float := 0.0;
                                  Hessian   : Float := 0.0)
                                  return Cv_Surf_Point;

   --
   type Cv_Surf_Params is
      record
         Extended         : Integer;
         Hessianthreshold : Long_Float;
         Noctaves         : Integer;
         Noctavelayers    : Integer;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Surf_Params);

   --              // returns default parameters
   function Cv_Create_Surf_Params (Threshold : Long_Float;
                                   Extended  : Integer := 0) return Cv_Surf_Params;

   -- Extracts Speeded Up Robust Features from an image.
   procedure Cv_Extract_Surf (Image                : Cv_Arr_Ptr;
                              Mask                 : Cv_Arr_Ptr;
                              Keypoints            : access Cv_Seq_Ptr;
                              Descriptors          : access Cv_Seq_Ptr;
                              Storage              : Cv_Mem_Storage_Ptr;
                              Params               : Cv_Surf_Params;
                              Use_Provided_Key_Pts : Integer := 0);
   procedure Cv_Extract_Surf (Image                : Cv_Mat_Ptr;
                              Mask                 : Cv_Mat_Ptr;
                              Keypoints            : access Cv_Seq_Ptr;
                              Descriptors          : access Cv_Seq_Ptr;
                              Storage              : Cv_Mem_Storage_Ptr;
                              Params               : Cv_Surf_Params;
                              Use_Provided_Key_Pts : Integer := 0);
   procedure Cv_Extract_Surf (Image                : Ipl_Image_Ptr;
                              Mask                 : Ipl_Image_Ptr;
                              Keypoints            : access Cv_Seq_Ptr;
                              Descriptors          : access Cv_Seq_Ptr;
                              Storage              : Cv_Mem_Storage_Ptr;
                              Params               : Cv_Surf_Params;
                              Use_Provided_Key_Pts : Integer := 0);

   type Cv_Mser_Params is record
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
   pragma Convention (C_Pass_By_Copy, Cv_Mser_Params);
   type Cv_Mser_Params_Ptr is access all Cv_Mser_Params;

   function Cv_Create_Mser_Params (Delta_Val      : Integer := 5;
                                   Min_Area       : Integer := 60;
                                   Max_Area       : Integer := 14400;
                                   Max_Variation  : Float := 0.25;
                                   Min_Diversity  : Float := 0.2;
                                   Max_Evolution  : Integer := 200;
                                   Area_Threshold : Long_Float := 1.01;
                                   Min_Margin     : Long_Float := 0.003;
                                   Edge_Blur_Size : Integer := 5)
                                   return Cv_Mser_Params;

   -- Extracts the contours of Maximally Stable Extremal Regions
   procedure Cv_Extract_Mser (Img      : Cv_Arr_Ptr;
                              Mask     : Cv_Arr_Ptr;
                              Contours : access Cv_Seq_Ptr;
                              Storage  : Cv_Mem_Storage_Ptr;
                              Params   : Cv_Mser_Params);
   procedure Cv_Extract_Mser (Img      : Cv_Mat_Ptr;
                              Mask     : Cv_Mat_Ptr;
                              Contours : access Cv_Seq_Ptr;
                              Storage  : Cv_Mem_Storage_Ptr;
                              Params   : Cv_Mser_Params);
   procedure Cv_Extract_Mser (Img      : Ipl_Image_Ptr;
                              Mask     : Ipl_Image_Ptr;
                              Contours : access Cv_Seq_Ptr;
                              Storage  : Cv_Mem_Storage_Ptr;
                              Params   : Cv_Mser_Params);


   type Cv_Star_Keypoint is record
      Pt       : Cv_Point;
      Size     : Integer;
      Response : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Star_Keypoint);
   type Cv_Star_Keypoint_Ptr is access all Cv_Star_Keypoint;

   function Cv_Create_Star_Keypoint (Pt       : Cv_Point;
                                     Size     : Integer;
                                     Response : Float)
                                     return Cv_Star_Keypoint;

   type Cv_Star_Detector_Params is
      record
         Maxsize                : Integer;
         Responethreshold       : Integer;
         Linethresholdprojected : Integer;
         Linethresholdbinarized : Integer;
         Suppressnonmaxsize     : Integer;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Star_Detector_Params);
   type Cv_Star_Detector_Params_Ptr is access all Cv_Star_Detector_Params;

   -- Constructor for Cv_Star_Detector_Params with default values.
   function Cv_Create_Star_Detector_Params (Maxsize                : Integer := 45;
                                            Responethreshold       : Integer := 30;
                                            Linethresholdprojected : Integer := 10;
                                            Linethresholdbinarized : Integer := 8;
                                            Suppressnonmaxsize     : Integer := 5) return Cv_Star_Detector_Params;

   --Retrieves keypoints using the StarDetector algorithm.
   function Cv_Get_Star_Keypoints (Image   : Cv_Arr_Ptr;
                                   Storage : Cv_Mem_Storage_Ptr;
                                   Params  : Cv_Star_Detector_Params := Cv_Create_Star_Detector_Params) return Cv_Seq_Ptr;
   function Cv_Get_Star_Keypoints (Image   : Cv_Mat_Ptr;
                                   Storage : Cv_Mem_Storage_Ptr;
                                   Params  : Cv_Star_Detector_Params := Cv_Create_Star_Detector_Params) return Cv_Seq_Ptr;
   function Cv_Get_Star_Keypoints (Image   : Ipl_Image_Ptr;
                                   Storage : Cv_Mem_Storage_Ptr;
                                   Params  : Cv_Star_Detector_Params := Cv_Create_Star_Detector_Params) return Cv_Seq_Ptr;

private
   pragma Import (C, Cv_Extract_Surf, "cvExtractSURF");
   pragma Import (C, Cv_Get_Star_Keypoints, "cvGetStarKeypoints");
   pragma Import (C, Cv_Create_Surf_Point, "cvSURFPoint");
   pragma Import (C, Cv_Create_Mser_Params, "cvMSERParams");
   pragma Import (C, Cv_Extract_Mser, "cvExtractMSER");
   pragma Import (C, Cv_Create_Star_Keypoint, "cvStarKeypoint");
end Features_2d;
