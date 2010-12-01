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
-- imgproc-types_c.ads - imgproc-types_c.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------
with Interfaces; use Interfaces;
with Interfaces.C;
with Core; use Core;
with Ada.Unchecked_Conversion;

package Imgproc is
   -- Connected component structure
   type Cv_Connected_Comp is
      record
         Area : Long_Float;
         Value : Cv_Scalar;
         Rect  : Cv_Rect;
         Contour : access Cv_Seq;
      end record;
   type Cv_Connected_Comp_P is access all Cv_Connected_Comp;

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Connected_Comp_P);

   -- Image smooth Methods
   type Smooth_Type is (CV_BLUR_NO_SCALE,
                        CV_BLUR,
                        CV_GAUSSIAN,
                        CV_MEDIAN,
                        CV_BILATERAL );
   for Smooth_Type use (CV_BLUR_NO_SCALE => 0,
                        CV_BLUR => 1,
                        CV_GAUSSIAN => 2,
                        CV_MEDIAN => 3,
                        CV_BILATERAL     => 4);

   -- Filters used in pyramid decomposition
   type Pyr_Filter is (CV_GAUSSIAN_5x5);
   for Pyr_Filter use (CV_GAUSSIAN_5x5 => 7);

   -- Inpainting algorithms
   type Inpaint_Algo is (CV_INPAINT_NS,
                         CV_INPAINT_TELEA);
   for Inpaint_Algo use (CV_INPAINT_NS => 0,
                         CV_INPAINT_TELEA  => 1);

   -- Special filters
   CV_SCHARR          : constant := -1;
   CV_MAX_SOBEL_KSIZE : constant := 7;

   -- Constants for color conversion
   type Color_Conversion is range 0 .. 100;
   CV_BGR2BGRA : constant Color_Conversion := 0;
   CV_RGB2RGBA : constant Color_Conversion := CV_BGR2BGRA;
   CV_BGRA2BGR : constant Color_Conversion := 1;
   CV_RGBA2RGB : constant Color_Conversion := CV_BGRA2BGR;
   CV_BGR2RGBA : constant Color_Conversion :=   2;
   CV_RGB2BGRA : constant Color_Conversion :=   CV_BGR2RGBA;
   CV_RGBA2BGR : constant Color_Conversion :=   3;
   CV_BGRA2RGB : constant Color_Conversion :=   CV_RGBA2BGR;
   CV_BGR2RGB : constant Color_Conversion := 4;
   CV_RGB2BGR : constant Color_Conversion := CV_BGR2RGB;
   CV_BGRA2RGBA : constant Color_Conversion := 5;
   CV_RGBA2BGRA : constant Color_Conversion := CV_BGRA2RGBA;
   CV_BGR2GRAY : constant Color_Conversion := 6;
   CV_RGB2GRAY : constant Color_Conversion := 7;
   CV_GRAY2BGR : constant Color_Conversion := 8;
   CV_GRAY2RGB : constant Color_Conversion := CV_GRAY2BGR;
   CV_GRAY2BGRA : constant Color_Conversion := 9;
   CV_GRAY2RGBA : constant Color_Conversion := CV_GRAY2BGRA;
   CV_BGRA2GRAY : constant Color_Conversion := 10;
   CV_RGBA2GRAY : constant Color_Conversion := 11;
   CV_BGR2BGR565 : constant Color_Conversion :=  12;
   CV_RGB2BGR565 : constant Color_Conversion :=  13;
   CV_BGR5652BGR : constant Color_Conversion :=  14;
   CV_BGR5652RGB : constant Color_Conversion := 15;
   CV_BGRA2BGR565 : constant Color_Conversion := 16;
   CV_RGBA2BGR565 : constant Color_Conversion := 17;
   CV_BGR5652BGRA : constant Color_Conversion := 18;
   CV_BGR5652RGBA : constant Color_Conversion := 19;
   CV_GRAY2BGR565 : constant Color_Conversion := 20;
   CV_BGR5652GRAY : constant Color_Conversion := 21;
   CV_BGR2BGR555 : constant Color_Conversion :=  22;
   CV_RGB2BGR555 : constant Color_Conversion := 23;
   CV_BGR5552BGR : constant Color_Conversion :=  24;
   CV_BGR5552RGB : constant Color_Conversion :=  25;
   CV_BGRA2BGR555 : constant Color_Conversion := 26;
   CV_RGBA2BGR555 : constant Color_Conversion := 27;
   CV_BGR5552BGRA : constant Color_Conversion := 28;
   CV_BGR5552RGBA : constant Color_Conversion := 29;
   CV_GRAY2BGR555 : constant Color_Conversion := 30;
   CV_BGR5552GRAY : constant Color_Conversion := 31;
   CV_BGR2XYZ : constant Color_Conversion := 32;
   CV_RGB2XYZ : constant Color_Conversion :=     33;
   CV_XYZ2BGR : constant Color_Conversion :=     34;
   CV_XYZ2RGB : constant Color_Conversion :=     35;
   CV_BGR2YCrCb  : constant Color_Conversion := 36;
   CV_RGB2YCrCb : constant Color_Conversion :=  37;
   CV_YCrCb2BGR : constant Color_Conversion :=  38;
   CV_YCrCb2RGB : constant Color_Conversion := 39;
   CV_BGR2HSV : constant Color_Conversion :=    40;
   CV_RGB2HSV : constant Color_Conversion :=    41;
   CV_BGR2Lab : constant Color_Conversion :=     44;
   CV_RGB2Lab : constant Color_Conversion :=    45;
   CV_BayerBG2BGR : constant Color_Conversion := 46;
   CV_BayerGB2BGR : constant Color_Conversion := 47;
   CV_BayerRG2BGR : constant Color_Conversion := 48;
   CV_BayerGR2BGR : constant Color_Conversion := 49;
   CV_BayerBG2RGB : constant Color_Conversion := CV_BayerRG2BGR;
   CV_BayerGB2RGB : constant Color_Conversion := CV_BayerGR2BGR;
   CV_BayerRG2RGB : constant Color_Conversion := CV_BayerBG2BGR;
   CV_BayerGR2RGB : constant Color_Conversion := CV_BayerGB2BGR;
   CV_BGR2Luv : constant Color_Conversion := 50;
   CV_RGB2Luv : constant Color_Conversion := 51;
   CV_BGR2HLS     : constant Color_Conversion := 52;
   CV_RGB2HLS     : constant Color_Conversion := 53;
   CV_HSV2BGR     : constant Color_Conversion := 54;
   CV_HSV2RGB     : constant Color_Conversion := 55;
   CV_Lab2BGR     : constant Color_Conversion := 56;
   CV_Lab2RGB     : constant Color_Conversion := 57;
   CV_Luv2BGR     : constant Color_Conversion := 58;
   CV_Luv2RGB     : constant Color_Conversion := 59;
   CV_HLS2BGR     : constant Color_Conversion := 60;
   CV_HLS2RGB    : constant Color_Conversion := 61;
   CV_COLORCVT_MAX : constant Color_Conversion :=  100;

   -- Sub-pixel interpolation methods
   type Cv_Inter is (CV_INTER_NN,
                     CV_INTER_LINEAR,
                     CV_INTER_CUBIC,
                     CV_INTER_AREA);
   for CV_Inter use (CV_INTER_NN => 0,
                     CV_INTER_LINEAR => 1,
                     CV_INTER_CUBIC => 2,
                     CV_INTER_AREA   => 3);

   -- ... and other image warping flags
   type Cv_Warp is (CV_WARP_FILL_OUTLIERS,
                    CV_WARP_INVERSE_MAP);

   for Cv_Warp use (CV_WARP_FILL_OUTLIERS => 8,
                    CV_WARP_INVERSE_MAP => 16);

   -- Shapes of a structuring element for morphological operations
   type Structuring_Shape is (CV_SHAPE_RECT,
                              CV_SHAPE_CROSS,
                              CV_SHAPE_ELLIPSE,
                              CV_SHAPE_CUSTOM);
   for Structuring_Shape use (CV_SHAPE_RECT    => 0,
                              CV_SHAPE_CROSS   => 1,
                              CV_SHAPE_ELLIPSE => 2,
                              CV_SHAPE_CUSTOM  => 100);

   -- Morphological operations
   type Morph_Operation is (CV_MOP_ERODE,
                            CV_MOP_DILATE,
                            CV_MOP_OPEN,
                            CV_MOP_CLOSE,
                            CV_MOP_GRADIENT,
                            CV_MOP_TOPHAT,
                            CV_MOP_BLACKHAT);
   for Morph_Operation use (CV_MOP_ERODE    => 0,
                            CV_MOP_DILATE   => 1,
                            CV_MOP_OPEN     => 2,
                            CV_MOP_CLOSE    => 3,
                            CV_MOP_GRADIENT => 4,
                            CV_MOP_TOPHAT   => 5,
                            CV_MOP_BLACKHAT => 6);

    --/* spatial and central moments */
   type Cv_Moments is
      record
         M00, M10, M01, M20, M11, M02, M30, M21, M12, M03 : Long_Float; --/* spatial moments */
         Mu20, Mu11, Mu02, Mu30, Mu21, Mu12, Mu03         : Long_Float;  --/* central moments */
         Inv_Sqrt_M00                                     : Long_Float; --/* m00 != 0 ? 1/sqrt(m00) : 0 */
      end record;
   type Cv_Moments_P is access Cv_Moments;

      --/* Hu invariants */
   type Cv_Hu_Moments is
      record
         Hu1, Hu2, Hu3, Hu4, Hu5, Hu6, Hu7 : Long_Float; --/  * Hu invariants *  /
      end record;
   type Cv_Hu_Moments_P is access Cv_Hu_Moments;

   -- Template matching methods
   type Cv_Tm is (CV_TM_SQDIFF,
                  CV_TM_SQDIFF_NORMED,
                  CV_TM_CCORR,
                  CV_TM_CCORR_NORMED,
                  CV_TM_CCOEFF,
                  CV_TM_CCOEFF_NORMED);
   for Cv_Tm use (CV_TM_SQDIFF => 0,
                  CV_TM_SQDIFF_NORMED => 1,
                  CV_TM_CCORR => 2,
                  CV_TM_CCORR_NORMED => 3,
                  CV_TM_CCOEFF => 4,
                  CV_TM_CCOEFF_NORMED => 5);

   type Cv_Distance_Function is access function (A          : access Float;
                                                 B          : access Float;
                                                 User_Param : Cv_Void_P)
                                                 return Float;
   pragma Convention (C, Cv_Distance_Function);

   -- Contour retrieval modes
   type Cv_Retr is (CV_RETR_EXTERNAL,
                    CV_RETR_LIST,
                    CV_RETR_CCOMP,
                    CV_RETS_TREE);
   for Cv_Retr use (CV_RETR_EXTERNAL => 0,
                    CV_RETR_LIST => 1,
                    CV_RETR_CCOMP => 2,
                    CV_RETS_TREE => 3);

   -- Contour approximation methods
   type Cv_Chain_Enum is new Integer range 0 .. 5;
   CV_CHAIN_CODE             : constant Cv_Chain_Enum := 0;
   CV_CHAIN_APPROX_NONE      : constant Cv_Chain_Enum := 1;
   CV_CHAIN_APPROX_SIMPLE    : constant Cv_Chain_Enum := 2;
   CV_CHAIN_APPROX_TC89_L1   : constant Cv_Chain_Enum := 3;
   CV_CHAIN_APPROX_TC89_KCOS : constant Cv_Chain_Enum := 4;
   CV_LINK_RUNS              : constant Cv_Chain_Enum := 5;

   -- Internal structure that is used for sequental retrieving contours from the image.
   -- It supports both hierarchical and plane variants of Suzuki algorithm.
   type Cv_Contour_Scanner is null record;
   type Cv_Contour_Scanner_P is access Cv_Contour_Scanner;

--     -- Freeman chain reader state
--     type Cv_Chain_Pt_Reader is
--        record
--        --CV_SEQ_READER_FIELDS()
--           HeaderSize : Integer;
--           Seq        : Cv_Seq_P;
--           Block      : Cv_Seq_Block_P;
--           Ptr        : Cv_Arr_Pointer;
--           BlockMin   : Cv_Arr_Pointer;
--           BlockMax   : Cv_Arr_Pointer;
--           DeltaIndex : Integer;
--           PrevElem   : Cv_Arr_Pointer;
--           --
--           Code       : Unsigned_8;
--           Pt         : Cv_Point;
--           Deltas     : Delta_Array;
--        end record;
--     type Cv_Chain_Pt_Reader_P is access Cv_Chain_Pt_Reader;

   -- initializes 8-element array for fast access to 3x3 neighborhood of a pixel
   procedure CV_INIT_3X3_DELTAS (Deltas : in out Cv_32S_Array;
                                 Step          : Integer;
                                 Nch           : Integer);

   -----------------------------------------------------------------------------
   -- Planar subdivisions
   -----------------------------------------------------------------------------
   type Cv_Subdiv_2D_Edge is new Interfaces.C.Size_T;
   type Cv_Subdiv_2D_Edge_P is access Cv_Subdiv_2D_Edge;

   --#define CV_SUBDIV2D_VIRTUAL_POINT_FLAG (1 << 30)
   CV_SUBDIV2D_VIRTUAL_POINT_FLAG : constant := 16#40000000#;

   -- Point of original or dual subdivision.
   type Cv_Subdiv_2D_Point is
      record
         Flags : Integer;
         First : Cv_Subdiv_2D_Edge;
         Pt    : Cv_Point_2D_32F;
         Id    : Integer;
      end record;

   type Cv_Subdiv_2D_Point_P is access Cv_Subdiv_2D_Point;

   -- Array for Cv_Quad_Edge_2D
   type Cv_Subdiv_2D_Point_P_Arr is array (Integer range <>) of Cv_Subdiv_2D_Point_P;
   type Cv_Subdiv_2D_Edge_Arr is array (Integer range <>) of Cv_Subdiv_2D_Edge;

   --/* quad-edge structure fields */
   type Cv_Quad_Edge_2D is
      record
         Flags : Integer;
         Pt    : Cv_Subdiv_2D_Point_P_Arr(1 .. 4);
         Next  : Cv_Subdiv_2D_Edge_Arr(1 .. 4);
      end record;
   type Cv_Quad_Edge_2D_P is access Cv_Quad_Edge_2D;

   -- Planar subdivision
   type Cv_Subdiv_2D is
      record
         Flags           : Integer;       --CV_TREE_NODE_FIELDS(CvSeq);
         HeaderSize      : Integer;
         HPrev           : Cv_Seq_P;
         HNext           : Cv_Seq_P;
         VPrev           : Cv_Seq_P;
         VNext           : Cv_Seq_P;

         Total           : Integer; --CV_SEQUENCE_FIELDS
         ElemSize        : Integer;
         BlockMax        : Cv_Void_P;
         Ptr             : Cv_Void_P;
         DeltaElems      : Integer;
         Storage         : Cv_Mem_Storage_P;
         FreeBlocks      : Cv_Seq_Block_P;
         First           : Cv_Seq_Block_P;

         FreeElem        : Cv_Set_Elem_P; -- CV_SET_FIELDS()
         ActiveCount     : Integer;

         Edges           : Cv_Set_P; -- Cv_Graph_Fields

         QuadEdges       : Integer;
         IsGeometryValid : Integer;
         RecentEdge      : Cv_Subdiv_2D_Edge;
         TopLeft         : Cv_Point_2D_32f;
         BottomRight     : Cv_Point_2D_32f;
      end record;
   type Cv_Subdiv_2D_P is access Cv_Subdiv_2D;

   type Cv_Subdiv_2D_Point_Location is range -2 .. 2;
   CV_PTLOC_ERROR : Cv_Subdiv_2D_Point_Location := -2;
   CV_PTLOC_OUTSIDE_RECT : Cv_Subdiv_2D_Point_Location := -1;
   CV_PTLOC_INSIDE : Cv_Subdiv_2D_Point_Location := 0;
   CV_PTLOC_VERTEX : Cv_Subdiv_2D_Point_Location := 1;
   CV_PTLOC_ON_EDGE : Cv_Subdiv_2D_Point_Location := 2;

   type Cv_Next_Edge_Type is (CV_NEXT_AROUND_ORG,
                              CV_PREV_AROUND_RIGHT,
                              CV_PREV_AROUND_ORG,
                              CV_NEXT_AROUND_LEFT,
                              CV_PREV_AROUND_LEFT,
                              CV_NEXT_AROUND_DST,
                              CV_NEXT_AROUND_RIGHT,
                              CV_PREV_AROUND_DST);
   for Cv_Next_Edge_Type use (CV_NEXT_AROUND_ORG   => 16#00#,
                              CV_PREV_AROUND_RIGHT => 16#02#,
                              CV_PREV_AROUND_ORG   => 16#11#,
                              CV_NEXT_AROUND_LEFT  => 16#13#,
                              CV_PREV_AROUND_LEFT  => 16#20#,
                              CV_NEXT_AROUND_DST   => 16#22#,
                              CV_NEXT_AROUND_RIGHT => 16#31#,
                              CV_PREV_AROUND_DST   => 16#33#);

   -- get the next edge with the same origin point (counterwise)
   function CV_SUBDIV2D_NEXT_EDGE (Edge : Cv_Quad_Edge_2D) return Cv_Quad_Edge_2D_P;

   -- Contour approximation algorithms
   CV_POLY_APPROX_DP : constant := 0;

   -- Shape matching methods
   CV_CONTOURS_MATCH_I1 : constant := 1;
   CV_CONTOURS_MATCH_I2 : constant := 2;
   CV_CONTOURS_MATCH_I3 : constant := 3;

   -- Shape orientation
   CV_CLOCKWISE         : constant := 1;
   CV_COUNTER_CLOCKWISE : constant := 2;

   type Cv_Convexity_Defect is record
      Start_Point : access Cv_Point;
      End_Point   : access Cv_Point;
      Depth_Point : access Cv_Point;
      Depth       : Float;
   end record;

   -- Histogram comparison methods
   type Hist_Compare_Method is (CV_COMP_CORREL,
                                CV_COMP_CHISQR,
                                CV_COMP_INTERSECT,
                                CV_COMP_BHATTACHARYYA);
   for Hist_Compare_Method use (CV_COMP_CORREL        => 0,
                                CV_COMP_CHISQR        => 1,
                                CV_COMP_INTERSECT     => 2,
                                CV_COMP_BHATTACHARYYA => 3);

   -- Mask size for distance transform
   CV_DIST_MASK_3       : constant := 3;
   CV_DIST_MASK_5       : constant := 5;
   CV_DIST_MASK_PRECISE : constant := 0;

   -- Distance types for Distance Transform and M-estimators
   CV_DIST_USER : constant := -1;
   CV_DIST_L1 : constant := 1;
   CV_DIST_L2 : constant := 2;
   CV_DIST_C : constant := 3;
   CV_DIST_L12 : constant := 4;
   CV_DIST_FAIR : constant := 5;
   CV_DIST_WELSCH : constant := 6;
   CV_DIST_HUBER : constant := 7;

   -- Threshold types
   type Threshold_Type is (CV_THRESH_BINARY,
                           CV_THRESH_BINARY_INV,
                           CV_THRESH_TRUNC,
                           CV_THRESH_TOZERO,
                           CV_THRESH_TOZERO_INV,
                           CV_THRESH_MASK,
                           CV_THRESH_OTSU);
   for Threshold_Type use (CV_THRESH_BINARY     => 0,
                           CV_THRESH_BINARY_INV => 1,
                           CV_THRESH_TRUNC      => 2,
                           CV_THRESH_TOZERO     => 3,
                           CV_THRESH_TOZERO_INV => 4,
                           CV_THRESH_MASK       => 7,
                           CV_THRESH_OTSU       => 8);

   -- Enumeration for Adaptive Method...
   type Adaptive_Method is (CV_ADAPTIVE_THRESH_MEAN_C,
                            CV_ADAPTIVE_THRESH_GAUSSIAN_C);
   for Adaptive_Method use (CV_ADAPTIVE_THRESH_MEAN_C     => 0,
                            CV_ADAPTIVE_THRESH_GAUSSIAN_C => 1);

   -- FloodFill flags
   CV_FLOODFILL_FIXED_RANGE : constant := 16#0001_0000#;
   CV_FLOODFILL_MASK_ONLY   : constant := 16#0002_0000#;

   -- Canny edge detector flags
   CV_CANNY_L2_GRADIENT : constant := 16#8000_0000#;

   -- Variants of a Hough transform
   CV_HOUGH_STANDARD : constant := 0;
   CV_HOUGH_PROBABILISTIC : constant := 1;
   CV_HOUGH_MULTI_SCALE : constant := 2;
   CV_HOUGH_GRADIENT : constant := 3;

   -- Fast search data structure
   type Cv_Feature_Tree is null record;
   type Cv_Feature_Tree_P is access Cv_Feature_Tree;

   type Cv_LSH is null record;
   type Cv_LSH_P is access Cv_LSH;

   type Cv_LSH_Operations is null record;
   type Cv_LSH_Operations_P is access Cv_LSH_Operations;

   -----------------------------------------------------------------------------
   -- Mmoved
   -----------------------------------------------------------------------------
   type Border_Type is (IPL_BORDER_CONSTANT,
                        IPL_BORDER_REPLICATE,
                        IPL_BORDER_REFLECT,
                        IPL_BORDER_WRAP
                       );
   for Border_Type use (IPL_BORDER_CONSTANT  => 0,
                        IPL_BORDER_REPLICATE => 1,
                          IPL_BORDER_REFLECT   => 2, --does not work
                        IPL_BORDER_WRAP      => 3 -- does not work
                       );
end Imgproc;
