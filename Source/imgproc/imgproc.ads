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
         Area    : Long_Float;
         Value   : Cv_Scalar;
         Rect    : Cv_Rect;
         Contour : access Cv_Seq;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Connected_Comp);
   type Cv_Connected_Comp_P is access all Cv_Connected_Comp;

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Connected_Comp_P);

   -- Image smooth Methods
   type Smooth_Type is new Integer;
   Cv_Blur_No_Scale : constant Smooth_Type := 0;
   Cv_Blur : constant Smooth_Type := 1;
   Cv_Gaussian : constant Smooth_Type := 2;
   Cv_Median : constant Smooth_Type := 3;
   Cv_Bilateral : constant Smooth_Type := 4;

   -- Filters used in pyramid decomposition
   type Pyr_Filter is new Integer;
   Cv_Gaussian_5x5 : constant Pyr_Filter := 7;

   -- Inpainting algorithms
   type Inpaint_Algo is new Integer;
   Cv_Inpaint_Ns : constant Inpaint_Algo := 0;
   Cv_Inpaint_Telea : constant Inpaint_Algo := 1;

   -- Special filters
   Cv_Scharr          : constant := -1;
   Cv_Max_Sobel_Ksize : constant := 7;

   -- Constants for color conversion
   type Color_Conversion is range 0 .. 100;
   Cv_Bgr2bgra : constant Color_Conversion := 0;
   Cv_Rgb2rgba : constant Color_Conversion := Cv_Bgr2bgra;
   Cv_Bgra2bgr : constant Color_Conversion := 1;
   Cv_Rgba2rgb : constant Color_Conversion := Cv_Bgra2bgr;
   Cv_Bgr2rgba : constant Color_Conversion :=   2;
   Cv_Rgb2bgra : constant Color_Conversion :=   Cv_Bgr2rgba;
   Cv_Rgba2bgr : constant Color_Conversion :=   3;
   Cv_Bgra2rgb : constant Color_Conversion :=   Cv_Rgba2bgr;
   Cv_Bgr2rgb : constant Color_Conversion := 4;
   Cv_Rgb2bgr : constant Color_Conversion := Cv_Bgr2rgb;
   Cv_Bgra2rgba : constant Color_Conversion := 5;
   Cv_Rgba2bgra : constant Color_Conversion := Cv_Bgra2rgba;
   Cv_Bgr2gray : constant Color_Conversion := 6;
   Cv_Rgb2gray : constant Color_Conversion := 7;
   Cv_Gray2bgr : constant Color_Conversion := 8;
   Cv_Gray2rgb : constant Color_Conversion := Cv_Gray2bgr;
   Cv_Gray2bgra : constant Color_Conversion := 9;
   Cv_Gray2rgba : constant Color_Conversion := Cv_Gray2bgra;
   Cv_Bgra2gray : constant Color_Conversion := 10;
   Cv_Rgba2gray : constant Color_Conversion := 11;
   Cv_Bgr2bgr565 : constant Color_Conversion :=  12;
   Cv_Rgb2bgr565 : constant Color_Conversion :=  13;
   Cv_Bgr5652bgr : constant Color_Conversion :=  14;
   Cv_Bgr5652rgb : constant Color_Conversion := 15;
   Cv_Bgra2bgr565 : constant Color_Conversion := 16;
   Cv_Rgba2bgr565 : constant Color_Conversion := 17;
   Cv_Bgr5652bgra : constant Color_Conversion := 18;
   Cv_Bgr5652rgba : constant Color_Conversion := 19;
   Cv_Gray2bgr565 : constant Color_Conversion := 20;
   Cv_Bgr5652gray : constant Color_Conversion := 21;
   Cv_Bgr2bgr555 : constant Color_Conversion :=  22;
   Cv_Rgb2bgr555 : constant Color_Conversion := 23;
   Cv_Bgr5552bgr : constant Color_Conversion :=  24;
   Cv_Bgr5552rgb : constant Color_Conversion :=  25;
   Cv_Bgra2bgr555 : constant Color_Conversion := 26;
   Cv_Rgba2bgr555 : constant Color_Conversion := 27;
   Cv_Bgr5552bgra : constant Color_Conversion := 28;
   Cv_Bgr5552rgba : constant Color_Conversion := 29;
   Cv_Gray2bgr555 : constant Color_Conversion := 30;
   Cv_Bgr5552gray : constant Color_Conversion := 31;
   Cv_Bgr2xyz : constant Color_Conversion := 32;
   Cv_Rgb2xyz : constant Color_Conversion :=     33;
   Cv_Xyz2bgr : constant Color_Conversion :=     34;
   Cv_Xyz2rgb : constant Color_Conversion :=     35;
   Cv_Bgr2ycrcb  : constant Color_Conversion := 36;
   Cv_Rgb2ycrcb : constant Color_Conversion :=  37;
   Cv_Ycrcb2bgr : constant Color_Conversion :=  38;
   Cv_Ycrcb2rgb : constant Color_Conversion := 39;
   Cv_Bgr2hsv : constant Color_Conversion :=    40;
   Cv_Rgb2hsv : constant Color_Conversion :=    41;
   Cv_Bgr2lab : constant Color_Conversion :=     44;
   Cv_Rgb2lab : constant Color_Conversion :=    45;
   Cv_Bayerbg2bgr : constant Color_Conversion := 46;
   Cv_Bayergb2bgr : constant Color_Conversion := 47;
   Cv_Bayerrg2bgr : constant Color_Conversion := 48;
   Cv_Bayergr2bgr : constant Color_Conversion := 49;
   Cv_Bayerbg2rgb : constant Color_Conversion := Cv_Bayerrg2bgr;
   Cv_Bayergb2rgb : constant Color_Conversion := Cv_Bayergr2bgr;
   Cv_Bayerrg2rgb : constant Color_Conversion := Cv_Bayerbg2bgr;
   Cv_Bayergr2rgb : constant Color_Conversion := Cv_Bayergb2bgr;
   Cv_Bgr2luv : constant Color_Conversion := 50;
   Cv_Rgb2luv : constant Color_Conversion := 51;
   Cv_Bgr2hls     : constant Color_Conversion := 52;
   Cv_Rgb2hls     : constant Color_Conversion := 53;
   Cv_Hsv2bgr     : constant Color_Conversion := 54;
   Cv_Hsv2rgb     : constant Color_Conversion := 55;
   Cv_Lab2bgr     : constant Color_Conversion := 56;
   Cv_Lab2rgb     : constant Color_Conversion := 57;
   Cv_Luv2bgr     : constant Color_Conversion := 58;
   Cv_Luv2rgb     : constant Color_Conversion := 59;
   Cv_Hls2bgr     : constant Color_Conversion := 60;
   Cv_Hls2rgb    : constant Color_Conversion := 61;
   Cv_Colorcvt_Max : constant Color_Conversion :=  100;

   -- Sub-pixel interpolation methods
   type Cv_Inter is new Integer;
   Cv_Inter_Nn : constant Cv_Inter := 0;
   Cv_Inter_Linear : constant Cv_Inter := 1;
   Cv_Inter_Cubic : constant Cv_Inter := 2;
   Cv_Inter_Area : constant Cv_Inter := 3;

   -- ... and other image warping flags
   type Cv_Warp is new Integer;
   Cv_Warp_Fill_Outliers  : constant Cv_Warp  := 8;
   Cv_Warp_Inverse_Map : constant Cv_Warp := 16;

   -- Shapes of a structuring element for morphological operations
   type Structuring_Shape is new Integer;
   Cv_Shape_Rect : constant Structuring_Shape := 0;
   Cv_Shape_Cross : constant Structuring_Shape := 1;
   Cv_Shape_Ellipse : constant Structuring_Shape := 2;
   Cv_Shape_Custom : constant Structuring_Shape := 100;

   -- Morphological operations
   type Morph_Operation is new Integer;
   Cv_Mop_Erode : constant Morph_Operation := 0;
   Cv_Mop_Dilate : constant Morph_Operation := 1;
   Cv_Mop_Open : constant Morph_Operation := 2; --constant constant constant constant constant  constant constant constant constant constant constant
   Cv_Mop_Close : constant Morph_Operation := 3;
   Cv_Mop_Gradient : constant Morph_Operation := 4;
   Cv_Mop_Tophat : constant Morph_Operation := 5;
   Cv_Mop_Blackhat : constant Morph_Operation := 6;

   --/* spatial and central moments */
   type Cv_Moments is
      record
         M00, M10, M01, M20, M11, M02, M30, M21, M12, M03 : Long_Float; --/* spatial moments */
         Mu20, Mu11, Mu02, Mu30, Mu21, Mu12, Mu03         : Long_Float;  --/* central moments */
         Inv_Sqrt_M00                                     : Long_Float; --/* m00 != 0 ? 1/sqrt(m00) : 0 */
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Moments);
   type Cv_Moments_P is access all cv_Moments;

   --/* Hu invariants */
   type Cv_Hu_Moments is
      record
         Hu1, Hu2, Hu3, Hu4, Hu5, Hu6, Hu7 : Long_Float; --/  * Hu invariants *  /
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Hu_Moments);
   type Cv_Hu_Moments_P is access all cv_Hu_Moments;

   -- Template matching methods
   type Cv_Tm is new Integer;
   Cv_Tm_Sqdiff : constant Cv_Tm := 0;
   Cv_Tm_Sqdiff_Normed : constant Cv_Tm := 1;
   Cv_Tm_Ccorr : constant Cv_Tm := 2;
   Cv_Tm_Ccorr_Normed : constant Cv_Tm := 3;
   Cv_Tm_Ccoeff : constant Cv_Tm := 4;
   Cv_Tm_Ccoeff_Normed : constant Cv_Tm := 5;

   type Cv_Distance_Function is access function (A          : access Float;
                                                 B          : access Float;
                                                 User_Param : Cv_Void_P)
                                                 return Float;
   pragma Convention (C, Cv_Distance_Function);

   -- Contour retrieval modes
   type Cv_Retr is new Integer;
   Cv_Retr_External : constant Cv_Retr := 0;
   Cv_Retr_List : constant Cv_Retr := 1;
   Cv_Retr_Ccomp : constant Cv_Retr := 2;
   Cv_Retr_Tree : constant Cv_Retr := 3;

   -- Contour approximation methods
   type Cv_Chain_Enum is new Integer range 0 .. 5;
   Cv_Chain_Code             : constant Cv_Chain_Enum := 0;
   Cv_Chain_Approx_None      : constant Cv_Chain_Enum := 1;
   Cv_Chain_Approx_Simple    : constant Cv_Chain_Enum := 2;
   Cv_Chain_Approx_Tc89_L1   : constant Cv_Chain_Enum := 3;
   Cv_Chain_Approx_Tc89_Kcos : constant Cv_Chain_Enum := 4;
   Cv_Link_Runs              : constant Cv_Chain_Enum := 5;

   -- Internal structure that is used for sequental retrieving contours from the image.
   -- It supports both hierarchical and plane variants of Suzuki algorithm.
   type Cv_Contour_Scanner is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Contour_Scanner);
   type Cv_Contour_Scanner_P is access all cv_Contour_Scanner;

   -- initializes 8-element array for fast access to 3x3 neighborhood of a pixel
   procedure Cv_Init_3x3_Deltas (Deltas        : in out Cv_32s_Array;
                                 Step          : Integer;
                                 Nch           : Integer);

   -----------------------------------------------------------------------------
   -- Planar subdivisions
   -----------------------------------------------------------------------------
   type Cv_Subdiv_2d_Edge is new Interfaces.C.Size_T;
   type Cv_Subdiv_2d_Edge_P is access all Cv_Subdiv_2d_Edge;

   --#define CV_SUBDIV2D_VIRTUAL_POINT_FLAG (1 << 30)
   Cv_Subdiv2d_Virtual_Point_Flag : constant := 16#40000000#;

   -- Point of original or dual subdivision.
   type Cv_Subdiv_2d_Point is
      record
         Flags : Integer;
         First : Cv_Subdiv_2d_Edge;
         Pt    : Cv_Point_2d_32f;
         Id    : Integer;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Subdiv_2d_Point);
   type Cv_Subdiv_2d_Point_P is access all cv_Subdiv_2d_Point;

   -- Array for Cv_Quad_Edge_2D
   type Cv_Subdiv_2d_Point_P_Arr is array (Integer range <>) of Cv_Subdiv_2d_Point_P;
   type Cv_Subdiv_2d_Edge_Arr is array (Integer range <>) of Cv_Subdiv_2d_Edge;

   --/* quad-edge structure fields */
   type Cv_Quad_Edge_2d is
      record
         Flags : Integer;
         Pt    : Cv_Subdiv_2d_Point_P_Arr (1 .. 4);
         Next  : Cv_Subdiv_2d_Edge_Arr (1 .. 4);
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Quad_Edge_2d);
   type Cv_Quad_Edge_2d_P is access all cv_Quad_Edge_2d;

   -- Planar subdivision
   type Cv_Subdiv_2d is
      record
         Flags           : Integer;       --CV_TREE_NODE_FIELDS(CvSeq);
         Headersize      : Integer;
         Hprev           : Cv_Seq_P;
         Hnext           : Cv_Seq_P;
         Vprev           : Cv_Seq_P;
         Vnext           : Cv_Seq_P;

         Total           : Integer; --CV_SEQUENCE_FIELDS
         Elemsize        : Integer;
         Blockmax        : Cv_Void_P;
         Ptr             : Cv_Void_P;
         Deltaelems      : Integer;
         Storage         : Cv_Mem_Storage_P;
         Freeblocks      : Cv_Seq_Block_P;
         First           : Cv_Seq_Block_P;

         Freeelem        : Cv_Set_Elem_P; -- CV_SET_FIELDS()
         Activecount     : Integer;

         Edges           : Cv_Set_P; -- Cv_Graph_Fields

         Quadedges       : Integer;
         Isgeometryvalid : Integer;
         Recentedge      : Cv_Subdiv_2d_Edge;
         Topleft         : Cv_Point_2d_32f;
         Bottomright     : Cv_Point_2d_32f;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Subdiv_2d);
   type Cv_Subdiv_2d_P is access all Cv_Subdiv_2d;

   function To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Subdiv_2d_P,
                                   Target => Cv_Arr_P);

   type Cv_Subdiv_2d_Point_Location is range -2 .. 2;
   Cv_Ptloc_Error : Cv_Subdiv_2d_Point_Location := -2;
   Cv_Ptloc_Outside_Rect : Cv_Subdiv_2d_Point_Location := -1;
   Cv_Ptloc_Inside : Cv_Subdiv_2d_Point_Location := 0;
   Cv_Ptloc_Vertex : Cv_Subdiv_2d_Point_Location := 1;
   Cv_Ptloc_On_Edge : Cv_Subdiv_2d_Point_Location := 2;

   type Cv_Next_Edge_Type is new Integer;
   Cv_Next_Around_Org : constant Cv_Next_Edge_Type := 16#00#;
   Cv_Prev_Around_Right : constant Cv_Next_Edge_Type := 16#02#;
   Cv_Prev_Around_Org : constant Cv_Next_Edge_Type := 16#11#;
   Cv_Next_Around_Left : constant Cv_Next_Edge_Type := 16#13#;
   Cv_Prev_Around_Left : constant Cv_Next_Edge_Type := 16#20#;
   Cv_Next_Around_Dst : constant Cv_Next_Edge_Type := 16#22#;
   Cv_Next_Around_Right : constant Cv_Next_Edge_Type := 16#31#;
   Cv_Prev_Around_Dst : constant Cv_Next_Edge_Type := 16#33#;

   -- get the next edge with the same origin point (counterwise)
   function Cv_Subdiv2d_Next_Edge (Edge : Cv_Quad_Edge_2d) return Cv_Quad_Edge_2d_P;

   -- Contour approximation algorithms
   Cv_Poly_Approx_Dp : constant := 0;

   -- Shape matching methods
   Cv_Contours_Match_I1 : constant := 1;
   Cv_Contours_Match_I2 : constant := 2;
   Cv_Contours_Match_I3 : constant := 3;

   -- Shape orientation
   Cv_Clockwise         : constant := 1;
   Cv_Counter_Clockwise : constant := 2;

   type Cv_Convexity_Defect is record
      Start_Point : access Cv_Point;
      End_Point   : access Cv_Point;
      Depth_Point : access Cv_Point;
      Depth       : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Convexity_Defect);

   -- Histogram comparison methods
   type Hist_Compare_Method is new Integer;
   Cv_Comp_Correl : constant Hist_Compare_Method := 0;
   Cv_Comp_Chisqr : constant Hist_Compare_Method := 1;
   Cv_Comp_Intersect : constant Hist_Compare_Method := 2;
   Cv_Comp_Bhattacharyya : constant Hist_Compare_Method := 3;

   -- Mask size for distance transform
   Cv_Dist_Mask_3       : constant := 3;
   Cv_Dist_Mask_5       : constant := 5;
   Cv_Dist_Mask_Precise : constant := 0;

   -- Distance types for Distance Transform and M-estimators
   Cv_Dist_User : constant := -1;
   Cv_Dist_L1 : constant := 1;
   Cv_Dist_L2 : constant := 2;
   Cv_Dist_C : constant := 3;
   Cv_Dist_L12 : constant := 4;
   Cv_Dist_Fair : constant := 5;
   Cv_Dist_Welsch : constant := 6;
   Cv_Dist_Huber : constant := 7;

   -- Threshold types
   type Threshold_Type is new Integer;
   Cv_Thresh_Binary : constant Threshold_Type := 0;
   Cv_Thresh_Binary_Inv : constant Threshold_Type := 1;
   Cv_Thresh_Trunc : constant Threshold_Type := 2;
   Cv_Thresh_Tozero : constant Threshold_Type := 3;
   Cv_Thresh_Tozero_Inv : constant Threshold_Type := 4;
   Cv_Thresh_Mask : constant Threshold_Type := 7;
   Cv_Thresh_Otsu : constant Threshold_Type := 8;

   -- Enumeration for Adaptive Method...
   type Adaptive_Method is new Integer;
   Cv_Adaptive_Thresh_Mean_C : constant Adaptive_Method := 0;
   Cv_Adaptive_Thresh_Gaussian_C : constant Adaptive_Method := 1;

   -- FloodFill flags
   Cv_Floodfill_Fixed_Range : constant := 16#0001_0000#;
   Cv_Floodfill_Mask_Only   : constant := 16#0002_0000#;

   -- Canny edge detector flags
   Cv_Canny_L2_Gradient : constant := 16#8000_0000#;

   -- Variants of a Hough transform
   Cv_Hough_Standard : constant := 0;
   Cv_Hough_Probabilistic : constant := 1;
   Cv_Hough_Multi_Scale : constant := 2;
   Cv_Hough_Gradient : constant := 3;

   -- Fast search data structure
   type Cv_Feature_Tree is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Feature_Tree);
   type Cv_Feature_Tree_P is access all Cv_Feature_Tree;

   type Cv_Lsh is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Lsh);
   type Cv_Lsh_P is access Cv_Lsh;

   type Cv_Lsh_Operations is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Lsh_Operations);
   type Cv_Lsh_Operations_P is access all cv_Lsh_Operations;

   -----------------------------------------------------------------------------
   -- Mmoved
   -----------------------------------------------------------------------------
   type Border_Type is new Integer;
   Ipl_Border_Constant : constant Border_Type := 0;
   Ipl_Border_Replicate : constant Border_Type := 1;
   Ipl_Border_Reflect : constant Border_Type := 2; --does not work
   Ipl_Border_Wrap : constant Border_Type := 3; -- does not work
end Imgproc;
