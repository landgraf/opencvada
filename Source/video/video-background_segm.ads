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
-- video-background_segm.ads - video-background_segm.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

with Core;
use Core;
with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Pointers;

package Video.Background_Segm is
--

   -----------------------------------------------------------------------------
   --
   -----------------------------------------------------------------------------
   Cv_Bg_Model_Fgd        : constant := 0;
   Cv_Bg_Model_Mog        : constant := 1; -- "Mixture of Gaussians"
   Cv_Bg_Model_Fgd_Simple : constant := 2;

   type Cv_Bg_Stat_Model;
   type Cv_Bg_Stat_Model_P is access Cv_Bg_Stat_Model;

   type Cv_Release_Bg_Stat_Model_Func is access procedure (Bg_Model : access Cv_Bg_Stat_Model_P);

   type Cv_Update_Bg_Stat_Model_Func is access function (Curr_Frame    : Ipl_Image_P;
                                                         Bg_Model      : Cv_Bg_Stat_Model_P;
                                                         Learning_Rate : Long_Float)
                                                         return Integer;

   type Cv_Bg_Stat_Model is record
      Model_Type         : Integer;           -- Type of BG model
      Release            : Cv_Release_Bg_Stat_Model_Func;
      Update             : Cv_Update_Bg_Stat_Model_Func;
      Background         : Ipl_Image_P;       -- 8UC3 reference background image
      Foreground         : Ipl_Image_P;       -- 8UC1 foreground image
      Layers             : Cv_Ipl_Image_P_Pointer; -- 8UC3 reference background image, can be null
      Layer_Count        : Integer;           -- Can be zero
      Storage            : Cv_Mem_Storage_P;  -- Storage for foreground_regions
      Foreground_Regions : Cv_Seq_P;          -- Foreground object contours
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Bg_Stat_Model);

   --     Releases memory used by BGStatModel
   procedure Cv_Release_Bg_Stat_Model (Bg_Model : access Cv_Bg_Stat_Model);

   --     Updates statistical model and returns number of found foreground
   --     regions
   function Cv_Update_Bg_Stat_Model (Current_Frame : Ipl_Image_P;
                                     Bg_Model      : Cv_Bg_Stat_Model_P;
                                     Learning_Rate : Long_Float := -1.0)
                                     return Integer;

   --     Performs FG post-processing using segmentation
   --     (all pixels of a region will be classified as foreground if majority
   --     of pixels of the region are FG).
   --     parameters:
   --        segments - pointer to result of segmentation (for example
   --                   MeanShiftSegmentation)
   --        bg_model - pointer to CvBGStatModel structure
   procedure Cv_Refine_Foreground_Mask_By_Segm (Segments : Cv_Seq_P;
                                                Bg_Mode  : Cv_Bg_Stat_Model_P);

   --     Common use change detection function
   function Cv_Change_Detection (Prev_Frame  : Ipl_Image_P;
                                 Curr_Frame  : Ipl_Image_P;
                                 Change_Mask : Ipl_Image_P)
                                 return Integer;

   -- Interface of ACM MM2003 algorithm ----------------------------------------
   -- Default parameters of foreground detection algorithm:
   Cv_Bgfg_Fgd_Lc   : constant := 128;
   Cv_Bgfg_Fgd_N1c  : constant := 15;
   Cv_Bgfg_Fgd_N2c  : constant := 25;

   Cv_Bgfg_Fgd_Lcc  : constant := 64;
   Cv_Bgfg_Fgd_N1cc : constant := 25;
   Cv_Bgfg_Fgd_N2cc : constant := 40;

   -- Background reference image update parameter:
   Cv_Bgfg_Fgd_Alpha_1 : constant := 0.1;

   -- Stat model update paramater
   -- 0.002f ~ 1K frame(~45sec), 0.005 ~ 18sec (if 25fps and absolutely static BG)
   Cv_Bgfg_Fgd_Alpha_2 : constant := 0.005;

   -- Start value for alpha parameter (to fast initiate statistic model)
   Cv_Bgfg_Fgd_Alpha_3         : constant := 0.1;
   Cv_Bgfg_Fgd_Delta           : constant := 2;
   Cv_Bgfg_Fgd_T               : constant := 0.9;
   Cv_Bgfg_Fgd_Minarea         : constant := 15.0;
   Cv_Bgfg_Fgd_Bg_Update_Tresh : constant := 0.5;

   --     See the above-referenced Li/Huang/Gu/Tian paper
   --     for a full description of these background-model
   --     tuning parameters.
   --
   --     Nomenclature:  'c'  = "color", a three-component red/green/blue vector.
   --                           We use histograms of these to model the range of
   --                           colors we've seen at a given background pixel.
   --
   --                    'cc' = "color co-occurrence", a six-component vector giving
   --                           RGB color for both this frame and preceding frame.
   --                           We use histograms of these to model the range of
   --                           color CHANGES we've seen at a given background pixel.
   type Cv_Fgd_Stat_Model_Params is record
      Lc                   : Integer; -- Quantized levels per color component. Power of two, typically 32, 64 or 128.
      N1c                  : Integer; -- Number of color vectors used to model normal background color variation at a given pixel.
      N2c                  : Integer; -- Number of color vectors retained at given pixel. must > N1c, typically ~ 5/3 of N1c.
      -- Used to allow the first N1c vectors to adapt over time to changing background.

      Lcc                  : Integer; -- Quantized levels per color co-occurrence component. Power of two, typically 16, 32 or 64.
      N1cc                 : Integer; -- Number of color co-occurrence vectors used to model normal background color variation at a given pixel.
      N2cc                 : Integer; -- Number of color co-occurrence vectors retained at given pixel. Must be > N1cc, typically ~ 5/3 of N1cc.
      -- Used to allow the first N1cc vectors to adapt over time to changing background.

      Is_Obj_Without_Holes : Integer; -- If TRUE we ignore holes within foreground blobs. Defaults to TRUE.
      Perform_Morphing     : Integer; -- Number of erode-dilate-erode foreground-bleb cleanup iterations.

      Alpha1               : Float; -- How quickly we forget old background pixel values seen. Typically set to 0.1.
      Alpha2               : Float; -- "Controls speed of feature learning". Depends on T. Typical value circa 0.005.
      Alpha3               : Float; -- Alternate to alpha2, used (e.g.) for quicker initial convergence. Typical value 0.1.

      Deltta               : Float; -- Affects color and color co-occurrence quantization, typically set to 2.
      T                    : Float; -- "A percentage value which determines when new features can be recognizes as new background." (Typically 0.9).
      Min_Area             : Float; -- Discards foreground blobs whose bounding box is smaller than this threshold.
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Fgd_Stat_Model_Params);
   type Cv_Fgd_Stat_Model_Params_P is access Cv_Fgd_Stat_Model_Params;

   type Cv_Bg_Pixel_C_Stat_Table is record
      Pv  : Float;
      Pvb : Float;
      V   : Cv_8u_Array (1 .. 3);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Bg_Pixel_C_Stat_Table);
   type Cv_Bg_Pixel_C_Stat_Table_P is access Cv_Bg_Pixel_C_Stat_Table;

   type Cv_Bg_Pixel_Cc_Stat_Table is record
      Pv  : Float;
      Pvb : Float;
      V   : Cv_8u_Array (1 .. 6);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Bg_Pixel_Cc_Stat_Table);
   type Cv_Bg_Pixel_Cc_Stat_Table_P is access Cv_Bg_Pixel_Cc_Stat_Table;

   type Cv_Bg_Pixel_Stat is record
      Pbc                  : Float;
      Pbcc                 : Float;
      Ctable               : Cv_Bg_Pixel_C_Stat_Table_P;
      Cctable              : Cv_Bg_Pixel_Cc_Stat_Table_P;
      Is_Trained_St_Model  : Unsigned_8; -- uchar
      Is_Trained_Dyn_Model : Unsigned_8; -- uchar
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Bg_Pixel_Stat);
   type Cv_Bg_Pixel_Stat_P is access Cv_Bg_Pixel_Stat;

   type Cv_Fgd_Stat_Model is record
      Model_Type         : Integer;           -- Type of BG model
      Release            : Cv_Release_Bg_Stat_Model_Func;
      Update             : Cv_Release_Bg_Stat_Model_Func;
      Background         : Ipl_Image_P;       -- 8UC3 reference background image
      Foreground         : Ipl_Image_P;       -- 8UC1 foreground image
      Layers             : Cv_Ipl_Image_P_Pointer; -- 8UC3 reference background image, can be null
      Layer_Count        : Integer;           -- Can be zero
      Storage            : Cv_Mem_Storage_P;  -- Storage for foreground_regions
      Foreground_Regions : Cv_Seq_P;          -- Foreground object contours

      Pixel_Stat         : Cv_Bg_Pixel_Stat_P;
      Ftd                : Ipl_Image_P;
      Fbd                : Ipl_Image_P;
      Prev_Frame         : Ipl_Image_P;
      Params             : Cv_Fgd_Stat_Model_Params;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Fgd_Stat_Model);
   type Cv_Fgd_Stat_Model_P is access Cv_Fgd_Stat_Model;


   function Cv_Create_Fgd_Stat_Model (First_Frame : Ipl_Image_P;
                                      Parameters  : Cv_Fgd_Stat_Model_P := null)
                                      return Cv_Bg_Stat_Model_P;

   --     Interface of Gaussian mixture algorithm
   --
   --     "An improved adaptive background mixture model for real-time tracking with shadow detection"
   --     P. KadewTraKuPong and R. Bowden,
   --     Proc. 2nd European Workshp on Advanced Video-Based Surveillance Systems, 2001."
   --     http://personal.ee.surrey.ac.uk/Personal/R.Bowden/publications/avbs01/avbs01.pdf

   --     Note: MOG = "Mixture of Gaussians"
   Cv_Bgfg_Mog_Max_Ngaussians : constant := 500;

   -- Default parameters of gaussian background detection algorithm
   Cv_Bgfg_Mog_Background_Threshold : constant := 0.7; -- Threshold sum of weights for background test.
   Cv_Bgfg_Mog_Std_Threshold        : constant := 2.5; -- Lambda=2.5 is 99%
   Cv_Bgfg_Mog_Window_Size          : constant := 200; -- Learning rate; alpha = 1/CV_GBG_WDINWO_SIZE
   Cv_Bgfg_Mog_Ngaussians           : constant := 5;   -- = K = Number of Gaussians in mixture
   Cv_Bgfg_Mog_Weight_Init          : constant := 0.05;
   Cv_Bgfg_Mog_Sigma_Init           : constant := 30;
   Cv_Bgfg_Mog_Minarea              : constant := 15.0;

   Cv_Bgfg_Mog_Ncolors              : constant := 3;

   type Cv_Gauss_Bg_Stat_Model_Params is record
      Win_Size      : Integer;
      N_Gauss       : Integer;
      Bg_Threshold  : Long_Float;
      Std_Threshold : Long_Float;
      Min_Area      : Long_Float;
      Weight_Init   : Long_Float;
      Variance_Init : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Gauss_Bg_Stat_Model_Params);
   type Cv_Gauss_Bg_Stat_Model_Params_P is access Cv_Gauss_Bg_Stat_Model_Params;

   subtype Cv_64f_Array_Ncolors is Cv_64f_Array (1 .. Cv_Bgfg_Mog_Ncolors);

   type Cv_Gauss_Bg_Values is record
      Match_Sum : Integer;
      Weight    : Long_Float;
      Variance  : Cv_64f_Array_Ncolors;
      Mean      : Cv_64f_Array_Ncolors;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Gauss_Bg_Values);
   type Cv_Gauss_Bg_Values_P is access Cv_Gauss_Bg_Values;

   type Cv_Gauss_Bg_Points is record
      G_Values : Cv_Gauss_Bg_Values_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Gauss_Bg_Points);
   type Cv_Gauss_Bg_Points_P is access Cv_Gauss_Bg_Points;

   type Cv_Gauss_Bg_Model is record
      Model_Type         : Integer;           -- Type of BG model
      Release            : Cv_Release_Bg_Stat_Model_Func;
      Update             : Cv_Release_Bg_Stat_Model_Func;
      Background         : Ipl_Image_P;       -- 8UC3 reference background image
      Foreground         : Ipl_Image_P;       -- 8UC1 foreground image
      Layers             : Cv_Ipl_Image_P_Pointer; -- 8UC3 reference background image, can be null
      Layer_Count        : Integer;           -- Can be zero
      Storage            : Cv_Mem_Storage_P;  -- Storage for foreground_regions
      Foreground_Regions : Cv_Seq_P;          -- Foreground object contours

      Params             : Cv_Gauss_Bg_Stat_Model_Params;
      G_Point            : Cv_Gauss_Bg_Points_P;
      Count_Frames       : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Gauss_Bg_Model);
   type Cv_Gauss_Bg_Model_P is access Cv_Gauss_Bg_Model;

   --     Creates Gaussian mixture background model
   function Cv_Create_Gaussian_Bg_Model (First_Frame : Ipl_Image_P;
                                         Parameters  : Cv_Gauss_Bg_Stat_Model_Params_P := null)
                                         return Cv_Bg_Stat_Model_P;

   type Cv_Bg_Code_Book_Elem;
   type Cv_Bg_Code_Book_Elem_P is access Cv_Bg_Code_Book_Elem;

   type Cv_Bg_Code_Book_Elem_P_Array is array (Integer range <>) of aliased Cv_Bg_Code_Book_Elem_P;

   package C_Bg_Code_Book_Elem_P_Arr_Ptr is
     new Interfaces.C.Pointers (Integer, Cv_Bg_Code_Book_Elem_P, Cv_Bg_Code_Book_Elem_P_Array, null);
   use type C_Bg_Code_Book_Elem_P_Arr_Ptr.Pointer;
   subtype Cv_Bg_Code_Book_Elem_Pointer is C_Bg_Code_Book_Elem_P_Arr_Ptr.Pointer;

   type Cv_Bg_Code_Book_Elem is record
      Next          : Cv_Bg_Code_Book_Elem_P;
      T_Last_Update : Integer;
      Stale         : Integer;
      Box_Min       : Cv_8u_Array (1 .. 3);
      Box_Max       : Cv_8u_Array (1 .. 3);
      Learn_Min     : Cv_8u_Array (1 .. 3);
      Learn_Max     : Cv_8u_Array (1 .. 3);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Bg_Code_Book_Elem);

   type Cv_Bg_Code_Book_Model is record
      Size      : Cv_Size;
      T         : Integer;
      Cb_Bounds : Cv_8u_Array (1 .. 3);
      Mod_Min   : Cv_8u_Array (1 .. 3);
      Mod_Max   : Cv_8u_Array (1 .. 3);
      Cb_Map    : Cv_Bg_Code_Book_Elem_Pointer;
      Storage   : Cv_Mem_Storage_P;
      Free_List : Cv_Bg_Code_Book_Elem_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Bg_Code_Book_Model);
   type Cv_Bg_Code_Book_Model_P is access Cv_Bg_Code_Book_Model;

   function Cv_Create_Bg_Code_Book_Model return Cv_Bg_Code_Book_Model_P;
   procedure Cv_Release_Bg_Code_Book_Model (Model : access Cv_Bg_Code_Book_Model_P);

   procedure Cv_Bg_Code_Book_Update (Model : Cv_Bg_Code_Book_Model_P;
                                     Image : Cv_Arr_P;
                                     Roi   : Cv_Rect := Cv_Create_Rect (0, 0, 0, 0);
                                     Mask  : Cv_Arr_P := null);

   function Cv_Bg_Code_Book_Diff (Model  : Cv_Bg_Code_Book_Model_P;
                                  Image  : Cv_Arr_P;
                                  Fgmask : Cv_Arr_P;
                                  Roi    : Cv_Rect := Cv_Create_Rect (0, 0, 0, 0))
                                  return Integer;

   procedure Cv_Bg_Code_Book_Clear_Stale (Model        : Cv_Bg_Code_Book_Model_P;
                                          Stale_Thresh : Integer;
                                          Roi          : Cv_Rect := Cv_Create_Rect (0, 0, 0, 0);
                                          Mask         : Cv_Arr_P := null);

   function Cv_Segment_Fg_Mask (Fgmask      : Cv_Arr_P;
                                Poly1_Hull0 : Integer := 1;
                                Perim_Scale : Float := 4.0;
                                Storage     : Cv_Mem_Storage_P := null;
                                Offset      : Cv_Point := Cv_Create_Point (0, 0))
                                return Cv_Seq_P;
private
   --
   pragma Import (C, Cv_Release_Bg_Stat_Model, "cvReleaseBGStatModel");
   pragma Import (C, Cv_Update_Bg_Stat_Model, "cvUpdateBGStatModel");
   pragma Import (C, Cv_Refine_Foreground_Mask_By_Segm, "cvRefineForegroundMaskBySegm");
   pragma Import (C, Cv_Change_Detection, "cvChangeDetection");
   --
   pragma Import (C, Cv_Create_Fgd_Stat_Model, "cvCreateFGDStatModel");
   pragma Import (C, Cv_Create_Gaussian_Bg_Model, "cvCreateGaussianBGModel");

   pragma Import (C, Cv_Create_Bg_Code_Book_Model, "cvCreateBGCodeBookModel");
   pragma Import (C, Cv_Release_Bg_Code_Book_Model, "cvReleaseBGCodeBookModel");
   pragma Import (C, Cv_Bg_Code_Book_Update, "cvBGCodeBookUpdate");
   pragma Import (C, Cv_Bg_Code_Book_Diff, "cvBGCodeBookDiff");
   pragma Import (C, Cv_Bg_Code_Book_Clear_Stale, "cvBGCodeBookClearStale");
   pragma Import (C, Cv_Segment_Fg_Mask, "cvSegmentFGMask");

end Video.Background_Segm;
