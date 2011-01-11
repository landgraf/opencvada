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
-- types_c.ads - types_c.h
-- Comments, Information, Other
-----------------------------------------------------------------------

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Numerics;
with System;
pragma Warnings (Off);
with System.CRTL; use System.CRTL;
pragma Warnings (On);
with Ada.Numerics.Generic_Elementary_Functions;
limited with Imgproc;


package Core is
--

   -----------------------------------------------------------------------------
   -- Ada stuff
   -----------------------------------------------------------------------------
   type Ipl_Image;
   type Cv_Set;
   type Cv_Point;
   type Cv_File_Node;
   type Cv_File_Node_P is access Cv_File_Node;
   -- Moved since we use them early.
   CV_8U       : constant := 0;
   CV_8S       : constant := 1;
   CV_16U      : constant := 2;
   CV_16S      : constant := 3;
   CV_32S      : constant := 4;
   CV_32F      : constant := 5;
   CV_64F      : constant := 6;
   CV_USRTYPE1 : constant := 7;

   type Float_P is access Float;

   type Cv_8u_Array is array (Integer range <>) of aliased Unsigned_8;
   type Cv_8s_Array is array (Integer range <>) of aliased Integer_8;
   type Cv_16u_Array is array (Integer range <>) of aliased Unsigned_16;
   type Cv_16s_Array is array (Integer range <>) of aliased Integer_16;
   type Cv_32s_Array is array (Integer range <>) of aliased Integer;
   type Cv_32u_Array is array (Integer range <>) of aliased Unsigned_32;
   type Cv_32f_Array is array (Integer range <>) of aliased Float;
   type Cv_64f_Array is array (Integer range <>) of aliased Long_Float;

   type Cv_8u_2d_Array is array (Integer range <>, Integer range <>) of aliased Unsigned_8;
   type Cv_8s_2d_Array is array (Integer range <>, Integer range <>) of aliased Integer_8;
   type Cv_16u_2d_Array is array (Integer range <>, Integer range <>) of aliased Unsigned_16;
   type Cv_16s_2d_Array is array (Integer range <>, Integer range <>) of aliased Integer_16;
   type Cv_32s_2d_Array is array (Integer range <>, Integer range <>) of aliased Integer;
   type Cv_32u_2d_Array is array (Integer range <>, Integer range <>) of aliased Unsigned_32;
   type Cv_32f_2d_Array is array (Integer range <>, Integer range <>) of aliased Float;
   type Cv_64f_2d_Array is array (Integer range <>, Integer range <>) of aliased Long_Float;

   type Cv_8u_Array_P is access Cv_8u_Array;
   type Cv_8s_Array_P is access Cv_8s_Array;
   type Cv_16u_Array_P is access Cv_16u_Array;
   type Cv_16s_Array_P is access Cv_16s_Array;
   type Cv_32s_Array_P is access Cv_32s_Array;
   type Cv_32u_Array_P is access Cv_32u_Array;
   type Cv_32f_Array_P is access Cv_32f_Array;
   type Cv_64f_Array_P is access Cv_64f_Array;

   package Cv_8u_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Unsigned_8, Cv_8u_Array, 0);
   use type Cv_8u_Pointer_Pkg.Pointer;
   subtype Cv_8u_Pointer is Cv_8u_Pointer_Pkg.Pointer;

   type Cv_8u_Pointer_Array is array (Integer range <>) of Cv_8u_Pointer;
   type Cv_8u_Pointer_Array_P is access Cv_8u_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_8u_2d_Array)
                           return Cv_8u_Pointer_Array;

   package Cv_8s_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Integer_8, Cv_8s_Array, 0);
   use type Cv_8s_Pointer_Pkg.Pointer;
   subtype Cv_8s_Pointer is Cv_8s_Pointer_Pkg.Pointer;

   type Cv_8s_Pointer_Array is array (Integer range <>) of Cv_8s_Pointer;
   type Cv_8s_Pointer_Array_P is access Cv_8s_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_8s_2d_Array)
                           return Cv_8s_Pointer_Array;

   package Cv_16u_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Unsigned_16, Cv_16u_Array, 0);
   use type Cv_16u_Pointer_Pkg.Pointer;
   subtype Cv_16u_Pointer is Cv_16u_Pointer_Pkg.Pointer;

   type Cv_16u_Pointer_Array is array (Integer range <>) of Cv_16u_Pointer;
   type Cv_16u_Pointer_Array_P is access Cv_16u_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_16u_2d_Array)
                           return Cv_16u_Pointer_Array;

   package Cv_16s_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Integer_16, Cv_16s_Array, 0);
   use type Cv_16s_Pointer_Pkg.Pointer;
   subtype Cv_16s_Pointer is Cv_16s_Pointer_Pkg.Pointer;

   type Cv_16s_Pointer_Array is array (Integer range <>) of Cv_16s_Pointer;
   type Cv_16s_Pointer_Array_P is access Cv_16s_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_16s_2d_Array)
                           return Cv_16s_Pointer_Array;

   package Cv_32s_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Integer, Cv_32s_Array, 0);
   use type Cv_32s_Pointer_Pkg.Pointer;
   subtype Cv_32s_Pointer is Cv_32s_Pointer_Pkg.Pointer;

   type Cv_32s_Pointer_Array is array (Integer range <>) of Cv_32s_Pointer;
   type Cv_32s_Pointer_Array_P is access Cv_32s_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_32s_2d_Array)
                           return Cv_32s_Pointer_Array;

   package Cv_32f_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Float, Cv_32f_Array, 0.0);
   use type Cv_32f_Pointer_Pkg.Pointer;
   subtype Cv_32f_Pointer is Cv_32f_Pointer_Pkg.Pointer;

   type Cv_32f_Pointer_Array is array (Integer range <>) of Cv_32f_Pointer;
   type Cv_32f_Pointer_Array_P is access Cv_32f_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_32f_2d_Array)
                           return Cv_32f_Pointer_Array;

   package Cv_64f_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Long_Float , Cv_64F_Array, 0.0);
   subtype Cv_64f_Pointer is Cv_64f_Pointer_Pkg.Pointer;
   type Cv_64f_Pointer_P is access Cv_64f_Pointer;

   type Cv_64F_Pointer_Array is array (Integer range <>) of Cv_64f_Pointer;
   type Cv_64F_Pointer_Array_P is access Cv_64F_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_64f_2d_Array)
                           return Cv_64f_Pointer_Array;

   package C_Strings renames Interfaces.C.Strings;

   type String_C is new String;

   Null_String_C : constant String_C (1 .. 0) := "";
   function "+" (Right : String) return String_C;

   subtype Void_P is System.Address;
   type Uchar is range 0 .. 255;
   for Uchar'Size use 8;

   type Cv_String_Array is array (Integer range <>) of aliased Interfaces.C.Strings.Chars_Ptr;

   -- fix this
   package Cv_String_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Interfaces.C.Strings.Chars_Ptr, Cv_String_Array, null);
   use type Cv_String_Pointer_Pkg.Pointer;
   subtype Cv_String_Pointer is Cv_String_Pointer_Pkg.Pointer;

   package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
   package Long_Float_Numerics is new Ada.Numerics.Generic_Elementary_Functions (Long_Float);

   -----------------------------------------------------------------------------

   --   /* CvArr* is used to pass arbitrary
   --   * array-like data structures
   --   * into functions where the particular
   --   * array type is recognized at runtime:
   --   */
   type Cv_Arr is new Integer;
   type Cv_Arr_P is access all Cv_Arr;
   type Cv_Void_P is access all Cv_Arr;

   type Cv_Arr_P_Array is array (Integer range <>) of aliased Cv_Arr_P;
   type Cv_Arr_P_Array_P is access Cv_Arr_P_Array;

   package Cv_Arr_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Arr_P, Cv_Arr_P_Array, null);
   use type Cv_Arr_Pointer_Pkg.Pointer;
   subtype C_Cv_Arr_P_Ptr is Cv_Arr_Pointer_Pkg.Pointer;
   type Cv_Arr_Pointer is new C_Cv_Arr_P_Ptr;


   type Suf is (S, U, F);
   type Cv_32_Suf (Option : Suf := S) is
      record
         case Option is
            when S =>
               I : Integer;
            when U =>
               U : Unsigned_32;
            when F =>
               F : Float;
         end case;
      end record;
   pragma Unchecked_Union (Cv_32_Suf);
   pragma Convention (C_Pass_By_Copy, Cv_32_Suf);

   type Cv_64_Suf (Option : Suf := S) is
      record
         case Option is
            when S =>
               I : Long_Integer;
            when U =>
               U : Unsigned_64;
            when F =>
               F : Long_Float;
         end case;
      end record;
   pragma Unchecked_Union (Cv_64_Suf);
   pragma Convention (C_Pass_By_Copy, Cv_64_Suf);

   type Cv_Status is new Integer;

   --     /* this part of CVStatus is compatible with IPLStatus
   --    Some of below symbols are not [yet] used in OpenCV
   --*/

   CV_StsOk : constant Cv_Status := 0;  --/* everithing is ok                */
   CV_StsBackTrace : constant Cv_Status := -1 ;  --/* pseudo error for back trace     */

   CV_StsError : constant Cv_Status := -2;  --/* unknown /unspecified error      */
   CV_StsInternal : constant Cv_Status := -3;  --/* internal error (bad state)      */
   CV_StsNoMem : constant Cv_Status := -4;  --/* insufficient memory             */
   CV_StsBadArg : constant Cv_Status := -5;  --/* function arg/param is bad       */
   CV_StsBadFunc : constant Cv_Status := -6;  --/* unsupported function            */
   CV_StsNoConv : constant Cv_Status := -7 ;  --/* iter. didn't converge           */
   CV_StsAutoTrace : constant Cv_Status := -8;  --/* tracing                         */

   CV_HeaderIsNull : constant Cv_Status := -9;  --/* image header is NULL            */
   CV_BadImageSize : constant Cv_Status := -10; --/* image size is invalid           */
   CV_BadOffset : constant Cv_Status := -11; --/* offset is invalid               */
   CV_BadDataPtr : constant Cv_Status := -12; --/**/
   CV_BadStep : constant Cv_Status := -13; --/**/
   CV_BadModelOrChSeq : constant Cv_Status := -14; --/**/
   CV_BadNumChannels : constant Cv_Status := -15; --/**/
   CV_BadNumChannel1U : constant Cv_Status := -16; --/**/
   CV_BadDepth : constant Cv_Status := -17; --/**/
   CV_BadAlphaChannel : constant Cv_Status := -18; --/**/
   CV_BadOrder : constant Cv_Status := -19; --/**/
   CV_BadOrigin : constant Cv_Status := -20; --/**/
   CV_BadAlign : constant Cv_Status := -21; --/**/
   CV_BadCallBack : constant Cv_Status := -22; --/**/
   CV_BadTileSize : constant Cv_Status := -23; --/**/
   CV_BadCOI : constant Cv_Status := -24; --/**/
   CV_BadROISize : constant Cv_Status := -25; --/**/

   CV_MaskIsTiled : constant Cv_Status := -26; --/**/

   CV_StsNullPtr : constant Cv_Status := -27; --/* null pointer */
   CV_StsVecLengthErr : constant Cv_Status := -28; --/* incorrect vector length */
   CV_StsFilterStructContentErr : constant Cv_Status := -29; --/* incorr. filter structure content */
   CV_StsKernelStructContentErr : constant Cv_Status := -30; --/* incorr. transform kernel content */
   CV_StsFilterOffsetErr : constant Cv_Status := -31; --/* incorrect filter ofset value */

   --/*extra for CV */
   CV_StsBadSize : constant Cv_Status := -201; --/* the input/output structure size is incorrect  */
   CV_StsDivByZero : constant Cv_Status := -202; --/* division by zero */
   CV_StsInplaceNotSupported : constant Cv_Status := -203; --/* in-place operation is not supported */
   CV_StsObjectNotFound : constant Cv_Status := -204; ---/* request can't be completed */
   CV_StsUnmatchedFormats : constant Cv_Status := -205; --/* formats of input/output arrays differ */
   CV_StsBadFlag : constant Cv_Status := -206; --/* flag is wrong or not supported */
   CV_StsBadPoint : constant Cv_Status := -207; --/* bad CvPoint */
   CV_StsBadMask : constant Cv_Status := -208; --/* bad format of mask (neither 8uC1 nor 8sC1)*/
   CV_StsUnmatchedSizes : constant Cv_Status := -209; --/* sizes of input/output structures do not match */
   CV_StsUnsupportedFormat : constant Cv_Status := -210; --/* the data format/type is not supported by the function*/
   CV_StsOutOfRange : constant Cv_Status := -211; --/* some of parameters are out of range */
   CV_StsParseError : constant Cv_Status := -212; --/* invalid syntax/structure of the parsed file */
   CV_StsNotImplemented : constant Cv_Status := -213; --/* the requested function/feature is not implemented */
   CV_StsBadMemBlock : constant Cv_Status := -214; --/* an allocated block has been corrupted */
   CV_StsAssert : constant Cv_Status := -215; --/* assertion failed */

   -----------------------------------------------------------------------------
   -- Common macros and inline functions
   -----------------------------------------------------------------------------
   CV_PI : constant := Ada.Numerics.Pi;
   CV_LOG2 : constant := 0.69314718055994530941723212145818;

   --     Converts a floating-point number to an integer.
   function CvRound (Value : Long_Float) return Integer;
   function CvRound (Value : Float) return Integer;
   function CvFloor (Value : Long_Float) return Integer;
   function CvCeil (Value : Long_Float) return Integer;

   --     Calculates the inverse square root.
   function CvInvSqrt (Value : Float)
                       return Float;
   --     Calculates the square root.
   function CvSqrt (Value : Float) return Float;

   --     Determines if the argument is Not A Number.
   function CvIsNaN (Value : Long_Float)
                     return Integer;

   --     Determines if the argument is Infinity.
   function CvIsInf (Value : Long_Float)
                     return Integer;

   -----------------------------------------------------------------------------
   -- Random number generation
   -----------------------------------------------------------------------------
   subtype Cv_RNG is Integer_64;

   --     Initializes a random number generator state.
   function CvRNG (Seed : Integer_64 := -1)
                   return Cv_RNG;

   --     Returns a 32-bit unsigned integer and updates RNG.
   function CvRandInt (Rng : access Cv_RNG)
                       return Unsigned_32;

   --     Returns a floating-point random number and updates RNG.
   function CvRandReal (Rng : access Integer_64)
                        return Long_Float;

   -----------------------------------------------------------------------------
   -- Image type Ipl_Image
   -----------------------------------------------------------------------------
   IPL_DEPTH_SIGN : constant Unsigned_32 := 16#80000000#;

   IPL_DEPTH_1U   : constant Unsigned_32 := 1;
   IPL_DEPTH_8U   : constant Unsigned_32 := 8;
   IPL_DEPTH_16U  : constant Unsigned_32 := 16;
   IPL_DEPTH_32F  : constant Unsigned_32 := 32;

   IPL_DEPTH_8S   : constant Unsigned_32 := IPL_DEPTH_SIGN or Unsigned_32 (8);
   IPL_DEPTH_16S  : constant Unsigned_32 := IPL_DEPTH_SIGN or Unsigned_32 (16);
   IPL_DEPTH_32S  : constant Unsigned_32 := IPL_DEPTH_SIGN or Unsigned_32 (32);

   IPL_DATA_ORDER_PIXEL : constant Unsigned_32 := 0;
   IPL_DATA_ORDER_PLANE : constant Unsigned_32 := 1;

   IPL_ORIGIN_TL     : constant Unsigned_32 := 0;
   IPL_ORIGIN_BL     : constant Unsigned_32 := 1;

   IPL_ALIGN_4BYTES  : constant Unsigned_32 := 4;
   IPL_ALIGN_8BYTES  : constant Unsigned_32 := 8;
   IPL_ALIGN_16BYTES : constant Unsigned_32 := 16;
   IPL_ALIGN_32BYTES : constant Unsigned_32 := 32;

   IPL_ALIGN_DWORD   : constant Unsigned_32 := IPL_ALIGN_4BYTES;
   IPL_ALIGN_QWORD   : constant Unsigned_32 := IPL_ALIGN_8BYTES;

   IPL_BORDER_CONSTANT  : constant Unsigned_32 := 0;
   IPL_BORDER_REPLICATE : constant Unsigned_32 := 1;
   IPL_BORDER_REFLECT   : constant Unsigned_32 := 2;
   IPL_BORDER_WRAP      : constant Unsigned_32 := 3;

   -- Ipl_Tile_Info ------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Ipl_Tile_Info is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, Ipl_Tile_Info);
   type Ipl_Tile_Info_P is access Ipl_Tile_Info;
   pragma Convention (C, Ipl_Tile_Info_P);

   -- Ipl_ROI ------------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Ipl_ROI is record
      Coi      : Integer;
      Height   : Integer;
      Width    : Integer;
      X_Offset : Integer;
      Y_Offset : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Ipl_ROI);
   type Ipl_ROI_P is access Ipl_ROI;
   pragma Convention (C, Ipl_ROI_P);

   -- Ipl_Image ----------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Ipl_Image_P is access Ipl_Image;
   pragma Convention (C, Ipl_Image_P);
   type Ipl_Image is record
      N_Size            : Integer;
      ID                : Integer;
      N_Channels        : Integer;
      Alpha_Channel     : Integer;
      Depth             : Unsigned_32;
      Color_Model       : Cv_8u_Array (1 .. 4);
      Channel_Seq       : Cv_8u_Array (1 .. 4);
      Data_Order        : Integer;
      Origin            : Integer;
      Align             : Integer;
      Width             : Integer;
      Height            : Integer;
      ROI               : Ipl_ROI_P;
      Mask_ROI          : Ipl_Image_P;
      Image_ID          : Void_P;
      Tile_Info         : Ipl_Tile_Info_P;
      Image_Size        : Integer;
      Image_Data        : Cv_8u_Pointer;
--        Image_Data        : C_Strings.Chars_Ptr; -- Test with a cv_8u_array later, same with Data_Origin
      Width_Step        : Integer;
      Border_Model      : Cv_32s_Array (1 .. 4);
      Border_Const      : Cv_32s_Array (1 .. 4);
      Image_Data_Origin : Cv_8u_Pointer;
--        Image_Data_Origin : C_Strings.Chars_Ptr; -- This might require some wrapper function to return a proper Ada string
   end record;
   pragma Convention (C_Pass_By_Copy, Ipl_Image);
   --   type Ipl_Image_Array is array (Integer range <>) of aliased Ipl_Image;
   type Ipl_Image_P_Array is array (Integer range<>) of aliased Ipl_Image_P;


   package Cv_Ipl_Image_P_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Ipl_Image_P, Ipl_Image_P_Array, null);
   use type Cv_Ipl_Image_P_Pointer_Pkg.Pointer;
   subtype Cv_Ipl_Image_P_Pointer is Cv_Ipl_Image_P_Pointer_Pkg.Pointer;

   type Ipl_Conv_Kernel is
      record
         N_Cols     : Integer;
         N_Rows     : Integer;
         Anchor_X   : Integer;
         Anchor_Y   : Integer;
         Values     : Cv_32U_Array_P;
         N_Shift_R  : Integer;
      end record;
   type Ipl_Conv_Kernel_P is access Ipl_Conv_Kernel;

   type Ipl_Conv_Kernel_FP is
      record
         NCols   : Integer;
         NRows   : Integer;
         AnchorX : Integer;
         AnchorY : Integer;
         Values  : Cv_32F_Array_P;
      end record;
   IPL_IMAGE_HEADER : constant := 1;
   IPL_IMAGE_DATA : constant := 2;
   IPL_IMAGE_ROI : constant := 4;

   IPL_BORDER_REFLECT_101 : constant := 4;
   IPL_IMAGE_MAGIC_VAL : constant := 112; -- ipl_image'size/8

   function CV_IS_IMAGE_HDR (Img : Ipl_Image_P) return Integer;
   function CV_IS_IMAGE (Img : IPL_IMAGE_P) return Integer;

   IPL_DEPTH_64F          : constant := 64;

   -----------------------------------------------------------------------------
   -- Matrix type Cv_Mat
   -----------------------------------------------------------------------------
   CV_CN_MAX        : constant := 512;
   CV_CN_SHIFT      : constant := 3;
   CV_DEPTH_MAX     : constant := 8; -- 1 << CV_CN_SHIFT

   function CV_MAT_DEPTH_MASK return Integer;
   function CV_MAT_DEPTH (M_Type : Integer) return Integer;

   function CV_MAKETYPE (Depth : Integer; Cn : Integer) return Integer;
   function CV_MAKE_TYPE (Depth : Integer; Cn : Integer) return Integer
                          renames CV_MAKETYPE;

   CV_AUTOSTEP : constant := 16#7fff_Ffff#;
   CV_AUTO_STEP : constant := 16#7fffffff#;

   function CV_MAT_CN_MASK return Integer;
   function CV_MAT_CN (Flags : Integer) return Integer;
   CV_MAT_TYPE_MASK : constant := CV_DEPTH_MAX * CV_CN_MAX - 1;
   function CV_MAT_TYPE (Flags : Integer) return Integer;
   CV_MAT_CONT_FLAG_SHIFT : constant Unsigned_32 := 14;
   CV_MAT_CONT_FLAG : constant := 16#0100_0000#; -- 1 << CV_MAT_CONT_FLAG_SHIFT
   function CV_IS_MAT_CONT (Flags : Integer) return Boolean;
   function CV_IS_CONT_MAT (Flags : Integer) return Boolean
                            renames CV_IS_MAT_CONT;
   CV_MAT_TEMP_FLAG_SHIFT : constant Unsigned_32 := 15;
   CV_MAT_TEMP_FLAG       : constant := 16#1000_0000#; -- 1 << CV_MAT_TEMP_FLAG_SHIFT
   function CV_IS_TEMP_MAT (Flags : Integer) return Boolean;

   CV_MAGIC_MASK    : constant := 16#FFFF_0000#;
   CV_MAT_MAGIC_VAL : constant := 16#4242_0000#;
   CV_TYPE_NAME_MAT : constant String := "opencv-matrix";

   type Mat_Type is (Cv_Mat_8u, Cv_Mat_8s, Cv_Mat_16u, Cv_Mat_16s, Cv_Mat_32s, Cv_Mat_32f, Cv_Mat_64f);
   type Mat_Data (Option : Mat_Type := Cv_Mat_32f) is record
      case Option is
         when Cv_Mat_8u =>
            Cv_8u  : Cv_8u_Pointer;
         when Cv_Mat_8s =>
            Cv_8s  : Cv_8s_Pointer;
         when Cv_Mat_16u =>
            Cv_16u : Cv_16u_Pointer;
         when Cv_Mat_16s =>
            Cv_16s : Cv_16s_Pointer;
         when Cv_Mat_32s =>
            Cv_32s : Cv_32s_Pointer;
         when Cv_Mat_32f =>
            Cv_32f : Cv_32f_Pointer;
         when Cv_Mat_64f =>
            Cv_64f : Cv_64f_Pointer;
      end case;
   end record;
   pragma Unchecked_Union (Mat_Data);
   pragma Convention (C_Pass_By_Copy, Mat_Data);

   type Cv_Mat is record
      Mat_Type     : Integer;
      Step         : Integer;
      Refcount     : access Integer := null;
      Hdr_Refcount : Integer := 0;
      Data         : Mat_Data;
      Rows         : Integer;
      Cols         : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Mat);
   type Cv_Mat_P is access Cv_Mat;
   type Cv_Mat_Array_AxB is array (Integer range <>, Integer range <>) of aliased Cv_Mat;

   function CV_IS_MAT_HDR (Mat : Cv_Mat_P) return Integer;

   function CV_IS_MAT (Mat : Cv_Mat_P) return Integer;

   function CV_IS_MASK_ARR (Mat : Cv_Mat_P) return Integer;

   function CV_ARE_TYPES_EQ (Mat1 : Cv_Mat_P;
                             Mat2 : Cv_Mat_P) return Integer;

   function CV_ARE_CNS_EQ (Mat1 : Cv_Mat_P;
                           Mat2 : Cv_Mat_P) return Integer;

   function CV_ARE_DEPTHS_EQ (Mat1 : Cv_Mat_P;
                              Mat2 : Cv_Mat_P) return Integer;

   function CV_ARE_SIZES_EQ (Mat1 : Cv_Mat_P;
                             Mat2 : Cv_Mat_P) return Integer;

   function CV_IS_MAT_CONST (Mat : Cv_Mat_P) return Integer;

   function CV_ELEM_SIZE_1 (E_Type : Integer) return Integer;
   function CV_ELEM_SIZE (E_Type : Integer) return Integer;

   function IPL2CV_DEPTH (Depth : Unsigned_32) return Integer;

   function CvMat (Rows   : Integer;
                   Cols   : Integer;
                   M_Type : Integer;
                   Data   : Mat_Data)
                   return Cv_Mat;

   function CV_MAT_ELEM_PTR_FAST (Mat      : Cv_Mat_P;
                                  Row      : Integer;
                                  Col      : Integer;
                                  Pix_Size : Integer) return Cv_8u_Pointer;

   function CV_MAT_ELEM_PTR (Mat : Cv_Mat_P;
                             Row : Integer;
                             Col : Integer) return Cv_8u_Pointer;

   function CV_MAT_ELEM (Mat      : Cv_Mat_P;
                         Elemtype : Integer;
                         Row      : Integer;
                         Col      : Integer) return Cv_8u_Pointer;

   --     Returns the particular element of single-channel floating-point
   --     matrix.
   function CvmGet (Mat : access Cv_Mat;
                    Row : Integer;
                    Col : Integer)
                    return Long_Float;

   --     Returns a specific element of a single-channel floating-point matrix.
   procedure CvmSet (Mat   : access Cv_Mat;
                     Row   : Integer;
                     Col   : Integer;
                     Value : Long_Float);

   function CvIplDepth (IType : Integer) return Integer;

   -----------------------------------------------------------------------------
   -- Multi-dimensional dense array (CvMatND)
   -----------------------------------------------------------------------------

   CV_MATND_MAGIC_VAL : constant := 16#42430000#;
   CV_TYPE_NAME_MATND : constant String := "opencv-nd-matrix";

   CV_MAX_DIM : constant Integer := 32;
   CV_MAX_DIM_HEAP : constant := 16#10000#;

   type Mat_Dimensions is record
      Size : Integer;
      Step : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Mat_Dimensions);

   type Mat_Dimensions_Array is array (Integer range <>) of Mat_Dimensions;
   --type Size_Array is array (Integer range <>) of Integer;

   type Cv_Mat_ND is record
      Mat_Type : Integer;
      Dims     : Integer;
      Refcount : Cv_32U_Array_P;
      Data     : Mat_Data;
      Dim      : Mat_Dimensions_Array (1 .. CV_MAX_DIM);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Mat_ND);
   type Cv_Mat_ND_P is access Cv_Mat_ND;

   function CV_IS_MATND_HDR (Mat : Cv_Mat_ND_P) return Integer;
   function CV_IS_MATND ( Mat : Cv_Mat_ND_P) return Integer renames CV_IS_MATND_HDR;

   -----------------------------------------------------------------------------
   -- Multi-dimensional sparse array (CvSparseMat)
   -----------------------------------------------------------------------------
   CV_SPARSE_MAT_MAGIC_VAL : constant := 16#42440000#;
   CV_TYPE_NAME_SPARSE_MAT : constant String := "opencv-sparse-matrix";

   type Cv_Sparse_Mat is record
      Mat_Type  : Integer;
      Dims      : Integer;
      Refcount  : access Integer;
      Hashtable : access Integer; -- Note that this is supposed to be a void**
      Heap      : access Cv_Set;
      Hashsize  : Integer;
      Total     : Integer;
      Valoffset : Integer;
      Idxoffset : Integer;
      Size      : Cv_32s_Array (1 .. CV_MAX_DIM);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Sparse_Mat);
   type Cv_Sparse_Mat_P is access Cv_Sparse_Mat;

   function CV_IS_SPARSE_MAT_HDR (Mat : Cv_Sparse_Mat_P) return Integer;
   function CV_IS_SPARSE_MAT (Mat : Cv_Sparse_Mat_P) return Integer renames CV_IS_SPARSE_MAT_HDR;

   type Cv_Sparse_Node;
   type Cv_Sparse_Node_P is access Cv_Sparse_Node;
   type Cv_Sparse_Node is record
      Hashval : Natural;
      Next    : aliased Cv_Sparse_Node_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Sparse_Node);

   type Cv_Sparse_Mat_Iterator is record
      Mat    : aliased Cv_Sparse_Mat_P;
      Node   : aliased Cv_Sparse_Node_P;
      Curidx : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Sparse_Mat_Iterator);
   type Cv_Sparse_Mat_Iterator_P is access Cv_Sparse_Mat_Iterator;

   -- Fix these two!
   --     #define CV_NODE_VAL(mat,node)   ((void*)((uchar*)(node) + (mat)->valoffset))
   --  #define CV_NODE_IDX(mat,node)   ((int*)((uchar*)(node) + (mat)->idxoffset))

   -----------------------------------------------------------------------------
   -- Histogram
   -----------------------------------------------------------------------------
   type Cv_Hist_Type is new Integer;

   CV_HIST_MAGIC_VAL : constant := 16#42450000#;
   CV_HIST_UNIFORM_FLAG : constant := 16#400#;

   --/* indicates whether bin ranges are set already or not */
   CV_HIST_RANGES_FLAG : constant := 16#800#;

   CV_HIST_ARRAY : constant := 0;
   CV_HIST_SPARSE : constant := 1;
   CV_HIST_TREE : constant := CV_HIST_SPARSE;

   --  /* should be used as a parameter only,
   --     it turns to CV_HIST_UNIFORM_FLAG of hist->type */
   CV_HIST_UNIFORM : constant := 1;

   type Thresh_Arr is array (Integer range 0 ..  CV_MAX_DIM, Integer range 0 .. 1) of Float;
   pragma Convention (C, Thresh_Arr);

   Cv_32F_Array_NULL : Cv_32F_Pointer_Array (1 .. 0);
   Cv_32s_Array_Null :  Cv_32s_Array (1 .. 0);


   type Cv_Histogram is
      record
         HistType : Integer;
         Bins     : Cv_Arr_P;
         Thresh   : Thresh_Arr;
         Thresh2  : Cv_32F_Pointer_Array_P;
         Mat      : Cv_Mat_ND;
      end record;
   type Cv_Histogram_P is access Cv_Histogram;

   type Cv_Histogram_P_Array is array (Integer range <>) of aliased Cv_Histogram_P;

   package Cv_Histogram_P_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Histogram_P, Cv_Histogram_P_Array, null);
   use type Cv_Histogram_P_Pointer_Pkg.Pointer;
   subtype Cv_Histogram_P_Pointer is Cv_Histogram_P_Pointer_Pkg.Pointer;

   function CV_IS_HIST (Hist : Cv_Histogram_P) return Integer;
   function CV_IS_UNIFORM_HIST (Hist : Cv_Histogram_P) return Integer;
   function CV_IS_SPARSE_HIST (Hist : Cv_Histogram_P) return Integer;
   function CV_HIST_HAS_RANGES (Hist : Cv_Histogram_P) return Integer;

   -----------------------------------------------------------------------------
   -- Other supplementary data type definitions
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Cv_Rect
   ----------------------------------------------------------------------------
   type Cv_Rect is record
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Rect);
   type Cv_Rect_P is access all Cv_Rect;
   type Cv_Rect_Array is array (Integer range <>) of aliased Cv_Rect;

   function CvRect (X : Integer; Y : Integer; Width : Integer; Height : Integer)
                    return Cv_Rect;
   function CvRectToROI (Rect : Cv_Rect; Coi : Integer) return Ipl_ROI;
   function CvROIToRect (Roi : Ipl_ROI) return Cv_Rect;

   -----------------------------------------------------------------------------
   -- Cv_Term_Criteria
   -----------------------------------------------------------------------------
   CV_TERMCRIT_ITER   : constant := 1;
   CV_TERMCRIT_NUMBER : constant := CV_TERMCRIT_ITER;
   CV_TERMCRIT_EPS    : constant := 2;

   type Cv_Term_Criteria is record
      Term_Type : Integer;
      Max_Iter  : Integer;
      Epsilon   : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Term_Criteria);
   type Cv_Term_Criteria_P is access Cv_Term_Criteria;

   function CvTermCriteria (T_Type  : Integer; Max_Iter : Integer;
                            Epsilon : Long_Float) return Cv_Term_Criteria;

   -----------------------------------------------------------------------------
   -- Cv_Point
   -----------------------------------------------------------------------------
   type Cv_Point is record
      X : Integer;
      Y : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point);
   type Cv_Point_P is access all Cv_Point;
   type Cv_Point_Array is array (Integer range <>) of aliased Cv_Point;
   type Cv_Point_2d_Array is array (Integer range <>, Integer range <>) of aliased Cv_Point;
   Cv_Point_Dummy        : Cv_Point;

   function CvPoint (X : Integer; Y : Integer) return Cv_Point;

   package Cv_Point_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Point, Cv_Point_Array, Cv_Point_Dummy);
   use type Cv_Point_Pointer_Pkg.Pointer;
   subtype Cv_Point_Pointer is Cv_Point_Pointer_Pkg.Pointer;

   type Cv_Point_Pointer_Array is array (Integer range <> ) of Cv_Point_Pointer;

   function To_2d_Pointer (Src : access Cv_Point_2d_Array)
                           return Cv_Point_Pointer_Array;

   type Cv_Point_2D_32f is record
      X : Float;
      Y : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point_2D_32f);
   type Cv_Point_2d_32f_P is access all Cv_Point_2d_32f;
   type Cv_Point_2D_32F_Array is array (Integer range <>) of aliased Cv_Point_2D_32f;
   Cv_Point_2d_32f_Array_Null : Cv_Point_2d_32f_Array (1 .. 0);

   function CvPoint2D32f (X : Long_Float; Y : Long_Float)
                          return Cv_Point_2D_32f;
   function CvPointTo32f (Point : Cv_Point) return Cv_Point_2D_32f;

   function CvPointFrom32f (Point : Cv_Point_2D_32f) return Cv_Point;


   type Cv_Point_3d_32f is record
      X : Float;
      Y : Float;
      Z : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point_3d_32f);
   type Cv_Point_3d_32f_P is access all Cv_Point_3d_32f;
   type Cv_Point_3D_32F_Array is array (Integer range <>) of aliased Cv_Point_3D_32F;
   Cv_Point_3d_32f_Array_Null : Cv_Point_3d_32f_Array (1 .. 0);

   function CvPoint3D32f (X : Long_Float; Y : Long_Float;
                          Z : Long_Float) return Cv_Point_3D_32f;


   type Cv_Point_2d_64f is record
      X : Long_Float;
      Y : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point_2d_64f);
   type Cv_Point_2d_64f_P is access all Cv_Point_2d_64f;
   type Cv_Point_2D_64F_Array is array (Integer range <>) of aliased Cv_Point_2D_64F;
   Cv_Point_2d_64f_Array_Null : Cv_Point_2d_64f_Array (1 .. 0);
   Cv_Point_2d_64f_Dummy : Cv_Point_2d_64f;

   function CvPoint2D64f (X : Long_Float; Y : Long_Float)
                          return Cv_Point_2D_64f;

   type Cv_Point_3d_64f is record
      X : Long_Float;
      Y : Long_Float;
      Z : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point_3d_64f);
   type Cv_Point_3d_64f_P is access all Cv_Point_3d_64f;
   type Cv_Point_3D_64F_Array is array (Integer range <>) of aliased Cv_Point_3D_64F;
   Cv_Point_3D_64F_Array_Null : Cv_Point_3D_64F_Array ( 1 .. 0);
   --     Cv_Point_3d_64f_Dummy : Cv_Point_3d_64f;

   function CvPoint3D64f (X : Long_Float; Y : Long_Float;
                          Z : Long_Float) return Cv_Point_3d_64f;

   --     package C_Point_3d_64f_Arr_Ptr is
   --       new Interfaces.C.Pointers (Integer, Cv_Point_3d_64f, Cv_Point_3d_64f_Array, Cv_Point_3d_64f_Dummy);
   --     use type C_Point_3d_64f_Arr_Ptr.Pointer;
   --     subtype CvPoint_3d_64f_Ptr is C_Point_3d_64f_Arr_Ptr.Pointer;

   -----------------------------------------------------------------------------
   -- Cv_Size & Cv_Box
   -----------------------------------------------------------------------------

   type Cv_Size is record
      Width  : Integer;
      Height : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Size);
   type Cv_Size_P is access Cv_Size;

   function CvSize (Width : Integer; Height : Integer) return Cv_Size;

   type Cv_Size_2d_32f is record
      Width  : Float;
      Height : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Size_2d_32f);

   function CvSize2d32f (Width : Float; Height : Float)
                         return Cv_Size_2d_32f;

   -- Represnation of a 2D box...
   type Cv_Box_2D is
      record
         Center : Cv_Point_2D_32F;
         Size   : Cv_Size_2D_32F;
         Angle  : Float;
      end record;
   type Cv_Box_2D_P is access Cv_Box_2D;

   type Cv_Line_Iterator is
      record
         Ptr         : Cv_Point_Pointer;
         Err         : Integer;
         Plus_Delta  : Integer;
         Minus_Delta : Integer;
         Plus_Step   : Integer;
         Minus_Step  : Integer;
      end record;
   type Cv_Line_Iterator_P is access Cv_Line_Iterator;

   -----------------------------------------------------------------------------
   -- CvSlice
   -----------------------------------------------------------------------------

   type Cv_Slice is record
      Start_Index : Integer;
      End_Index   : Integer;
   end record;
   type Cv_Slice_P is access Cv_Slice;

   CV_WHOLE_SEQ_END_INDEX : constant := 16#3fff_Ffff#;

   function CvSlice (Start_Index : Integer;
                     End_Index   : Integer := CV_WHOLE_SEQ_END_INDEX)
                     return Cv_Slice;
   function CV_WHOLE_SEQ (Start_Index : Integer := 0;
                          End_Index   : Integer := CV_WHOLE_SEQ_END_INDEX) return Cv_Slice renames CvSlice;

   -----------------------------------------------------------------------------
   -- CvScalar
   -----------------------------------------------------------------------------
   type Cv_Scalar is record
      Val : aliased Cv_64f_Array (1 .. 4);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Scalar);
   type Cv_Scalar_P is access Cv_Scalar;
   type Cv_Scalar_Array is array (Integer range <>) of Cv_Scalar;

   function CvScalar (V0 : Long_Float; V1 : Long_Float := 0.0;
                      V2 : Long_Float := 0.0; V3 : Long_Float := 0.0)
                      return Cv_Scalar;
   function CvRealScalar (V0 : Long_Float) return Cv_Scalar;
   function CvScalarAll (V0123 : Long_Float) return Cv_Scalar;

   -----------------------------------------------------------------------------
   -- Dynamic Data Structures
   -----------------------------------------------------------------------------
   -- Ada stuff
   type Cv_Mem_Block;
   type Cv_Mem_Storage;
   type Cv_Seq;
   type Cv_Seq_Block;
   type Cv_Set_Elem;
   type Cv_Graph_Edge;
   type Cv_Graph_Vtx;
   --

   type Cv_Mem_Block is record
      Prev : access Cv_Mem_Block;
      Next : access Cv_Mem_Block;
   end record;

   CV_STORAGE_MAGIC_VAL : constant := 16#42890000#;

   type Cv_Mem_Storage is record
      Signature  : Integer;
      Bottom     : access Cv_Mem_Block;
      Top        : access Cv_Mem_Block;
      Parent     : access Cv_Mem_Storage;
      Block_Size : Integer;
      Free_Space : Integer;
   end record;
   type Cv_Mem_Storage_P is access Cv_Mem_Storage;

   function CV_IS_STORAGE (Storage : Cv_Mem_Storage_P) return Integer;

   type Cv_Mem_Storage_Pos is record
      Top        : access Cv_Mem_Block;
      Free_Space : Integer;
   end record;
   type Cv_Mem_Storage_Pos_P is access Cv_Mem_Storage_Pos;

   -----------------------------------------------------------------------------
   -- Sequence
   -----------------------------------------------------------------------------
   type Cv_Seq_Block is record
      Prev        : access Cv_Seq_Block;
      Next        : access Cv_Seq_Block;
      Start_Index : Integer;
      Count       : Integer;
      Data        : Interfaces.C.Strings.Chars_Ptr;
   end record;
   type Cv_Seq_Block_P is access Cv_Seq_Block;

   procedure CvChangeSeqBlock (Reader    : Cv_Void_P;
                               Direction : Integer);

   type Cv_Seq_P is access all Cv_Seq;
   type Cv_Seq is record
      Flags       : Unsigned_32;
      Header_Size : Integer;
      H_Prev      : Cv_Seq_P;
      H_Next      : Cv_Seq_P;
      V_Prev      : Cv_Seq_P;
      V_Next      : Cv_Seq_P;
      Total       : Integer;
      Elem_Size   : Integer;
      Block_Max   : Interfaces.C.Strings.Chars_Ptr;
      Ptr         : Cv_Arr_Pointer; --test this
      Delta_Elems : Integer;
      Storage     : Cv_Mem_Storage_P;
      Free_Blocks : Cv_Seq_Block_P;
      First       : Cv_Seq_Block_P;
   end record;


   CV_TYPE_NAME_SEQ : constant String := "opencv-sequence";
   CV_TYPE_NAME_SEQ_TREE : constant String := "opencv-sequence-tree";

   -----------------------------------------------------------------------------
   -- Cv_Set
   -----------------------------------------------------------------------------
   type Cv_Set_Elem is record
      Flags     : Integer;
      Next_Free : access Cv_Set_Elem;
   end record;
   type Cv_Set_Elem_P is access Cv_Set_Elem;
   type Cv_Set_Elem_Array is array (Integer range <>) of aliased Cv_Set_Elem;
   Cv_Set_Elem_Dummy : Cv_Set_Elem;

   package C_Set_Elem_Arr_Ptr is
     new Interfaces.C.Pointers (Integer, Cv_Set_Elem, Cv_Set_Elem_Array, Cv_Set_Elem_Dummy);
   use type C_Set_Elem_Arr_Ptr.Pointer;
   subtype Cv_Set_Elem_Pointer is C_Set_Elem_Arr_Ptr.Pointer;

   type Cv_Set is record
      Flags        : Integer;
      Header_Size  : Integer;
      H_Prev       : access Cv_Seq;
      H_Next       : access Cv_Seq;
      V_Prev       : access Cv_Seq;
      V_Next       : access Cv_Seq;
      Total        : Integer;
      Elem_Size    : Integer;
      Block_Max    : Interfaces.C.Strings.Chars_Ptr; -- kolla upp
      Ptr          : Interfaces.C.Strings.Chars_Ptr;
      Delta_Elems  : Integer;
      Storage      : access Cv_Mem_Storage;
      Free_Blocks  : access Cv_Seq_Block;
      First        : access Cv_Seq_Block;
      Free_Elems   : access Cv_Set_Elem;
      Active_Count : Integer;
   end record;
   type Cv_Set_P is access Cv_Set;
   type Cv_Set_Array is array (Integer range <>) of aliased Cv_Set;
   Cv_Set_Dummy : Cv_Set;

   package C_Set_Arr_Ptr is
     new Interfaces.C.Pointers (Integer, Cv_Set, Cv_Set_Array, Cv_Set_Dummy);
   use type C_Set_Arr_Ptr.Pointer;
   subtype Cv_Set_Pointer is C_Set_Arr_Ptr.Pointer;

   CV_SET_ELEM_IDX_MASK : constant := (16#4000000# - 1);
   CV_SET_ELEM_FREE_FLAG : constant := 16#80000000#;

   function CV_IS_SET_ELEM (Ptr : CV_Set_Elem_P) return Integer;

   -----------------------------------------------------------------------------
   -- Cv_Graph
   -----------------------------------------------------------------------------
   --type Cv_Graph_Edge;
   type Cv_Graph_Edge_P is access Cv_Graph_Edge;
   type Cv_Graph_Edge_P_Array is array (1 .. 2) of Cv_Graph_Edge_P;

   type Cv_Graph_Vtx_P is access Cv_Graph_Vtx;
   type Cv_Graph_Vtx_P_Array is array (1 .. 2) of Cv_Graph_Vtx_P;

   type Cv_Graph_Edge is record
      Flags  : Integer;
      Weight : Float;
      Next   : Cv_Graph_Edge_P_Array;
      Vtx    : Cv_Graph_Vtx_P_Array;
   end record;



   type Cv_Graph_Vtx is record
      Flags : Integer;
      First : access Cv_Graph_Vtx;
   end record;


   type Cv_Graph_Vtx_2D is
      record
         Flags : Integer; --CV_GRAPH_VERTEX_FIELDS()
         First : Cv_Graph_Edge_P;
         Ptr   : Cv_Point_2D_32F_P;
      end record;
   type Cv_Graph_Vtx_2D_P is access Cv_Graph_Vtx_2D;

   type Cv_Graph is record
      Flags        : Integer;
      Header_Size  : Integer;
      H_Prev       : access Cv_Seq;
      H_Next       : access Cv_Seq;
      V_Prev       : access Cv_Seq;
      V_Next       : access Cv_Seq;
      Total        : Integer;
      Elem_Size    : Integer;
      Block_Max    : Interfaces.C.Strings.Chars_Ptr;
      Ptr          : Interfaces.C.Strings.Chars_Ptr;
      Delta_Elems  : Integer;
      Storage      : access Cv_Mem_Storage;
      Free_Blocks  : access Cv_Seq_Block;
      First        : access Cv_Seq_Block;
      Free_Elems   : access Cv_Set_Elem;
      Active_Count : Integer;
      Edges        : access Cv_Set;
   end record;
   type Cv_Graph_P is access Cv_Graph;

   CV_TYPE_NAME_GRAPH : constant String := "opencv-graph";

   -----------------------------------------------------------------------------
   -- Cv_Chain/ Cv_Contour
   -----------------------------------------------------------------------------
   type Cv_Chain is
      record
      -- CV_TREE_NODE_FIELDS
         Flags        : Integer;
         Header_Size  : Integer;
         H_Prev       : access Cv_Seq;
         H_Next       : access Cv_Seq;
         V_Prev       : access Cv_Seq;
         V_Next       : access Cv_Seq;

         -- CV_SEQUENCE_FIELDS
         Total        : Integer;
         Elem_Size    : Integer;
         Block_Max    : Cv_Void_P;
         Ptr          : Cv_Void_P;
         Delta_Elems  : Integer;
         Storage      : access Cv_Mem_Storage;
         Free_Blocks  : access Cv_Seq_Block;
         First        : access Cv_Seq_Block;
         --
         Origin       : Cv_Point;
      end record;
   type Cv_Chain_P is access Cv_Chain;


   --/* Freeman chain reader state */
   type Cv_Chain_Pt_Reader is
      record
      --CV_SEQ_READER_FIELDS()
         HeaderSize : Integer;
         Seq        : Cv_Seq_P;
         Block      : Cv_Seq_Block_P;
         Ptr        : Cv_Arr_Pointer;
         BlockMin   : Cv_Arr_Pointer;
         BlockMax   : Cv_Arr_Pointer;
         DeltaIndex : Integer;
         PrevElem   : Cv_Arr_Pointer;
         --
         Code       : Unsigned_8;
         Pt         : Cv_Point;
         Deltas     : Cv_8u_2d_Array (1 .. 8, 1 .. 2);
      end record;
   type Cv_Chain_Pt_Reader_P is access Cv_Chain_Pt_Reader;

   type Cv_Contour is
      record
      -- CV_TREE_NODE_FIELDS
         Flags        : Integer;
         Header_Size  : Integer;
         H_Prev       : access Cv_Seq;
         H_Next       : access Cv_Seq;
         V_Prev       : access Cv_Seq;
         V_Next       : access Cv_Seq;

         -- CV_SEQUENCE_FIELDS
         Total        : Integer;
         Elem_Size    : Integer;
         Block_Max    : Cv_Void_P;
         Ptr          : Cv_Void_P;
         Delta_Elems  : Integer;
         Storage      : access Cv_Mem_Storage;
         Free_Blocks  : access Cv_Seq_Block;
         First        : access Cv_Seq_Block;
         --           CV_CONTOUR_FIELDS()
         Rect         : Cv_Rect;
         Color        : Integer;
         Reserved     : Cv_32s_Array (1 .. 3);
      end record;
   type Cv_Contour_P is access Cv_Contour;
   type Cv_Point_2D_Seq is new Cv_Contour;



   -----------------------------------------------------------------------------
   -- Sequence types
   -----------------------------------------------------------------------------
   CV_SEQ_MAGIC_VAL : constant := 16#42990000#;

   --#define CV_IS_SEQ(seq) \
   --    ((seq) != NULL && (((CvSeq*)(seq))->flags & CV_MAGIC_MASK) == CV_SEQ_MAGIC_VAL)
   function CV_IS_SEQ (Seq : Cv_Seq_P) return Integer;

   CV_SET_MAGIC_VAL : constant := 16#42980000#;

   --#define CV_IS_SET(set) \
   --((set) != NULL && (((CvSeq*)(set))->flags & CV_MAGIC_MASK) == CV_SET_MAGIC_VAL)
   function CV_IS_SET (Set : Cv_Seq_P) return Integer;

   CV_SEQ_ELTYPE_BITS : constant := 12;

   CV_SEQ_ELTYPE_MASK : constant := (16#1000# - 1); --(16#200# - 1);

   function CV_SEQ_ELTYPE_POINT return Integer;  --/* (x,y) */
   function CV_SEQ_ELTYPE_CODE return Integer;
   CV_SEQ_ELTYPE_GENERIC : constant := 0;
   CV_SEQ_ELTYPE_PTR : constant := CV_USRTYPE1;
   CV_SEQ_ELTYPE_PPOINT : constant := CV_SEQ_ELTYPE_PTR;  --/* &(x,y) */
   function CV_SEQ_ELTYPE_INDEX return Integer;
   CV_SEQ_ELTYPE_GRAPH_EDGE : constant := 0;  --/* &next_o, &next_d, &vtx_o, &vtx_d */
   CV_SEQ_ELTYPE_GRAPH_VERTEX : constant := 0;  --/* first_edge, &(x,y) */
   CV_SEQ_ELTYPE_TRIAN_ATR : constant := 0;  --/* vertex of the binary tree   */
   CV_SEQ_ELTYPE_CONNECTED_COMP : constant := 0;  --/* connected component  */
   function CV_SEQ_ELTYPE_POINT3D return Integer;

   CV_SEQ_KIND_BITS : constant := 2;
   CV_SEQ_KIND_MASK : constant := (16#4000#); --(4 >> 12)

   --/* types of sequences */
   CV_SEQ_KIND_GENERIC : constant := 0;
   CV_SEQ_KIND_CURVE : constant := 16#1000#; --(1 << CV_SEQ_ELTYPE_BITS) ( 1 << 12)
   CV_SEQ_KIND_BIN_TREE : constant := 16#2000#; --(2 << CV_SEQ_ELTYPE_BITS)

   --/* types of sparse sequences (sets) */
   CV_SEQ_KIND_GRAPH : constant := 16#1000#; -- #define CV_SEQ_KIND_GRAPH       (3 << CV_SEQ_ELTYPE_BITS)
   CV_SEQ_KIND_SUBDIV2D : constant := 16#2000#; --(2 << CV_SEQ_ELTYPE_BITS);

   CV_SEQ_FLAG_SHIFT : constant := 14; --(CV_SEQ_KIND_BITS + CV_SEQ_ELTYPE_BITS)(2+12);

   --/* flags for curves */
   CV_SEQ_FLAG_CLOSED : constant := 16#4000#; --(1 << CV_SEQ_FLAG_SHIFT);
   CV_SEQ_FLAG_SIMPLE : constant := 16#0#; --(0 << CV_SEQ_FLAG_SHIFT);
   CV_SEQ_FLAG_CONVEX : constant := 16#0#; --(0 << CV_SEQ_FLAG_SHIFT);
   CV_SEQ_FLAG_HOLE : constant := 16#8000#; --(2 << CV_SEQ_FLAG_SHIFT);

   --/* flags for graphs */
   CV_GRAPH_FLAG_ORIENTED : constant := 16#4000#;

   CV_GRAPH_C : constant := CV_SEQ_KIND_GRAPH; --16#600#
   CV_ORIENTED_GRAPH : constant := 16#5000#;      --(CV_SEQ_KIND_GRAPH | CV_GRAPH_FLAG_ORIENTED)

   --/* point sets */
   function CV_SEQ_POINT_SET return Integer;

   function CV_SEQ_POINT3D_SET return Integer; --: constant := (CV_SEQ_KIND_GENERIC | CV_SEQ_ELTYPE_POINT3D)
   function CV_SEQ_POLYLINE return Integer; --: constant := (CV_SEQ_KIND_CURVE  | CV_SEQ_ELTYPE_POINT)
   function CV_SEQ_POLYGON return Integer; --(CV_SEQ_FLAG_CLOSED | CV_SEQ_POLYLINE
   function CV_SEQ_CONTOUR return Integer renames CV_SEQ_POLYGON;
   function CV_SEQ_SIMPLE_POLYGON return Integer; --(CV_SEQ_FLAG_SIMPLE | CV_SEQ_POLYGON  )

   --/* chain-coded curves */
   function CV_SEQ_CHAIN return Integer; --(CV_SEQ_KIND_CURVE  | CV_SEQ_ELTYPE_CODE)
   function CV_SEQ_CHAIN_CONTOUR return Integer;  --(CV_SEQ_FLAG_CLOSED | CV_SEQ_CHAIN)

   --/* binary tree for the contour */
   function CV_SEQ_POLYGON_TREE return Integer; --(CV_SEQ_KIND_BIN_TREE  | CV_SEQ_ELTYPE_TRIAN_ATR)

   --/* sequence of the connected components */
   function CV_SEQ_CONNECTED_COMP return Integer; --(CV_SEQ_KIND_GENERIC  | CV_SEQ_ELTYPE_CONNECTED_COMP)

   --/* sequence of the integer numbers */
   function CV_SEQ_INDEX return Integer; --(CV_SEQ_KIND_GENERIC  | CV_SEQ_ELTYPE_INDEX)

   function  CV_SEQ_ELTYPE ( Seq : Cv_Seq_P ) return Integer;  -- ((Seq)- > Flags & CV_SEQ_ELTYPE_MASK)
   function CV_SEQ_KIND ( Seq : Cv_Seq_P ) return Integer;    --((Seq)- > Flags & CV_SEQ_KIND_MASK )

   --/* flag checking */
   function CV_IS_SEQ_INDEX ( Seq : Cv_Seq_P) return Integer;

   function CV_IS_SEQ_CURVE ( Seq : Cv_Seq_P) return Integer; --      (CV_SEQ_KIND (Seq) =  = CV_SEQ_KIND_CURVE)
   function CV_IS_SEQ_CLOSED ( Seq : Cv_Seq_P) return Integer; --     (((Seq)- > Flags & CV_SEQ_FLAG_CLOSED) ! = 0)
   function CV_IS_SEQ_CONVEX ( Seq : Cv_Seq_P) return Integer; -- (((Seq)- > Flags & CV_SEQ_FLAG_CONVEX) ! = 0)
   function CV_IS_SEQ_HOLE ( Seq : Cv_Seq_P) return Integer; --      (((Seq)- > Flags & CV_SEQ_FLAG_HOLE) ! = 0)
   function CV_IS_SEQ_SIMPLE ( Seq : Cv_Seq_P) return Integer; --    ((((Seq)- > Flags & CV_SEQ_FLAG_SIMPLE) ! = 0) |  | CV_IS_SEQ_CONVEX(seq))

   --/* type checking macros */
   function CV_IS_SEQ_POINT_SET ( Seq : CV_Seq_P) return Integer;

   function CV_IS_SEQ_POINT_SUBSET ( Seq : Cv_Seq_P) return Integer;

   function CV_IS_SEQ_POLYLINE ( Seq : Cv_Seq_P ) return Integer; --      (CV_SEQ_KIND(seq) == CV_SEQ_KIND_CURVE && CV_IS_SEQ_POINT_SET(seq))

   function CV_IS_SEQ_POLYGON ( Seq : Cv_Seq_P) return Integer; --      (CV_IS_SEQ_POLYLINE(seq) && CV_IS_SEQ_CLOSED(seq))

   function CV_IS_SEQ_CHAIN ( Seq : Cv_Seq_P) return Integer; --      (CV_SEQ_KIND(seq) == CV_SEQ_KIND_CURVE && (seq)->elem_size == 1)

   function CV_IS_SEQ_CONTOUR ( Seq : Cv_Seq_P) return Integer; --      (CV_IS_SEQ_CLOSED(seq) && (CV_IS_SEQ_POLYLINE(seq) || CV_IS_SEQ_CHAIN(seq)))

   function CV_IS_SEQ_CHAIN_CONTOUR ( Seq : Cv_Seq_P ) return Integer; --      (CV_IS_SEQ_CHAIN( seq ) && CV_IS_SEQ_CLOSED( seq ))

   function CV_IS_SEQ_POLYGON_TREE ( Seq  : Cv_Seq_P) return Integer; --      (CV_SEQ_ELTYPE (seq) ==  CV_SEQ_ELTYPE_TRIAN_ATR &&    \

   function CV_IS_GRAPH ( Seq : Cv_Seq_P) return Integer; --(CV_IS_SET(seq) && CV_SEQ_KIND((CvSet*)(seq)) == CV_SEQ_KIND_GRAPH)

   function CV_IS_GRAPH_ORIENTED ( Seq : Cv_Seq_P) return Integer; --      (((seq)->flags & CV_GRAPH_FLAG_ORIENTED) != 0)

   function CV_IS_SUBDIV2D ( Seq : Cv_Seq_P) return Integer; --      (CV_IS_SET(seq) && CV_SEQ_KIND((CvSet*)(seq)) == CV_SEQ_KIND_SUBDIV2D)

   -----------------------------------------------------------------------------
   -- Sequence writer & reader
   -----------------------------------------------------------------------------
   type Cv_Seq_Writer is
      record
         Header_Size : Integer;
         Seq         : Cv_Seq_P;
         Block       : Cv_Seq_Block_P;
         Ptr         : Cv_Arr_Pointer;
         Block_Min   : Cv_Arr_Pointer;
         Block_Max   : Cv_Arr_Pointer;
      end record;
   type Cv_Seq_Writer_P is access Cv_Seq_Writer;

   --Should not be here
   procedure CvCreateSeqBlock (Writer : Cv_Seq_Writer_P);

   type Cv_Seq_Reader is
      record
         HeaderSize : Integer;
         Seq        : Cv_Seq_P;
         Block      : Cv_Seq_Block_P;
         Ptr        : Cv_Arr_Pointer;
         BlockMin   : Cv_Arr_Pointer;
         BlockMax   : Cv_Arr_Pointer;
         DeltaIndex : Integer;
         PrevElem   : Cv_Arr_Pointer;
      end record;
   type Cv_Seq_Reader_P is access all Cv_Seq_Reader;

   -----------------------------------------------------------------------------
   -- Operations on sequences
   -----------------------------------------------------------------------------
   --CV_SEQ_ELEM ( Seq, Elem_Type, Index )                    \
   --  /* assert gives some guarantee that <seq> parameter is valid */  \
   --  (   assert(sizeof((seq)->first[0]) == sizeof(CvSeqBlock) &&      \
   --      (seq)->elem_size == sizeof(elem_type)),                      \
   --      (elem_type*)((seq)->first && (unsigned)index <               \
   --      (unsigned)((seq)->first->count) ?                            \
   --      (seq)->first->data + (index) * sizeof(elem_type) :           \
   --      cvGetSeqElem( (CvSeq*)(seq), (index) )))
   --  #define CV_GET_SEQ_ELEM( elem_type, seq, index ) CV_SEQ_ELEM( (seq), elem_type, (index) )
   --

   --  /* Add element to sequence: */
   procedure CV_WRITE_SEQ_ELEM_VAR ( Elem_Ptr : Cv_Arr_Pointer;
                                    Writer   : Cv_Seq_Writer_P );

   -- Not portable to Ada.
   procedure CV_WRITE_SEQ_ELEM ( Elem_Ptr : Cv_Arr_Pointer;
                                Writer   : Cv_Seq_Writer_P ) renames CV_WRITE_SEQ_ELEM_VAR;

   --  /* Move reader position forward: */
   procedure CV_NEXT_SEQ_ELEM (Elem_Size : Integer;
                               Reader    : Cv_Seq_Reader_P);
   procedure CV_NEXT_SEQ_ELEM (Elem_Size : Integer;
                               Reader    : Cv_Chain_Pt_Reader_P);

   --  /* Move reader position backward: */
   procedure CV_PREV_SEQ_ELEM ( Elem_Size : Integer;
                               Reader    : Cv_Seq_Reader_P );

   --  /* Read element and move read position forward: */
   procedure CV_READ_SEQ_ELEM ( Elem  : Cv_Arr_Pointer;
                               Reader : Cv_Seq_Reader_P );
   procedure CV_READ_SEQ_ELEM ( Elem  : Unsigned_8;
                               Reader : Cv_Chain_Pt_Reader_P );

   --  /* Read element and move read position backward: */
   procedure CV_REV_READ_SEQ_ELEM ( Elem  : Cv_Arr_Pointer;
                                   Reader : Cv_Seq_Reader_P );

   procedure CV_READ_CHAIN_POINT ( Pt    : out Cv_Point;
                                  Reader : Cv_Chain_Pt_Reader_P );

   function CV_CURRENT_POINT ( Reader : Cv_Chain_Pt_Reader_P ) return Cv_Point_P; --  (*((CvPoint*)((reader).ptr)))
   function CV_PREV_POINT ( Reader : Cv_Chain_Pt_Reader_P) return Cv_Point_P; -- ( * ((CvPoint * ) ((Reader).Prev_Elem)))

   procedure CV_READ_EDGE ( Pt1   : out Cv_Point_P;
                           Pt2    : out Cv_Point_P;
                           Reader : Cv_Chain_Pt_Reader_P );

   -----------------------------------------------------------------------------
   -- Graph macros
   -----------------------------------------------------------------------------
   --  /* Return next graph edge for given vertex: */
   function CV_NEXT_GRAPH_EDGE ( Edge  : Cv_Graph_Edge_P;
                                Vertex : Cv_Graph_Vtx_P ) return Cv_Graph_Edge_P;

   -----------------------------------------------------------------------------
   -- Data structures for persistence (a.k.a serialization) functionality
   -----------------------------------------------------------------------------


   type Cv_File_Storage is null record;
   type Cv_File_Storage_P is access Cv_File_Storage;

   CV_STORAGE_READ         : constant := 0;
   CV_STORAGE_WRITE        : constant := 1;
   CV_STORAGE_WRITE_TEXT   : constant := CV_STORAGE_WRITE;
   CV_STORAGE_WRITE_BINARY : constant := CV_STORAGE_WRITE;
   CV_STORAGE_APPEND       : constant := 2;

   type Cv_Attr_List;
   type Cv_Attr_List_P is access all Cv_Attr_List;
   type Cv_Attr_List is record
      Attr : Cv_String_Pointer;
      Next : Cv_Attr_List_P;
   end record;


   function CvAttrList (Attr : Cv_String_Pointer := null;
                        Next : Cv_Attr_List_P := null)
                        return Cv_Attr_List;

   type Cv_Type_Info;
   type Cv_Type_Info_P is access Cv_Type_Info;


   type Cv_Is_Instance_Func is access function (Struct_Ptr : Cv_Void_P)
                                                return Integer;
   pragma Convention (C, Cv_Is_Instance_Func);

   type Cv_Release_Proc is access procedure (Struct_Dblptr : access Cv_Void_P);
   pragma Convention (C, Cv_Release_Proc);

   type Cv_Read_Func is access function (Storage : Cv_File_Storage_P;
                                         Node    : Cv_File_Node_P)
                                         return Cv_Void_P;
   pragma Convention (C, Cv_Read_Func);

   type Cv_Write_Proc is access procedure (Storage    : Cv_File_Storage_P;
                                           Name       : Interfaces.C.Strings.Chars_Ptr;
                                           Struct_Ptr : Cv_Void_P;
                                           Attributes : Cv_Attr_List);
   pragma Convention (C, Cv_Write_Proc);

   type Cv_Clone_Func is access function (Struct_Ptr : Cv_Void_P)
                                          return Cv_Void_P;
   pragma Convention (C, Cv_Clone_Func);


   type Cv_Type_Info is record
      Flags       : Unsigned_32;
      Header_Size : Integer;
      Prev        : Cv_Type_Info_P;
      Next        : Cv_Type_Info_P;
      Type_Name   : Interfaces.C.Strings.Chars_Ptr; -- Will require wrapper?
      Is_Instance : Cv_Is_Instance_Func;
      Release     : Cv_Release_Proc;
      Read        : Cv_Read_Func;
      Write       : Cv_Write_Proc;
      Clone       : Cv_Clone_Func;
   end record;


   CV_NODE_NONE            : constant := 0;
   CV_NODE_INT             : constant := 1;
   CV_NODE_INTEGER         : constant := CV_NODE_INT;
   CV_NODE_REAL            : constant := 2;
   CV_NODE_FLOAT           : constant := CV_NODE_REAL;
   CV_NODE_STR             : constant := 3;
   CV_NODE_STRING          : constant := CV_NODE_STR;
   CV_NODE_REF             : constant := 4; -- Not used
   CV_NODE_SEQ             : constant := 5;
   CV_NODE_MAP             : constant := 6;
   CV_NODE_TYPE_MASK       : constant := 7;

   CV_NODE_FLOW            : constant Unsigned_32 := 8;
   CV_NODE_USER            : constant Unsigned_32 := 16;
   CV_NODE_EMPTY           : constant Unsigned_32 := 32;
   CV_NODE_NAMED           : constant Unsigned_32 := 64;

   function CV_NODE_TYPE (Flags : Unsigned_32)
                          return Unsigned_32;

   function CV_NODE_IS_INT (Flags : Unsigned_32)
                            return Boolean;

   function CV_NODE_IS_REAL (Flags : Unsigned_32)
                             return Boolean;

   function CV_NODE_IS_STRING (Flags : Unsigned_32)
                               return Boolean;

   function CV_NODE_IS_SEQ (Flags : Unsigned_32)
                            return Boolean;

   function CV_NODE_IS_MAP (Flags : Unsigned_32)
                            return Boolean;

   function CV_NODE_IS_COLLECTION (Flags : Unsigned_32)
                                   return Boolean;

   function CV_NODE_IS_FLOW (Flags : Unsigned_32)
                             return Boolean;

   function CV_NODE_IS_EMPTY (Flags : Unsigned_32)
                              return Boolean;

   function CV_NODE_IS_USER (Flags : Unsigned_32)
                             return Boolean;

   function CV_NODE_HAS_NAME (Flags : Unsigned_32)
                              return Boolean;

   CV_NODE_SEQ_SIMPLE      : constant Unsigned_32 := 256;

   function CV_NODE_SEQ_IS_SIMPLE (Seq : access Cv_Seq)
                                   return Boolean;

   type Cv_String is
      record
         Len : Integer;
         Pts : Interfaces.C.Strings.Chars_Ptr;
      end record;

   type Cv_String_Hash_Node;
   type Cv_String_Hash_Node_P is access Cv_String_Hash_Node;
   type Cv_String_Hash_Node is record
      Hashval : Unsigned_32;
      Str     : Cv_String;
      Next    : Cv_String_Hash_Node_P;
   end record;


   type Cv_Generic_Hash is record
   -- CV_TREE_NODE_FIELDS
      Flags        : Integer;
      Header_Size  : Integer;
      H_Prev       : access Cv_Seq;
      H_Next       : access Cv_Seq;
      V_Prev       : access Cv_Seq;
      V_Next       : access Cv_Seq;

      -- CV_SEQUENCE_FIELDS
      Total        : Integer;
      Elem_Size    : Integer;
      Block_Max    : Cv_Void_P;
      Ptr          : Cv_Void_P;
      Delta_Elems  : Integer;
      Storage      : access Cv_Mem_Storage;
      Free_Blocks  : access Cv_Seq_Block;
      First        : access Cv_Seq_Block;

      -- CV_SET_FIELDS
      Free_Elems   : access Cv_Set_Elem;
      Active_Count : Integer;

      Tab_Size     : Integer;
      Table        : access Cv_Void_P;
   end record;
   subtype Cv_File_Node_Hash is Cv_Generic_Hash;
   type Cv_File_Node_Hash_P is access Cv_File_Node_Hash;

   type Cv_File_Node_Data_Enum is (F, I, Str, Seq, Map);
   type Cv_File_Node_Data (Option : Cv_File_Node_Data_Enum := Str) is record
      case Option is
         when F =>
            F   : Long_Float;
         when I =>
            I   : Integer;
         when Str =>
            Str : Interfaces.C.Strings.Chars_Ptr;
         when Seq =>
            Seq : Cv_Seq_P;
         when Map =>
            Map : Cv_File_Node_Hash_P;
      end case;
   end record;
   pragma Unchecked_Union (Cv_File_Node_Data);
   pragma Convention (C, Cv_File_Node_Data);


   type Cv_File_Node is record
      Tag  : Integer;
      Info : Cv_Type_Info_P;
      Data : Cv_File_Node_Data;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_File_Node);

   type Cv_Plugin_Func_Info is record
      Func_Addr         : access Cv_Void_P;
      Default_Func_Addr : Cv_Void_P;
      Func_Names        : Interfaces.C.Strings.Chars_Ptr;
      Search_Modules    : Integer;
      Loaded_From       : Integer;
   end record;
   type Cv_Plugin_Func_Info_P is access all Cv_Plugin_Func_Info;

   type Cv_Module_Info;
   type Cv_Module_Info_P is access all Cv_Module_Info;
   type Cv_Module_Info is record
      Next     : Cv_Module_Info_P;
      Name     : Interfaces.C.Strings.Chars_Ptr;
      Version  : Interfaces.C.Strings.Chars_Ptr;
      Func_Tab : Cv_Plugin_Func_Info_P;
   end record;

   -- Unchecked Conversions ----------------------------------------------------
   ------------ Arr conversions ------------------------------------------------
   pragma Warnings (Off);
   function To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_8u_Array_P,
                                   Target => Cv_Arr_P);
   function To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Contour_P,
                                   Target => Cv_Arr_P);

   function To_Arr is
     new Ada.Unchecked_Conversion (Source => Ipl_Image_P,
                                   Target => Cv_Arr_P);

   function To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Scalar_P,
                                   Target => Cv_Arr_P);

   function To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Mat_P,
                                   Target => Cv_Arr_P);

   function To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P_Array_P,
                                   Target => Cv_Arr_P);


   function From_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Ipl_Image_P);

   function From_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Cv_Scalar_P);

   function From_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Cv_Mat_P);

   function From_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Cv_Sparse_Mat_P);

   function From_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Cv_Point_P);

   function From_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Character);

   -- Unchecked Conversions ----------------------------------------------------
   ------------ Void conversions -----------------------------------------------
   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Point_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Seq_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Contour_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Ipl_Image_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Scalar_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Arr_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Mat_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Float_P);

   function To_Void is
     new Ada.Unchecked_Conversion (Source => Ipl_Image_P,
                                   Target => Cv_Void_P);

   function To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Scalar_P,
                                   Target => Cv_Void_P);

   function To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Cv_Void_P);

   function To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Mat_P,
                                   Target => Cv_Void_P);

   function To_Void is
     new Ada.Unchecked_Conversion (Source => Float_P,
                                   Target => Cv_Void_P);
   function Image_To_Arr is
     new Ada.Unchecked_Conversion (Source => Ipl_Image_P,
                                   Target => Cv_Arr_P);

   function Scalar_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Scalar_P,
                                   Target => Cv_Arr_P);
   function Mat_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Mat_P,
                                   Target => Cv_Arr_P);

   function Arr_To_Image is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Ipl_Image_P);

   function Arr_To_Scalar is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Cv_Scalar_P);

   function Arr_To_Mat is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                   Target => Cv_Mat_P);

   function Void_To_Char is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Interfaces.C.Strings.Chars_Ptr);
   pragma Warnings (On);
   --
   pragma Warnings (Off);
   function Arr_P_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_P_Array_P,
                                   Target => Cv_Arr_P);

   function Cv_8u_To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_8u_Array_P,
                                   Target => Cv_Void_P);

   function Cv_8s_To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_8s_Array_P,
                                   Target => Cv_Void_P);

   function Cv_16u_To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_16u_Array_P,
                                   Target => Cv_Void_P);

   function Cv_16s_To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_16s_Array_P,
                                   Target => Cv_Void_P);

   function Cv_32s_To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_32s_Array_P,
                                   Target => Cv_Void_P);

   function Cv_32f_To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_32f_Array_P,
                                   Target => Cv_Void_P);

   function Cv_64f_To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_64f_Array_P,
                                   Target => Cv_Void_P);


   function Cv_8u_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_8u_Array_P,
                                   Target => Cv_Arr_P);

   function Cv_8s_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_8s_Array_P,
                                   Target => Cv_Arr_P);

   function Cv_16u_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_16u_Array_P,
                                   Target => Cv_Arr_P);

   function Cv_16s_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_16s_Array_P,
                                   Target => Cv_Arr_P);

   function Cv_32s_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_32s_Array_P,
                                   Target => Cv_Arr_P);

   function Cv_32f_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_32f_Array_P,
                                   Target => Cv_Arr_P);

   function Cv_64f_To_Arr is
     new Ada.Unchecked_Conversion (Source => Cv_64f_Array_P,
                                   Target => Cv_Arr_P);

   function "+" (Right : Ipl_Image_P) return Cv_Arr_P;

   function To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Chain_Pt_Reader_P,
                                   Target => Cv_Void_P);

   function To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Seq_Reader_P,
                                   Target => Cv_Void_P);

   function To_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Seq_P,
                                   Target => Cv_Void_P);

   -----------------------------------------------------------------------------
   -- Cv_Seq conversions
   -----------------------------------------------------------------------------

   function To_Seq is
     new Ada.Unchecked_Conversion (Source => Cv_Set_P,
                                   Target => Cv_Seq_P);

   pragma Warnings (On);

   -----------------------------------------------------------------------------
   -- C ** conversions
   -----------------------------------------------------------------------------
   -- START Unbounded array pointers --
   Mat_Data_Requirement : Mat_Data; -- This is just a dummy, do not use!
   type Cv_Mat_Array is array (Integer range <>) of aliased Cv_Mat;
   type Cv_Mat_P_Array is array (Integer range <>) of aliased Cv_Mat_P;
   type Cv_Size_Array is array (Integer range <>) of aliased Cv_Size;
   type Cv_Size_P_Array is array (Integer range <>) of aliased Cv_Size_P;

   --     package C_Mat_Arr_Ptr is
   --       new Interfaces.C.Pointers (Integer, Cv_Mat, Cv_Mat_Array, (0, 0, null, 0, Mat_Data_Requirement, 0, 0));
   --     use type C_Mat_Arr_Ptr.Pointer;
   --     subtype C_Mat_Ptr is C_Mat_Arr_Ptr.Pointer;

   package C_Mat_P_Arr_Ptr is
     new Interfaces.C.Pointers (Integer, Cv_Mat_P, Cv_Mat_P_Array, null);
   use type C_Mat_P_Arr_Ptr.Pointer;
   subtype Cv_Mat_P_Pointer is C_Mat_P_Arr_Ptr.Pointer;

   package C_Size_Arr_Ptr is
     new Interfaces.C.Pointers (Integer, Cv_Size, Cv_Size_Array, (0, 0));
   use type C_Size_Arr_Ptr.Pointer;
   subtype Cv_Size_Pointer is C_Size_Arr_Ptr.Pointer;

   --     package C_Size_P_Arr_Ptr is
   --       new Interfaces.C.Pointers (Integer, Cv_Size_P, Cv_Size_P_Array, null);
   --     use type C_Size_P_Arr_Ptr.Pointer;
   --     subtype C_Size_P_Ptr is C_Size_P_Arr_Ptr.Pointer;


   function CvMatElem (Mat       : Cv_Mat_P;
                       Elem_Size : Integer;
                       Row       : Integer;
                       Col       : Integer)
                       return Cv_Void_P;
      pragma Import (C, CvMatElem, "cvMatElem_wrap");


   -----------------------------------------------------------------------------
   -- Fix for Interfaces.C.Pointers
   -----------------------------------------------------------------------------


private
   pragma Import (C, CvIplDepth, "cvIplDepth");
   --     pragma Import (C, CvRound, "cvRound"); -- implemented as an ada function
   pragma Import (C, CvFloor, "cvFloor");
   pragma Import (C, CvCeil, "cvCeil");
   pragma Import (C, CvIsInf, "cvIsInf");
   pragma Import (C, CvIsNaN, "cvIsNaN");
   pragma Import (C, CvRNG, "cvRNG");
   pragma Import (C, CvRandInt, "cvRandInt");
   pragma Import (C, CvRandReal, "cvRandReal");
   pragma Import (C, CvmGet, "cvmGet");
   pragma Import (C, CvmSet, "cvmSet");
   pragma Import (C, CvCreateSeqBlock, "cvCreateSeqBlock");
   pragma Import (C, CvChangeSeqBlock, "cvChangeSeqBlock");


end Core;
