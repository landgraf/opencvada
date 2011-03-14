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
with System.Crtl; use System.Crtl;
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
   type Cv_File_Node_Ptr is access all Cv_File_Node;

   -- Never declare a variable of this type.
   type Null_Record is private;
   -- Moved since we use them early.
   Cv_8u       : constant := 0;
   Cv_8s       : constant := 1;
   Cv_16u      : constant := 2;
   Cv_16s      : constant := 3;
   Cv_32s      : constant := 4;
   Cv_32f      : constant := 5;
   Cv_64f      : constant := 6;
   Cv_Usrtype1 : constant := 7;

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

   type Cv_8u_Array_Ptr is access Cv_8u_Array;
   type Cv_8s_Array_Ptr is access Cv_8s_Array;
   type Cv_16u_Array_Ptr is access Cv_16u_Array;
   type Cv_16s_Array_Ptr is access Cv_16s_Array;
   type Cv_32s_Array_Ptr is access Cv_32s_Array;
   type Cv_32u_Array_Ptr is access Cv_32u_Array;
   type Cv_32f_Array_Ptr is access Cv_32f_Array;
   type Cv_64f_Array_Ptr is access Cv_64f_Array;

   package Cv_8u_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Unsigned_8, Cv_8u_Array, 0);
   subtype Cv_8u_Pointer is Cv_8u_Pointer_Pkg.Pointer;

   type Cv_8u_Pointer_Array is array (Integer range <>) of Cv_8u_Pointer;
   type Cv_8u_Pointer_Array_Ptr is access Cv_8u_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_8u_2d_Array)
                           return Cv_8u_Pointer_Array;

   package Cv_8s_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Integer_8, Cv_8s_Array, 0);
   subtype Cv_8s_Pointer is Cv_8s_Pointer_Pkg.Pointer;

   type Cv_8s_Pointer_Array is array (Integer range <>) of Cv_8s_Pointer;
   type Cv_8s_Pointer_Array_Ptr is access Cv_8s_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_8s_2d_Array)
                           return Cv_8s_Pointer_Array;

   package Cv_16u_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Unsigned_16, Cv_16u_Array, 0);
   subtype Cv_16u_Pointer is Cv_16u_Pointer_Pkg.Pointer;

   type Cv_16u_Pointer_Array is array (Integer range <>) of Cv_16u_Pointer;
   type Cv_16u_Pointer_Array_Ptr is access Cv_16u_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_16u_2d_Array)
                           return Cv_16u_Pointer_Array;

   package Cv_16s_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Integer_16, Cv_16s_Array, 0);
   subtype Cv_16s_Pointer is Cv_16s_Pointer_Pkg.Pointer;

   type Cv_16s_Pointer_Array is array (Integer range <>) of Cv_16s_Pointer;
   type Cv_16s_Pointer_Array_Ptr is access Cv_16s_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_16s_2d_Array)
                           return Cv_16s_Pointer_Array;

   package Cv_32s_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Integer, Cv_32s_Array, 0);
   subtype Cv_32s_Pointer is Cv_32s_Pointer_Pkg.Pointer;

   type Cv_32s_Pointer_Array is array (Integer range <>) of Cv_32s_Pointer;
   type Cv_32s_Pointer_Array_Ptr is access Cv_32s_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_32s_2d_Array)
                           return Cv_32s_Pointer_Array;

   package Cv_32f_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Float, Cv_32f_Array, 0.0);
   subtype Cv_32f_Pointer is Cv_32f_Pointer_Pkg.Pointer;

   type Cv_32f_Pointer_Array is array (Integer range <>) of Cv_32f_Pointer;
   type Cv_32f_Pointer_Array_Ptr is access Cv_32f_Pointer_Array;

   -- Changes an ada 2d array into a C compatible 2d array
   function To_2d_Pointer (Src : access Cv_32f_2d_Array)
                           return Cv_32f_Pointer_Array;

   package Cv_64f_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Long_Float , Cv_64f_Array, 0.0);
   subtype Cv_64f_Pointer is Cv_64f_Pointer_Pkg.Pointer;
   type Cv_64f_Pointer_Ptr is access Cv_64f_Pointer;

   type Cv_64f_Pointer_Array is array (Integer range <>) of Cv_64f_Pointer;
   type Cv_64f_Pointer_Array_Ptr is access Cv_64f_Pointer_Array;

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
   subtype Cv_String_Pointer is Cv_String_Pointer_Pkg.Pointer;

   package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
   package Long_Float_Numerics is new Ada.Numerics.Generic_Elementary_Functions (Long_Float);

   -----------------------------------------------------------------------------

   --    CvArr* is used to pass arbitrary
   --    array-like data structures
   --    into functions where the particular
   --    array type is recognized at runtime:
   type Cv_Arr is new Integer;
   type Cv_Arr_Ptr is access all Cv_Arr;
   type Cv_Void_Ptr is access all Cv_Arr;

   type Cv_Void_Ptr_Array is array (Integer range <>) of aliased Cv_Void_Ptr;

   type Cv_Arr_Ptr_Array is array (Integer range <>) of aliased Cv_Arr_Ptr;
   type Cv_Arr_Ptr_Array_Ptr is access Cv_Arr_Ptr_Array;

   package Cv_Arr_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Arr_Ptr, Cv_Arr_Ptr_Array, null);
   subtype Cv_Arr_Pointer is Cv_Arr_Pointer_Pkg.Pointer;


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

   Cv_Stsok : constant Cv_Status := 0;  --/* everithing is ok                */
   Cv_Stsbacktrace : constant Cv_Status := -1 ;  --/* pseudo error for back trace     */

   Cv_Stserror : constant Cv_Status := -2;  --/* unknown /unspecified error      */
   Cv_Stsinternal : constant Cv_Status := -3;  --/* internal error (bad state)      */
   Cv_Stsnomem : constant Cv_Status := -4;  --/* insufficient memory             */
   Cv_Stsbadarg : constant Cv_Status := -5;  --/* function arg/param is bad       */
   Cv_Stsbadfunc : constant Cv_Status := -6;  --/* unsupported function            */
   Cv_Stsnoconv : constant Cv_Status := -7 ;  --/* iter. didn't converge           */
   Cv_Stsautotrace : constant Cv_Status := -8;  --/* tracing                         */

   Cv_Headerisnull : constant Cv_Status := -9;  --/* image header is NULL            */
   Cv_Badimagesize : constant Cv_Status := -10; --/* image size is invalid           */
   Cv_Badoffset : constant Cv_Status := -11; --/* offset is invalid               */
   Cv_Baddataptr : constant Cv_Status := -12; --/**/
   Cv_Badstep : constant Cv_Status := -13; --/**/
   Cv_Badmodelorchseq : constant Cv_Status := -14; --/**/
   Cv_Badnumchannels : constant Cv_Status := -15; --/**/
   Cv_Badnumchannel1u : constant Cv_Status := -16; --/**/
   Cv_Baddepth : constant Cv_Status := -17; --/**/
   Cv_Badalphachannel : constant Cv_Status := -18; --/**/
   Cv_Badorder : constant Cv_Status := -19; --/**/
   Cv_Badorigin : constant Cv_Status := -20; --/**/
   Cv_Badalign : constant Cv_Status := -21; --/**/
   Cv_Badcallback : constant Cv_Status := -22; --/**/
   Cv_Badtilesize : constant Cv_Status := -23; --/**/
   Cv_Badcoi : constant Cv_Status := -24; --/**/
   Cv_Badroisize : constant Cv_Status := -25; --/**/

   Cv_Maskistiled : constant Cv_Status := -26; --/**/

   Cv_Stsnullptr : constant Cv_Status := -27; --/* null pointer */
   Cv_Stsveclengtherr : constant Cv_Status := -28; --/* incorrect vector length */
   Cv_Stsfilterstructcontenterr : constant Cv_Status := -29; --/* incorr. filter structure content */
   Cv_Stskernelstructcontenterr : constant Cv_Status := -30; --/* incorr. transform kernel content */
   Cv_Stsfilteroffseterr : constant Cv_Status := -31; --/* incorrect filter ofset value */

   --/*extra for CV */
   Cv_Stsbadsize : constant Cv_Status := -201; --/* the input/output structure size is incorrect  */
   Cv_Stsdivbyzero : constant Cv_Status := -202; --/* division by zero */
   Cv_Stsinplacenotsupported : constant Cv_Status := -203; --/* in-place operation is not supported */
   Cv_Stsobjectnotfound : constant Cv_Status := -204; ---/* request can't be completed */
   Cv_Stsunmatchedformats : constant Cv_Status := -205; --/* formats of input/output arrays differ */
   Cv_Stsbadflag : constant Cv_Status := -206; --/* flag is wrong or not supported */
   Cv_Stsbadpoint : constant Cv_Status := -207; --/* bad CvPoint */
   Cv_Stsbadmask : constant Cv_Status := -208; --/* bad format of mask (neither 8uC1 nor 8sC1)*/
   Cv_Stsunmatchedsizes : constant Cv_Status := -209; --/* sizes of input/output structures do not match */
   Cv_Stsunsupportedformat : constant Cv_Status := -210; --/* the data format/type is not supported by the function*/
   Cv_Stsoutofrange : constant Cv_Status := -211; --/* some of parameters are out of range */
   Cv_Stsparseerror : constant Cv_Status := -212; --/* invalid syntax/structure of the parsed file */
   Cv_Stsnotimplemented : constant Cv_Status := -213; --/* the requested function/feature is not implemented */
   Cv_Stsbadmemblock : constant Cv_Status := -214; --/* an allocated block has been corrupted */
   Cv_Stsassert : constant Cv_Status := -215; --/* assertion failed */

   -----------------------------------------------------------------------------
   -- Common macros and inline functions
   -----------------------------------------------------------------------------
   Cv_Pi : constant := Ada.Numerics.Pi;
   Cv_Log2 : constant := 0.69314718055994530941723212145818;

   --     Converts a floating-point number to an integer.
   function Cv_Round (Value : Long_Float) return Integer;
   function Cv_Round (Value : Float) return Integer;
   function Cv_Floor (Value : Long_Float) return Integer;
   function Cv_Ceil (Value : Long_Float) return Integer;

   --     Calculates the inverse square root.
   function Cv_Inv_Sqrt (Value : Float)
                         return Float;
   --     Calculates the square root.
   function Cv_Sqrt (Value : Float) return Float;

   --     Determines if the argument is Not A Number.
   function Cv_Is_Nan (Value : Long_Float)
                       return Integer;

   --     Determines if the argument is Infinity.
   function Cv_Is_Inf (Value : Long_Float)
                       return Integer;

   -----------------------------------------------------------------------------
   -- Random number generation
   -----------------------------------------------------------------------------
   subtype Cv_Rng is Integer_64;

   --     Initializes a random number generator state.
   function Cv_Create_Rng (Seed : Integer_64 := -1)
                           return Cv_Rng;

   --     Returns a 32-bit unsigned integer and updates RNG.
   function Cv_Rand_Int (Rng : access Cv_Rng)
                         return Unsigned_32;

   --     Returns a floating-point random number and updates RNG.
   function Cv_Rand_Real (Rng : access Integer_64)
                          return Long_Float;

   -----------------------------------------------------------------------------
   -- Image type Ipl_Image
   -----------------------------------------------------------------------------
   Ipl_Depth_Sign : constant Unsigned_32 := 16#80000000#;

   Ipl_Depth_1u   : constant Unsigned_32 := 1;
   Ipl_Depth_8u   : constant Unsigned_32 := 8;
   Ipl_Depth_16u  : constant Unsigned_32 := 16;
   Ipl_Depth_32f  : constant Unsigned_32 := 32;

   Ipl_Depth_8s   : constant Unsigned_32 := Ipl_Depth_Sign or Unsigned_32 (8);
   Ipl_Depth_16s  : constant Unsigned_32 := Ipl_Depth_Sign or Unsigned_32 (16);
   Ipl_Depth_32s  : constant Unsigned_32 := Ipl_Depth_Sign or Unsigned_32 (32);

   Ipl_Data_Order_Pixel : constant Unsigned_32 := 0;
   Ipl_Data_Order_Plane : constant Unsigned_32 := 1;

   Ipl_Origin_Tl     : constant Unsigned_32 := 0;
   Ipl_Origin_Bl     : constant Unsigned_32 := 1;

   Ipl_Align_4bytes  : constant Unsigned_32 := 4;
   Ipl_Align_8bytes  : constant Unsigned_32 := 8;
   Ipl_Align_16bytes : constant Unsigned_32 := 16;
   Ipl_Align_32bytes : constant Unsigned_32 := 32;

   Ipl_Align_Dword   : constant Unsigned_32 := Ipl_Align_4bytes;
   Ipl_Align_Qword   : constant Unsigned_32 := Ipl_Align_8bytes;

   Ipl_Border_Constant  : constant Unsigned_32 := 0;
   Ipl_Border_Replicate : constant Unsigned_32 := 1;
   Ipl_Border_Reflect   : constant Unsigned_32 := 2;
   Ipl_Border_Wrap      : constant Unsigned_32 := 3;

   -- Ipl_Tile_Info ------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Ipl_Tile_Info is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, Ipl_Tile_Info);
   type Ipl_Tile_Info_Ptr is access all Ipl_Tile_Info;
   pragma Convention (C, Ipl_Tile_Info_Ptr);

   -- Ipl_ROI ------------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Ipl_Roi is record
      Coi      : Integer;
      Height   : Integer;
      Width    : Integer;
      X_Offset : Integer;
      Y_Offset : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Ipl_Roi);
   type Ipl_Roi_Ptr is access all Ipl_Roi;
   pragma Convention (C, Ipl_Roi_Ptr);

   -- Ipl_Image ----------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Ipl_Image_Ptr is access all Ipl_Image;
   pragma Convention (C, Ipl_Image_Ptr);
   type Ipl_Image is record
      N_Size            : Integer;
      Id                : Integer;
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
      Roi               : Ipl_Roi_Ptr;
      Mask_Roi          : Ipl_Image_Ptr;
      Image_Id          : Void_P;
      Tile_Info         : Ipl_Tile_Info_Ptr;
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
   type Ipl_Image_Ptr_Array is array (Integer range<>) of aliased Ipl_Image_Ptr;


   package Ipl_Image_Ptr_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Ipl_Image_Ptr, Ipl_Image_Ptr_Array, null);
   subtype Ipl_Image_Ptr_Pointer is Ipl_Image_Ptr_Pointer_Pkg.Pointer;

   type Ipl_Conv_Kernel is
      record
         N_Cols     : Integer;
         N_Rows     : Integer;
         Anchor_X   : Integer;
         Anchor_Y   : Integer;
         Values     : Cv_32u_Array_Ptr;
         N_Shift_R  : Integer;
      end record;
   pragma Convention (C_Pass_By_Copy, Ipl_Conv_Kernel);
   type Ipl_Conv_Kernel_Ptr is access all Ipl_Conv_Kernel;

   type Ipl_Conv_Kernel_Fp is
      record
         Ncols   : Integer;
         Nrows   : Integer;
         Anchorx : Integer;
         Anchory : Integer;
         Values  : Cv_32f_Array_Ptr;
      end record;
   pragma Convention (C_Pass_By_Copy, Ipl_Conv_Kernel_Fp);
   Ipl_Image_Header : constant := 1;
   Ipl_Image_Data : constant := 2;
   Ipl_Image_Roi : constant := 4;

   Ipl_Border_Reflect_101 : constant := 4;
   Ipl_Image_Magic_Val : constant := 112; -- ipl_image'size/8

   function Cv_Is_Image_Hdr (Img : Ipl_Image_Ptr) return Integer;
   function Cv_Is_Image (Img : Ipl_Image_Ptr) return Integer;

   Ipl_Depth_64f          : constant := 64;

   -----------------------------------------------------------------------------
   -- Matrix type Cv_Mat
   -----------------------------------------------------------------------------
   Cv_Cn_Max        : constant Unsigned_32 := 512;
   Cv_Cn_Shift      : constant Unsigned_32 := 3;
   Cv_Depth_Max     : constant Unsigned_32 := 8; -- 1 << CV_CN_SHIFT

   function Cv_Mat_Depth_Mask return Unsigned_32;
   function Cv_Mat_Depth (M_Type : Unsigned_32) return Unsigned_32; -- used to be Integer

   function Cv_Maketype (Depth : Integer; Cn : Integer) return Unsigned_32; -- used to be Integer
   function Cv_Make_Type (Depth : Integer; Cn : Integer) return Unsigned_32 -- used to be Integer
                          renames Cv_Maketype;

   Cv_Autostep : constant Unsigned_32 := 16#7fff_Ffff#;
   Cv_Auto_Step : constant Unsigned_32 := 16#7fffffff#;

   function Cv_Mat_Cn_Mask return Unsigned_32;
   function Cv_Mat_Cn (Flags : Unsigned_32) return Unsigned_32; -- used to be Integer
   Cv_Mat_Type_Mask : constant Unsigned_32 := Cv_Depth_Max * Cv_Cn_Max - 1;
   function Cv_Mat_Type (Flags : Unsigned_32) return Unsigned_32; -- used to be Integer
   Cv_Mat_Cont_Flag_Shift : constant Unsigned_32 := 14;
   Cv_Mat_Cont_Flag : constant Unsigned_32 := 16#0100_0000#; -- 1 << CV_MAT_CONT_FLAG_SHIFT
   function Cv_Is_Mat_Cont (Flags : Integer) return Boolean;
   function Cv_Is_Cont_Mat (Flags : Integer) return Boolean
                            renames Cv_Is_Mat_Cont;
   Cv_Mat_Temp_Flag_Shift : constant Unsigned_32 := 15;
   Cv_Mat_Temp_Flag       : constant := 16#1000_0000#; -- 1 << CV_MAT_TEMP_FLAG_SHIFT
   function Cv_Is_Temp_Mat (Flags : Integer) return Boolean;

   Cv_Magic_Mask    : constant Unsigned_32 := 16#Ffff_0000#;
   Cv_Mat_Magic_Val : constant Unsigned_32 := 16#4242_0000#;
   Cv_Type_Name_Mat : constant String := "opencv-matrix";

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
   type Mat_Data_Ptr is access all Mat_Data;

--     type Cv_Mat is null record;
--     pragma Warnings (Off); -- 224 bits of "Cv_Mat" unused
--     for Cv_Mat'Size use 224;
--     pragma Warnings (On);
   type Cv_Mat is record
      Mat_Type     : Unsigned_32; -- used to be Integer
      Step         : Integer;
      Refcount     : access Integer := null;
      Hdr_Refcount : Integer := 0;
      Data         : Mat_Data;
      Rows         : Integer;
      Cols         : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Mat);
   type Cv_Mat_Ptr is access all Cv_Mat;
   type Cv_Mat_Array_Axb is array (Integer range <>, Integer range <>) of aliased Cv_Mat;

   function Cv_Is_Mat_Hdr (Mat : Cv_Mat_Ptr) return Integer;
   function Cv_Is_Mat_Hdr (Mat : Cv_Arr_Ptr) return Integer;

   function Cv_Is_Mat (Mat : Cv_Mat_Ptr) return Integer;
   function Cv_Is_Mat (Mat : Cv_Arr_Ptr) return Integer;

   function Cv_Is_Mask_Arr (Mat : Cv_Mat_Ptr) return Integer;
   function Cv_Is_Mask_Arr (Mat : Cv_Arr_Ptr) return Integer;

   function Cv_Are_Types_Eq (Mat1 : Cv_Mat_Ptr;
                             Mat2 : Cv_Mat_Ptr)
                             return Integer;
   function Cv_Are_Types_Eq (Mat1 : Cv_Arr_Ptr;
                             Mat2 : Cv_Arr_Ptr)
                             return Integer;

   function Cv_Are_Cns_Eq (Mat1 : Cv_Mat_Ptr;
                           Mat2 : Cv_Mat_Ptr)
                           return Integer;
   function Cv_Are_Cns_Eq (Mat1 : Cv_Arr_Ptr;
                           Mat2 : Cv_Arr_Ptr)
                           return Integer;

   function Cv_Are_Depths_Eq (Mat1 : Cv_Mat_Ptr;
                              Mat2 : Cv_Mat_Ptr)
                              return Integer;
   function Cv_Are_Depths_Eq (Mat1 : Cv_Arr_Ptr;
                              Mat2 : Cv_Arr_Ptr)
                              return Integer;

   function Cv_Are_Sizes_Eq (Mat1 : Cv_Mat_Ptr;
                             Mat2 : Cv_Mat_Ptr)
                             return Integer;
   function Cv_Are_Sizes_Eq (Mat1 : Cv_Arr_Ptr;
                             Mat2 : Cv_Arr_Ptr)
                             return Integer;

   function Cv_Is_Mat_Const (Mat : Cv_Mat_Ptr) return Integer;
   function Cv_Is_Mat_Const (Mat : Cv_Arr_Ptr) return Integer;

   function Cv_Mat_Elem_Ptr_Fast (Mat      : Cv_Mat_Ptr;
                                  Row      : Integer;
                                  Col      : Integer;
                                  Pix_Size : Integer)
                                  return Cv_Void_Ptr;
   function Cv_Mat_Elem_Ptr_Fast (Mat      : Cv_Arr_Ptr;
                                  Row      : Integer;
                                  Col      : Integer;
                                  Pix_Size : Integer)
                                  return Cv_Void_Ptr;

   function Cv_Mat_Elem_Ptr (Mat : Cv_Mat_Ptr;
                             Row : Integer;
                             Col : Integer)
                             return Cv_Void_Ptr;
   function Cv_Mat_Elem_Ptr (Mat : Cv_Arr_Ptr;
                             Row : Integer;
                             Col : Integer)
                             return Cv_Void_Ptr;

--     function Cv_Is_Mat_Hdr (Mat : Cv_Mat_Ptr) return Integer;
--
--     function Cv_Is_Mat (Mat : Cv_Mat_Ptr) return Integer;
--
--     function Cv_Is_Mask_Arr (Mat : Cv_Mat_Ptr) return Integer;
--
--     function Cv_Are_Types_Eq (Mat1 : Cv_Mat_Ptr;
--                               Mat2 : Cv_Mat_Ptr) return Integer;
--
--     function Cv_Are_Cns_Eq (Mat1 : Cv_Mat_Ptr;
--                             Mat2 : Cv_Mat_Ptr) return Integer;
--
--     function Cv_Are_Depths_Eq (Mat1 : Cv_Mat_Ptr;
--                                Mat2 : Cv_Mat_Ptr) return Integer;
--
--     function Cv_Are_Sizes_Eq (Mat1 : Cv_Mat_Ptr;
--                               Mat2 : Cv_Mat_Ptr) return Integer;
--
--     function Cv_Is_Mat_Const (Mat : Cv_Mat_Ptr) return Integer;

   function Cv_Elem_Size_1 (E_Type : Unsigned_32) return Unsigned_32; -- used to be Integer
   function Cv_Elem_Size (E_Type : Unsigned_32) return Unsigned_32; -- used to be Integer

   function Ipl_To_Cv_Depth (Depth : Unsigned_32) return Integer;

   function Cv_Create_Mat (Rows   : Integer;
                           Cols   : Integer;
                           M_Type : Unsigned_32; -- used to be Integer
                           Data   : Cv_Void_Ptr := null)
                           return Cv_Mat;


--     function Cv_Mat_Elem_Ptr_Fast (Mat      : Cv_Mat_Ptr;
--                                    Row      : Integer;
--                                    Col      : Integer;
--                                    Pix_Size : Unsigned_32) return Cv_8u_Pointer;

--     function Cv_Mat_Elem_Ptr (Mat : Cv_Mat_Ptr;
--                               Row : Integer;
--                               Col : Integer) return Cv_8u_Pointer;

   function Cv_Mat_Elem (Mat      : Cv_Mat_Ptr;
                         Elemtype : Unsigned_32; -- used to be Integer
                         Row      : Integer;
                         Col      : Integer) return Cv_8u_Pointer;

   --     Returns the particular element of single-channel floating-point
   --     matrix.
   function Cvm_Get (Mat : access Cv_Mat;
                     Row : Integer;
                     Col : Integer)
                     return Long_Float;

   --     Returns a specific element of a single-channel floating-point matrix.
   procedure Cvm_Set (Mat   : access Cv_Mat;
                      Row   : Integer;
                      Col   : Integer;
                      Value : Long_Float);

   function Cv_Ipl_Depth (Itype : Integer) return Integer;

   -----------------------------------------------------------------------------
   -- Multi-dimensional dense array (CvMatND)
   -----------------------------------------------------------------------------

   Cv_Matnd_Magic_Val : constant := 16#42430000#;
   Cv_Type_Name_Matnd : constant String := "opencv-nd-matrix";

   Cv_Max_Dim : constant Integer := 32;
   Cv_Max_Dim_Heap : constant := 16#10000#;

   type Mat_Dimensions is record
      Size : Integer;
      Step : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Mat_Dimensions);

   type Mat_Dimensions_Array is array (Integer range <>) of Mat_Dimensions;
   --type Size_Array is array (Integer range <>) of Integer;

--     type Cv_Mat_ND is null record;
--     pragma Warnings (Off); -- 2208 bits of "Cv_Mat_ND" unused
--     for Cv_Mat_ND'Size use 2208;
--     pragma Warnings (On);
   type Cv_Mat_Nd is record
      Mat_Type     : Integer;
      Dims         : Integer;
      Refcount     : access Integer;
      Hdr_Refcount : Integer;
      Data         : Mat_Data;
      Dim          : Mat_Dimensions_Array (0 .. Cv_Max_Dim - 1);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Mat_Nd);
   type Cv_Mat_Nd_Ptr is access all Cv_Mat_Nd;

   function Cv_Is_Mat_ND_Hdr (Mat : Cv_Mat_ND_Ptr) return Integer;
   function Cv_Is_Mat_ND (Mat : Cv_Mat_ND_Ptr) return Integer;

   -----------------------------------------------------------------------------
   -- Multi-dimensional sparse array (CvSparseMat)
   -----------------------------------------------------------------------------
   Cv_Sparse_Mat_Magic_Val : constant := 16#42440000#;
   Cv_Type_Name_Sparse_Mat : constant String := "opencv-sparse-matrix";

--     type Cv_Sparse_Mat is null record;
--     pragma Warnings (Off); -- 1312 bits of "Cv_Sparse_Mat" unused
--     for Cv_Sparse_Mat'Size use 1312;
--     pragma Warnings (On);
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
      Size      : Cv_32s_Array (1 .. Cv_Max_Dim);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Sparse_Mat);
   type Cv_Sparse_Mat_Ptr is access all Cv_Sparse_Mat;

   function Cv_Is_Sparse_Mat_Hdr (Mat : Cv_Sparse_Mat_Ptr) return Integer;
   function Cv_Is_Sparse_Mat (Mat : Cv_Sparse_Mat_Ptr) return Integer;

   type Cv_Sparse_Node;
   type Cv_Sparse_Node_Ptr is access all Cv_Sparse_Node;
   type Cv_Sparse_Node is record
      Hashval : Natural;
      Next    : aliased Cv_Sparse_Node_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Sparse_Node);

   type Cv_Sparse_Mat_Iterator is record
      Mat    : aliased Cv_Sparse_Mat_Ptr;
      Node   : aliased Cv_Sparse_Node_Ptr;
      Curidx : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Sparse_Mat_Iterator);
   type Cv_Sparse_Mat_Iterator_Ptr is access all Cv_Sparse_Mat_Iterator;

   -- Fix these two!
   --     #define CV_NODE_VAL(mat,node)   ((void*)((uchar*)(node) + (mat)->valoffset))
   --  #define CV_NODE_IDX(mat,node)   ((int*)((uchar*)(node) + (mat)->idxoffset))

   -----------------------------------------------------------------------------
   -- Histogram
   -----------------------------------------------------------------------------
   type Cv_Hist_Type is new Integer;

   Cv_Hist_Magic_Val : constant := 16#42450000#;
   Cv_Hist_Uniform_Flag : constant := 16#400#;

   --/* indicates whether bin ranges are set already or not */
   Cv_Hist_Ranges_Flag : constant := 16#800#;

   Cv_Hist_Array : constant := 0;
   Cv_Hist_Sparse : constant := 1;
   Cv_Hist_Tree : constant := Cv_Hist_Sparse;

   --  /* should be used as a parameter only,
   --     it turns to CV_HIST_UNIFORM_FLAG of hist->type */
   Cv_Hist_Uniform : constant := 1;

   type Thresh_Arr is array (Integer range 0 ..  Cv_Max_Dim, Integer range 0 .. 1) of Float;
   pragma Convention (C, Thresh_Arr);

   Cv_32f_Array_Null : Cv_32f_Pointer_Array (1 .. 0);
   Cv_32s_Array_Null : Cv_32s_Array (1 .. 0);


   type Cv_Histogram is
      record
         Histtype : Integer;
         Bins     : Cv_Arr_Ptr;
         Thresh   : Thresh_Arr;
         Thresh2  : Cv_32f_Pointer_Array_Ptr;
         Mat      : Cv_Mat_ND;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Histogram);
   type Cv_Histogram_Ptr is access all Cv_Histogram;

   type Cv_Histogram_Ptr_Array is array (Integer range <>) of aliased Cv_Histogram_Ptr;

   package Cv_Histogram_Ptr_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Histogram_Ptr, Cv_Histogram_Ptr_Array, null);
   subtype Cv_Histogram_Ptr_Pointer is Cv_Histogram_Ptr_Pointer_Pkg.Pointer;

   function Cv_Is_Hist (Hist : Cv_Histogram_Ptr) return Integer;
   function Cv_Is_Uniform_Hist (Hist : Cv_Histogram_Ptr) return Integer;
   function Cv_Is_Sparse_Hist (Hist : Cv_Histogram_Ptr) return Integer;
   function Cv_Hist_Has_Ranges (Hist : Cv_Histogram_Ptr) return Integer;

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
   type Cv_Rect_Ptr is access all Cv_Rect;
   type Cv_Rect_Array is array (Integer range <>) of aliased Cv_Rect;

   function Cv_Create_Rect (X : Integer; Y : Integer; Width : Integer; Height : Integer)
                            return Cv_Rect;
   function Cv_Rect_To_Roi (Rect : Cv_Rect; Coi : Integer) return Ipl_Roi;
   function Cv_Roi_To_Rect (Roi : Ipl_Roi) return Cv_Rect;

   -----------------------------------------------------------------------------
   -- Cv_Term_Criteria
   -----------------------------------------------------------------------------
   Cv_Termcrit_Iter   : constant := 1;
   Cv_Termcrit_Number : constant := Cv_Termcrit_Iter;
   Cv_Termcrit_Eps    : constant := 2;

   type Cv_Term_Criteria is record
      Term_Type : Integer;
      Max_Iter  : Integer;
      Epsilon   : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Term_Criteria);
   type Cv_Term_Criteria_Ptr is access Cv_Term_Criteria;

   function Cv_Create_Term_Criteria (T_Type  : Integer; Max_Iter : Integer;
                                     Epsilon : Long_Float) return Cv_Term_Criteria;

   -----------------------------------------------------------------------------
   -- Cv_Point
   -----------------------------------------------------------------------------
   type Cv_Point is record
      X : Integer;
      Y : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point);
   type Cv_Point_Ptr is access all Cv_Point;
   type Cv_Point_Array is array (Integer range <>) of aliased Cv_Point;
   type Cv_Point_Array_Ptr is access all Cv_Point_Array;
   type Cv_Point_2d_Array is array (Integer range <>, Integer range <>) of aliased Cv_Point;
   Cv_Point_Dummy        : Cv_Point;

   function Cv_Create_Point (X : Integer; Y : Integer) return Cv_Point;

   package Cv_Point_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Point, Cv_Point_Array, Cv_Point_Dummy);
   subtype Cv_Point_Pointer is Cv_Point_Pointer_Pkg.Pointer;

   type Cv_Point_Pointer_Array is array (Integer range <> ) of Cv_Point_Pointer;

   function To_2d_Pointer (Src : access Cv_Point_2d_Array)
                           return Cv_Point_Pointer_Array;

   type Cv_Point_2d_32f is record
      X : Float;
      Y : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point_2d_32f);
   Cv_Point_2d_32f_Dummy : Cv_Point_2d_32f;
   type Cv_Point_2d_32f_Ptr is access all Cv_Point_2d_32f;
   type Cv_Point_2d_32f_Array is array (Integer range <>) of aliased Cv_Point_2d_32f;
   type Cv_Point_2d_32f_Array_Ptr is access all Cv_Point_2d_32f_Array;
   Cv_Point_2d_32f_Array_Null : Cv_Point_2d_32f_Array (1 .. 0);

   package Cv_Point_2d_32f_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Point_2d_32f, Cv_Point_2d_32f_Array, Cv_Point_2d_32f_Dummy);
   subtype Cv_Point_2d_32f_Pointer is Cv_Point_2d_32f_Pointer_Pkg.Pointer;

   function Cv_Create_Point_2d_32f (X : Long_Float; Y : Long_Float)
                                    return Cv_Point_2d_32f;
   function Cv_Point_To_32f (Point : Cv_Point) return Cv_Point_2d_32f;

   function Cv_Point_From_32f (Point : Cv_Point_2d_32f) return Cv_Point;


   type Cv_Point_3d_32f is record
      X : Float;
      Y : Float;
      Z : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point_3d_32f);
   type Cv_Point_3d_32f_Ptr is access all Cv_Point_3d_32f;
   type Cv_Point_3d_32f_Array is array (Integer range <>) of aliased Cv_Point_3d_32f;
   Cv_Point_3d_32f_Array_Null : Cv_Point_3d_32f_Array (1 .. 0);

   function Cv_Create_Point_3d_32f (X : Long_Float; Y : Long_Float;
                                    Z : Long_Float) return Cv_Point_3d_32f;


   type Cv_Point_2d_64f is record
      X : Long_Float;
      Y : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point_2d_64f);
   type Cv_Point_2d_64f_Ptr is access all Cv_Point_2d_64f;
   type Cv_Point_2d_64f_Array is array (Integer range <>) of aliased Cv_Point_2d_64f;
   Cv_Point_2d_64f_Array_Null : Cv_Point_2d_64f_Array (1 .. 0);
   Cv_Point_2d_64f_Dummy : Cv_Point_2d_64f;

   function Cv_Create_Point_2d_64f (X : Long_Float; Y : Long_Float)
                                    return Cv_Point_2d_64f;

   type Cv_Point_3d_64f is record
      X : Long_Float;
      Y : Long_Float;
      Z : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Point_3d_64f);
   type Cv_Point_3d_64f_Ptr is access all Cv_Point_3d_64f;
   type Cv_Point_3d_64f_Array is array (Integer range <>) of aliased Cv_Point_3d_64f;
   Cv_Point_3d_64f_Array_Null : Cv_Point_3d_64f_Array ( 1 .. 0);
   --     Cv_Point_3d_64f_Dummy : Cv_Point_3d_64f;

   function Cv_Create_Point_3d_64f (X : Long_Float; Y : Long_Float;
                                    Z : Long_Float) return Cv_Point_3d_64f;

   -----------------------------------------------------------------------------
   -- Cv_Size & Cv_Box
   -----------------------------------------------------------------------------

   type Cv_Size is record
      Width  : Integer;
      Height : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Size);
   type Cv_Size_Ptr is access all Cv_Size;

   function Cv_Create_Size (Width : Integer; Height : Integer) return Cv_Size;

   type Cv_Size_2d_32f is record
      Width  : Float;
      Height : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Size_2d_32f);

   function Cv_Create_Size_2d_32f (Width : Float; Height : Float)
                                   return Cv_Size_2d_32f;

   -- Represnation of a 2D box...
   type Cv_Box_2d is
      record
         Center : Cv_Point_2d_32f;
         Size   : Cv_Size_2d_32f;
         Angle  : Float;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Box_2d);
   type Cv_Box_2d_Ptr is access all Cv_Box_2d;

   type Cv_Line_Iterator is
      record
         Ptr         : Cv_Point_Pointer;
         Err         : Integer;
         Plus_Delta  : Integer;
         Minus_Delta : Integer;
         Plus_Step   : Integer;
         Minus_Step  : Integer;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Line_Iterator);
   type Cv_Line_Iterator_Ptr is access all Cv_Line_Iterator;

   -----------------------------------------------------------------------------
   -- CvSlice
   -----------------------------------------------------------------------------

   type Cv_Slice is record
      Start_Index : Integer;
      End_Index   : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Slice);
   type Cv_Slice_Ptr is access all Cv_Slice;

   Cv_Whole_Seq_End_Index : constant := 16#3fff_Ffff#;

   function Cv_Create_Slice (Start_Index : Integer;
                             End_Index   : Integer := Cv_Whole_Seq_End_Index)
                             return Cv_Slice;
   function Cv_Whole_Seq (Start_Index : Integer := 0;
                          End_Index   : Integer := Cv_Whole_Seq_End_Index) return Cv_Slice renames Cv_Create_Slice;

   -----------------------------------------------------------------------------
   -- CvScalar
   -----------------------------------------------------------------------------
   type Cv_Scalar is record
      Val : aliased Cv_64f_Array (1 .. 4);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Scalar);
   type Cv_Scalar_Ptr is access all Cv_Scalar;
   type Cv_Scalar_Array is array (Integer range <>) of Cv_Scalar;

   function Cv_Create_Scalar (V0 : Long_Float; V1 : Long_Float := 0.0;
                              V2 : Long_Float := 0.0; V3 : Long_Float := 0.0)
                              return Cv_Scalar;
   function Cv_Real_Scalar (V0 : Long_Float) return Cv_Scalar;
   function Cv_Scalar_All (V0123 : Long_Float) return Cv_Scalar;

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
   pragma Convention (C_Pass_By_Copy, Cv_Mem_Block);

   Cv_Storage_Magic_Val : constant := 16#42890000#;

   type Cv_Mem_Storage is record
      Signature  : Integer;
      Bottom     : access Cv_Mem_Block;
      Top        : access Cv_Mem_Block;
      Parent     : access Cv_Mem_Storage;
      Block_Size : Integer;
      Free_Space : Integer;
   end record;
   type Cv_Mem_Storage_Ptr is access all Cv_Mem_Storage;

   function Cv_Is_Storage (Storage : Cv_Mem_Storage_Ptr) return Integer;

   type Cv_Mem_Storage_Pos is record
      Top        : access Cv_Mem_Block;
      Free_Space : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Mem_Storage_Pos);
   type Cv_Mem_Storage_Pos_Ptr is access all Cv_Mem_Storage_Pos;

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
   pragma Convention (C_Pass_By_Copy, Cv_Seq_Block);
   type Cv_Seq_Block_Ptr is access all Cv_Seq_Block;

   procedure Cv_Change_Seq_Block (Reader    : Cv_Void_Ptr;
                                  Direction : Integer);

   type Cv_Seq_Ptr is access all Cv_Seq;
   type Cv_Seq is record
      Flags       : Unsigned_32;
      Header_Size : Integer;
      H_Prev      : Cv_Seq_Ptr;
      H_Next      : Cv_Seq_Ptr;
      V_Prev      : Cv_Seq_Ptr;
      V_Next      : Cv_Seq_Ptr;
      Total       : Integer;
      Elem_Size   : Integer;
      Block_Max   : Interfaces.C.Strings.Chars_Ptr;
      Ptr         : Cv_Arr_Pointer; --test this
      Delta_Elems : Integer;
      Storage     : Cv_Mem_Storage_Ptr;
      Free_Blocks : Cv_Seq_Block_Ptr;
      First       : Cv_Seq_Block_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Seq);
   type Cv_Seq_Ptr_Array is array (Integer range <>) of aliased Cv_Seq_Ptr;

   Cv_Type_Name_Seq : constant String := "opencv-sequence";
   Cv_Type_Name_Seq_Tree : constant String := "opencv-sequence-tree";

   -----------------------------------------------------------------------------
   -- Cv_Set
   -----------------------------------------------------------------------------
   type Cv_Set_Elem is record
      Flags     : Integer;
      Next_Free : access Cv_Set_Elem;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Set_Elem);
   type Cv_Set_Elem_Ptr is access all Cv_Set_Elem;
   type Cv_Set_Elem_Array is array (Integer range <>) of aliased Cv_Set_Elem;
   Cv_Set_Elem_Dummy : Cv_Set_Elem;

   package Cv_Set_Elem_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Set_Elem, Cv_Set_Elem_Array, Cv_Set_Elem_Dummy);
   subtype Cv_Set_Elem_Pointer is Cv_Set_Elem_Pointer_Pkg.Pointer;

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
   pragma Convention (C_Pass_By_Copy, Cv_Set);
   type Cv_Set_Ptr is access all Cv_Set;
   type Cv_Set_Array is array (Integer range <>) of aliased Cv_Set;
   Cv_Set_Dummy : Cv_Set;

   package Cv_Set_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Set, Cv_Set_Array, Cv_Set_Dummy);
   subtype Cv_Set_Pointer is Cv_Set_Pointer_Pkg.Pointer;

   Cv_Set_Elem_Idx_Mask : constant := (16#4000000# - 1);
   Cv_Set_Elem_Free_Flag : constant := 16#80000000#;

   function Cv_Is_Set_Elem (Ptr : Cv_Set_Elem_Ptr) return Integer;

   -----------------------------------------------------------------------------
   -- Cv_Graph
   -----------------------------------------------------------------------------
   --type Cv_Graph_Edge;
   type Cv_Graph_Edge_Ptr is access all Cv_Graph_Edge;
   type Cv_Graph_Edge_Ptr_Array is array (1 .. 2) of Cv_Graph_Edge_Ptr;

   type Cv_Graph_Vtx_Ptr is access all Cv_Graph_Vtx;
   type Cv_Graph_Vtx_Ptr_Array is array (1 .. 2) of Cv_Graph_Vtx_Ptr;

   type Cv_Graph_Edge is record
      Flags  : Integer;
      Weight : Float;
      Next   : Cv_Graph_Edge_Ptr_Array;
      Vtx    : Cv_Graph_Vtx_Ptr_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Graph_Edge);

   type Cv_Graph_Vtx is record
      Flags : Integer;
      First : access Cv_Graph_Vtx;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Graph_Vtx);

   type Cv_Graph_Vtx_2d is
      record
         Flags : Integer; --CV_GRAPH_VERTEX_FIELDS()
         First : Cv_Graph_Edge_Ptr;
         Ptr   : Cv_Point_2d_32f_Ptr;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Graph_Vtx_2d);
   type Cv_Graph_Vtx_2d_Ptr is access all Cv_Graph_Vtx_2d;

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
   pragma Convention (C_Pass_By_Copy, Cv_Graph);
   type Cv_Graph_Ptr is access Cv_Graph;

   Cv_Type_Name_Graph : constant String := "opencv-graph";

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
         Block_Max    : Cv_Void_Ptr;
         Ptr          : Cv_Void_Ptr;
         Delta_Elems  : Integer;
         Storage      : access Cv_Mem_Storage;
         Free_Blocks  : access Cv_Seq_Block;
         First        : access Cv_Seq_Block;
         --
         Origin       : Cv_Point;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Chain);
   type Cv_Chain_Ptr is access all Cv_Chain;


   --/* Freeman chain reader state */
   type Cv_Chain_Pt_Reader is
      record
      --CV_SEQ_READER_FIELDS()
         Headersize : Integer;
         Seq        : Cv_Seq_Ptr;
         Block      : Cv_Seq_Block_Ptr;
         Ptr        : Cv_Arr_Pointer;
         Blockmin   : Cv_Arr_Pointer;
         Blockmax   : Cv_Arr_Pointer;
         Deltaindex : Integer;
         Prevelem   : Cv_Arr_Pointer;
         --
         Code       : Unsigned_8;
         Pt         : Cv_Point;
         Deltas     : Cv_8s_2d_Array (1 .. 8, 1 .. 2);
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Chain_Pt_Reader);
   type Cv_Chain_Pt_Reader_Ptr is access all Cv_Chain_Pt_Reader;

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
         Block_Max    : Cv_Void_Ptr;
         Ptr          : Cv_Void_Ptr;
         Delta_Elems  : Integer;
         Storage      : access Cv_Mem_Storage;
         Free_Blocks  : access Cv_Seq_Block;
         First        : access Cv_Seq_Block;
         --           CV_CONTOUR_FIELDS()
         Rect         : Cv_Rect;
         Color        : Integer;
         Reserved     : Cv_32s_Array (1 .. 3);
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Contour);
   type Cv_Contour_Ptr is access all Cv_Contour;
   type Cv_Point_2d_Seq is new Cv_Contour;



   -----------------------------------------------------------------------------
   -- Sequence types
   -----------------------------------------------------------------------------
   Cv_Seq_Magic_Val : constant := 16#42990000#;

   --#define CV_IS_SEQ(seq) \
   --    ((seq) != NULL && (((CvSeq*)(seq))->flags & CV_MAGIC_MASK) == CV_SEQ_MAGIC_VAL)
   function Cv_Is_Seq (Seq : Cv_Seq_Ptr) return Integer;

   Cv_Set_Magic_Val : constant := 16#42980000#;

   --#define CV_IS_SET(set) \
   --((set) != NULL && (((CvSeq*)(set))->flags & CV_MAGIC_MASK) == CV_SET_MAGIC_VAL)
   function Cv_Is_Set (Set : Cv_Seq_Ptr) return Integer;

   Cv_Seq_Eltype_Bits : constant := 12;

   Cv_Seq_Eltype_Mask : constant := (16#1000# - 1); --(16#200# - 1);

   function Cv_Seq_Eltype_Point return Unsigned_32;  --/* (x,y) */ -- used to be Integer
   function Cv_Seq_Eltype_Code return Unsigned_32; -- used to be Integer
   Cv_Seq_Eltype_Generic : constant := 0;
   Cv_Seq_Eltype_Ptr : constant := Cv_Usrtype1;
   Cv_Seq_Eltype_Ppoint : constant := Cv_Seq_Eltype_Ptr;  --/* &(x,y) */
   function Cv_Seq_Eltype_Index return Unsigned_32; -- used to be Integer
   Cv_Seq_Eltype_Graph_Edge : constant := 0;  --/* &next_o, &next_d, &vtx_o, &vtx_d */
   Cv_Seq_Eltype_Graph_Vertex : constant := 0;  --/* first_edge, &(x,y) */
   Cv_Seq_Eltype_Trian_Atr : constant := 0;  --/* vertex of the binary tree   */
   Cv_Seq_Eltype_Connected_Comp : constant := 0;  --/* connected component  */
   function Cv_Seq_Eltype_Point3d return Unsigned_32; -- used to be Integer

   Cv_Seq_Kind_Bits : constant := 2;
   Cv_Seq_Kind_Mask : constant := (16#4000#); --(4 >> 12)

   --/* types of sequences */
   Cv_Seq_Kind_Generic : constant := 0;
   Cv_Seq_Kind_Curve : constant := 16#1000#; --(1 << CV_SEQ_ELTYPE_BITS) ( 1 << 12)
   Cv_Seq_Kind_Bin_Tree : constant := 16#2000#; --(2 << CV_SEQ_ELTYPE_BITS)

   --/* types of sparse sequences (sets) */
   Cv_Seq_Kind_Graph : constant := 16#1000#; -- #define CV_SEQ_KIND_GRAPH       (3 << CV_SEQ_ELTYPE_BITS)
   Cv_Seq_Kind_Subdiv2d : constant := 16#2000#; --(2 << CV_SEQ_ELTYPE_BITS);

   Cv_Seq_Flag_Shift : constant := 14; --(CV_SEQ_KIND_BITS + CV_SEQ_ELTYPE_BITS)(2+12);

   --/* flags for curves */
   Cv_Seq_Flag_Closed : constant := 16#4000#; --(1 << CV_SEQ_FLAG_SHIFT);
   Cv_Seq_Flag_Simple : constant := 16#0#; --(0 << CV_SEQ_FLAG_SHIFT);
   Cv_Seq_Flag_Convex : constant := 16#0#; --(0 << CV_SEQ_FLAG_SHIFT);
   Cv_Seq_Flag_Hole : constant := 16#8000#; --(2 << CV_SEQ_FLAG_SHIFT);

   --/* flags for graphs */
   Cv_Graph_Flag_Oriented : constant := 16#4000#;

   Cv_Graph_C : constant := Cv_Seq_Kind_Graph; --16#600#
   Cv_Oriented_Graph : constant := 16#5000#;      --(CV_SEQ_KIND_GRAPH | CV_GRAPH_FLAG_ORIENTED)

   --/* point sets */
   function Cv_Seq_Point_Set return Integer;

   function Cv_Seq_Point3d_Set return Integer; --: constant := (CV_SEQ_KIND_GENERIC | CV_SEQ_ELTYPE_POINT3D)
   function Cv_Seq_Polyline return Integer; --: constant := (CV_SEQ_KIND_CURVE  | CV_SEQ_ELTYPE_POINT)
   function Cv_Seq_Polygon return Integer; --(CV_SEQ_FLAG_CLOSED | CV_SEQ_POLYLINE
   function Cv_Seq_Contour return Integer renames Cv_Seq_Polygon;
   function Cv_Seq_Simple_Polygon return Integer; --(CV_SEQ_FLAG_SIMPLE | CV_SEQ_POLYGON  )

   --/* chain-coded curves */
   function Cv_Seq_Chain return Integer; --(CV_SEQ_KIND_CURVE  | CV_SEQ_ELTYPE_CODE)
   function Cv_Seq_Chain_Contour return Integer;  --(CV_SEQ_FLAG_CLOSED | CV_SEQ_CHAIN)

   --/* binary tree for the contour */
   function Cv_Seq_Polygon_Tree return Integer; --(CV_SEQ_KIND_BIN_TREE  | CV_SEQ_ELTYPE_TRIAN_ATR)

   --/* sequence of the connected components */
   function Cv_Seq_Connected_Comp return Integer; --(CV_SEQ_KIND_GENERIC  | CV_SEQ_ELTYPE_CONNECTED_COMP)

   --/* sequence of the integer numbers */
   function Cv_Seq_Index return Integer; --(CV_SEQ_KIND_GENERIC  | CV_SEQ_ELTYPE_INDEX)

   function  Cv_Seq_Eltype ( Seq : Cv_Seq_Ptr ) return Unsigned_32;  -- ((Seq)- > Flags & CV_SEQ_ELTYPE_MASK) -- used to be Integer
   function Cv_Seq_Kind ( Seq : Cv_Seq_Ptr ) return Integer;    --((Seq)- > Flags & CV_SEQ_KIND_MASK )

   --/* flag checking */
   function Cv_Is_Seq_Index ( Seq : Cv_Seq_Ptr) return Integer;

   function Cv_Is_Seq_Curve ( Seq : Cv_Seq_Ptr) return Integer; --      (CV_SEQ_KIND (Seq) =  = CV_SEQ_KIND_CURVE)
   function Cv_Is_Seq_Closed ( Seq : Cv_Seq_Ptr) return Integer; --     (((Seq)- > Flags & CV_SEQ_FLAG_CLOSED) ! = 0)
   function Cv_Is_Seq_Convex ( Seq : Cv_Seq_Ptr) return Integer; -- (((Seq)- > Flags & CV_SEQ_FLAG_CONVEX) ! = 0)
   function Cv_Is_Seq_Hole ( Seq : Cv_Seq_Ptr) return Integer; --      (((Seq)- > Flags & CV_SEQ_FLAG_HOLE) ! = 0)
   function Cv_Is_Seq_Simple ( Seq : Cv_Seq_Ptr) return Integer; --    ((((Seq)- > Flags & CV_SEQ_FLAG_SIMPLE) ! = 0) |  | CV_IS_SEQ_CONVEX(seq))

   --/* type checking macros */
   function Cv_Is_Seq_Point_Set ( Seq : Cv_Seq_Ptr) return Integer;

   function Cv_Is_Seq_Point_Subset ( Seq : Cv_Seq_Ptr) return Integer;

   function Cv_Is_Seq_Polyline ( Seq : Cv_Seq_Ptr ) return Integer; --      (CV_SEQ_KIND(seq) == CV_SEQ_KIND_CURVE && CV_IS_SEQ_POINT_SET(seq))

   function Cv_Is_Seq_Polygon ( Seq : Cv_Seq_Ptr) return Integer; --      (CV_IS_SEQ_POLYLINE(seq) && CV_IS_SEQ_CLOSED(seq))

   function Cv_Is_Seq_Chain ( Seq : Cv_Seq_Ptr) return Integer; --      (CV_SEQ_KIND(seq) == CV_SEQ_KIND_CURVE && (seq)->elem_size == 1)

   function Cv_Is_Seq_Contour ( Seq : Cv_Seq_Ptr) return Integer; --      (CV_IS_SEQ_CLOSED(seq) && (CV_IS_SEQ_POLYLINE(seq) || CV_IS_SEQ_CHAIN(seq)))

   function Cv_Is_Seq_Chain_Contour ( Seq : Cv_Seq_Ptr ) return Integer; --      (CV_IS_SEQ_CHAIN( seq ) && CV_IS_SEQ_CLOSED( seq ))

   function Cv_Is_Seq_Polygon_Tree ( Seq  : Cv_Seq_Ptr) return Integer; --      (CV_SEQ_ELTYPE (seq) ==  CV_SEQ_ELTYPE_TRIAN_ATR &&    \

   function Cv_Is_Graph ( Seq : Cv_Seq_Ptr) return Integer; --(CV_IS_SET(seq) && CV_SEQ_KIND((CvSet*)(seq)) == CV_SEQ_KIND_GRAPH)

   function Cv_Is_Graph_Oriented ( Seq : Cv_Seq_Ptr) return Integer; --      (((seq)->flags & CV_GRAPH_FLAG_ORIENTED) != 0)

   function Cv_Is_Subdiv2d ( Seq : Cv_Seq_Ptr) return Integer; --      (CV_IS_SET(seq) && CV_SEQ_KIND((CvSet*)(seq)) == CV_SEQ_KIND_SUBDIV2D)

   -----------------------------------------------------------------------------
   -- Sequence writer & reader
   -----------------------------------------------------------------------------
   type Cv_Seq_Writer is
      record
         Header_Size : Integer;
         Seq         : Cv_Seq_Ptr;
         Block       : Cv_Seq_Block_Ptr;
         Ptr         : Cv_Arr_Pointer;
         Block_Min   : Cv_Arr_Pointer;
         Block_Max   : Cv_Arr_Pointer;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Seq_Writer);
   type Cv_Seq_Writer_Ptr is access all Cv_Seq_Writer;

   --Should not be here
   procedure Cv_Create_Seq_Block (Writer : Cv_Seq_Writer_Ptr);

   type Cv_Seq_Reader is
      record
         Headersize : Integer;
         Seq        : Cv_Seq_Ptr;
         Block      : Cv_Seq_Block_Ptr;
         Ptr        : Cv_Arr_Pointer;
         Blockmin   : Cv_Arr_Pointer;
         Blockmax   : Cv_Arr_Pointer;
         Deltaindex : Integer;
         Prevelem   : Cv_Arr_Pointer;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Seq_Reader);
   type Cv_Seq_Reader_Ptr is access all Cv_Seq_Reader;

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
   procedure Cv_Write_Seq_Elem_Var ( Elem_Ptr : Cv_Arr_Pointer;
                                    Writer   : Cv_Seq_Writer );

   -- Not portable to Ada.
   procedure Cv_Write_Seq_Elem ( Elem_Ptr : Cv_Arr_Pointer;
                                Writer   : Cv_Seq_Writer ) renames Cv_Write_Seq_Elem_Var;

   --  /* Move reader position forward: */
   procedure Cv_Next_Seq_Elem (Elem_Size : Integer;
                               Reader    : Cv_Seq_Reader_Ptr);
   procedure Cv_Next_Seq_Elem (Elem_Size : Integer;
                               Reader    : Cv_Chain_Pt_Reader_Ptr);


   --  /* Move reader position backward: */
   procedure Cv_Prev_Seq_Elem ( Elem_Size : Integer;
                               Reader    : Cv_Seq_Reader );

   --  /* Read element and move read position forward: */
   procedure Cv_Read_Seq_Elem ( Elem  : Cv_Arr_Pointer;
                               Reader : Cv_Seq_Reader_Ptr );
   procedure Cv_Read_Seq_Elem ( Elem  : Unsigned_8;
                               Reader : Cv_Chain_Pt_Reader_Ptr );

   --  /* Read element and move read position backward: */
   procedure Cv_Rev_Read_Seq_Elem ( Elem  : Cv_Arr_Pointer;
                                   Reader : Cv_Seq_Reader );

   procedure Cv_Read_Chain_Point ( Pt    : access Cv_Point;
                                  Reader : Cv_Chain_Pt_Reader_Ptr );

   function Cv_Current_Point ( Reader : Cv_Chain_Pt_Reader ) return Cv_Point; --  (*((CvPoint*)((reader).ptr)))
   function Cv_Prev_Point ( Reader : Cv_Chain_Pt_Reader) return Cv_Point; -- ( * ((CvPoint * ) ((Reader).Prev_Elem)))

   procedure Cv_Read_Edge ( Pt1   : Cv_Point_Ptr;
                           Pt2    : out Cv_Point_Ptr;
                           Reader : Cv_Chain_Pt_Reader_Ptr );

   -----------------------------------------------------------------------------
   -- Graph macros
   -----------------------------------------------------------------------------
   --  /* Return next graph edge for given vertex: */
   procedure Cv_Next_Graph_Edge ( Edge  : Cv_Graph_Edge_Ptr;
                                 Vertex : Cv_Graph_Vtx_Ptr );

   -----------------------------------------------------------------------------
   -- Data structures for persistence (a.k.a serialization) functionality
   -----------------------------------------------------------------------------


   type Cv_File_Storage_Ptr is access all Null_Record;

   Cv_Storage_Read         : constant := 0;
   Cv_Storage_Write        : constant := 1;
   Cv_Storage_Write_Text   : constant := Cv_Storage_Write;
   Cv_Storage_Write_Binary : constant := Cv_Storage_Write;
   Cv_Storage_Append       : constant := 2;

   type Cv_Attr_List;
   type Cv_Attr_List_Ptr is access all Cv_Attr_List;
   type Cv_Attr_List is record
      Attr : Cv_String_Pointer;
      Next : Cv_Attr_List_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Attr_List);


   function Cv_Create_Attr_List (Attr : Cv_String_Pointer := null;
                                 Next : Cv_Attr_List_Ptr := null)
                                 return Cv_Attr_List;

   -- Black box type only use the access type
   type Cv_Type_Info;
   type Cv_Type_Info_Ptr is access all Cv_Type_Info;


   type Cv_Is_Instance_Func is access function (Struct_Pointer : Cv_Void_Ptr)
                                                return Integer;
   pragma Convention (C, Cv_Is_Instance_Func);

   type Cv_Release_Proc is access procedure (Struct_Dblptr : access Cv_Void_Ptr);
   pragma Convention (C, Cv_Release_Proc);

   type Cv_Read_Func is access function (Storage : Cv_File_Storage_Ptr;
                                         Node    : Cv_File_Node_Ptr)
                                         return Cv_Void_Ptr;
   pragma Convention (C, Cv_Read_Func);

   type Cv_Write_Proc is access procedure (Storage    : Cv_File_Storage_Ptr;
                                           Name       : Interfaces.C.Strings.Chars_Ptr;
                                           Struct_Ptr : Cv_Void_Ptr;
                                           Attributes : Cv_Attr_List);
   pragma Convention (C, Cv_Write_Proc);

   type Cv_Clone_Func is access function (Struct_Pointer : Cv_Void_Ptr)
                                          return Cv_Void_Ptr;
   pragma Convention (C, Cv_Clone_Func);


   type Cv_Type_Info is record
      Flags       : Unsigned_32;
      Header_Size : Integer;
      Prev        : Cv_Type_Info_Ptr;
      Next        : Cv_Type_Info_Ptr;
      Type_Name   : Interfaces.C.Strings.Chars_Ptr; -- Will require wrapper?
      Is_Instance : Cv_Is_Instance_Func;
      Release     : Cv_Release_Proc;
      Read        : Cv_Read_Func;
      Write       : Cv_Write_Proc;
      Clone       : Cv_Clone_Func;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Type_Info);


   Cv_Node_None            : constant := 0;
   Cv_Node_Int             : constant := 1;
   Cv_Node_Integer         : constant := Cv_Node_Int;
   Cv_Node_Real            : constant := 2;
   Cv_Node_Float           : constant := Cv_Node_Real;
   Cv_Node_Str             : constant := 3;
   Cv_Node_String          : constant := Cv_Node_Str;
   Cv_Node_Ref             : constant := 4; -- Not used
   Cv_Node_Seq             : constant := 5;
   Cv_Node_Map             : constant := 6;
   Cv_Node_Type_Mask       : constant := 7;

   Cv_Node_Flow            : constant Unsigned_32 := 8;
   Cv_Node_User            : constant Unsigned_32 := 16;
   Cv_Node_Empty           : constant Unsigned_32 := 32;
   Cv_Node_Named           : constant Unsigned_32 := 64;

   function Cv_Node_Type (Flags : Unsigned_32)
                          return Unsigned_32;

   function Cv_Node_Is_Int (Flags : Unsigned_32)
                            return Boolean;

   function Cv_Node_Is_Real (Flags : Unsigned_32)
                             return Boolean;

   function Cv_Node_Is_String (Flags : Unsigned_32)
                               return Boolean;

   function Cv_Node_Is_Seq (Flags : Unsigned_32)
                            return Boolean;

   function Cv_Node_Is_Map (Flags : Unsigned_32)
                            return Boolean;

   function Cv_Node_Is_Collection (Flags : Unsigned_32)
                                   return Boolean;

   function Cv_Node_Is_Flow (Flags : Unsigned_32)
                             return Boolean;

   function Cv_Node_Is_Empty (Flags : Unsigned_32)
                              return Boolean;

   function Cv_Node_Is_User (Flags : Unsigned_32)
                             return Boolean;

   function Cv_Node_Has_Name (Flags : Unsigned_32)
                              return Boolean;

   Cv_Node_Seq_Simple      : constant Unsigned_32 := 256;

   function Cv_Node_Seq_Is_Simple (Seq : access Cv_Seq)
                                   return Boolean;

   type Cv_String is
      record
         Len : Integer;
         Pts : Interfaces.C.Strings.Chars_Ptr;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_String);

   type Cv_String_Hash_Node;
   type Cv_String_Hash_Node_Ptr is access all Cv_String_Hash_Node;
   type Cv_String_Hash_Node is record
      Hashval : Unsigned_32;
      Str     : Cv_String;
      Next    : Cv_String_Hash_Node_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_String_Hash_Node);


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
      Block_Max    : Cv_Void_Ptr;
      Ptr          : Cv_Void_Ptr;
      Delta_Elems  : Integer;
      Storage      : access Cv_Mem_Storage;
      Free_Blocks  : access Cv_Seq_Block;
      First        : access Cv_Seq_Block;

      -- CV_SET_FIELDS
      Free_Elems   : access Cv_Set_Elem;
      Active_Count : Integer;

      Tab_Size     : Integer;
      Table        : access Cv_Void_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Generic_Hash);
   subtype Cv_File_Node_Hash is Cv_Generic_Hash;
   type Cv_File_Node_Hash_Ptr is access all Cv_File_Node_Hash;

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
            Seq : Cv_Seq_Ptr;
         when Map =>
            Map : Cv_File_Node_Hash_Ptr;
      end case;
   end record;
   pragma Unchecked_Union (Cv_File_Node_Data);
   pragma Convention (C, Cv_File_Node_Data);


   type Cv_File_Node is record
      Tag  : Integer;
      Info : Cv_Type_Info_Ptr;
      Data : Cv_File_Node_Data;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_File_Node);

   type Cv_Plugin_Func_Info is record
      Func_Addr         : access Cv_Void_Ptr;
      Default_Func_Addr : Cv_Void_Ptr;
      Func_Names        : Interfaces.C.Strings.Chars_Ptr;
      Search_Modules    : Integer;
      Loaded_From       : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Plugin_Func_Info);
   type Cv_Plugin_Func_Info_Ptr is access all Cv_Plugin_Func_Info;

   type Cv_Module_Info;
   type Cv_Module_Info_Ptr is access all Cv_Module_Info;
   type Cv_Module_Info is record
      Next     : Cv_Module_Info_Ptr;
      Name     : Interfaces.C.Strings.Chars_Ptr;
      Version  : Interfaces.C.Strings.Chars_Ptr;
      Func_Tab : Cv_Plugin_Func_Info_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Module_Info);

   -- Unchecked Conversions ----------------------------------------------------
   ------------ Arr conversions ------------------------------------------------
   --     pragma Warnings (Off);

   function To_Arr_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Contour_Ptr,
                                   Target => Cv_Arr_Ptr);

   function To_Arr_Ptr is
     new Ada.Unchecked_Conversion (Source => Ipl_Image_Ptr,
                                   Target => Cv_Arr_Ptr);

   function To_Arr_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Scalar_Ptr,
                                   Target => Cv_Arr_Ptr);

   function To_Arr_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Mat_Ptr,
                                   Target => Cv_Arr_Ptr);

   function To_Image_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_Ptr,
                                   Target => Ipl_Image_Ptr);

   function To_Scalar_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_Ptr,
                                   Target => Cv_Scalar_Ptr);

   function To_Mat_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_Ptr,
                                   Target => Cv_Mat_Ptr);

   function To_Sparse_Mat_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_Ptr,
                                   Target => Cv_Sparse_Mat_Ptr);

   function To_Point_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_Ptr,
                                   Target => Cv_Point_Ptr);

   -- Unchecked Conversions ----------------------------------------------------
   ------------ Void conversions -----------------------------------------------
   function To_8u_Pointer is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_8u_Pointer);

   function To_8s_Pointer is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_8s_Pointer);

   function To_16u_Pointer is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_16u_Pointer);

   function To_16s_Pointer is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_16s_Pointer);

   function To_32s_Pointer is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_32s_Pointer);

   function To_32f_Pointer is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_32f_Pointer);

   function To_64f_Pointer is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_64f_Pointer);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_8u_Pointer,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_8s_Pointer,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_16u_Pointer,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_16s_Pointer,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_32s_Pointer,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_32f_Pointer,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_64f_Pointer,
                                   Target => Cv_Void_Ptr);

   function To_Point_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_Point_Ptr);

   function To_Seq_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_Seq_Ptr);

   function To_Contour_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_Contour_Ptr);

   function To_Image_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Ipl_Image_Ptr);

   function To_Scalar_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_Scalar_Ptr);

   function To_Arr_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_Arr_Ptr);

   function To_Mat_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                   Target => Cv_Mat_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Ipl_Image_Ptr,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Scalar_Ptr,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Arr_Ptr,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Mat_Ptr,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Chain_Pt_Reader_Ptr,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Seq_Reader_Ptr,
                                   Target => Cv_Void_Ptr);

   function To_Void_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Seq_Ptr,
                                   Target => Cv_Void_Ptr);

   function "+" (Right : Ipl_Image_Ptr) return Cv_Arr_Ptr;

   -----------------------------------------------------------------------------
   -- Cv_Seq conversions
   -----------------------------------------------------------------------------

   function To_Seq_Ptr is
     new Ada.Unchecked_Conversion (Source => Cv_Set_Ptr,
                                   Target => Cv_Seq_Ptr);

   pragma Warnings (On);

   -----------------------------------------------------------------------------
   -- C ** conversions
   -----------------------------------------------------------------------------
   -- START Unbounded array pointers --
   Mat_Data_Requirement : Mat_Data; -- This is just a dummy, do not use!
   type Cv_Mat_Array is array (Integer range <>) of aliased Cv_Mat;
   type Cv_Mat_Ptr_Array is array (Integer range <>) of aliased Cv_Mat_Ptr;
   type Cv_Size_Array is array (Integer range <>) of aliased Cv_Size;
   type Cv_Size_Ptr_Array is array (Integer range <>) of aliased Cv_Size_Ptr;


   package Cv_Mat_Ptr_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Mat_Ptr, Cv_Mat_Ptr_Array, null);
   subtype Cv_Mat_Ptr_Pointer is Cv_Mat_Ptr_Pointer_Pkg.Pointer;

   package Cv_Size_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Cv_Size, Cv_Size_Array, (0, 0));
   subtype Cv_Size_Pointer is Cv_Size_Pointer_Pkg.Pointer;

   function Cv_Mat_Elem (Mat       : Cv_Mat_Ptr;
                         Elem_Size : Integer;
                         Row       : Integer;
                         Col       : Integer)
                         return Cv_Void_Ptr;


   -----------------------------------------------------------------------------
   -- From Operations
   -----------------------------------------------------------------------------

   Cv_Max_Arr : constant := 10;

   Cv_No_Depth_Check : constant := 1;
   Cv_No_Cn_Check : constant := 2;
   Cv_No_Size_Check : constant := 4;

   type Cv_N_Array_Cv_Mat_Nd_Ptr_Array is array (Integer range 1 .. Cv_Max_Arr) of Cv_Mat_Nd_Ptr;

   type Cv_N_Array_Iterator is
      record
         Count : Integer;
         Dims  : Integer;
         Size  : Cv_Size;
         Ptr   : Cv_Void_Ptr_Array (1 .. Cv_Max_Arr);
         Stack : Cv_32s_Array (1 .. Cv_Max_Dim);
         Hdr   : Cv_N_Array_Cv_Mat_Nd_Ptr_Array;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_N_Array_Iterator);

   type Compare_Op is new Integer;
   Cv_Cmp_Eq : constant Compare_Op := 0;
   Cv_Cmp_Gt : constant Compare_Op := 1;
   Cv_Cmp_Ge : constant Compare_Op := 2;
   Cv_Cmp_Lt : constant Compare_Op := 3;
   Cv_Cmp_Le : constant Compare_Op := 4;
   Cv_Cmp_Ne : constant Compare_Op := 5;

   type Cv_Cmp_Func is access function (A        : Cv_Void_Ptr;
                                        B        : Cv_Void_Ptr;
                                        Userdata : Cv_Void_Ptr)
                                        return Integer;
   pragma Convention (C, Cv_Cmp_Func);

   type Cv_Graph_Scanner is record
      Vtx   : Cv_Graph_Vtx_Ptr;
      Dst   : Cv_Graph_Vtx_Ptr;
      Edge  : Cv_Graph_Edge_Ptr;

      Graph : Cv_Graph_Ptr;
      Stack : Cv_Seq_Ptr;
      Index : Integer;
      Mask  : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Graph_Scanner);
   type Cv_Graph_Scanner_Ptr is access all Cv_Graph_Scanner;

   type Cv_Font_Face is new Integer;
   Cv_Font_Hershey_Simplex  : constant Cv_Font_Face := 0;
   Cv_Font_Hershey_Plain  : constant Cv_Font_Face := 1;
   Cv_Font_Hershey_Duplex  : constant Cv_Font_Face :=  2;
   Cv_Font_Hershey_Complex  : constant Cv_Font_Face := 3;
   Cv_Font_Hershey_Triplex  : constant Cv_Font_Face := 4;
   Cv_Font_Hershey_Complex_Small  : constant Cv_Font_Face := 5;
   Cv_Font_Hershey_Script_Simplex  : constant Cv_Font_Face := 6;
   Cv_Font_Hershey_Script_Complex  : constant Cv_Font_Face := 7;

   type Cv_Font is record
      Name_Font         : access String_C;
      Color             : Cv_Scalar;
      Font_Face         : Cv_Font_Face;
      Ascii             : Cv_32u_Array_Ptr;
      Greek             : Cv_32u_Array_Ptr;
      Cyrillic          : Cv_32u_Array_Ptr;
      Hscale, Vscale    : Float;
      Shear             : Float;
      Thickness         : Integer;
      Dx                : Float;
      Line_Type         : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Font);
   type Cv_Font_Ptr is access all Cv_Font;

   type Cv_Tree_Node_Iterator is record
      Node      : Cv_Void_Ptr;
      Level     : Integer;
      Max_Level : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Tree_Node_Iterator);
   type Cv_Tree_Node_Iterator_Ptr is access all Cv_Tree_Node_Iterator;

   type Cv_Alloc_Func is access function (Size     : Interfaces.C.Size_T;
                                          Userdata : Cv_Void_Ptr)
                                          return Cv_Void_Ptr;
   pragma Convention (C, Cv_Alloc_Func);

   type Cv_Free_Func is access function (Pptr     : Cv_Void_Ptr;
                                         Userdata : Cv_Void_Ptr)
                                         return Integer;
   pragma Convention (C, Cv_Free_Func);

   -- typedef IplImage* (CV_STDCALL* Cv_iplCreateImageHeader)
   --   (int,int,int,char*,char*,int,int,int,int,int,IplROI*,IplImage*,void*,IplTileInfo*);
   type Cv_Ipl_Create_Image_Header is access function (N_Size        : Integer;
                                                       Id            : Integer;
                                                       N_Channels    : Integer;
                                                       Alpha_Channel : Integer;
                                                       Depth         : Unsigned_32;
                                                       Color_Model   : Interfaces.C.Strings.Chars_Ptr;
                                                       Channel_Seq   : Interfaces.C.Strings.Chars_Ptr;
                                                       Data_Order    : Integer;
                                                       Origin        : Integer;
                                                       Align         : Integer;
                                                       Width         : Integer;
                                                       Height        : Integer;
                                                       Roi           : Ipl_Roi_Ptr;
                                                       Image         : Ipl_Image_Ptr;
                                                       Image_Id      : Cv_Void_Ptr)
                                                       return Ipl_Image_Ptr;
   pragma Convention (C, Cv_Ipl_Create_Image_Header);

   -- typedef void (CV_STDCALL * Cv_iplAllocateImageData) (IplImage * , int, int);
   type Cv_Ipl_Allocate_Image_Data is access procedure (Image  : Ipl_Image_Ptr;
                                                        Width  : Integer;
                                                        Height : Integer);
   pragma Convention (C, Cv_Ipl_Allocate_Image_Data);

   -- typedef void (CV_STDCALL * Cv_iplDeallocate) (IplImage * , int);
   type Cv_Ipl_Deallocate is access procedure (Image : Ipl_Image_Ptr;
                                               I     : Integer);
   pragma Convention (C, Cv_Ipl_Deallocate);

   -- typedef IplROI * (CV_STDCALL * Cv_iplCreateROI) (int, int, int, int, int);
   type Cv_Ipl_Create_Roi is access function (Coi      : Integer;
                                              Height   : Integer;
                                              Width    : Integer;
                                              X_Offset : Integer;
                                              Y_Offset : Integer)
                                              return Ipl_Roi_Ptr;
   pragma Convention (C, Cv_Ipl_Create_Roi);

   -- typedef IplImage * (CV_STDCALL * Cv_iplCloneImage) (const IplImage * );
   type Cv_Ipl_Clone_Image is access function (Image : Ipl_Image_Ptr)
                                               return Ipl_Image_Ptr;
   pragma Convention (C, Cv_Ipl_Clone_Image);

   type Cv_Error_Callback is access function (Status    : Integer;
                                              Func_Name : Interfaces.C.Strings.Chars_Ptr;
                                              Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                                              File_Name : Interfaces.C.Strings.Chars_Ptr;
                                              Line      : Integer)
                                              return Integer;
   pragma Convention (C, Cv_Error_Callback);


   -----------------------------------------------------
   -- Constants from core-operations
   -----------------------------------------------------

   -- Cv_Check_Arr
   Cv_Check_Range : constant := 1;
   Cv_Check_Quiet : constant := 2;

   -- Cv_Rand_Arr
   Cv_Rand_Uni    : constant := 0;
   Cv_Rand_Normal : constant := 1;

   -- Cv_Sort
   Cv_Sort_Every_Row : constant := 0;
   Cv_Sort_Every_Column : constant := 1;
   Cv_Sort_Ascending : constant := 0;
   Cv_Sort_Descending : constant := 16;

   -- Cv_Gemm
   Cv_Gemm_A_T  : constant Integer := 1;
   Cv_Gemm_B_T  : constant Integer := 2;
   Cv_Gemm_C_T  : constant Integer := 4;

   -- Cv_Svdecomp
   Cv_Svd_Modify_A : constant Unsigned_32 := 1;
   Cv_Svd_U_T   : constant Unsigned_32 := 2;
   Cv_Svd_V_T   : constant Unsigned_32 := 4;

   -- Cv_Invert
   Cv_Lu       : constant := 0;
   Cv_Svd      : constant := 1;
   Cv_Svd_Sym  : constant := 2;
   Cv_Cholesky : constant := 3;
   Cv_Qr       : constant := 4;
   Cv_Normal   : constant := 16;

   -- Cv_Calc_Covar_Matrix
   Cv_Covar_Scrambled : constant Unsigned_32 := 0;
   Cv_Covar_Normal    : constant Unsigned_32 := 1;
   Cv_Covar_Use_Avg   : constant Unsigned_32 := 2;
   Cv_Covar_Scale     : constant Unsigned_32 := 4;
   Cv_Covar_Rows      : constant Unsigned_32 := 8;
   Cv_Covar_Cols      : constant Unsigned_32 := 16;

   -- Cv_Calc_Pca
   Cv_Pca_Data_As_Row : constant := 0;
   Cv_Pca_Data_As_Col : constant := 1;
   Cv_Pca_Use_Avg : constant := 2;

   -- Cv_Norm
   Cv_C         : constant Unsigned_32 := 1;
   Cv_L1        : constant Unsigned_32 := 2;
   Cv_L2        : constant Unsigned_32 := 4;
   Cv_Norm_Mask : constant Unsigned_32 := 7;
   Cv_Relative  : constant Unsigned_32 := 8;
   Cv_Diff      : constant Unsigned_32 := 16;
   Cv_Minmax    : constant Unsigned_32 := 32;

   -- Cv_Norm
   Cv_Diff_C : constant := (Cv_Diff or Cv_C);
   Cv_Diff_L1 : constant := (Cv_Diff or Cv_L1);
   Cv_Diff_L2 : constant := (Cv_Diff or Cv_L2) ;
   Cv_Relative_C : constant := (Cv_Relative or Cv_C);
   Cv_Relative_L1 : constant := (Cv_Relative or Cv_L1);
   Cv_Relative_L2 : constant := (Cv_Relative or Cv_L2);

   -- Cv_Reduce
   Cv_Reduce_Sum : constant := 0;
   Cv_Reduce_Avg : constant := 1;
   Cv_Reduce_Max : constant := 2;
   Cv_Reduce_Min : constant := 3;

   -- Discrete Linear Transforms and Related Functions
   Cv_Dxt_Forward       : constant Unsigned_32 := 0;
   Cv_Dxt_Inverse       : constant Unsigned_32 := 1;
   Cv_Dxt_Scale         : constant Unsigned_32 := 2; -- Divide result by size of array
   Cv_Dxt_Inv_Scale     : constant Unsigned_32 := Cv_Dxt_Inverse + Cv_Dxt_Scale;
   Cv_Dxt_Inverse_Scale : constant Unsigned_32 := Cv_Dxt_Inv_Scale;
   Cv_Dxt_Rows          : constant Unsigned_32 := 4; -- Transform each row individually
   Cv_Dxt_Mul_Conj      : constant Unsigned_32 := 8; -- Conjugate the second argument of cvMulSpectrums

   -- Back and Front....
   Cv_Front : constant := 1;
   Cv_Back : constant := 0;

   -- Cv_Graph
   Cv_Graph_Vertex : constant := 1;
   Cv_Graph_Tree_Edge : constant := 2;
   Cv_Graph_Back_Edge : constant :=  4;
   Cv_Graph_Forward_Edge : constant := 8;
   Cv_Graph_Cross_Edge : constant := 16;
   Cv_Graph_Any_Edge : constant := 30;
   Cv_Graph_New_Tree : constant := 32;
   Cv_Graph_Backtracking : constant := 64;
   Cv_Graph_Over : constant := -1;
   Cv_Graph_All_Items : constant := -1;
   Cv_Graph_Item_Visited_Flag : constant := 16#40000000#;
   --     #define  CV_GRAPH_SEARCH_TREE_NODE_FLAG   (1 << 29)
   Cv_Graph_Search_Tree_Node_Flag : constant := 16#20000000#;
   --  #define  CV_GRAPH_FORWARD_EDGE_FLAG       (1 << 28)
   Cv_Graph_Forward_Edge_Flag : constant := 16#10000000#;

   -- Drawing
   Cv_Filled : constant := -1;
   Cv_Aa : constant := 16;

   -- Fonts
   Cv_Font_Italic : constant := 16;
   Cv_Font_Vector0 : constant Cv_Font_Face := (Cv_Font_Hershey_Simplex);

   -- Cv_K_Means2
   Cv_Kmeans_Use_Initial_Labels : constant := 1;

   -- Cv_Check_Hardware_Support
   Cv_Cpu_None : constant := 0;
   Cv_Cpu_Mmx : constant := 1;
   Cv_Cpu_Sse : constant := 2;
   Cv_Cpu_Sse2 : constant := 3;
   Cv_Cpu_Sse3 : constant := 4;
   Cv_Cpu_Ssse3 : constant := 5;
   Cv_Cpu_Sse4_1 : constant := 6;
   Cv_Cpu_Sse4_2 : constant := 7;
   Cv_Cpu_Avx : constant := 10;
   Cv_Hardware_Max_Feature : constant := 255;

   -- Error stuff
   Cv_Errmodeleaf   : constant := 0;
   Cv_Errmodeparent : constant := 1;
   Cv_Errmodesilent : constant := 2;
private

   --------------
   -- Null record type
   --------------
   type Null_Record is null record;

   pragma Import (C, Cv_Create_Mat, "CvMat_wrap");
   pragma Import (C, Cv_Ipl_Depth, "cvIplDepth");
   pragma Import (C, Cv_Floor, "cvFloor");
   pragma Import (C, Cv_Ceil, "cvCeil");
   pragma Import (C, Cv_Is_Inf, "cvIsInf");
   pragma Import (C, Cv_Is_Nan, "cvIsNaN");
   pragma Import (C, Cv_Create_Rng, "cvRNG");
   pragma Import (C, Cv_Rand_Int, "cvRandInt");
   pragma Import (C, Cv_Rand_Real, "cvRandReal");
   pragma Import (C, Cvm_Get, "cvmGet");
   pragma Import (C, Cvm_Set, "cvmSet");
   pragma Import (C, Cv_Create_Seq_Block, "cvCreateSeqBlock");
   pragma Import (C, Cv_Change_Seq_Block, "cvChangeSeqBlock");
   pragma Import (C, Cv_Next_Seq_Elem, "cvNextSeqElem");
   pragma Import (C, Cv_Mat_Elem, "cvMatElem_wrap");
--     pragma Import (C, Cv_Mat_Elem_Ptr_Fast, "Cv_Mat_Elem_Ptr_Fast");
   pragma Import (C, Cv_Write_Seq_Elem_Var, "Cv_Write_Seq_Elem_Var");
   pragma Import (C, Cv_Prev_Seq_Elem, "Cv_Prev_Seq_Elem");
   pragma Import (C, Cv_Rev_Read_Seq_Elem, "Cv_Rev_Read_Seq_Elem");
   pragma Import (C, Cv_Read_Seq_Elem, "Cv_Read_Seq_Elem");
   pragma Import (C, Cv_Read_Chain_Point, "Cv_Read_Chain_Point");
   pragma Import (C, Cv_Next_Graph_Edge, "Cv_Next_Graph_Edge");
   pragma Import (C, Cv_Read_Edge, "Cv_Read_Edge");

   pragma Import (C, Cv_Is_Mat_Hdr, "Cv_Is_Mat_Hdr");
   pragma Import (C, Cv_Is_Mat, "Cv_Is_Mat");
   pragma Import (C, Cv_Is_Mask_Arr, "Cv_Is_Mask_Arr");
   pragma Import (C, Cv_Are_Types_Eq, "Cv_Are_Types_Eq");
   pragma Import (C, Cv_Are_Cns_Eq, "Cv_Are_Cns_Eq");
   pragma Import (C, Cv_Are_Depths_Eq, "Cv_Are_Depths_Eq");
   pragma Import (C, Cv_Are_Sizes_Eq, "Cv_Are_Sizes_Eq");
   pragma Import (C, Cv_Is_Mat_Const, "Cv_Is_Mat_Const");
   pragma Import (C, Cv_Mat_Elem_Ptr_Fast, "Cv_Mat_Elem_Ptr_Fast");
   pragma Import (C, Cv_Mat_Elem_Ptr, "Cv_Mat_Elem_Ptr");

   pragma Import (C, Cv_Is_Mat_ND_Hdr, "Cv_Is_Mat_ND_Hdr");
   pragma Import (C, Cv_Is_Mat_ND, "Cv_Is_Mat_ND");

   pragma Import (C, Cv_Is_Sparse_Mat_Hdr, "Cv_Is_Sparse_Mat_Hdr");
   pragma Import (C, Cv_Is_Sparse_Mat, "Cv_Is_Sparse_Mat");
   pragma Import (C, Cv_Is_Sparse_Hist, "Cv_Is_Sparse_Hist");
end Core;
