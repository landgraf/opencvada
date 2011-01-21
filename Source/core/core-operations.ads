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
-- core_c.ads - core_c.h
-- Comments, Information, Other
-----------------------------------------------------------------------

--  with Interfaces; use Interfaces;
--  with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with GNAT.Source_Info;
with Imgproc;

package Core.Operations is
--

   -----------------------------------------------------------------------------
   -- Array allocation, deallocation, initialization and access to elements
   -----------------------------------------------------------------------------
   --     Allocates a memory buffer.
   function Cv_Alloc (Size : Interfaces.C.Size_T)
                     return Cv_Void_P;

   --     Deallocates a memory buffer.
   procedure Cv_Free (Ptr : access Cv_Void_P);

   --     Creates an image header but does not allocate the image data.
   function Cv_Create_Image_Header (Size    : Cv_Size;
                                    Depth   : Unsigned_32;
                                    Channel : Integer)
                                    return Ipl_Image_P;

   --     Initializes an image header that was previously allocated.
   function Cv_Init_Image_Header (Image    : access Ipl_Image;
                                  Size     : Cv_Size;
                                  Depth    : Unsigned_32;
                                  Channels : Integer;
                                  Origin   : Integer := 0;
                                  Align    : Integer := 4)
                                  return Ipl_Image_P;

   --     Creates an image header and allocates the image data.
   function Cv_Create_Image (Size     : Cv_Size;
                             Depth    : Unsigned_32;
                             Channels : Integer)
                             return Ipl_Image_P;

   --     Deallocates an image header.
   procedure Cv_Release_Image_Header (Image : access Ipl_Image_P);

   --     Deallocates the image header and the image data.
   procedure Cv_Release_Image (Image : access Ipl_Image_P);

   --     Makes a full copy of an image, including the header, data, and ROI.
   function Cv_Clone_Image (Image : access Ipl_Image)
                            return Ipl_Image_P;

   --     Sets the channel of interest in an IplImage.
   procedure Cv_Set_Image_COI (Image : access Ipl_Image;
                            Coi   : Integer);

   --     Returns the index of the channel of interest.
   function Cv_Get_Image_COI (Image : access Ipl_Image)
                              return Integer;

   --     Sets an image Region Of Interest (ROI) for a given rectangle.
   procedure Cv_Set_Image_ROI (Image : access Ipl_Image;
                               Rect  : Cv_Rect);

   --     Resets the image ROI to include the entire image and releases the
   --     ROI structure.
   procedure Cv_Reset_Image_ROI (Image : access Ipl_Image);

   --     Returns the image ROI.
   function Cv_Get_Image_ROI (Image : access Ipl_Image)
                              return Cv_Rect;


   --     Creates a matrix header but does not allocate the matrix data.
   function Cv_Create_Mat_Header (Rows     : Integer;
                                  Cols     : Integer;
                                  Mat_Type : Integer)
                                  return Cv_Mat_P;

   --     Initializes a pre-allocated matrix header.
   function Cv_Init_Mat_Header (Mat   : access Cv_Mat;
                                Rows  : Integer;
                                Cols  : Integer;
                                Mat_T : Integer;
                                Data  : access Mat_Data := null; -- void*
                                Step  : Unsigned_32 := CV_AUTOSTEP)
                                return Cv_Mat_P;

   --     Creates a matrix header and allocates the matrix data.
   function Cv_Create_Mat (Rows     : Integer;
                           Cols     : Integer;
                           Mat_Type : Integer)
                           return Cv_Mat_P;

   --     Deallocates a matrix.
   procedure Cv_Release_Mat (Mat : access Cv_Mat_P);

   --     Decrements an array data reference counter.
   procedure Cv_Dec_Ref_Data (Arr : access Cv_Arr);

   --     Increments array data reference counter.
   function Cv_Inc_Ref_Data (Arr : access Cv_Arr)
                          return Integer;

   --     Creates a full matrix copy.
   function Cv_Clone_Mat (Mat : access Cv_Mat)
                        return Cv_Mat_P;

   --     Returns matrix header corresponding to the rectangular sub-array of
   --     input image or matrix.
   function Cv_Get_Sub_Rect (Arr    : access Cv_Arr;
                             Submat : access Cv_Mat;
                             Rect   : Cv_Rect)
                          return Cv_Mat_P;
   function Cv_Get_Sub_Arr (Arr    : access Cv_Arr;
                            Submat : access Cv_Mat;
                            Rect   : Cv_Rect)
                            return Cv_Mat_P renames Cv_Get_Sub_Rect;

   --     Returns row span.
   function Cv_Get_Rows (Arr       : access Cv_Arr;
                         Submat    : access Cv_Mat;
                         Start_Row : Integer;
                         End_Row   : Integer;
                         Delta_Row : Integer := 1)
                         return Cv_Mat_P;

   --     Returns array row.
   function Cv_Get_Row (Arr    : access Cv_Arr;
                        Submat : access Cv_Mat;
                        Row    : Integer)
                        return Cv_Mat_P;

   --     Returns array column span.
   function Cv_Get_Cols (Arr       : access Cv_Arr;
                         Submat    : access Cv_Mat;
                         Start_Col : Integer;
                         End_Col   : Integer)
                         return Cv_Mat_P;

   --     Returns array column.
   function Cv_Get_Col (Arr    : access Cv_Arr;
                        Submat : access Cv_Mat;
                        Col    : Integer)
                        return Cv_Mat_P;

   --     Returns one of array diagonals.
   function Cv_Get_Diag (Arr    : access Cv_Arr;
                         Submat : access Cv_Mat;
                         Diag   : Integer := 0)
                         return Cv_Mat_P;

   --/* low-level scalar <-> raw data conversion functions */
   procedure Cv_Scalar_To_Raw_Data (Scalar     : access Cv_Scalar;
                                    Data       : Cv_Void_P;
                                    Itype      : Integer;
                                    ExtendTo12 : Integer := 0);

   procedure Cv_Raw_Data_To_Scalar (Data   : Cv_Void_P;
                                    Itype  : Integer;
                                    Scalar : access Cv_Scalar);

   --     Creates a new matrix header but does not allocate the matrix data.
   function Cv_Create_Mat_ND_Header (Dims     : Integer;
                                     Sizes    : access Integer;
                                     Mat_Type : Integer)
                                     return Cv_Mat_ND_P;

   --     Creates the header and allocates the data for a multi-dimensional
   --     dense array.
   function Cv_Create_Mat_ND (Dims     : Integer;
                              Sizes    : access Integer;
                              Mat_Type : Integer)
                              return Cv_Mat_ND_P;

   --     Initializes a pre-allocated multi-dimensional array header.
   function Cv_Init_Mat_ND_Header (Mat   : access Cv_Mat_ND;
                                   Dims  : Integer;
                                   Sizes : Cv_32s_Array;
                                   Mat_T : Integer;
                                   Data  : access Mat_Data := null) -- void*
                                   return Cv_Mat_ND_P;

   --     Deallocates a multi-dimensional array.
   procedure Cv_Release_Mat_ND (Mat : access Cv_Mat_ND_P);

   --     Creates full copy of a multi-dimensional array and returns a pointer
   --     to the copy.
   function Cv_Clone_Mat_ND (Mat : access Cv_Mat_ND)
                             return Cv_Mat_ND_P;

   --     Creates sparse array.
   function Cv_Create_Sparse_Mat (Dims     : Integer;
                               Sizes    : access Integer;
                               Mat_Type : Integer)
                               return Cv_Sparse_Mat_P;

   --     Deallocates sparse array.
   procedure Cv_Release_Sparse_Mat (Mat : access Cv_Sparse_Mat_P);

   --     Creates full copy of sparse array.
   function Cv_Clone_Sparse_Mat (Mat : access Cv_Sparse_Mat)
                                 return Cv_Sparse_Mat_P;

   --     Initializes sparse array elements iterator.
   function Cv_Init_Sparse_Mat_Iterator (Mat      : access Cv_Sparse_Mat;
                                         Mat_Iter : access Cv_Sparse_Mat_Iterator)
                                         return Cv_Sparse_Node_P;

   --     Returns the next sparse matrix element
   function Cv_Get_Next_Sparse_Node (Mat_Iterator : access Cv_Sparse_Mat_Iterator)
                                     return Cv_Sparse_Node;

   -----------------------------------------------------------------------------
   -- matrix iterator: used for n-ary operations on dense arrays
   -----------------------------------------------------------------------------
   CV_MAX_ARR : constant := 10;

   type Cv_N_Array_Ptr_Array is array (Integer range 1 .. CV_MAX_ARR) of Cv_Void_P;
   type Cv_N_Array_CvMatND_P_Array is array (Integer range 1 .. CV_MAX_ARR) of Cv_Mat_ND_P;

   type Cv_N_Array_Iterator is
      record
         Count : Integer;
         Dims  : Integer;
         Size  : Cv_Size;
         Ptr   : Cv_N_Array_Ptr_Array;
         Stack : Cv_32s_Array(1 .. CV_MAX_DIM);
         Hdr   : Cv_N_Array_CvMatND_P_Array;
      end record;

   CV_NO_DEPTH_CHECK : constant := 1;
   CV_NO_CN_CHECK : constant := 2;
   CV_NO_SIZE_CHECK : constant := 4;

   --     /* initializes iterator that traverses through several arrays simulteneously
   --     (the function together with cvNextArraySlice is used for
   --      N-ari element-wise operations) */
   procedure Cv_Init_N_Array_Iterator (Count         : Integer;
                                       Arrs          : Cv_Arr_P_Array;
                                       Mask          : Cv_Arr_P;
                                       Stubs         : Cv_Mat_ND_P;
                                       ArrayIterator : CV_N_Array_Iterator;
                                       Flags         : Integer := 0);

   --  /* returns zero value if iteration is finished, non-zero (slice length) otherwise */
   function Cv_Next_N_Array_Slice (Array_Iterator : Cv_N_Array_Iterator) return Integer;

   --     Returns type of array elements.
   function Cv_Get_Elem_Type (Arr : access Cv_Arr)
                              return Integer;

   --     Return number of array dimensions and their sizes.
   function Cv_Get_Dims (Arr   : access Cv_Arr;
                         Sizes : Cv_32s_Array)
                         return Integer;

   --     Return the size of a particular dimension.
   function Cv_Get_Dim_Size (Arr   : access Cv_Arr;
                             Index : Integer)
                             return Integer;

   --     Return pointer to a particular array element.
   function Cv_Ptr_1D (Arr   : access Cv_Arr;
                       Idx0  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_P;

   --     Return pointer to a particular array element.
   function Cv_Ptr_2D (Arr   : access Cv_Arr;
                       Idx0  : Integer;
                       Idx1  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_P;

   --     Return pointer to a particular array element.
   function Cv_Ptr_3D (Arr   : access Cv_Arr;
                       Idx0  : Integer;
                       Idx1  : Integer;
                       Idx2  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_P;

   --     Return pointer to a particular array element.
   function Cv_Ptr_ND (Arr             : access Cv_Arr;
                       Idx             : Cv_32s_Array;
                       Mat_T           : access Integer := null;
                       Create_Node     : Integer := 1;
                       Precalc_Hashval : access Unsigned_32 := null)
                       return Cv_Void_P;

   --     Return a specific array element.
   function Cv_Get_1D (Arr  : access Cv_Arr;
                       Idx0 : Integer)
                       return Cv_Scalar;

   --     Return a specific array element.
   function Cv_Get_2D (Arr  : access Cv_Arr;
                       Idx0 : Integer;
                       Idx1 : Integer)
                       return Cv_Scalar;

   --     Return a specific array element.
   function Cv_Get_3D (Arr  : access Cv_Arr;
                       Idx0 : Integer;
                       Idx1 : Integer;
                       Idx2 : Integer)
                       return Cv_Scalar;

   --     Return a specific array element.
   function Cv_Get_ND (Arr : access Cv_Arr;
                       Idx : Cv_32s_Array)
                       return Cv_Scalar;

   --     Return a specific element of single-channel 1D array.
   function Cv_Get_Real_1D (Arr  : access Cv_Arr;
                            Idx0 : Integer)
                            return Long_Float;

   --     Return a specific element of single-channel 2D array.
   function Cv_Get_Real_2D (Arr  : access Cv_Arr;
                            Idx0 : Integer;
                            Idx1 : Integer)
                            return Long_Float;

   --     Return a specific element of single-channel array.
   function Cv_Get_Real_3D (Arr  : access Cv_Arr;
                            Idx0 : Integer;
                            Idx1 : Integer;
                            Idx2 : Integer)
                            return Long_Float;

   --     Return a specific element of single-channel array.
   function Cv_Get_Real_ND (Arr  : access Cv_Arr;
                            Idx  : Cv_32s_Array)
                            return Long_Float;

   --     Change the particular array element.
   procedure Cv_Set_1D (Arr   : access Cv_Arr;
                        Idx0  : Integer;
                        Value : Cv_Scalar);

   --     Change the particular array element.
   procedure Cv_Set_2D (Arr   : access Cv_Arr;
                        Idx0  : Integer;
                        Idx1  : Integer;
                        Value : Cv_Scalar);

   --     Change the particular array element.
   procedure Cv_Set_3D (Arr   : access Cv_Arr;
                        Idx0  : Integer;
                        Idx1  : Integer;
                        Idx2  : Integer;
                        Value : Cv_Scalar);

   --     Change the particular array element.
   procedure Cv_Set_ND (Arr   : access Cv_Arr;
                        Idx   : Cv_32s_Array;
                        Value : Cv_Scalar);

   --     Change a specific array element.
   procedure Cv_Set_Real_1D (Arr   : access Cv_Arr;
                             Idx0  : Integer;
                             Value : Long_Float);

   --     Change a specific array element.
   procedure Cv_Set_Real_2D (Arr   : access Cv_Arr;
                             Idx0  : Integer;
                             Idx1  : Integer;
                             Value : Long_Float);

   --     Change a specific array element.
   procedure Cv_Set_Real_3D (Arr   : access Cv_Arr;
                             Idx0  : Integer;
                             Idx1  : Integer;
                             Idx2  : Integer;
                             Value : Long_Float);

   --     Change a specific array element.
   procedure Cv_Set_Real_ND (Arr   : access Cv_Arr;
                          Idx0  : Cv_32s_Array;
                          Value : Long_Float);

   --     Clears a specific array element.
   procedure Cv_Clear_ND (Arr : access Cv_Arr;
                          Idx : access Integer);

   --     Returns matrix header for arbitrary array.
   function Cv_Get_Mat (Arr     : access Cv_Arr;
                        Header  : access Cv_Mat;
                        Coi     : access Integer := null;
                        AllowND : Integer := 0)
                        return Cv_Mat_P;

   --     Returns image header for arbitrary array.
   function Cv_Get_Image (Arr          : access Cv_Arr;
                          Image_Header : access Ipl_Image)
                          return Ipl_Image_P;

   --     Changes the shape of a multi-dimensional array without copying
   --     the data.
   function Cv_Reshape_Mat_ND (Arr           : access Cv_Arr;
                               Sizeof_Header : Integer;
                               Header        : access Cv_Arr;
                               New_Cn        : Integer;
                               New_Dims      : Integer;
                               New_Sizes     : Cv_32s_Array)
                               return access Cv_Arr;

   -- #define cvReshapeND( arr, header, new_cn, new_dims, new_sizes )   \
   --        cvReshapeMatND( (arr), sizeof(*(header)), (header),         \
   --                        (new_cn), (new_dims), (new_sizes))
   function Cv_Reshape_ND (Arr      : Cv_Arr_P;
                           Header   : Cv_Arr_P;
                           NewCn    : Integer;
                           NewDims  : Integer;
                           NewSizes : Cv_32s_Array) return Cv_Arr_P;

   --     Changes shape of matrix/image without copying data.
   function Cv_Reshape (Arr      : access Cv_Arr;
                        Header   : access Cv_Mat;
                        New_Cn   : Integer;
                        New_Rows : Integer := 0)
                        return Cv_Mat_P;

   --     Fill the destination array with repeated copies of the source array.
   procedure Cv_Repeat (Src : access Cv_Arr;
                        Dst : access Cv_Arr);

   --     Allocates array data
   procedure Cv_Create_Data (Arr : access Cv_Arr);

   --     Releases array data.
   procedure Cv_Release_Data (Arr : access Cv_Arr);

   --     Assigns user data to the array header.
   procedure Cv_Set_Data (Arr  : Cv_Arr_P;
                          Data : System.Address;
                          Step : Integer);

   --     Retrieves low-level information about the array.
   procedure Cv_Get_Raw_Data (Arr      : access Cv_Arr;
                              Data     : access Cv_Arr_P; -- uchar**
                              Step     : access Integer := null;
                              Roi_Size : access Cv_Size := null);

   --     Returns size of matrix or image ROI.
   function Cv_Get_Size (Arr : access Cv_Arr)
                         return Cv_Size;

   --     Copies one array to another.
   procedure Cv_Copy (Src  : access Cv_Arr;
                      Dst  : access Cv_Arr;
                      Mask : access Cv_Arr := null);

   --     Sets every element of an array to a given value.
   procedure Cv_Set_All (Arr   : access Cv_Arr;
                         Value : Cv_Scalar;
                         Mask  : access Cv_Arr := null);

   --     Clears the array.
   procedure Cv_Set_Zero (Arr : access Cv_Arr);
   procedure Cv_Zero (Arr : access Cv_Arr) renames Cv_Set_Zero;


   --     Divides multi-channel array into several single-channel arrays or
   --     extracts a single channel from the array.
   procedure Cv_Split (Src  : access Cv_Arr;
                       Dst0 : access Cv_Arr;
                       Dst1 : access Cv_Arr;
                       Dst2 : access Cv_Arr;
                       Dst3 : access Cv_Arr);

   --     Composes a multi-channel array from several single-channel arrays or
   --     inserts a single channel into the array.
   procedure Cv_Merge (Src0 : access Cv_Arr;
                       Src1 : access Cv_Arr;
                       Src2 : access Cv_Arr;
                       Src3 : access Cv_Arr;
                       Dst  : access Cv_Arr);

   --     Copies several channels from input arrays to certain channels of
   --     output arrays
   procedure Cv_Mix_Channels (Src        : access Cv_Arr_P;
                              Src_Count  : Integer;
                              Dst        : access Cv_Arr_P;
                              Dst_Count  : Integer;
                              From_To    : Cv_32s_Array;
                              Pair_Count : Integer);

   --     Converts one array to another with optional linear transformation.
   procedure Cv_Convert_Scale (Src   : access Cv_Arr;
                               Dst   : access Cv_Arr;
                               Scale : Long_Float := 1.0;
                               Shift : Long_Float := 0.0);

   procedure Cv_Cvt_Scale (Src   : access Cv_Arr;
                           Dst   : access Cv_Arr;
                           Scale : Long_Float := 1.0;
                           Shift : Long_Float := 0.0) renames Cv_Convert_Scale;

   procedure Cv_Scale (Src   : access Cv_Arr;
                       Dst   : access Cv_Arr;
                       Scale : Long_Float := 1.0;
                       Shift : Long_Float := 0.0) renames Cv_Convert_Scale;

   procedure Cv_Convert (Src : Cv_Arr_P;
                         Dst : Cv_Arr_P);

   --     Converts input array elements to another 8-bit unsigned integer with
   --     optional linear transformation.
   procedure Cv_Convert_Scale_Abs (Src   : access Cv_Arr;
                                   Dst   : access Cv_Arr;
                                   Scale : Long_Float := 1.0;
                                   Shift : Long_Float := 0.0);

   --     Converts input array elements to another 8-bit unsigned integer with
   --     optional linear transformation.
   procedure Cv_Cvt_Scale_Abs (Src   : access Cv_Arr;
                               Dst   : access Cv_Arr;
                               Scale : Long_Float := 1.0;
                               Shift : Long_Float := 0.0) renames Cv_Convert_Scale_Abs;

   function Cv_Check_Term_Criteria ( Criteria         : Cv_Term_Criteria;
                                    Default_Eps       : Long_Float;
                                    Default_Max_Iters : Integer ) return Cv_Term_Criteria;

   -----------------------------------------------------------------------------
   -- Arithmetic, logic and comparison operations
   -----------------------------------------------------------------------------
   --     Computes the per-element sum of two arrays.
   procedure Cv_Add (Src1 : access Cv_Arr;
                     Src2 : access Cv_Arr;
                     Dst  : access Cv_Arr;
                     Mask : access Cv_Arr := null);

   --     Computes the sum of an array and a scalar.
   procedure Cv_Add_S (Src   : access Cv_Arr;
                      Value : Cv_Scalar;
                      Dst   : access Cv_Arr;
                      Mask  : access Cv_Arr := null);

   --     Computes the per-element difference between two arrays.
   procedure Cv_Sub (Src1 : access Cv_Arr;
                     Src2 : access Cv_Arr;
                     Dst  : access Cv_Arr;
                     Mask : access Cv_Arr := null);

   --     Computes the difference between an array and a scalar.
   procedure Cv_Sub_S (Src   : access Cv_Arr;
                       Value : Cv_Scalar;
                       Dst   : access Cv_Arr;
                       Mask  : access Cv_Arr := null);

   --     Computes the difference between a scalar and an array.
   procedure Cv_Sub_RS (Src   : access Cv_Arr;
                        Value : Cv_Scalar;
                        Dst   : access Cv_Arr;
                        Mask  : access Cv_Arr := null);

   --     Calculates the per-element product of two arrays.
   procedure Cv_Mul (Src1  : access Cv_Arr;
                     Src2  : access Cv_Arr;
                     Dst   : access Cv_Arr;
                     Scale : Long_Float := 1.0);

   --     Performs per-element division of two arrays.
   procedure Cv_Div (Src1  : access Cv_Arr;
                     Src2  : access Cv_Arr;
                     Dst   : access Cv_Arr;
                     Scale : Long_Float := 1.0);

   --     Calculates the sum of a scaled array and another array.
   procedure Cv_Scale_Add (Src1  : access Cv_Arr;
                           Scale : Cv_Scalar;
                           Src2  : access Cv_Arr;
                           Dst   : access Cv_Arr);
   procedure Cv_AXPY (Src1  : Cv_Arr_P;
                      Scale : Long_Float;
                      Src2  : Cv_Arr_P;
                      Dst   : Cv_Arr_P);

   --     Computes the weighted sum of two arrays.
   procedure Cv_Add_Weighted (Src1  : access Cv_Arr;
                              Alpha : Long_Float;
                              Src2  : access Cv_Arr;
                              Beta  : Long_Float;
                              Gamma : Long_Float;
                              Dst   : access Cv_Arr);

   --     Calculates the dot product of two arrays in Euclidian metrics.
   function Cv_Dot_Product (Src1 : access Cv_Arr;
                            Src2 : access Cv_Arr)
                            return Long_Float;

   --     Calculates per-element bit-wise conjunction of two arrays.
   procedure Cv_And (Src1 : access Cv_Arr;
                     Src2 : access Cv_Arr;
                     Dst  : access Cv_Arr;
                     Mask : access Cv_Arr := null);

   --     Calculates per-element bit-wise conjunction of an array and a scalar.
   procedure Cv_And_S (Src   : access Cv_Arr;
                       Value : Cv_Scalar;
                       Dst   : access Cv_Arr;
                       Mask  : access Cv_Arr := null);

   --     Calculates per-element bit-wise disjunction of two arrays.
   procedure Cv_Or (Src1 : access Cv_Arr;
                    Src2 : access Cv_Arr;
                    Dst  : access Cv_Arr;
                    Mask : access Cv_Arr := null);

   --     Calculates a per-element bit-wise disjunction of an array and
   --     a scalar.
   procedure Cv_Or_S (Src   : access Cv_Arr;
                      Value : Cv_Scalar;
                      Dst   : access Cv_Arr;
                      Mask  : access Cv_Arr := null);

   --     Performs per-element bit-wise exclusive or operation on two arrays.
   procedure Cv_Xor (Src1 : access Cv_Arr;
                     Src2 : access Cv_Arr;
                     Dst  : access Cv_Arr;
                     Mask : access Cv_Arr := null);

   --     Performs per-element bit-wise exclusive or operation on an array
   --     and a scalar.
   procedure Cv_Xor_S (Src   : access Cv_Arr;
                       Value : Cv_Scalar;
                       Dst   : access Cv_Arr;
                       Mask  : access Cv_Arr := null);

   --     Performs per-element bit-wise inversion of array elements.
   procedure Cv_Not (Src : access Cv_Arr;
                     Dst : access Cv_Arr);

   --     Checks that array elements lie between the elements of two other
   --     arrays.
   procedure Cv_In_Range (Src   : access Cv_Arr;
                          Lower : access Cv_Arr;
                          Upper : access Cv_Arr;
                          Dst   : access Cv_Arr);

   --     Checks that array elements lie between two scalars.
   procedure Cv_In_Range_S (Src   : access Cv_Arr;
                            Lower : Cv_Scalar;
                            Upper : Cv_Scalar;
                            Dst   : access Cv_Arr);

   type Compare_Op is new Integer;-- (Cv_Cmp_Eq, Cv_Cmp_Gt,
--                         Cv_Cmp_Ge, Cv_Cmp_Lt,
--                         Cv_Cmp_Le, Cv_Cmp_Ne);
   Cv_Cmp_Eq : constant Compare_Op := 0;
   Cv_Cmp_Gt : constant Compare_Op := 1;
   Cv_Cmp_Ge : constant Compare_Op := 2;
   Cv_Cmp_Lt : constant Compare_Op := 3;
   Cv_Cmp_Le : constant Compare_Op := 4;
   Cv_Cmp_Ne : constant Compare_Op := 5;

   --     Performs per-element comparison of two arrays.
   procedure Cv_Cmp (Src1   : access Cv_Arr;
                     Src2   : access Cv_Arr;
                     Dst    : access Cv_Arr;
                     Cmp_Op : Compare_Op);

   --     Performs per-element comparison of an array and a scalar.
   procedure Cv_Cmp_S (Src    : access Cv_Arr;
                       Value  : Long_Float;
                       Dst    : access Cv_Arr;
                       Cmp_Op : Compare_Op);

   --     Finds per-element minimum of two arrays.
   procedure Cv_Min (Src1 : access Cv_Arr;
                     Src2 : access Cv_Arr;
                     Dst  : access Cv_Arr);

   --     Finds per-element maximum of two arrays.
   procedure Cv_Max (Src1 : access Cv_Arr;
                     Src2 : access Cv_Arr;
                     Dst  : access Cv_Arr);

   --     Finds per-element minimum of an array and a scalar.
   procedure Cv_Min_S (Src   : access Cv_Arr;
                       Value : Long_Float;
                       Dst   : access Cv_Arr);

   --     Finds per-element maximum of array and scalar.
   procedure Cv_Max_S (Src   : access Cv_Arr;
                       Value : Long_Float;
                       Dst   : access Cv_Arr);

   --     Calculates absolute difference between two arrays.
   procedure Cv_Abs_Diff (Src1 : access Cv_Arr;
                          Src2 : access Cv_Arr;
                          Dst  : access Cv_Arr);

   --     Calculates absolute difference between an array and a scalar.
   procedure Cv_Abs_Diff_S (Src   : access Cv_Arr;
                            Dst   : access Cv_Arr;
                            S1    : Long_Float;
                            S2    : Long_Float;
                            S3    : Long_Float;
                            S4    : Long_Float);

   -----------------------------------------------------------------------------
   -- Math Operations
   -----------------------------------------------------------------------------
   --     Calculates the magnitude and/or angle of 2d vectors.
   procedure Cv_Cart_To_Polar (X              : access Cv_Arr;
                               Y              : access Cv_Arr;
                               Magnitude      : access Cv_Arr;
                               Angle          : access Cv_Arr := null;
                               AngleInDegrees : Integer := 0);

   --     Calculates Cartesian coordinates of 2d vectors represented in
   --     polar form.
   procedure Cv_Polar_To_Cart (Magnitude        : access Cv_Arr;
                               Angle            : access Cv_Arr;
                               X                : access Cv_Arr;
                               Y                : access Cv_Arr;
                               Angle_In_Degrees : Integer := 0);

   --     Raises every array element to a power.
   procedure Cv_Pow (Src   : access Cv_Arr;
                     Dst   : access Cv_Arr;
                     Power : Long_Float);

   --     Calculates the exponent of every array element.
   procedure Cv_Exp (Src : access Cv_Arr;
                     Dst : access Cv_Arr);

   --     Calculates the natural logarithm of every array elements absolute value.
   procedure Cv_Log (Src : access Cv_Arr;
                     Dst : access Cv_Arr);

   --     Calculates the angle of a 2D vector.
   function Cv_Fast_Arctan (Y : Float;
                            X : Float)
                            return Float;

   --     Calculates the cubic root
   function Cv_Cbrt (Value : Float)
                     return Float;

   CV_CHECK_RANGE : constant := 1;
   CV_CHECK_QUIET : constant := 2;

   function Cv_Check_Arr (Arr     : Cv_Arr_P;
                          Flags   : Integer := 0;
                          MinVal  : Long_Float := 0.0;
                          Max_Val : Long_Float := 0.0) return Integer;

   function Cv_Check_Array (Arr     : Cv_Arr_P;
                            Flags   : Integer := 0;
                            MinVal  : Long_Float := 0.0;
                            Max_Val : Long_Float := 0.0) return Integer renames Cv_Check_Arr;

   CV_RAND_UNI    : constant := 0;
   CV_RAND_NORMAL : constant := 1;

   --     Fills an array with random numbers and updates the RNG state.
   procedure Cv_Rand_Arr (Rng       : Integer_64;
                          Arr       : access Cv_Arr;
                          Dist_Type : Integer;
                          Param1    : Cv_Scalar;
                          Param2    : Cv_Scalar);

   procedure Cv_Rand_Shuffle (Mat        : Cv_Arr_P;
                              Rng        : access Cv_RNG;
                              IterFactor : Long_Float := 1.0);

   CV_SORT_EVERY_ROW : constant := 0;
   CV_SORT_EVERY_COLUMN : constant := 1;
   CV_SORT_ASCENDING : constant := 0;
   CV_SORT_DESCENDING : constant := 16;

   procedure Cv_Sort (Src    : Cv_Arr_P;
                      Dst    : Cv_Arr_P := null;
                      Idxmat : Cv_Arr_P := null;
                      Flags  : Integer := 0);

   --     Finds the real roots of a cubic equation.
   procedure Cv_Solve_Cubic (Coeffs : access Cv_Arr;
                             Roots  : access Cv_Arr);


   -- /* Finds all real and complex roots of a polynomial equation */
   procedure Cv_Solve_Poly (Coeffs  : Cv_Mat_P;
                            Roots2  : Cv_Mat_P;
                            Maxiter : Integer := 20;
                            Fig     : Integer := 100);

   -----------------------------------------------------------------------------
   -- Matrix Operations
   -----------------------------------------------------------------------------
   --     Calculates the cross product of two 3D vectors.
   procedure Cv_Cross_Product (Src1 : access Cv_Arr;
                               Src2 : access Cv_Arr;
                               Dst  : access Cv_Arr);

   procedure Cv_Mat_Mul_Add (Src1  : access Cv_Arr;
                             Src2  : access Cv_Arr;
                             Src3  : access Cv_Arr;
                             Dst   : access Cv_Arr);
   pragma Inline (Cv_Mat_Mul_Add);

   procedure Cv_Mat_Mul (Src1 : access Cv_Arr;
                         Src2 : access Cv_Arr;
                         Dst  : access Cv_Arr);
   pragma Inline (Cv_Mat_Mul);

   CV_GEMM_A_T  : constant Integer := 1;
   CV_GEMM_B_T  : constant Integer := 2;
   CV_GEMM_C_T  : constant Integer := 4;

   --     Performs generalized matrix multiplication.
   procedure Cv_GEMM (Src1  : access Cv_Arr;
                      Src2  : access Cv_Arr;
                      Alpha : Long_Float;
                      Src3  : access Cv_Arr;
                      Beta  : Long_Float;
                      Dst   : access Cv_Arr;
                      TABC  : Integer := 0);

   procedure Cv_Mat_Mul_Add_Ex (Src1  : access Cv_Arr;
                                Src2  : access Cv_Arr;
                                Src3  : access Cv_Arr;
                                Dst   : access Cv_Arr)
                                renames Cv_Mat_Mul_Add;

   --     Performs matrix transformation of every array element.
   procedure Cv_Transform (Src      : access Cv_Arr;
                           Dst      : access Cv_Arr;
                           Transmat : access Cv_Mat;
                           Shiftvec : access Cv_Mat := null);

   procedure Cv_Mat_Mul_Add_S (Src      : access Cv_Arr;
                               Dst      : access Cv_Arr;
                               Transmat : access Cv_Mat;
                               Shiftvec : access Cv_Mat := null) renames Cv_Transform;

   --     Performs perspective matrix transformation of a vector array.
   procedure Cv_Perspective_Transform (Src : access Cv_Arr;
                                       Dst : access Cv_Arr;
                                       Mat : access Cv_Arr);

   --     Calculates the product of an array and a transposed array.
   procedure Cv_Mul_Transposed (Src       : access Cv_Arr;
                                Dst       : access Cv_Arr;
                                Order     : Integer;
                                Delta_Arr : access Cv_Arr := null;
                                Scale     : Long_Float := 1.0);

   --     Transposes a matrix.
   procedure Cv_Transpose (Src : access Cv_Arr;
                           Dst : access Cv_Arr);

   procedure Cv_T (Src : access Cv_Arr;
                   Dst : access Cv_Arr) renames Cv_Transpose;

   --/* Completes the symmetric matrix from the lower (LtoR=0) or from the upper (LtoR!=0) part */
   procedure Cv_Complete_Symm (Matrix : Cv_Mat_P;
                               LtoR   : Integer := 0);

   --     Flip a 2D array around vertical, horizontal or both axes.
   procedure Cv_Flip (Src      : access Cv_Arr;
                      Dst      : access Cv_Arr := null;
                      FlipMode : Integer := 0);

   --     Synonym for Flip.
   procedure Cv_Mirror (Src      : access Cv_Arr;
                        Dst      : access Cv_Arr := null;
                        Flipmode : Integer := 0)
                        renames Cv_Flip;

   CV_SVD_MODIFY_A : constant Unsigned_32 := 1;
   CV_SVD_U_T   : constant Unsigned_32 := 2;
   CV_SVD_V_T   : constant Unsigned_32 := 4;

   --     Performs singular value decomposition of a real floating-point matrix.
   procedure Cv_SVDecomp (A     : access Cv_Arr;
                     W     : access Cv_Arr;
                     U     : access Cv_Arr := null;
                     V     : access Cv_Arr := null;
                     Flags : Unsigned_32 := 0);

   --     Performs singular value back substitution.
   procedure Cv_SVBkSb (W     : access Cv_Arr;
                        U     : access Cv_Arr;
                        V     : access Cv_Arr;
                        B     : access Cv_Arr;
                        X     : access Cv_Arr;
                        Flags : Unsigned_32);

   CV_LU       : constant := 0;
   CV_SVD      : constant := 1;
   CV_SVD_SYM  : constant := 2;
   CV_CHOLESKY : constant := 3;
   CV_QR       : constant := 4;
   CV_NORMAL   : constant := 16;

   --     Finds the inverse or pseudo-inverse of a matrix.
   function Cv_Invert (Src    : access Cv_Arr;
                       Dst    : access Cv_Arr;
                       Method : Integer := CV_LU)
                       return Long_Float;
   function Cv_Inv (Src    : access Cv_Arr;
                    Dst    : access Cv_Arr;
                    Method : Integer := CV_LU)
                    return Long_Float renames Cv_Invert;

   --     Solves a linear system or least-squares problem.
   function Cv_Solve (Src1   : access Cv_Arr;
                      Src2   : access Cv_Arr;
                      Dst    : access Cv_Arr;
                      Method : Integer := CV_LU)
                      return Integer;

   --     Returns the determinant of a matrix.
   function Cv_Det (Mat : access Cv_Arr)
                    return Long_Float;

   --     Returns the trace of a matrix.
   function Cv_Trace (Mat : access Cv_Arr) return Cv_Scalar;

   --     Computes eigenvalues and eigenvectors of a symmetric matrix.
   procedure Cv_Eigen_VV (Mat       : access Cv_Arr;
                          Evects    : access Cv_Arr;
                          Evals     : access Cv_Arr;
                          Eps       : Long_Float := 0.0;
                          Lowindex  : Integer := -1;
                          Highindex : Integer := -1);

   --     Initializes a scaled identity matrix.
   procedure Cv_Set_Identity (Mat   : access Cv_Arr;
                              Value : Cv_Scalar);

   -- /* Fills matrix with given range of numbers */
   function Cv_Range (Mat   : Cv_Arr_P;
                      Start : Long_Float;
                      Ende  : Long_Float) return Cv_Arr_P;

   CV_COVAR_SCRAMBLED : constant Unsigned_32 := 0;
   CV_COVAR_NORMAL    : constant Unsigned_32 := 1;
   CV_COVAR_USE_AVG   : constant Unsigned_32 := 2;
   CV_COVAR_SCALE     : constant Unsigned_32 := 4;
   CV_COVAR_ROWS      : constant Unsigned_32 := 8;
   CV_COVAR_COLS      : constant Unsigned_32 := 16;

   --     Calculates covariance matrix of a set of vectors.
   procedure Cv_Calc_Covar_Matrix (Vects  : access Cv_Arr;
                                   Count  : Integer;
                                   CovMat : access Cv_Arr;
                                   Avg    : access Cv_Arr;
                                   Flags  : Unsigned_32);

   CV_PCA_DATA_AS_ROW : constant := 0;
   CV_PCA_DATA_AS_COL : constant := 1;
   CV_PCA_USE_AVG : constant := 2;

   procedure Cv_Calc_PCA (Data       : Cv_Arr_P;
                          Mean       : Cv_Arr_P;
                          Eigenvals  : Cv_Arr_P;
                          Eigenvects : Cv_Arr_P;
                          Flags      : Integer);

   procedure Cv_Project_PCA (Data       : Cv_Arr_P;
                             Mean       : Cv_Arr_P;
                             Eigenvects : Cv_Arr_P;
                             Result     : Cv_Arr_P);

   procedure Cv_Back_Project_PCA (Proj       : Cv_Arr_P;
                                  Mean       : Cv_Arr_P;
                                  Eigenvects : Cv_Arr_P;
                                  Result     : Cv_Arr_P);

   --     Calculates the Mahalonobis distance between two vectors.
   function Cv_Mahalanobis (Vec1 : access Cv_Arr;
                            Vec2 : access Cv_Arr;
                            Mat  : access Cv_Arr)
                            return Long_Float;
   function Cv_Mahalonobis (Vec1 : access Cv_Arr;
                            Vec2 : access Cv_Arr;
                            Mat  : access Cv_Arr)
                            return Long_Float renames Cv_Mahalanobis;

   -----------------------------------------------------------------------------
   -- Array Statistics
   -----------------------------------------------------------------------------
   --     Adds up array elements.
   function Cv_Sum (Arr : access Cv_Arr) return Cv_Scalar;

   --     Counts non-zero array elements.
   function Cv_Count_Non_Zero (Arr : access Cv_Arr)
                               return Integer;

   --     Calculates average (mean) of array elements.
   function Cv_Avg (Arr  : access Cv_Arr;
                    Mask : access Cv_Arr := null)
                    return Cv_Scalar;

   --     Calculates average (mean) of array elements.
   procedure Cv_Avg_Sdv (Arr    : access Cv_Arr;
                         Mean   : access Cv_Scalar;
                         StdDev : access Cv_Scalar;
                         Mask   : access Cv_Arr := null);

   --     Finds global minimum and maximum in array or subarray.
   procedure Cv_Min_Max_Loc (Arr     : access Cv_Arr;
                             Min_Val : access Long_Float;
                             Max_Val : access Long_Float;
                             Min_Loc : access Cv_Point := null;
                             Max_Loc : access Cv_Point := null;
                             Mask    : access Cv_Arr := null);

   CV_C         : constant Unsigned_32 := 1;
   CV_L1        : constant Unsigned_32 := 2;
   CV_L2        : constant Unsigned_32 := 4;
   CV_NORM_MASK : constant Unsigned_32 := 7;
   CV_RELATIVE  : constant Unsigned_32 := 8;
   CV_DIFF      : constant Unsigned_32 := 16;
   CV_MINMAX    : constant Unsigned_32 := 32;

   CV_DIFF_C : constant := (CV_DIFF or CV_C);
   CV_DIFF_L1 : constant := (CV_DIFF or CV_L1);
   CV_DIFF_L2 : constant := (CV_DIFF or CV_L2) ;
   CV_RELATIVE_C : constant := (CV_RELATIVE or CV_C);
   CV_RELATIVE_L1 : constant := (CV_RELATIVE or CV_L1);
   CV_RELATIVE_L2 : constant := (CV_RELATIVE or CV_L2);

   --     Calculates absolute array norm, absolute difference norm, or relative difference norm.
   function Cv_Norm (Arr1      : access Cv_Arr;
                     Arr2      : access Cv_Arr := null;
                     Norm_Type : Unsigned_32 := CV_L2;
                     Mask      : access Cv_Arr := null)
                     return Long_Float;

   procedure Cv_Normalize (Src      : Cv_Arr_P;
                           Dst      : Cv_Arr_P;
                           A        : Long_Float := 1.0;
                           B        : Long_Float := 0.0;
                           NormType : Integer := Integer (CV_L2);
                           Mask     : Cv_Arr_P := null);

   CV_REDUCE_SUM : constant := 0;
   CV_REDUCE_AVG : constant := 1;
   CV_REDUCE_MAX : constant := 2;
   CV_REDUCE_MIN : constant := 3;

   --     Reduces a matrix to a vector.
   procedure Cv_Reduce (Src : access Cv_Arr;
                        Dst : access Cv_Arr;
                        Dim : Integer := -1;
                        Op  : Integer := CV_REDUCE_SUM);

   -----------------------------------------------------------------------------
   -- Discrete Linear Transforms and Related Functions
   -----------------------------------------------------------------------------
   CV_DXT_FORWARD       : constant Unsigned_32 := 0;
   CV_DXT_INVERSE       : constant Unsigned_32 := 1;
   CV_DXT_SCALE         : constant Unsigned_32 := 2; -- Divide result by size of array
   CV_DXT_INV_SCALE     : constant Unsigned_32 := CV_DXT_INVERSE + CV_DXT_SCALE;
   CV_DXT_INVERSE_SCALE : constant Unsigned_32 := CV_DXT_INV_SCALE;
   CV_DXT_ROWS          : constant Unsigned_32 := 4; -- Transform each row individually
   CV_DXT_MUL_CONJ      : constant Unsigned_32 := 8; -- Conjugate the second argument of cvMulSpectrums

   --     Performs a forward or inverse Discrete Fourier transform of a 1D or 2D
   --     floating - point array.
   procedure Cv_DFT (Src         : access Cv_Arr;
                     Dst         : access Cv_Arr;
                     Flags       : Unsigned_32;
                     NonzeroRows : Integer := 0);
   procedure Cv_FFT (Src         : access Cv_Arr;
                     Dst         : access Cv_Arr;
                     Flags       : Unsigned_32;
                     NonzeroRows : Integer := 0) renames Cv_DFT;

   --     Performs per-element multiplication of two Fourier spectrums.
   procedure Cv_Mul_Spectrums (Src1  : access Cv_Arr;
                               Src2  : access Cv_Arr;
                               Dst   : access Cv_Arr;
                               Flags : Unsigned_32);

   --     Returns optimal DFT size for a given vector size.
   function Cv_Get_Optimal_DFT_Size (Size0 : Integer)
                                     return Integer;

   --     Performs a forward or inverse Discrete Cosine transform of a 1D or 2D
   --     floating - point array.
   procedure Cv_DCT (Src   : access Cv_Arr;
                     Dst   : access Cv_Arr;
                     Flags : Unsigned_32);

   -----------------------------------------------------------------------------
   -- Dynamic data structures
   -----------------------------------------------------------------------------
   --/  * Calculates Length of Sequence Slice (with Support of Negative Indices). *  /
   function Cv_Slice_Length (Slice : Cv_Slice;
                             Seq   : Cv_Seq_P) return Integer;

   --Creates memory storage.
   function Cv_Create_Mem_Storage (Blocksize : Integer := 0) return Cv_Mem_Storage_P; -- 0 is about 64k...

   -- Creates child memory storage.
   function Cv_Create_Child_Mem_Storage (Parent : Cv_Mem_Storage_P) return Cv_Mem_Storage_P;

   -- Releases memory storage.
   procedure Cv_Release_Mem_Storage (Storage : access Cv_Mem_Storage_P);

   -- Clears memory storage.
   procedure Cv_Clear_Mem_Storage (Storage : Cv_Mem_Storage_P);

   -- Saves memory storage position.
   procedure Cv_Save_Mem_Storage_Pos (Storage : Cv_Mem_Storage_P;
                                      Pos     : Cv_Mem_Storage_Pos_P);

   -- Restores memory storage position.
   procedure Cv_Restore_Mem_Storage_Pos (Storage : Cv_Mem_Storage_P;
                                         Pos     : Cv_Mem_Storage_Pos_P);

   -- Allocates a memory buffer in a storage block.
   function Cv_Mem_Storage_Alloc (Storage : Cv_Mem_Storage_P;
                                  Size    : Interfaces.C.Size_T) return Cv_Void_P;

   -- Allocates a memory buffer in a storage block.
   function Cv_Mem_Storage_Alloc_String (Storage : Cv_Mem_Storage_P;
                                         Ptr     : String;
                                         Len     : Integer := -1) return Cv_String;

   --Creates a sequence.
   function Cv_Create_Seq (SeqFlags   : Integer; -- No clue what this could be...
                           HeaderSize : Integer;
                           ElemSize   : Integer;
                           Storage    : Cv_Mem_Storage_P) return Cv_Seq_P;

   -- Sets up sequence block size.
   procedure Cv_Set_Seq_Block_Size (Seq        : Cv_Seq_P;
                                    DeltaElems : Integer);

   -- Adds an element to the end of a sequence.
   function Cv_Seq_Push (Seq     : Cv_Seq_P;
                         Element : Cv_Void_P := null) return access Character;

   -- Adds an element to the beginning of a sequence.
   function Cv_Seq_Push_Front (Seq     : Cv_Seq_P;
                               Element : Cv_Void_P := null) return access Character;

   -- Removes an element from the end of a sequence.
   procedure Cv_Seq_Pop (Seq     : Cv_Seq_P;
                         Element : Cv_Void_P := null);

   -- Removes an element from the beginning of a sequence.
   procedure Cv_Seq_Pop_Front (Seq     : Cv_Seq_P;
                               Element : Cv_Void_P := null);

   -- Back and Front....
   CV_FRONT : constant := 1;
   CV_BACK : constant := 0;

   -- Pushes several elements to either end of a sequence.
   procedure Cv_Seq_Push_Multi (Seq      : Cv_Seq_P;
                                Elements : Cv_Void_P; -- maybe not
                                Count    : Integer;
                                InFron   : Integer := 0);

   -- Removes several elements from either end of a sequence.
   procedure Cv_Seq_Pop_Multi (Seq      : Cv_Seq_P;
                               Elements : Cv_Void_P;
                               Count    : Integer;
                               InFront  : Integer := 0);

   -- Inserts an element in the middle of a sequence.
   function Cv_Seq_Insert (Seq         : Cv_Seq_P;
                           BeforeIndex : Integer;
                           Element     : Cv_Void_P := null) return Cv_Void_P; --return access Character;

   --Removes an element from the middle of a sequence.
   procedure Cv_Seq_Remove (Seq   : Cv_Seq_P;
                            Index : Integer);

   --Clears a sequence.
   procedure Cv_Clear_Seq (Seq : Cv_Seq_P);

   -- Returns a pointer to a sequence element according to its index.
   function Cv_Get_Seq_Elem (Seq   : Cv_Seq_P;
                             Index : Integer) return Cv_Void_P; --return access Character;

   -- Returns the index of a specific sequence element.
   function Cv_Seq_Elem_Idx (Seq     : Cv_Seq_P;
                             Element : Cv_Void_P;
                             Block   : access Cv_Seq_Block_P := null) return Integer;

   -- Initializes the process of writing data to a sequence.
   procedure Cv_Start_Append_To_Seq (Seq    : Cv_Seq_P;
                                     Writer : Cv_Seq_Writer_P);

   -- Creates a new sequence and initializes a writer for it.
   procedure Cv_Start_Write_Seq (SeqFlags   : Integer;
                                 HeaderSize : Integer;
                                 ElemSize   : Integer;
                                 Storage    : Cv_Mem_Storage_P;
                                 Writer     : Cv_Seq_Writer_P);

   -- Finishes the process of writing a sequence.
   function Cv_End_Write_Seq ( Writer : Cv_Seq_Writer_P) return Cv_Seq_P;

   -- Updates sequence headers from the writer.
   procedure Cv_Flush_Seq_Writer (Writer : Cv_Seq_Writer_P);

   -- Initializes the process of sequential reading from a sequence
   procedure Cv_Start_Read_Seq (Seq       : Cv_Seq_P;
                                Reader    : Cv_Seq_Reader_P;
                                IsReverse : Integer := 0);

   -- Returns the current reader position.
   function Cv_Get_Seq_Reader_Pos (Reader : Cv_Seq_Reader_P) return Integer;

   --Moves the reader to the specified position.
   procedure Cv_Set_Seq_Reader_Pos (Reader     : Cv_Seq_Reader_P;
                                    Index      : Integer;
                                    IsRelative : Integer := 0);

   -- Copies a sequence to one continuous block of memory.
   function Cv_Cvt_Seq_To_Array (Seq      : Cv_Seq_P;
                                 Elements : Cv_Void_P;
                                 Slice    : Cv_Slice := Cv_Create_Slice (0)) return Cv_Void_P;

   --Constructs a sequence header for an array.
   function Cv_Make_Seq_Header_For_Array (SeqType    : Integer;
                                          HeaderSize : Integer;
                                          ElemSize   : Integer;
                                          Elements   : Cv_Void_P;
                                          Seq        : Cv_Seq_P;
                                          Block      : Cv_Seq_Block_P) return Cv_Seq_P;

   -- Makes a separate header for a sequence slice.
   function Cv_Seq_Slice (Seq      : Cv_Seq_P;
                          Slice    : Cv_Slice;
                          Storage  : Cv_Mem_Storage_P := null;
                          CopyData : Integer := 0) return Cv_Seq_P;

   -- Creates a copy of a sequence.
   procedure Cv_Clone_Seq (Seq     : Cv_Seq_P;
                           Storage : Cv_Mem_Storage_P := null);

   -- Removes a sequence slice.
   procedure Cv_Seq_Remove_Slice (Seq   : Cv_Seq_P;
                                  Slice : Cv_Slice_P);

   -- Inserts an array in the middle of a sequence.
   procedure Cv_Seq_Insert_Slice (Seq         : Cv_Seq_P;
                                  BeforeIndex : Integer;
                                  FromArr     : Cv_Arr_P);

   type Cv_Cmp_Func is access function (A        : Cv_Void_P;
                                        B        : Cv_Void_P;
                                        Userdata : Cv_Void_P)
                                        return Integer;

   -- Sorts sequence element using the specified comparison function.
   procedure Cv_Seq_Sort (Seq      : Cv_Seq_P;
                          Func     : Cv_Cmp_Func;
                          Userdata : Cv_Void_P := null);
   pragma Convention (C, Cv_Cmp_Func);

   -- Searches for an element in a sequence.
   function Cv_Seq_Search (Seq         : Cv_Seq_P;
                           Elem        : Cv_Void_P;
                           Func        : Cv_Cmp_Func;
                           IsSorted    : Integer;
                           ElemIdx     : access Integer;
                           Userdata    : Cv_Void_P := null) return Cv_Void_P; --return access Character;

   -- Reverses the order of sequence elements.
   procedure Cv_Seq_Invert (Seq : Cv_Seq_P);

   --     Splits a sequence into equivalency classes.
   function Cv_Seq_Partition (Seq      : access Cv_Seq;
                              Storage  : access Cv_Mem_Storage;
                              Labels   : access Cv_Seq_P;
                              Is_Equal : Cv_Cmp_Func;
                              Userdata : Cv_Void_P)
                              return Integer;

   procedure Cv_Change_Seq_Block (Reader    : Cv_Void_P;
                                  Direction : Integer) renames Core.Cv_Change_Seq_Block;
   procedure Cv_Create_Seq_Block (Writer : Cv_Seq_Writer_P) renames Core.Cv_Create_Seq_Block;

   -- Creates an empty set.
   function Cv_Create_Set ( SetFlags  : Integer;
                           HeaderSize : Integer;
                           ElemSize   : Integer;
                           Storage    : Cv_Mem_Storage_P) return Cv_Set_P;

   -- Occupies a node in the set.
   function Cv_Set_Add (SetHeader    : Cv_Set_P;
                        Elem         : Cv_Set_Elem_P := null;
                        InsertedElem : access Cv_Set_Elem_P := null) return Integer;

   -- Adds an element to a set (fast variant).
   function Cv_Set_New (SetHeader : Cv_Set_P) return Cv_Set_Elem_P;

   -- Removes a set element based on its pointer.
   procedure Cv_Set_Remove_By_Ptr (SetHeader : Cv_Set_P;
                                   Elem      : Cv_Void_P);

   -- Removes an element from a set.
   procedure Cv_Set_Remove (SetHeader : Cv_Set_P;
                            Index     : Integer);

   -- Finds a set element by its index.
   function Cv_Get_Set_Elem (SetHeader : Cv_Set_P;
                             Index     : Integer) return Cv_Set_Elem_P;

   -- Clears a set.
   procedure Cv_Clear_Set (SetHeader : Cv_Set_P);

   -- Creates an empty graph.
   function Cv_Create_Graph (GraphFlags : Integer;
                             HeaderSize : Integer;
                             VtxSize    : Integer;
                             EdgeSize   : Integer;
                             Storage    : Cv_Mem_Storage_P) return Cv_Graph_P;

   -- Adds a vertex to a graph.
   function Cv_Graph_Add_Vtx (Graph       : Cv_Graph_P;
                              Vtx         : Cv_Graph_Vtx_P := null;
                              InsertedVtx : access Cv_Graph_Vtx_P := null) return Integer;

   -- Removes a vertex from a graph.
   function Cv_Graph_Remove_Vtx (Graph : Cv_Graph_P;
                                 Index : Integer) return Integer;

   --Removes a vertex from a graph by using its pointer.
   function Cv_Graph_Remove_Vtx_By_Ptr (Graph : Cv_Graph_P;
                                        Vtx   : Cv_Graph_Vtx_P) return Integer;

   -- Adds an edge to a graph.
   function Cv_Graph_Add_Edge (Graph        : Cv_Graph_P;
                               StartIdx     : Integer;
                               EndIdx       : Integer;
                               Edge         : Cv_Graph_Edge_P := null;
                               InsertedEdge : access Cv_Graph_Edge_P := null) return Integer;

   -- Adds an edge to a graph by using its pointer.
   function Cv_Graph_Add_Edge_By_Ptr (Graph        : Cv_Graph_P;
                                      StartVtx     : Cv_Graph_Vtx_P;
                                      EndVtx       : Cv_Graph_Vtx_P;
                                      Edge         : Cv_Graph_Edge_P := null;
                                      InsertedEdge : access Cv_Graph_Edge_P := null) return Integer;

   -- Removes an edge from a graph.
   procedure Cv_Graph_Remove_Edge (Graph    : Cv_Graph_P;
                                   StartIdx : Integer;
                                   EndIdx   : Integer);

   --Removes an edge from a graph by using its pointer.
   procedure Cv_Graph_Remove_Edge_By_Ptr (Graph    : Cv_Graph_P;
                                          StartVtx : Cv_Graph_Vtx_P;
                                          EndVtx   : Cv_Graph_Vtx_P);

   -- Finds an edge in a graph.
   function Cv_Find_Graph_Edge (Graph    : Cv_Graph_P;
                                StartIdx : Integer;
                                EndIdx   : Integer) return Cv_Graph_Edge_P;
   function Cv_Graph_Find_Edge (Graph    : Cv_Graph_P;
                                StartIdx : Integer;
                                EndIdx   : Integer) return Cv_Graph_Edge_P renames Cv_Find_Graph_Edge;

   -- Finds an edge in a graph by using its pointer.
   function Cv_Find_Graph_Edge_By_Ptr (Graph    : Cv_Graph_P;
                                       StartVtx : Cv_Graph_Vtx_P;
                                       EndVtx   : Cv_Graph_Vtx_P) return Cv_Graph_Edge_P;
   function Cv_Graph_Find_Edge_By_Ptr (Graph    : Cv_Graph_P;
                                       StartVtx : Cv_Graph_Vtx_P;
                                       EndVtx   : Cv_Graph_Vtx_P) return Cv_Graph_Edge_P renames Cv_Find_Graph_Edge_By_Ptr;

   --Clears a graph.
   procedure Cv_Clear_Graph (Graph : Cv_Graph_P);

   -- Counts the number of edges indicent to the vertex.
   function Cv_Graph_Vtx_Degree (Graph  : Cv_Graph_P;
                                 Vtxldx : Integer) return Integer;

   -- Finds an edge in a graph.
   function Cv_Graph_Vtx_Degree_By_Ptr (Graph : Cv_Graph_P;
                                        Vtx   : Cv_Graph_Vtx_P) return Integer;

   -- Finds a graph vertex by using its index.
   function Cv_Get_Graph_Vtx (Graph  : Cv_Graph_P;
                              VtxIdx : Integer) return Cv_Graph_Vtx_P;

   -- Returns the index of a graph vertex.
   function Cv_Graph_Vtx_Idx (Graph : Cv_Graph_P;
                              Vtx   : Cv_Graph_Vtx_P) return Integer;

   -- Returns the index of a graph edge.
   function Cv_Graph_Edge_Idx (Graph : Cv_Graph_P;
                               Edge  : Cv_Graph_Edge_P) return Integer;

   --#define cvGraphGetVtxCount( graph ) ((graph)->active_count)
   function Cv_Graph_Get_Vtx_Count (Graph : Cv_Graph_P) return Integer;
   --#define cvGraphGetEdgeCount( graph ) ((graph)->edges->active_count)
   function Cv_Graph_Get_Edge_Count (Graph : Cv_Graph_P) return Integer;

   CV_GRAPH_VERTEX : constant := 1;
   CV_GRAPH_TREE_EDGE : constant := 2;
   CV_GRAPH_BACK_EDGE : constant :=  4;
   CV_GRAPH_FORWARD_EDGE : constant := 8;
   CV_GRAPH_CROSS_EDGE : constant := 16;
   CV_GRAPH_ANY_EDGE : constant := 30;
   CV_GRAPH_NEW_TREE : constant := 32;
   CV_GRAPH_BACKTRACKING : constant := 64;
   CV_GRAPH_OVER : constant := -1;
   CV_GRAPH_ALL_ITEMS : constant := -1;

   CV_GRAPH_ITEM_VISITED_FLAG : constant := 16#40000000#;

   --     #define  CV_IS_GRAPH_VERTEX_VISITED(vtx) \
   --      (((CvGraphVtx*)(vtx))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
   function CV_IS_GRAPH_VERTEX_VISISTED (Vtx : Cv_Graph_Vtx_P) return Integer;

   --   #define  CV_IS_GRAPH_EDGE_VISITED(edge) \
   --      (((CvGraphEdge*)(edge))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
   function CV_IS_GRAPH_EDGE_VISITED (Edge : Cv_Graph_Edge_P) return Integer;

   --     #define  CV_GRAPH_SEARCH_TREE_NODE_FLAG   (1 << 29)
   CV_GRAPH_SEARCH_TREE_NODE_FLAG : constant := 16#20000000#;
   --  #define  CV_GRAPH_FORWARD_EDGE_FLAG       (1 << 28)
   CV_GRAPH_FORWARD_EDGE_FLAG : constant := 16#10000000#;

   type Cv_Graph_Scanner is record
      Vtx   : access Cv_Graph_Vtx;
      Dst   : access Cv_Graph_Vtx;
      Edge  : access Cv_Graph_Edge;

      Graph : access Cv_Graph;
      Stack : access Cv_Seq;
      Index : Integer;
      Mask  : Integer;
   end record;
   type Cv_Graph_Scanner_P is access Cv_Graph_Scanner;

   -- Creates structure for depth-first graph traversal.
   function Cv_Create_Graph_Scanner (Graph : Cv_Graph_P;
                                     Vtx   : Cv_Graph_Vtx_P := null; --null = start from beginning
                                     Mask  : Integer := CV_GRAPH_ALL_ITEMS) return Cv_Graph_Scanner_P;

   -- Completes the graph traversal procedure.
   procedure Cv_Release_Graph_Scanner (Scanner : access Cv_Graph_Scanner_P);

   -- Executes one or more steps of the graph traversal procedure.
   function Cv_Next_Graph_Item (Scanner : Cv_Graph_Scanner_P) return Integer;

   -- Clones a graph.
   procedure Cv_Clone_Graph (Graph   : Cv_Graph_P;
                             Storage : Cv_Mem_Storage_P);

   -----------------------------------------------------------------------------
   -- DrawingCV_RGB
   -----------------------------------------------------------------------------
   -- Creates a Cv_Scalar color from RGB values
   function CV_RGB (R : Integer;
                    G : Integer;
                    B : Integer) return Cv_Scalar;

   CV_FILLED : constant := -1;
   CV_AA : constant := 16;

   -- Draws a line segment connecting two points.
   procedure Cv_Line (Img       : Cv_Arr_P;
                      Pt1       : Cv_Point;
                      Pt2       : Cv_Point;
                      Color     : Cv_Scalar;
                      Thickness : Integer := 1;
                      Line_Type : Integer := 8;
                      Shift     : Integer := 0);

   -- Draws a simple, thick, or filled rectangle.
   procedure Cv_Rectangle (Img       : Cv_Arr_P;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer :=  1;
                           LineType  : Integer := 8;
                           Shift     : Integer := 0);

   -- /* Draws a rectangle specified by a CvRect structure */
   procedure Cv_Rectangle_R (Img       : Cv_Arr_P;
                             R         : Cv_Rect;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             LineType  : Integer := 8;
                             Shift     : Integer := 0);

   -- Draws a circle.
   procedure Cv_Circle (Img       : Cv_Arr_P;
                        Center    : Cv_Point;
                        Radius    : Integer;
                        Color     : Cv_Scalar;
                        Thickness : Integer := 1;
                        LineType  : Integer := 8;
                        Shift     : Integer := 0);

   -- Draws a simple or thick elliptic arc or an fills ellipse sector.
   procedure Cv_Ellipse (Img        : Cv_Arr_P;
                         Center     : Cv_Point;
                         Axes       : Cv_Size;
                         Angle      : Long_Float;
                         StartAngle : Long_Float;
                         EndAngle   : Long_Float;
                         Color      : Cv_Scalar;
                         Thickness  : Integer := 1;
                         LineType   : Integer := 8;
                         Shift      : Integer := 0);

   -- Draws a simple or thick elliptic arc or fills an ellipse sector.
   procedure Cv_Ellipse_Box (Img       : Cv_Arr_P;
                             Box       : Cv_Box_2D;
                             Color     : Cv_Scalar;
                             Thickness : Integer;
                             LineType  : Integer := 8;
                             Shift     : Integer := 0);

   -- Fills a convex polygon.
   procedure Cv_Fill_Convex_Poly (Img      : Cv_Arr_P;
                                  Pts      : Cv_Point_Array;
                                  Npts     : Integer;
                                  Color    : Cv_Scalar;
                                  LineType : Integer := 8;
                                  Shift    : Integer := 0);

   -- Fills a polygon's interior.
   procedure Cv_Fill_Poly (Img      : Cv_Arr_P;
                           Pts      : Cv_Point_Pointer_Array;
                           Npts     : Cv_32U_Array;
                           Contours : Integer;
                           Color    : Cv_Scalar;
                           LineType : Integer := 8;
                           Shift    : Integer := 0);

   -- Draws simple or thick polygons.
   procedure Cv_Poly_Line (Img       : Cv_Arr_P;
                           Pts       : Cv_Point_Pointer_Array;
                           Npts      : Cv_32U_Array;
                           Contours  : Integer;
                           IsClosed  : Integer;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           LineTyoe  : Integer := 8;
                           Shift     : Integer := 0);

   procedure Cv_Draw_Rect (Img       : Cv_Arr_P;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer :=  1;
                           LineType  : Integer := 8;
                           Shift     : Integer := 0) renames Cv_Rectangle;

   procedure Cv_Draw_Line (Img       : Cv_Arr_P;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           Line_Type : Integer := 8;
                           Shift     : Integer := 0) renames Cv_Line;

   procedure Cv_Draw_Circle (Img       : Cv_Arr_P;
                             Center    : Cv_Point;
                             Radius    : Integer;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             LineType  : Integer := 8;
                             Shift     : Integer := 0) renames Cv_Circle;

   procedure Cv_Draw_Ellipse (Img        : Cv_Arr_P;
                              Center     : Cv_Point;
                              Axes       : Cv_Size;
                              Angle      : Long_Float;
                              StartAngle : Long_Float;
                              EndAngle   : Long_Float;
                              Color      : Cv_Scalar;
                              Thickness  : Integer := 1;
                              LineType   : Integer := 8;
                              Shift      : Integer := 0) renames Cv_Ellipse;

   procedure Cv_Draw_Poly_Line (Img       : Cv_Arr_P;
                                Pts       : Cv_Point_Pointer_Array;
                                Npts      : Cv_32U_Array;
                                Contours  : Integer;
                                IsClosed  : Integer;
                                Color     : Cv_Scalar;
                                Thickness : Integer := 1;
                                LineTyoe  : Integer := 8;
                                Shift     : Integer := 0) renames Cv_Poly_Line;

   -- Clips the line against the image rectangle.
   function Cv_Clip_Line (ImgSize : Cv_Size;
                          Pt1     : access Cv_Point;
                          Pt2     : access Cv_Point) return Integer;

   -- Initializes the line iterator.
   function Cv_Init_Line_Iterator (Image        : Cv_Arr_P;
                                   Pt1          : Cv_Point;
                                   Pt2          : Cv_Point;
                                   LineIterator : access Cv_Line_Iterator;
                                   Connectivity : Integer := 8;
                                   LeftToRight  : Integer := 0) return Integer;

   procedure CV_NEXT_LINE_POINT (LineIterator : Cv_Line_Iterator_P);

   -- Fonts
   type Cv_Font_Face is new Integer;
   CV_FONT_HERSHEY_SIMPLEX  : constant Cv_Font_Face := 0;
   CV_FONT_HERSHEY_PLAIN  : constant Cv_Font_Face := 1;
   CV_FONT_HERSHEY_DUPLEX  : constant Cv_Font_Face :=  2;
   CV_FONT_HERSHEY_COMPLEX  : constant Cv_Font_Face := 3;
   CV_FONT_HERSHEY_TRIPLEX  : constant Cv_Font_Face := 4;
   CV_FONT_HERSHEY_COMPLEX_SMALL  : constant Cv_Font_Face := 5;
   CV_FONT_HERSHEY_SCRIPT_SIMPLEX  : constant Cv_Font_Face := 6;
   CV_FONT_HERSHEY_SCRIPT_COMPLEX  : constant Cv_Font_Face := 7;

   CV_FONT_ITALIC : constant := 16;

   CV_FONT_VECTOR0 : constant Cv_Font_Face := (CV_FONT_HERSHEY_SIMPLEX);

   --
   type Cv_Font is record
      Name_Font         : access String_C;
      Color             : Cv_Scalar;
      Font_Face         : Cv_Font_Face;
      Ascii             : Cv_32u_Array_P;
      Greek             : Cv_32u_Array_P;
      Cyrillic          : Cv_32u_Array_P;
      Hscale, Vscale    : Float;
      Shear             : Float;
      Thickness         : Integer;
      Dx                : Float;
      Line_Type         : Integer;
   end record;
   type Cv_Font_P is access Cv_Font;



   -- Initializes font structure.
   procedure Cv_Init_Font (Font      : access Cv_Font;
                           FontFace  : Cv_Font_Face;
                           Hscale    : Long_Float;
                           Vscale    : Long_Float;
                           Shear     : Long_Float := 0.0;
                           Thickness : Integer := 1;
                           LineType  : Integer := 8);

   function Cv_Create_Font (Scale     : Long_Float;
                            Thickness : Integer := 1) return Cv_Font;

   procedure Cv_Put_Text (Img   : Cv_Arr_P;
                          Text  : String;
                          Org   : Cv_Point;
                          Font  : access Cv_Font;
                          Color : Cv_Scalar);

   -- Retrieves the width and height of a text string.
   procedure Cv_Get_Text_Size (TextString : String;
                               Font       : Cv_Font;
                               TextSize   : access Cv_Size;
                               Baseline   : access Integer);

   --     /* Unpacks color value, if arrtype is CV_8UC?, <color> is treated as
   --     packed color value, otherwise the first channels (depending on arrtype)
   --     of destination scalar are set to the same value = <color> */
   function Cv_Color_To_Scalar (Packed_Color : Long_Float;
                                Arrtype      : Integer) return Cv_Scalar;

   --     /* Returns the polygon points which make up the given ellipse.  The ellipse is define by
   --     the box of size 'axes' rotated 'angle' around the 'center'.  A partial sweep
   --     of the ellipse arc can be done by spcifying arc_start and arc_end to be something
   --     other than 0 and 360, respectively.  The input array 'pts' must be large enough to
   --     hold the result.  The total number of points stored into 'pts' is returned by this
   --     function. */
   function Cv_Ellipse_To_Poly (Center      : Cv_Point;
                                Axes        : Cv_Size;
                                Angle       : Integer;
                                ArcStart    : Integer;
                                ArcEnd      : Integer;
                                Pts         : Cv_Point_Array;
                                DeltaVal    : Integer) return Integer;

   -- Draws contour outlines or interiors in an image.
   procedure Cv_Draw_Contours (Img           : Cv_Arr_P;
                               Contour       : access Cv_Seq;
                               ExternalColor : Cv_Scalar;
                               HoleColor     : Cv_Scalar;
                               MaxLevel      : Integer;
                               Thickness     : Integer := 1;
                               LineType      : Integer := 8;
                               Offset        : Cv_Point := Cv_Create_Point (0, 0));

   --     Performs a look-up table transform of an array.
   procedure Cv_LUT (Src : access Cv_Arr;
                     Dst : access Cv_Arr;
                     Lut : access Cv_Arr);

   -----------------------------------------------------------------------------
   -- Iteration through the sequence tree
   -----------------------------------------------------------------------------
   type Cv_Tree_Node_Iterator is record
      Node      : Cv_Void_P; -- Note: This is actually a void*
      Level     : Integer;
      Max_Level : Integer;
   end record;
   type Cv_Tree_Node_Iterator_P is access Cv_Tree_Node_Iterator;

   -- Initializes the tree node iterator.
   procedure Cv_Init_Tree_Node_Iterator (TreeIterator : Cv_Tree_Node_Iterator_P;
                                         First        : Cv_Void_P;
                                         MaxLevel     : Integer);

   -- Returns the currently observed node and moves the iterator toward the next node.
   function Cv_Next_Tree_Node (TreeIterator : Cv_Tree_Node_Iterator_P) return Cv_Void_P;

   -- Returns the currently observed node and moves the iterator toward the previous node.
   function Cv_Prev_Tree_Node (TreeIterator : Cv_Tree_Node_Iterator_P) return Cv_Void_P;

   --Adds a new node to a tree.
   procedure Cv_Insert_Node_Into_Tree (Node   : Cv_Void_P;
                                       Parent : Cv_Void_P;
                                       Fram   : Cv_Void_P);

   --/* Removes contour from tree (together with the contour children). */
   procedure Cv_Remove_Node_From_Tree (Node  : Cv_Void_P;
                                       Frame : Cv_Void_P);

   --Gathers all node pointers to a single sequence.
   function Cv_Tree_To_Node_Seq (First      : Cv_Void_P;
                                 HeaderSize : Integer;
                                 Storage    : Cv_Mem_Storage_P) return Cv_Seq_P;

   CV_KMEANS_USE_INITIAL_LABELS : constant := 1;

   function Cv_K_Means2 (Samples     : access Cv_Arr;
                         Labels      : access Cv_Arr;
                         Termcrit    : Cv_Term_Criteria;
                         Attempts    : Integer := 1;
                         Rng         : Integer_64 := 0;
                         Flags       : Unsigned_32 := 0;
                         Centers     : access Cv_Arr := null;
                         Compactness : access Long_Float := null) return Integer;

   -----------------------------------------------------------------------------
   -- System Functions
   -----------------------------------------------------------------------------
   --     Registers another module.
   function Cv_Register_Module (Module_Info : access Cv_Module_Info)
                                return Integer;

   --     Switches between optimized/non-optimized modes.
   function Cv_Use_Optimized (Onoff : Integer)
                              return Integer;

   --     Retrieves information about registered module(s) and plugins.
   procedure Cv_Get_Module_Info (Module_Name          : Interfaces.C.Strings.Chars_Ptr;
                                 Version              : access Interfaces.C.Strings.Chars_Ptr;
                                 Loaded_Addon_Plugins : access Interfaces.C.Strings.Chars_Ptr);

   type Cv_Alloc_Func is access function (Size     : Interfaces.C.Size_T;
                                          Userdata : Cv_Void_P)
                                          return Cv_Void_P;
   pragma Convention (C, Cv_Alloc_Func);

   type Cv_Free_Func is access function (Pptr     : Cv_Void_P;
                                         Userdata : Cv_Void_P)
                                         return Integer;
   pragma Convention (C, Cv_Free_Func);

   --     Accesses custom/default memory managing functions.
   procedure Cv_Set_Memory_Manager (Alloc_Func : Cv_Alloc_Func := null;
                                    Free_Func  : Cv_Free_Func := null;
                                    Userdata   : Cv_Void_P := null);

   -- typedef IplImage* (CV_STDCALL* Cv_iplCreateImageHeader)
   --   (int,int,int,char*,char*,int,int,int,int,int,IplROI*,IplImage*,void*,IplTileInfo*);
   type Cv_Ipl_Create_Image_Header is access function (N_Size        : Integer;
                                                       ID            : Integer;
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
                                                       ROI           : Ipl_ROI_P;
                                                       Image         : Ipl_Image_P;
                                                       Image_Id      : Cv_Void_P)
                                                       return Ipl_Image_P;
   pragma Convention (C, Cv_Ipl_Create_Image_Header);

   -- typedef void (CV_STDCALL * Cv_iplAllocateImageData) (IplImage * , int, int);
   type Cv_Ipl_Allocate_Image_Data is access procedure (Image  : access Ipl_Image;
                                                        Width  : Integer;
                                                        Height : Integer);
   pragma Convention (C, Cv_Ipl_Allocate_Image_Data);

   -- typedef void (CV_STDCALL * Cv_iplDeallocate) (IplImage * , int);
   type Cv_Ipl_Deallocate is access procedure (Image : access Ipl_Image;
                                               I     : Integer);
   pragma Convention (C, Cv_Ipl_Deallocate);

   -- typedef IplROI * (CV_STDCALL * Cv_iplCreateROI) (int, int, int, int, int);
   type Cv_Ipl_Create_ROI is access function (COI      : Integer;
                                              Height   : Integer;
                                              Width    : Integer;
                                              X_Offset : Integer;
                                              Y_Offset : Integer)
                                              return Ipl_ROI_P;
   pragma Convention (C, Cv_Ipl_Create_ROI);

   -- typedef IplImage * (CV_STDCALL * Cv_iplCloneImage) (const IplImage * );
   type Cv_Ipl_Clone_Image is access function (Image : access Ipl_Image)
                                               return Ipl_Image_P;
   pragma Convention (C, Cv_Ipl_Clone_Image);

   --     Switches to IPL functions for image allocation/deallocation.
   procedure Cv_Set_IPL_Allocators (Create_Header : Cv_Ipl_Create_Image_Header;
                                    Allocate_Data : Cv_Ipl_Allocate_Image_Data;
                                    Deallocate    : Cv_Ipl_Deallocate;
                                    Create_ROI    : Cv_Ipl_Create_ROI;
                                    Clone_Image   : Cv_Ipl_Clone_Image);

   -- N/A
   procedure CV_TURN_ON_IPL_COMPATIBILITY;

   -----------------------------------------------------------------------------
   -- Data Persistence
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- High-level functions
   -----------------------------------------------------------------------------
   --     Opens file storage for reading or writing data.
   function Cv_Open_File_Storage (Fílename   : Interfaces.C.Strings.Chars_Ptr;
                                  Memstorage : access Cv_Mem_Storage;
                                  Flags      : Unsigned_32)
                                  return Cv_File_Storage_P;

   --     Releases file storage.
   procedure Cv_Release_File_Storage (Fs : access Cv_File_Storage_P);

   --/* returns attribute value or 0 (NULL) if there is no such attribute */
   function Cv_Attr_Value (Attr       : Cv_Attr_List_P;
                           Attr_Name  : Interfaces.C.Strings.Chars_Ptr) return Interfaces.C.Strings.Chars_Ptr;

   --     Starts writing a new structure.
   procedure Cv_Start_Write_Struct (Fs           : access Cv_File_Storage;
                                    Name         : Interfaces.C.Strings.Chars_Ptr;
                                    Struct_Flags : Unsigned_32;
                                    Type_Name    : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr;
                                    Attributes   : Cv_Attr_List := Cv_Create_Attr_List);

   --     Ends the writing of a structure.
   procedure Cv_End_Write_Struct (Fs : access Cv_File_Storage);

   --     Writes an integer value.
   procedure Cv_Write_Int (Fs    : access Cv_File_Storage;
                           Name  : Interfaces.C.Strings.Chars_Ptr;
                           Value : Integer);

   --     Writes a floating-point value.
   procedure Cv_Write_Real (Fs    : access Cv_File_Storage;
                            Name  : Interfaces.C.Strings.Chars_Ptr;
                            Value : Long_Float);

   --     Writes a text string.
   procedure Cv_Write_String (Fs    : access Cv_File_Storage;
                              Name  : Interfaces.C.Strings.Chars_Ptr;
                              Str   : Interfaces.C.Strings.Chars_Ptr;
                              Quote : Integer := 0);

   --     Writes a comment.
   procedure Cv_Write_Comment (Fs          : access Cv_File_Storage;
                               Comment     : Interfaces.C.Strings.Chars_Ptr;
                               Eol_Comment : Integer);

   --     Writes a user object.
   procedure Cv_Write (Fs         : access Cv_File_Storage;
                       Name       : Interfaces.C.Strings.Chars_Ptr;
                       Ptr        : Cv_Void_P;
                       Attributes : Cv_Attr_List := Cv_Create_Attr_List);

   --     Starts the next stream.
   procedure Cv_Start_Next_Stream (Fs : access Cv_File_Storage);

   --     Writes multiple numbers.
   procedure Cv_Write_Raw_Data (Fs  : access Cv_File_Storage;
                                Src : Cv_Void_P;
                                Len : Integer;
                                Dt  : Interfaces.C.Strings.Chars_Ptr);

   --     Returns a unique pointer for a given name.
   function Cv_Get_Hashed_Key (Fs             : access Cv_File_Storage;
                               Name           : String_C;
                               Len            : Integer := -1;
                               Create_Missing : Integer := 0)
                               return Cv_String_Hash_Node_P;

   --     Retrieves one of the top-level nodes of the file storage.
   function Cv_Get_Root_File_Node (Fs           : access Cv_File_Storage;
                                   Stream_Index : Integer := 0)
                                   return Cv_File_Node_P;

   --     Finds a node in a map or file storage.
   function Cv_Get_File_Node (Fs             : access Cv_File_Storage;
                              Map            : access Cv_File_Node;
                              Key            : access Cv_String_Hash_Node;
                              Create_Missing : Integer := 0)
                              return Cv_File_Node_P;

   --     Finds a node in a map or file storage.
   function Cv_Get_File_Node_By_Name (Fs   : access Cv_File_Storage;
                                      Map  : access Cv_File_Node;
                                      Name : Interfaces.C.Strings.Chars_Ptr)
                                      return Cv_File_Node_P;

   --     Finds a file node and returns its value.
   function Cv_Read_Int (Node          : access Cv_File_Node;
                         Default_Value : Integer := 0)
                         return Integer;

   function Cv_Read_Int_By_Name (Fs            : Cv_File_Storage_P;
                                 Map           : Cv_File_Node_P ;
                                 Name          : Interfaces.C.Strings.Chars_Ptr;
                                 Default_Value : Integer := 0) return Integer;

   --     Retrieves a floating-point value from a file node.
   function Cv_Read_Real (Node          : access Cv_File_Node;
                          Default_Value : Long_Float := 0.0)
                          return Long_Float;

   --     Finds a file node and returns its value.
   function Cv_Read_Real_By_Name (Fs            : access Cv_File_Storage;
                                  Map           : access Cv_File_Node;
                                  Name          : Interfaces.C.Strings.Chars_Ptr;
                                  Default_Value : Long_Float := 0.0)
                                  return Long_Float;

   --     Retrieves a text string from a file node.
   function Cv_Read_String (Node          : access Cv_File_Node;
                            Default_Value : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr)
                            return Interfaces.C.Strings.Chars_Ptr;

   --     Finds a file node by its name and returns its value.
   function Cv_Read_String_By_Name (Fs            : access Cv_File_Storage;
                                    Map           : access Cv_File_Node;
                                    Name          : Interfaces.C.Strings.Chars_Ptr;
                                    Default_Value : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr)
                                    return Interfaces.C.Strings.Chars_Ptr;

   --     Decodes an object and returns a pointer to it.
   function Cv_Read (Fs         : access Cv_File_Storage;
                     Node       : access Cv_File_Node;
                     Attributes : access Cv_Attr_List := null)
                     return Cv_Void_P;

   --     Finds an object by name and decodes it.
   function Cv_Read_By_Name (Fs         : access Cv_File_Storage;
                             Map        : access Cv_File_Node;
                             Name       : Interfaces.C.Strings.Chars_Ptr;
                             Attributes : access Cv_Attr_List := null)
                             return Cv_Void_P;

   --     Initializes the file node sequence reader.
   procedure Cv_Start_Read_Raw_Data (Fs     : access Cv_File_Storage;
                                     Src    : access Cv_File_Node;
                                     Reader : access Cv_Seq_Reader);

   --     Initializes file node sequence reader.
   procedure Cv_Read_Raw_Data_Slice (Fs     : access Cv_File_Storage;
                                     Reader : access Cv_Seq_Reader;
                                     Count  : Integer;
                                     Dst    : Cv_Void_P;
                                     Dt     : Interfaces.C.Strings.Chars_Ptr);

   --     Reads multiple numbers.
   procedure Cv_Read_Raw_Data (Fs  : access Cv_File_Storage;
                               Src : access Cv_File_Node;
                               Dst : Cv_Void_P;
                               Dt  : Interfaces.C.Strings.Chars_Ptr);

   --     Writes a file node to another file storage.
   procedure Cv_Write_File_Node (Fs            : access Cv_File_Storage;
                                 New_Node_Name : Interfaces.C.Strings.Chars_Ptr;
                                 Node          : access Cv_File_Node;
                                 Embed         : Integer);

   --     Returns the name of a file node.
   function Cv_Get_File_Node_Name (Node : access Cv_File_Node)
                                   return Interfaces.C.Strings.Chars_Ptr;

   -----------------------------------------------------------------------------
   -- Adding own types
   -----------------------------------------------------------------------------
   --     Registers a new type.
   procedure Cv_Register_Type (Info : access Cv_Type_Info);

   --     Unregisters the type.
   procedure Cv_Unregister_Type (Type_Name : Interfaces.C.Strings.Chars_Ptr);

   --     Returns the beginning of a type list.
   function Cv_First_Type return Cv_Type_Info_P;

   --     Finds a type by its name.
   function Cv_Find_Type (Type_Name : Interfaces.C.Strings.Chars_Ptr)
                          return Cv_Type_Info_P;

   --     Returns the type of an object.
   function Cv_Type_Of (Struct_Ptr : Cv_Void_P)
                        return Cv_Type_Info_P;

   --     Releases an object.
   procedure Cv_Release (Struct_Ptr : access Cv_Void_P);

   --     Makes a clone of an object.
   function Cv_Clone (Struct_Ptr : Cv_Void_P)
                      return Cv_Void_P;

   --     Saves an object to a file.
   procedure Cv_Save (Filename   : Interfaces.C.Strings.Chars_Ptr;
                      Struct_Ptr : Cv_Void_P;
                      Name       : Interfaces.C.Strings.Chars_Ptr;
                      Comment    : Interfaces.C.Strings.Chars_Ptr;
                      Attributes : Cv_Attr_List := Cv_Create_Attr_List);

   --     Loads an object from a file.
   function Cv_Load (Filename  : String_C;
                     Storage   : access Cv_Mem_Storage := null;
                     Name      : Interfaces.C.Strings.Chars_Ptr := Null_Ptr;
                     Real_Name : access Interfaces.C.Strings.Chars_Ptr := null)
                     return Cv_Void_P;

   -----------------------------------------------------------------------------
   -- Measuring Execution Time
   -----------------------------------------------------------------------------
   --     Returns the number of ticks.
   function Cv_Get_Tick_Count return Integer_64;

   --     Returns the number of ticks per microsecond.
   function Cv_Get_Tick_Frequency return Long_Float;

   -----------------------------------------------------------------------------
   -- CPU capabilities
   -----------------------------------------------------------------------------
   CV_CPU_NONE : constant := 0;
   CV_CPU_MMX : constant := 1;
   CV_CPU_SSE : constant := 2;
   CV_CPU_SSE2 : constant := 3;
   CV_CPU_SSE3 : constant := 4;
   CV_CPU_SSSE3 : constant := 5;
   CV_CPU_SSE4_1 : constant := 6;
   CV_CPU_SSE4_2 : constant := 7;
   CV_CPU_AVX : constant := 10;
   CV_HARDWARE_MAX_FEATURE : constant := 255;

   function Cv_Check_Hardware_Support (Feature : Integer) return Integer;

   -----------------------------------------------------------------------------
   -- Multi-Threading
   -----------------------------------------------------------------------------
   --/* retrieve/set the number of threads used in OpenMP implementations */
   function Cv_Get_Num_Threads return Integer;
   procedure Cv_Set_Num_Threads ( Threads : Integer := 0);

   --/* get index of the thread being executed */
   function Cv_Get_Thread_Num return Integer;
   -----------------------------------------------------------------------------
   -- Error Handling
   -----------------------------------------------------------------------------



   --     Returns the current error status.
   function Cv_Get_Err_Status return Integer;

   --     Sets the error status.
   procedure Cv_Set_Err_Status (Status : Integer);

   CV_ErrModeLeaf   : constant := 0;
   CV_ErrModeParent : constant := 1;
   CV_ErrModeSilent : constant := 2;

   --     Returns the current error mode.
   function Cv_Get_Err_Mode return Integer;

   --     Sets the error mode.
   function Cv_Set_Err_Mode (Mode : Integer)
                             return Integer;

   --     Raises an error.
   function Cv_Error (Status    : Integer;
                      Func_Name : Interfaces.C.Strings.Chars_Ptr;
                      Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                      Filename  : Interfaces.C.Strings.Chars_Ptr;
                      Line      : Integer)
                      return Integer;

   --     Returns textual description of an error status code.
   function Cv_Error_Str (Status : Integer)
                          return Interfaces.C.Strings.Chars_Ptr;

   --/* Retrieves detailed information about the last error occured */
   -- This doesn't work anyway so why do I care! it returns 0 yay!
   function Cv_Get_Err_Info (Errcode_Desc : access Interfaces.C.Strings.Chars_Ptr;
                             Desciption   : access Interfaces.C.Strings.Chars_Ptr;
                             Filename     : access Interfaces.C.Strings.Chars_Ptr;
                             Line         : access Integer) return Integer;

   --   /* Maps IPP error codes to the counterparts from OpenCV */
   function Cv_Error_From_Ipp_Status (IppStatus : Integer) return Integer;

   type Cv_Error_Callback is access function (Status    : Integer;
                                              Func_Name : Interfaces.C.Strings.Chars_Ptr;
                                              Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                                              File_Name : Interfaces.C.Strings.Chars_Ptr;
                                              Line      : Integer)
                                              return Integer;
   pragma Convention (C, Cv_Error_Callback);

   --     Sets a new error handler.
   function Cv_Redirect_Error (Error_Handler : Cv_Error_Callback;
                               Userdata      : Cv_Void_P;
                               Prev_Userdata : access Cv_Void_P)
                               return Cv_Error_Callback;

   --     Provide standard error handling.
   function Cv_Nul_Dev_Report (Status    : Integer;
                               Func_Name : Interfaces.C.Strings.Chars_Ptr;
                               Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                               File_Name : Interfaces.C.Strings.Chars_Ptr;
                               Line      : Integer;
                               Userdata  : Cv_Void_P)
                               return Integer;

   function Cv_Std_Err_Report (Status    : Integer;
                               Func_Name : Interfaces.C.Strings.Chars_Ptr;
                               Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                               File_Name : Interfaces.C.Strings.Chars_Ptr;
                               Line      : Integer;
                               Userdata  : Cv_Void_P) return Integer;

   function Cv_Gui_Box_Report (Status    : Integer;
                               Func_Name : Interfaces.C.Strings.Chars_Ptr;
                               Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                               File_Name : Interfaces.C.Strings.Chars_Ptr;
                               Line      : Integer;
                               Userdata  : Cv_Void_P) return Integer;

   procedure OPENCV_ERROR (Status  : Integer;
                           Func    : String := GNAT.Source_Info.Enclosing_Entity;
                           Context : String := GNAT.Source_Info.Enclosing_Entity;
                           File    : String := GNAT.Source_Info.File;
                           Line    : Integer := GNAT.Source_Info.Line);

   -- This function is a rename...
   procedure CV_ERROR (Status  : Integer;
                       Func    : String := GNAT.Source_Info.Enclosing_Entity;
                       Context : String := GNAT.Source_Info.Enclosing_Entity;
                       File    : String := GNAT.Source_Info.File;
                       Line    : Integer := GNAT.Source_Info.Line);

   -- Don't use this it doesn't work.
   procedure OPENCV_ERRCHK (Func    : String := GNAT.Source_Info.Enclosing_Entity;
                            Context : String := GNAT.Source_Info.Enclosing_Entity;
                            File    : String := GNAT.Source_Info.File;
                            Line    : Integer := GNAT.Source_Info.Line);

   procedure OPENCV_ASSERT (Expression : Boolean;
                            Func       : String := GNAT.Source_Info.Enclosing_Entity;
                            Context    : String := GNAT.Source_Info.Enclosing_Entity;
                            File       : String := GNAT.Source_Info.File;
                            Line       : Integer := GNAT.Source_Info.Line);

   --(cvSetErrStatus(CV_StsOk))
   procedure OPENCV_RSTERR;

   procedure OPENCV_CALL;

   procedure CV_ERROR_FROM_CODE (Status  : Integer;
                                 Func    : String := GNAT.Source_Info.Enclosing_Entity;
                                 Context : String := GNAT.Source_Info.Enclosing_Entity;
                                 File    : String := GNAT.Source_Info.File;
                                 Line    : Integer := GNAT.Source_Info.Line) renames OPENCV_ERROR;

   procedure CV_CHECK (Func    : String := GNAT.Source_Info.Enclosing_Entity;
                       Context : String := GNAT.Source_Info.Enclosing_Entity;
                       File    : String := GNAT.Source_Info.File;
                       Line    : Integer := GNAT.Source_Info.Line);

   procedure CV_CALL renames OPENCV_CALL;

   procedure CV_ASSERT (Expression : Boolean;
                        Func       : String := GNAT.Source_Info.Enclosing_Entity;
                        Context    : String := GNAT.Source_Info.Enclosing_Entity;
                        File       : String := GNAT.Source_Info.File;
                        Line       : Integer := GNAT.Source_Info.Line) renames OPENCV_ASSERT;
   -----------------------------------------------------------------------------
   -- Private
   -----------------------------------------------------------------------------
private
   procedure Cv_Free_Wrapper (Ptr : access Cv_Void_P);
   -- Wrapper due to String
   function W_Cv_Mem_Storage_Alloc_String (Storage : Cv_Mem_Storage_P;
                                           Ptr     : String_C;
                                           Len     : Integer := -1) return Cv_String;

   procedure W_Cv_Put_Text (Img   : Cv_Arr_P;
                            Text  : String_C;
                            Org   : Cv_Point;
                            Font  : access Cv_Font;
                            Color : Cv_Scalar);

   procedure W_Cv_Get_Text_Size (TextString : String_C;
                                 Font       : Cv_Font;
                                 TextSize   : access Cv_Size;
                                 Baseline   : access Integer);

   pragma Import (C, Cv_Alloc, "cvAlloc");
   pragma Import (C, Cv_Free_Wrapper, "cvFree_");
   pragma Import (C, Cv_Create_Image_Header, "cvCreateImageHeader");
   pragma Import (C, Cv_Init_Image_Header, "cvInitImageHeader");
   pragma Import (C, Cv_Create_Image, "cvCreateImage");
   pragma Import (C, Cv_Release_Image_Header, "cvReleaseImageHeader");
   pragma Import (C, Cv_Release_Image, "cvReleaseImage");
   pragma Import (C, Cv_Clone_Image, "cvCloneImage");
   pragma Import (C, Cv_Set_Image_COI, "cvSetImageCOI");
   pragma Import (C, Cv_Get_Image_COI, "cvGetImageCOI");
   pragma Import (C, Cv_Set_Image_ROI, "cvSetImageROI");
   pragma Import (C, Cv_Reset_Image_ROI, "cvResetImageROI");
   pragma Import (C, Cv_Get_Image_ROI, "cvGetImageROI");
   pragma Import (C, Cv_Create_Mat_Header, "cvCreateMatHeader");
   pragma Import (C, Cv_Init_Mat_Header, "cvInitMatHeader");
   pragma Import (C, Cv_Create_Mat, "cvCreateMat");
   pragma Import (C, Cv_Release_Mat, "cvReleaseMat");
   pragma Import (C, Cv_Dec_Ref_Data, "cvDecRefData");
   pragma Import (C, Cv_Inc_Ref_Data, "cvIncRefData");
   pragma Import (C, Cv_Clone_Mat, "cvCloneMat");
   pragma Import (C, Cv_Get_Sub_Rect, "cvGetSubRect");
   pragma Import (C, Cv_Get_Rows, "cvGetRows");
   pragma Import (C, Cv_Get_Row, "cvGetRow");
   pragma Import (C, Cv_Get_Cols, "cvGetCols");
   pragma Import (C, Cv_Get_Col, "cvGetCol");
   pragma Import (C, Cv_Get_Diag, "cvGetDiag");
   pragma Import (C, Cv_Scalar_To_Raw_Data, "cvScalarToRawData");
   pragma Import (C, Cv_Raw_Data_To_Scalar, "cvRawDataToScalar");
   pragma Import (C, Cv_Create_Mat_ND_Header, "cvCreateMatNDHeader");
   pragma Import (C, Cv_Create_Mat_ND, "cvCreateMatND");
   pragma Import (C, Cv_Init_Mat_ND_Header, "cvInitMatNDHeader");
   pragma Import (C, Cv_Release_Mat_ND, "cvReleaseMatND");
   pragma Import (C, Cv_Clone_Mat_ND, "cvCloneMatND");
   pragma Import (C, Cv_Create_Sparse_Mat, "cvCreateSparseMat");
   pragma Import (C, Cv_Release_Sparse_Mat, "cvReleaseSparseMat");
   pragma Import (C, Cv_Clone_Sparse_Mat, "cvCloneSparseMat");
   pragma Import (C, Cv_Init_Sparse_Mat_Iterator, "cvInitSparseMatIterator");
   pragma Import (C, Cv_Get_Next_Sparse_Node, "cvGetNextSparseNode");
   pragma Import (C, Cv_Init_N_Array_Iterator, "cvInitNArrayIterator");
   pragma Import (C, Cv_Next_N_Array_Slice, "cvNextNArraySlice");
   pragma Import (C, Cv_Get_Elem_Type, "cvGetElemType");
   pragma Import (C, Cv_Get_Dims, "cvGetDims");
   pragma Import (C, Cv_Get_Dim_Size, "cvGetDimSize");
   pragma Import (C, Cv_Set_Real_1D, "cvSetReal1D");
   pragma Import (C, Cv_Set_Real_2D, "cvSetReal2D");
   pragma Import (C, Cv_Set_Real_3D, "cvSetReal3D");
   pragma Import (C, Cv_Set_Real_ND, "cvSetRealND");
   pragma Import (C, Cv_Ptr_1D, "cvPtr1D");
   pragma Import (C, Cv_Ptr_2D, "cvPtr2D");
   pragma Import (C, Cv_Ptr_3D, "cvPtr3D");
   pragma Import (C, Cv_Ptr_ND, "cvPtrND");
   pragma Import (C, Cv_Get_1D, "cvGet1D");
   pragma Import (C, Cv_Get_2D, "cvGet2D");
   pragma Import (C, Cv_Get_3D, "cvGet3D");
   pragma Import (C, Cv_Get_ND, "cvGetND");
   pragma Import (C, Cv_Get_Real_1D, "cvGetReal1D");
   pragma Import (C, Cv_Get_Real_2D, "cvGetReal2D");
   pragma Import (C, Cv_Get_Real_3D, "cvGetReal3D");
   pragma Import (C, Cv_Get_Real_ND, "cvGetRealND");
   pragma Import (C, Cv_Set_1D, "CvSet1D");
   pragma Import (C, Cv_Set_2D, "CvSet2D");
   pragma Import (C, Cv_Set_3D, "CvSet3D");
   pragma Import (C, Cv_Set_ND, "CvSetND");
   pragma Import (C, Cv_Clear_ND, "cvClearND");
   pragma Import (C, Cv_Get_Mat, "cvGetMat");
   pragma Import (C, Cv_Get_Image, "cvGetImage");
   pragma Import (C, Cv_Reshape_Mat_ND, "cvReshapeMatND");
   pragma Import (C, Cv_Reshape, "cvReshape");
   pragma Import (C, Cv_Repeat, "cvRepeat");
   pragma Import (C, Cv_Create_Data, "cvCreateData");
   pragma Import (C, Cv_Release_Data, "cvReleaseData");
   pragma Import (C, Cv_Set_Data, "cvSetData");
   pragma Import (C, Cv_Get_Raw_Data, "cvGetRawData");
   pragma Import (C, Cv_Get_Size, "cvGetSize");
   pragma Import (C, Cv_Copy, "cvCopy");
   pragma Import (C, Cv_Set_All, "cvSet");
   pragma Import (C, Cv_Set_Zero, "cvSetZero");
   pragma Import (C, Cv_Split, "cvSplit");
   pragma Import (C, Cv_Merge, "cvMerge");
   pragma Import (C, Cv_Mix_Channels, "cvMixChannels");
   pragma Import (C, Cv_Convert_Scale, "cvConvertScale");
   pragma Import (C, Cv_Convert_Scale_Abs, "cvConvertScaleAbs");
   pragma Import (C, Cv_Check_Term_Criteria, "cvCheckTermCriteria");
   pragma Import (C, Cv_Add, "cvAdd");
   pragma Import (C, Cv_Add_S, "cvAddS");
   pragma Import (C, Cv_Sub, "cvSub");
   pragma Import (C, Cv_Sub_S, "cvSubS");
   pragma Import (C, Cv_Sub_RS, "cvSubRS");
   pragma Import (C, Cv_Mul, "cvMul");
   pragma Import (C, Cv_Div, "cvDiv");
   pragma Import (C, Cv_Scale_Add, "cvScaleAdd");
   pragma Import (C, Cv_Add_Weighted, "cvAddWeighted");
   pragma Import (C, Cv_Dot_Product, "cvDotProduct");
   pragma Import (C, Cv_And, "cvAnd");
   pragma Import (C, Cv_And_S, "cvAndS");
   pragma Import (C, Cv_Or, "cvOr");
   pragma Import (C, Cv_Or_S, "cvOrS");
   pragma Import (C, Cv_Xor, "cvXor");
   pragma Import (C, Cv_Xor_S, "cvXorS");
   pragma Import (C, Cv_Not, "cvNot");
   pragma Import (C, Cv_In_Range, "cvInRange");
   pragma Import (C, Cv_In_Range_S, "cvInRangeS");
   pragma Import (C, Cv_Cmp, "cvCmp");
   pragma Import (C, Cv_Cmp_S, "cvCmpS");
   pragma Import (C, Cv_Min, "cvMin");
   pragma Import (C, Cv_Max, "cvMax");
   pragma Import (C, Cv_Min_S, "cvMinS");
   pragma Import (C, Cv_Max_S, "cvMaxS");
   pragma Import (C, Cv_Abs_Diff, "cvAbsDiff");
   pragma Import (C, Cv_Abs_Diff_S, "cvAbsDiffS");
   pragma Import (C, Cv_Cart_To_Polar, "cvCartToPolar");
   pragma Import (C, Cv_Polar_To_Cart, "cvPolarToCart");
   pragma Import (C, Cv_Pow, "cvPow");
   pragma Import (C, Cv_Exp, "cvExp");
   pragma Import (C, Cv_Log, "cvLog");
   pragma Import (C, Cv_Fast_Arctan, "cvFastArctan");
   pragma Import (C, Cv_Cbrt, "cvCbrt");
   pragma Import (C, Cv_Check_Arr, "cvCheckArr");
   pragma Import (C, Cv_Rand_Arr, "cvRandArr");
   pragma Import (C, Cv_Rand_Shuffle, "cvRandShuffle");
   pragma Import (C, Cv_Sort, "cvSort");
   pragma Import (C, Cv_Solve_Cubic, "cvSolveCubic");
   pragma Import (C, Cv_Solve_Poly, "cvSolvePoly");
   pragma Import (C, Cv_Cross_Product, "cvCrossProduct");
   pragma Import (C, Cv_GEMM, "cvGEMM");
   pragma Import (C, Cv_Transform, "cvTransform");
   pragma Import (C, Cv_Perspective_Transform, "cvPerspectiveTransform");
   pragma Import (C, Cv_Mul_Transposed, "cvMulTransposed");
   pragma Import (C, Cv_Transpose, "cvTranspose");
   pragma Import (C, Cv_Complete_Symm, "cvCompleteSymm");
   pragma Import (C, Cv_Flip, "cvFlip");
   pragma Import (C, Cv_SVDecomp, "cvSVD");
   pragma Import (C, Cv_SVBkSb, "cvSVBkSb");
   pragma Import (C, Cv_Invert, "cvInvert");
   pragma Import (C, Cv_Solve, "cvSolve");
   pragma Import (C, Cv_Det, "cvDet");
   pragma Import (C, Cv_Trace, "cvTrace");
   pragma Import (C, Cv_Eigen_VV, "cvEigenVV");
   pragma Import (C, Cv_Set_Identity, "cvSetIdentity");
   pragma Import (C, Cv_Range, "cvRange");
   pragma Import (C, Cv_Calc_Covar_Matrix, "cvCalcCovarMatrix");
   pragma Import (C, Cv_Calc_PCA, "cvCalcPCA");
   pragma Import (C, Cv_Project_PCA, "cvProjectPCA");
   pragma Import (C, Cv_Back_Project_PCA, "cvBackProjectPCA");
   pragma Import (C, Cv_Mahalanobis, "cvMahalanobis");
   pragma Import (C, Cv_Sum, "cvSum");
   pragma Import (C, Cv_Count_Non_Zero, "cvCountNonZero");
   pragma Import (C, Cv_Avg, "cvAvg");
   pragma Import (C, Cv_Avg_Sdv, "cvAvgSdv");
   pragma Import (C, Cv_Min_Max_Loc, "cvMinMaxLoc");
   pragma Import (C, Cv_Norm, "cvNorm");
   pragma Import (C, Cv_Normalize, "cvNormalize");
   pragma Import (C, Cv_Reduce, "cvReduce");
   pragma Import (C, Cv_DFT, "cvDFT");
   pragma Import (C, Cv_Mul_Spectrums, "cvMulSpectrums");
   pragma Import (C, Cv_Get_Optimal_DFT_Size, "cvGetOptimalDFTSize");
   pragma Import (C, Cv_DCT, "cvDCT");
   pragma Import (C, Cv_Slice_Length, "cvSliceLength");
   pragma Import (C, Cv_Create_Mem_Storage, "cvCreateMemStorage");
   pragma Import (C, Cv_Create_Child_Mem_Storage, "cvCreateChildMemStorage");
   pragma Import (C, Cv_Release_Mem_Storage, "cvReleaseMemStorage");
   pragma Import (C, Cv_Clear_Mem_Storage, "cvClearMemStorage");
   pragma Import (C, Cv_Save_Mem_Storage_Pos, "cvSaveMemStoragePos");
   pragma Import (C, Cv_Restore_Mem_Storage_Pos, "cvRestoreMemStoragePos");
   pragma Import (C, Cv_Mem_Storage_Alloc, "cvMemStorageAlloc");
   pragma Import (C, W_Cv_Mem_Storage_Alloc_String, "cvMemStorageAllocString");
   pragma Import (C, Cv_Create_Seq, "cvCreateSeq");
   pragma Import (C, Cv_Set_Seq_Block_Size, "cvSetSeqBlockSize");
   pragma Import (C, Cv_Seq_Push, "cvSeqPush");
   pragma Import (C, Cv_Seq_Push_Front, "cvSeqPushFront");
   pragma Import (C, Cv_Seq_Pop, "cvSeqPop");
   pragma Import (C, Cv_Seq_Pop_Front, "cvSeqPopFront");
   pragma Import (C, Cv_Seq_Push_Multi, "cvSeqPushMulti");
   pragma Import (C, Cv_Seq_Pop_Multi, "cvSeqPopMulti");
   pragma Import (C, Cv_Seq_Insert, "cvSeqInsert");
   pragma Import (C, Cv_Seq_Remove, "cvSeqRemove");
   pragma Import (C, Cv_Clear_Seq, "cvClearSeq");
   pragma Import (C, Cv_Get_Seq_Elem, "cvGetSeqElem");
   pragma Import (C, Cv_Seq_Elem_Idx, "cvSeqElemIdx");
   pragma Import (C, Cv_Start_Append_To_Seq, "cvStartAppendToSeq");
   pragma Import (C, Cv_Start_Write_Seq, "cvStartWriteSeq");
   pragma Import (C, Cv_End_Write_Seq, "cvEndWriteSeq");
   pragma Import (C, Cv_Flush_Seq_Writer, "cvFlushSeqWriter");
   pragma Import (C, Cv_Start_Read_Seq, "cvStartReadSeq");
   pragma Import (C, Cv_Get_Seq_Reader_Pos, "cvGetSeqReaderPos");
   pragma Import (C, Cv_Set_Seq_Reader_Pos, "cvSetSeqReaderPos");
   pragma Import (C, Cv_Cvt_Seq_To_Array, "cvCvtSeqToArray");
   pragma Import (C, Cv_Make_Seq_Header_For_Array, "cvMakeSeqHeaderForArray");
   pragma Import (C, Cv_Seq_Slice, "cvSeqSlice");
   pragma Import (C, Cv_Clone_Seq, "cvCloneSeq");
   pragma Import (C, Cv_Seq_Remove_Slice, "cvSeqRemoveSlice");
   pragma Import (C, Cv_Seq_Sort, "cvSeqSort");
   pragma Import (C, Cv_Seq_Search, "cvSeqSearch");
   pragma Import (C, Cv_Seq_Insert_Slice, "cvSeqInsertSlice");
   pragma Import (C, Cv_Seq_Invert, "cvSeqInvert");
   pragma Import (C, Cv_Seq_Partition, "cvSeqPartition");
   pragma Import (C, Cv_Create_Set, "cvCreateSet");
   pragma Import (C, Cv_Set_Add, "cvSetAdd");
   pragma Import (C, Cv_Set_New, "cvSetNew");
   pragma Import (C, Cv_Set_Remove_By_Ptr, "cvSetRemoveByPtr");
   pragma Import (C, Cv_Set_Remove, "cvSetRemove");
   pragma Import (C, Cv_Get_Set_Elem, "cvGetSetElem");
   pragma Import (C, Cv_Clear_Set, "cvClearSet");
   pragma Import (C, Cv_Create_Graph, "cvCreateGraph");
   pragma Import (C, Cv_Graph_Add_Vtx, "cvGraphAddVtx");
   pragma Import (C, Cv_Graph_Remove_Vtx, "cvGraphRemoveVtx");
   pragma Import (C, Cv_Graph_Remove_Vtx_By_Ptr, "cvGraphRemoveVtxByPtr");
   pragma Import (C, Cv_Graph_Add_Edge, "cvGraphAddEdge");
   pragma Import (C, Cv_Graph_Add_Edge_By_Ptr, "cvGraphAddEdgeByPtr");
   pragma Import (C, Cv_Graph_Remove_Edge, "cvGraphRemoveEdge");
   pragma Import (C, Cv_Graph_Remove_Edge_By_Ptr, "cvGraphRemoveEdgeByPtr");
   pragma Import (C, Cv_Find_Graph_Edge, "cvFindGraphEdge");
   pragma Import (C, Cv_Find_Graph_Edge_By_Ptr, "cvFindGraphEdgeByPtr");
   pragma Import (C, Cv_Clear_Graph, "cvClearGraph");
   pragma Import (C, Cv_Graph_Vtx_Degree, "cvGraphVtxDegree");
   pragma Import (C, Cv_Graph_Vtx_Degree_By_Ptr, "cvGraphVtxDegreeByPtr");
   pragma Import (C, Cv_Get_Graph_Vtx, "cvGetSetElem");
   pragma Import (C, Cv_Create_Graph_Scanner, "cvCreateGraphScanner");
   pragma Import (C, Cv_Release_Graph_Scanner, "cvReleaseGraphScanner");
   pragma Import (C, Cv_Next_Graph_Item, "cvNextGraphItem");
   pragma Import (C, Cv_Clone_Graph, "cvCloneGraph");
   pragma Import (C, Cv_Line, "cvLine");
   pragma Import (C, Cv_Rectangle, "cvRectangle");
   pragma Import (C, Cv_Rectangle_R, "cvRectangleR");
   pragma Import (C, Cv_Circle, "cvCircle");
   pragma Import (C, Cv_Ellipse, "cvEllipse");
   pragma Import (C, Cv_Ellipse_Box, "cvEllipseBox");
   pragma Import (C, Cv_Fill_Convex_Poly, "cvFillConvexPoly");
   pragma Import (C, Cv_Fill_Poly, "cvFillPoly");
   pragma Import (C, Cv_Poly_Line, "cvPolyLine");
   pragma Import (C, Cv_Clip_Line, "cvClipLine");
   pragma Import (C, Cv_Init_Line_Iterator, "cvInitLineIterator");
   pragma Import (C, Cv_Init_Font, "cvInitFont");
   pragma Import (C, Cv_Create_Font, "cvFont");
   pragma Import (C, W_Cv_Put_Text, "cvPutText");
   pragma Import (C, W_Cv_Get_Text_Size, "cvGetTextSize");
   pragma Import (C, Cv_Color_To_Scalar, "cvColorToScalar");
   pragma Import (C, Cv_Ellipse_To_Poly, "cvEllipse2Poly");
   pragma Import (C, Cv_Draw_Contours, "cvDrawContours");
   pragma Import (C, Cv_LUT, "cvLUT");
   pragma Import (C, Cv_Init_Tree_Node_Iterator, "cvInitTreeNodeIterator");
   pragma Import (C, Cv_Next_Tree_Node, "cvNextTreeNode");
   pragma Import (C, Cv_Prev_Tree_Node, "cvPrevTreeNode");
   pragma Import (C, Cv_Insert_Node_Into_Tree, "cvInsertNodeIntoTree");
   pragma Import (C, Cv_Remove_Node_From_Tree, "cvRemoveNodeFromTree");
   pragma Import (C, Cv_Tree_To_Node_Seq, "cvTreeToNodeSeq");
   pragma Import (C, Cv_K_Means2, "cvKMeans2");
   pragma Import (C, Cv_Register_Module, "cvRegisterModule");
   pragma Import (C, Cv_Use_Optimized, "cvUseOptimized");
   pragma Import (C, Cv_Get_Module_Info, "cvGetModuleInfo");
   pragma Import (C, Cv_Set_Memory_Manager, "cvSetMemoryManager");
   pragma Import (C, Cv_Set_IPL_Allocators, "cvSetIPLAllocators");
   pragma Import (C, Cv_Open_File_Storage, "cvOpenFileStorage");
   pragma Import (C, Cv_Release_File_Storage, "cvReleaseFileStorage");
   pragma Import (C, Cv_Attr_Value, "cvAttrValue");
   pragma Import (C, Cv_Start_Write_Struct, "cvStartWriteStruct");
   pragma Import (C, Cv_End_Write_Struct, "CvEndWriteStruct");
   pragma Import (C, Cv_Write_Int, "cvWriteInt");
   pragma Import (C, Cv_Write_Real, "cvWriteReal");
   pragma Import (C, Cv_Write_String, "cvWriteString");
   pragma Import (C, Cv_Write_Comment, "cvWriteComment");
   pragma Import (C, Cv_Write, "cvWrite");
   pragma Import (C, Cv_Start_Next_Stream, "cvStartNextStream");
   pragma Import (C, Cv_Write_Raw_Data, "cvWriteRawData");
   pragma Import (C, Cv_Get_Hashed_Key, "cvGetHashedKey");
   pragma Import (C, Cv_Get_Root_File_Node, "cvGetRootFileNode");
   pragma Import (C, Cv_Get_File_Node, "cvGetFileNode");
   pragma Import (C, Cv_Get_File_Node_By_Name, "cvGetFileNodeByName");
   pragma Import (C, Cv_Read_Int, "cvReadInt");
   pragma Import (C, Cv_Read_Int_By_Name, "cvReadIntByName");
   pragma Import (C, Cv_Read_Real, "cvReadReal");
   pragma Import (C, Cv_Read_Real_By_Name, "cvReadRealByName");
   pragma Import (C, Cv_Read_String, "cvReadString");
   pragma Import (C, Cv_Read_String_By_Name, "cvReadStringByName");
   pragma Import (C, Cv_Read, "cvRead");
   pragma Import (C, Cv_Read_By_Name, "cvReadByName");
   pragma Import (C, Cv_Start_Read_Raw_Data, "cvStartReadRawData");
   pragma Import (C, Cv_Read_Raw_Data_Slice, "cvReadRawDataSlice");
   pragma Import (C, Cv_Read_Raw_Data, "cvReadRawData");
   pragma Import (C, Cv_Write_File_Node, "cvWriteFileNode");
   pragma Import (C, Cv_Get_File_Node_Name, "cvGetFileNodeName");
   pragma Import (C, Cv_Register_Type, "cvRegisterType");
   pragma Import (C, Cv_Unregister_Type, "cvUnregisterType");
   pragma Import (C, Cv_First_Type, "cvFirstType");
   pragma Import (C, Cv_Find_Type, "cvFindType");
   pragma Import (C, Cv_Type_Of, "cvTypeOf");
   pragma Import (C, Cv_Release, "cvRelease");
   pragma Import (C, Cv_Clone, "cvClone");
   pragma Import (C, Cv_Save, "cvSave");
   pragma Import (C, Cv_Load, "cvLoad");
   pragma Import (C, Cv_Get_Tick_Count, "cvGetTickCount");
   pragma Import (C, Cv_Get_Tick_Frequency, "cvGetTickFrequency");
   pragma Import (C, Cv_Check_Hardware_Support, "cvCheckHardwareSupport");
   pragma Import (C, Cv_Get_Num_Threads, "cvGetNumThreads");
   pragma Import (C, Cv_Set_Num_Threads, "cvSetNumThreads");
   pragma Import (C, Cv_Get_Thread_Num, "cvGetThreadNum");
   pragma Import (C, Cv_Get_Err_Status, "cvGetErrStatus");
   pragma Import (C, Cv_Set_Err_Status, "cvSetErrStatus");
   pragma Import (C, Cv_Get_Err_Mode, "cvGetErrMode");
   pragma Import (C, Cv_Set_Err_Mode, "cvSetErrMode");
   pragma Import (C, Cv_Error, "cvError");
   pragma Import (C, Cv_Error_Str, "cvErrorStr");
   pragma Import (C, Cv_Get_Err_Info, "cvGetErrInfo");
   pragma Import (C, Cv_Error_From_Ipp_Status, "cvErrorFromIppStatus");
   pragma Import (C, Cv_Redirect_Error, "cvRedirectError");
   pragma Import (C, Cv_Nul_Dev_Report, "cvNulDevReport");
   pragma Import (C, Cv_Std_Err_Report, "cvStdErrReport");
   pragma Import (C, Cv_Gui_Box_Report, "cvGuiBoxReport");
end Core.Operations;
