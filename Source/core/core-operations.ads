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
   function CvAlloc (Size : Interfaces.C.Size_T)
                     return Cv_Void_P;

   --     Deallocates a memory buffer.
   procedure CvFree (Ptr : access Cv_Void_P);

   --     Creates an image header but does not allocate the image data.
   function CvCreateImageHeader (Size    : Cv_Size;
                                 Depth   : Unsigned_32;
                                 Channel : Integer)
                                 return Ipl_Image_P;

   --     Initializes an image header that was previously allocated.
   function CvInitImageHeader (Image    : access Ipl_Image;
                               Size     : Cv_Size;
                               Depth    : Unsigned_32;
                               Channels : Integer;
                               Origin   : Integer := 0;
                               Align    : Integer := 4)
                               return Ipl_Image_P;

   --     Creates an image header and allocates the image data.
   function CvCreateImage (Size     : Cv_Size;
                           Depth    : Unsigned_32;
                           Channels : Integer)
                           return Ipl_Image_P;

   --     Deallocates an image header.
   procedure CvReleaseImageHeader (Image : access Ipl_Image_P);

   --     Deallocates the image header and the image data.
   procedure CvReleaseImage (Image : access Ipl_Image_P);

   --     Makes a full copy of an image, including the header, data, and ROI.
   function CvCloneImage (Image : access Ipl_Image)
                          return Ipl_Image_P;

   --     Sets the channel of interest in an IplImage.
   procedure CvSetImageCOI (Image : access Ipl_Image;
                            Coi   : Integer);

   --     Returns the index of the channel of interest.
   function CvGetImageCOI (Image : access Ipl_Image)
                           return Integer;

   --     Sets an image Region Of Interest (ROI) for a given rectangle.
   procedure CvSetImageROI (Image : access Ipl_Image;
                            Rect  : Cv_Rect);

   --     Resets the image ROI to include the entire image and releases the
   --     ROI structure.
   procedure CvResetImageROI (Image : access Ipl_Image);

   --     Returns the image ROI.
   function CvGetImageROI (Image : access Ipl_Image)
                           return Cv_Rect;


   --     Creates a matrix header but does not allocate the matrix data.
   function CvCreateMatHeader (Rows     : Integer;
                               Cols     : Integer;
                               Mat_Type : Integer)
                               return Cv_Mat_P;

   --     Initializes a pre-allocated matrix header.
   function CvInitMatHeader (Mat   : access Cv_Mat;
                             Rows  : Integer;
                             Cols  : Integer;
                             Mat_T : Integer;
                             Data  : access Mat_Data := null; -- void*
                             Step  : Integer := CV_AUTOSTEP)
                             return Cv_Mat_P;

   --     Creates a matrix header and allocates the matrix data.
   function CvCreateMat (Rows     : Integer;
                         Cols     : Integer;
                         Mat_Type : Integer)
                         return Cv_Mat_P;

   --     Deallocates a matrix.
   procedure CvReleaseMat (Mat : access Cv_Mat_P);

   --     Decrements an array data reference counter.
   procedure CvDecRefData (Arr : access Cv_Arr);

   --     Increments array data reference counter.
   function CvIncRefData (Arr : access Cv_Arr)
                          return Integer;

   --     Creates a full matrix copy.
   function CvCloneMat (Mat : access Cv_Mat)
                        return Cv_Mat_P;

   --     Returns matrix header corresponding to the rectangular sub-array of
   --     input image or matrix.
   function CvGetSubRect (Arr    : access Cv_Arr;
                          Submat : access Cv_Mat;
                          Rect   : Cv_Rect)
                          return Cv_Mat_P;
   function CvGetSubArr (Arr    : access Cv_Arr;
                         Submat : access Cv_Mat;
                         Rect   : Cv_Rect)
                         return Cv_Mat_P renames CvGetSubRect;

   --     Returns row span.
   function CvGetRows (Arr       : access Cv_Arr;
                       Submat    : access Cv_Mat;
                       Start_Row : Integer;
                       End_Row   : Integer;
                       Delta_Row : Integer := 1)
                       return Cv_Mat_P;

   --     Returns array row.
   function CvGetRow (Arr    : access Cv_Arr;
                      Submat : access Cv_Mat;
                      Row    : Integer)
                      return Cv_Mat_P;

   --     Returns array column span.
   function CvGetCols (Arr       : access Cv_Arr;
                       Submat    : access Cv_Mat;
                       Start_Col : Integer;
                       End_Col   : Integer)
                       return Cv_Mat_P;

   --     Returns array column.
   function CvGetCol (Arr    : access Cv_Arr;
                      Submat : access Cv_Mat;
                      Col    : Integer)
                      return Cv_Mat_P;

   --     Returns one of array diagonals.
   function CvGetDiag (Arr    : access Cv_Arr;
                       Submat : access Cv_Mat;
                       Diag   : Integer := 0)
                       return Cv_Mat_P;

   --/* low-level scalar <-> raw data conversion functions */
   procedure CvScalarToRawData (Scalar     : access Cv_Scalar;
                                Data       : Cv_Void_P;
                                Itype      : Integer;
                                ExtendTo12 : Integer := 0);
   procedure CvRawDataToScalar (Data   : Cv_Void_P;
                                Itype  : Integer;
                                Scalar : access Cv_Scalar);

   --     Creates a new matrix header but does not allocate the matrix data.
   function CvCreateMatNDHeader (Dims     : Integer;
                                 Sizes    : access Integer;
                                 Mat_Type : Integer)
                                 return Cv_Mat_ND_P;

   --     Creates the header and allocates the data for a multi-dimensional
   --     dense array.
   function CvCreateMatND (Dims     : Integer;
                           Sizes    : access Integer;
                           Mat_Type : Integer)
                           return Cv_Mat_ND_P;

   --     Initializes a pre-allocated multi-dimensional array header.
   function CvInitMatNDHeader (Mat   : access Cv_Mat_ND;
                               Dims  : Integer;
                               Sizes : Cv_32s_Array;
                               Mat_T : Integer;
                               Data  : access Mat_Data := null) -- void*
                               return Cv_Mat_ND_P;

   --     Deallocates a multi-dimensional array.
   procedure CvReleaseMatND (Mat : access Cv_Mat_ND_P);

   --     Creates full copy of a multi-dimensional array and returns a pointer
   --     to the copy.
   function CvCloneMatND (Mat : access Cv_Mat_ND)
                          return Cv_Mat_ND_P;

   --     Creates sparse array.
   function CvCreateSparseMat (Dims     : Integer;
                               Sizes    : access Integer;
                               Mat_Type : Integer)
                               return Cv_Sparse_Mat_P;

   --     Deallocates sparse array.
   procedure CvReleaseSparseMat (Mat : access Cv_Sparse_Mat_P);

   --     Creates full copy of sparse array.
   function CvCloneSparseMat (Mat : access Cv_Sparse_Mat)
                              return Cv_Sparse_Mat_P;

   --     Initializes sparse array elements iterator.
   function CvInitSparseMatIterator (Mat      : access Cv_Sparse_Mat;
                                     Mat_Iter : access Cv_Sparse_Mat_Iterator)
                                     return Cv_Sparse_Node_P;

   --     Returns the next sparse matrix element
   function CvGetNextSparseNode (Mat_Iterator : access Cv_Sparse_Mat_Iterator)
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
   procedure CvInitNArrayIterator (Count         : Integer;
                                   Arrs          : Cv_Arr_P_Array;
                                   Mask          : Cv_Arr_P;
                                   Stubs         : Cv_Mat_ND_P;
                                   ArrayIterator : CV_N_Array_Iterator;
                                   Flags         : Integer := 0);

   --  /* returns zero value if iteration is finished, non-zero (slice length) otherwise */
   function CvNextNArraySlice (Array_Iterator : Cv_N_Array_Iterator) return Integer;

   --     Returns type of array elements.
   function CvGetElemType (Arr : access Cv_Arr)
                           return Integer;

   --     Return number of array dimensions and their sizes.
   function CvGetDims (Arr   : access Cv_Arr;
                       Sizes : Cv_32s_Array)
                       return Integer;

   --     Return the size of a particular dimension.
   function CvGetDimSize (Arr   : access Cv_Arr;
                          Index : Integer)
                          return Integer;

   --     Return pointer to a particular array element.
   function CvPtr1D (Arr   : access Cv_Arr;
                     Idx0  : Integer;
                     Mat_T : access Integer := null)
                     return Cv_Void_P;

   --     Return pointer to a particular array element.
   function CvPtr2D (Arr   : access Cv_Arr;
                     Idx0  : Integer;
                     Idx1  : Integer;
                     Mat_T : access Integer := null)
                     return Cv_Void_P;

   --     Return pointer to a particular array element.
   function CvPtr3D (Arr   : access Cv_Arr;
                     Idx0  : Integer;
                     Idx1  : Integer;
                     Idx2  : Integer;
                     Mat_T : access Integer := null)
                     return Cv_Void_P;

   --     Return pointer to a particular array element.
   function CvPtrND (Arr             : access Cv_Arr;
                     Idx             : Cv_32s_Array;
                     Mat_T           : access Integer := null;
                     Create_Node     : Integer := 1;
                     Precalc_Hashval : access Unsigned_32 := null)
                     return Cv_Void_P;

   --     Return a specific array element.
   function CvGet1D (Arr  : access Cv_Arr;
                     Idx0 : Integer)
                     return Cv_Scalar;

   --     Return a specific array element.
   function CvGet2D (Arr  : access Cv_Arr;
                     Idx0 : Integer;
                     Idx1 : Integer)
                     return Cv_Scalar;

   --     Return a specific array element.
   function CvGet3D (Arr  : access Cv_Arr;
                     Idx0 : Integer;
                     Idx1 : Integer;
                     Idx2 : Integer)
                     return Cv_Scalar;

   --     Return a specific array element.
   function CvGetND (Arr : access Cv_Arr;
                     Idx : Cv_32s_Array)
                     return Cv_Scalar;

   --     Return a specific element of single-channel 1D array.
   function CvGetReal1D (Arr  : access Cv_Arr;
                         Idx0 : Integer)
                         return Long_Float;

   --     Return a specific element of single-channel 2D array.
   function CvGetReal2D (Arr  : access Cv_Arr;
                         Idx0 : Integer;
                         Idx1 : Integer)
                         return Long_Float;

   --     Return a specific element of single-channel array.
   function CvGetReal3D (Arr  : access Cv_Arr;
                         Idx0 : Integer;
                         Idx1 : Integer;
                         Idx2 : Integer)
                         return Long_Float;

   --     Return a specific element of single-channel array.
   function CvGetRealND (Arr  : access Cv_Arr;
                         Idx  : Cv_32s_Array)
                         return Long_Float;

   --     Change the particular array element.
   procedure CvSet1D (Arr   : access Cv_Arr;
                      Idx0  : Integer;
                      Value : Cv_Scalar);

   --     Change the particular array element.
   procedure CvSet2D (Arr   : access Cv_Arr;
                      Idx0  : Integer;
                      Idx1  : Integer;
                      Value : Cv_Scalar);

   --     Change the particular array element.
   procedure CvSet3D (Arr   : access Cv_Arr;
                      Idx0  : Integer;
                      Idx1  : Integer;
                      Idx2  : Integer;
                      Value : Cv_Scalar);

   --     Change the particular array element.
   procedure CvSetND (Arr   : access Cv_Arr;
                      Idx   : Cv_32s_Array;
                      Value : Cv_Scalar);

   --     Change a specific array element.
   procedure CvSetReal1D (Arr   : access Cv_Arr;
                          Idx0  : Integer;
                          Value : Long_Float);

   --     Change a specific array element.
   procedure CvSetReal2D (Arr   : access Cv_Arr;
                          Idx0  : Integer;
                          Idx1  : Integer;
                          Value : Long_Float);

   --     Change a specific array element.
   procedure CvSetReal3D (Arr   : access Cv_Arr;
                          Idx0  : Integer;
                          Idx1  : Integer;
                          Idx2  : Integer;
                          Value : Long_Float);

   --     Change a specific array element.
   procedure CvSetRealND (Arr   : access Cv_Arr;
                          Idx0  : Cv_32s_Array;
                          Value : Long_Float);

   --     Clears a specific array element.
   procedure CvClearND (Arr : access Cv_Arr;
                        Idx : access Integer);

   --     Returns matrix header for arbitrary array.
   function CvGetMat (Arr     : access Cv_Arr;
                      Header  : access Cv_Mat;
                      Coi     : access Integer := null;
                      AllowND : Integer := 0)
                      return Cv_Mat_P;

   --     Returns image header for arbitrary array.
   function CvGetImage (Arr          : access Cv_Arr;
                        Image_Header : access Ipl_Image)
                        return Ipl_Image_P;

   --     Changes the shape of a multi-dimensional array without copying
   --     the data.
   function CvReshapeMatND (Arr           : access Cv_Arr;
                            Sizeof_Header : Integer;
                            Header        : access Cv_Arr;
                            New_Cn        : Integer;
                            New_Dims      : Integer;
                            New_Sizes     : Cv_32s_Array)
                            return access Cv_Arr;

   -- #define cvReshapeND( arr, header, new_cn, new_dims, new_sizes )   \
   --        cvReshapeMatND( (arr), sizeof(*(header)), (header),         \
   --                        (new_cn), (new_dims), (new_sizes))
   function CvReshapeND (Arr      : Cv_Arr_P;
                         Header   : Cv_Arr_P;
                         NewCn    : Integer;
                         NewDims  : Integer;
                         NewSizes : Cv_32s_Array) return Cv_Arr_P;

   --     Changes shape of matrix/image without copying data.
   function CvReshape (Arr      : access Cv_Arr;
                       Header   : access Cv_Mat;
                       New_Cn   : Integer;
                       New_Rows : Integer := 0)
                       return Cv_Mat_P;

   --     Fill the destination array with repeated copies of the source array.
   procedure CvRepeat (Src : access Cv_Arr;
                       Dst : access Cv_Arr);

   --     Allocates array data
   procedure CvCreateData (Arr : access Cv_Arr);

   --     Releases array data.
   procedure CvReleaseData (Arr : access Cv_Arr);

   --     Assigns user data to the array header.
   procedure CvSetData (Arr  : Cv_Arr_P;
                        Data : System.Address;
                        Step : Integer);

   --     Retrieves low-level information about the array.
   procedure CvGetRawData (Arr      : access Cv_Arr;
                           Data     : access Cv_Arr_P; -- uchar**
                           Step     : access Integer := null;
                           Roi_Size : access Cv_Size := null);

   --     Returns size of matrix or image ROI.
   function CvGetSize (Arr : access Cv_Arr)
                       return Cv_Size;

   --     Copies one array to another.
   procedure CvCopy (Src  : access Cv_Arr;
                     Dst  : access Cv_Arr;
                     Mask : access Cv_Arr := null);

   --     Sets every element of an array to a given value.
   procedure CvSet (Arr   : access Cv_Arr;
                    Value : Cv_Scalar;
                    Mask  : access Cv_Arr := null);

   --     Clears the array.
   procedure CvSetZero (Arr : access Cv_Arr);
   procedure CvZero (Arr : access Cv_Arr) renames CvSetZero;


   --     Divides multi-channel array into several single-channel arrays or
   --     extracts a single channel from the array.
   procedure CvSplit (Src  : access Cv_Arr;
                      Dst0 : access Cv_Arr;
                      Dst1 : access Cv_Arr;
                      Dst2 : access Cv_Arr;
                      Dst3 : access Cv_Arr);

   --     Composes a multi-channel array from several single-channel arrays or
   --     inserts a single channel into the array.
   procedure CvMerge (Src0 : access Cv_Arr;
                      Src1 : access Cv_Arr;
                      Src2 : access Cv_Arr;
                      Src3 : access Cv_Arr;
                      Dst  : access Cv_Arr);

   --     Copies several channels from input arrays to certain channels of
   --     output arrays
   procedure CvMixChannels (Src        : access Cv_Arr_P;
                            Src_Count  : Integer;
                            Dst        : access Cv_Arr_P;
                            Dst_Count  : Integer;
                            From_To    : Cv_32s_Array;
                            Pair_Count : Integer);

   --     Converts one array to another with optional linear transformation.
   procedure CvConvertScale (Src   : access Cv_Arr;
                             Dst   : access Cv_Arr;
                             Scale : Long_Float := 1.0;
                             Shift : Long_Float := 0.0);
   procedure CvCvtScale (Src   : access Cv_Arr;
                         Dst   : access Cv_Arr;
                         Scale : Long_Float := 1.0;
                         Shift : Long_Float := 0.0) renames CvConvertScale;
   procedure CvScale (Src   : access Cv_Arr;
                      Dst   : access Cv_Arr;
                      Scale : Long_Float := 1.0;
                      Shift : Long_Float := 0.0) renames CvConvertScale;
   procedure CvConvert (Src : Cv_Arr_P;
                        Dst : Cv_Arr_P);

   --     Converts input array elements to another 8-bit unsigned integer with
   --     optional linear transformation.
   procedure CvConvertScaleAbs (Src   : access Cv_Arr;
                                Dst   : access Cv_Arr;
                                Scale : Long_Float := 1.0;
                                Shift : Long_Float := 0.0);
   --     Converts input array elements to another 8-bit unsigned integer with
   --     optional linear transformation.
   procedure CvCvtScaleAbs (Src   : access Cv_Arr;
                            Dst   : access Cv_Arr;
                            Scale : Long_Float := 1.0;
                            Shift : Long_Float := 0.0) renames CvConvertScaleAbs;

   function CvCheckTermCriteria ( Criteria         : Cv_Term_Criteria;
                                 Default_Eps       : Long_Float;
                                 Default_Max_Iters : Integer ) return Cv_Term_Criteria;

   -----------------------------------------------------------------------------
   -- Arithmetic, logic and comparison operations
   -----------------------------------------------------------------------------
   --     Computes the per-element sum of two arrays.
   procedure CvAdd (Src1 : access Cv_Arr;
                    Src2 : access Cv_Arr;
                    Dst  : access Cv_Arr;
                    Mask : access Cv_Arr := null);

   --     Computes the sum of an array and a scalar.
   procedure CvAddS (Src   : access Cv_Arr;
                     Value : Cv_Scalar;
                     Dst   : access Cv_Arr;
                     Mask  : access Cv_Arr := null);

   --     Computes the per-element difference between two arrays.
   procedure CvSub (Src1 : access Cv_Arr;
                    Src2 : access Cv_Arr;
                    Dst  : access Cv_Arr;
                    Mask : access Cv_Arr := null);

   --     Computes the difference between an array and a scalar.
   procedure CvSubS (Src   : access Cv_Arr;
                     Value : Cv_Scalar;
                     Dst   : access Cv_Arr;
                     Mask  : access Cv_Arr := null);

   --     Computes the difference between a scalar and an array.
   procedure CvSubRS (Src   : access Cv_Arr;
                      Value : Cv_Scalar;
                      Dst   : access Cv_Arr;
                      Mask  : access Cv_Arr := null);

   --     Calculates the per-element product of two arrays.
   procedure CvMul (Src1  : access Cv_Arr;
                    Src2  : access Cv_Arr;
                    Dst   : access Cv_Arr;
                    Scale : Long_Float := 1.0);

   --     Performs per-element division of two arrays.
   procedure CvDiv (Src1  : access Cv_Arr;
                    Src2  : access Cv_Arr;
                    Dst   : access Cv_Arr;
                    Scale : Long_Float := 1.0);

   --     Calculates the sum of a scaled array and another array.
   procedure CvScaleAdd (Src1  : access Cv_Arr;
                         Scale : Cv_Scalar;
                         Src2  : access Cv_Arr;
                         Dst   : access Cv_Arr);
   procedure CvAXPY (Src1  : Cv_Arr_P;
                     Scale : Long_Float;
                     Src2  : Cv_Arr_P;
                     Dst   : Cv_Arr_P);

   --     Computes the weighted sum of two arrays.
   procedure CvAddWeighted (Src1  : access Cv_Arr;
                            Alpha : Long_Float;
                            Src2  : access Cv_Arr;
                            Beta  : Long_Float;
                            Gamma : Long_Float;
                            Dst   : access Cv_Arr);

   --     Calculates the dot product of two arrays in Euclidian metrics.
   function CvDotProduct (Src1 : access Cv_Arr;
                          Src2 : access Cv_Arr)
                          return Long_Float;

   --     Calculates per-element bit-wise conjunction of two arrays.
   procedure CvAnd (Src1 : access Cv_Arr;
                    Src2 : access Cv_Arr;
                    Dst  : access Cv_Arr;
                    Mask : access Cv_Arr := null);

   --     Calculates per-element bit-wise conjunction of an array and a scalar.
   procedure CvAndS (Src   : access Cv_Arr;
                     S1    : Long_Float;
                     S2    : Long_Float;
                     S3    : Long_Float;
                     S4    : Long_Float;
                     Dst   : access Cv_Arr;
                     Mask  : access Cv_Arr := null);

   --     Calculates per-element bit-wise disjunction of two arrays.
   procedure CvOr (Src1 : access Cv_Arr;
                   Src2 : access Cv_Arr;
                   Dst  : access Cv_Arr;
                   Mask : access Cv_Arr := null);

   --     Calculates a per-element bit-wise disjunction of an array and
   --     a scalar.
   procedure CvOrS (Src   : access Cv_Arr;
                    Value : Cv_Scalar;
                    Dst   : access Cv_Arr;
                    Mask  : access Cv_Arr := null);

   --     Performs per-element bit-wise exclusive or operation on two arrays.
   procedure CvXor (Src1 : access Cv_Arr;
                    Src2 : access Cv_Arr;
                    Dst  : access Cv_Arr;
                    Mask : access Cv_Arr := null);

   --     Performs per-element bit-wise exclusive or operation on an array
   --     and a scalar.
   procedure CvXorS (Src   : access Cv_Arr;
                     Value : Cv_Scalar;
                     Dst   : access Cv_Arr;
                     Mask  : access Cv_Arr := null);

   --     Performs per-element bit-wise inversion of array elements.
   procedure CvNot (Src : access Cv_Arr;
                    Dst : access Cv_Arr);

   --     Checks that array elements lie between the elements of two other
   --     arrays.
   procedure CvInRange (Src   : access Cv_Arr;
                        Lower : access Cv_Arr;
                        Upper : access Cv_Arr;
                        Dst   : access Cv_Arr);

   --     Checks that array elements lie between two scalars.
   procedure CvInRangeS (Src   : access Cv_Arr;
                         Lower : Cv_Scalar;
                         Upper : Cv_Scalar;
                         Dst   : access Cv_Arr);

   type Compare_Op is (Cv_Cmp_Eq, Cv_Cmp_Gt,
                       Cv_Cmp_Ge, Cv_Cmp_Lt,
                       Cv_Cmp_Le, Cv_Cmp_Ne);
   for Compare_Op use (Cv_Cmp_Eq => 0, Cv_Cmp_Gt => 1,
                       Cv_Cmp_Ge => 2, Cv_Cmp_Lt => 3,
                       Cv_Cmp_Le => 4, Cv_Cmp_Ne => 5);

   --     Performs per-element comparison of two arrays.
   procedure CvCmp (Src1   : access Cv_Arr;
                    Src2   : access Cv_Arr;
                    Dst    : access Cv_Arr;
                    Cmp_Op : Compare_Op);

   --     Performs per-element comparison of an array and a scalar.
   procedure CvCmpS (Src    : access Cv_Arr;
                     Value  : Long_Float;
                     Dst    : access Cv_Arr;
                     Cmp_Op : Compare_Op);

   --     Finds per-element minimum of two arrays.
   procedure CvMin (Src1 : access Cv_Arr;
                    Src2 : access Cv_Arr;
                    Dst  : access Cv_Arr);

   --     Finds per-element maximum of two arrays.
   procedure CvMax (Src1 : access Cv_Arr;
                    Src2 : access Cv_Arr;
                    Dst  : access Cv_Arr);

   --     Finds per-element minimum of an array and a scalar.
   procedure CvMinS (Src   : access Cv_Arr;
                     Value : Long_Float;
                     Dst   : access Cv_Arr);

   --     Finds per-element maximum of array and scalar.
   procedure CvMaxS (Src   : access Cv_Arr;
                     Value : Long_Float;
                     Dst   : access Cv_Arr);

   --     Calculates absolute difference between two arrays.
   procedure CvAbsDiff (Src1 : access Cv_Arr;
                        Src2 : access Cv_Arr;
                        Dst  : access Cv_Arr);

   --     Calculates absolute difference between an array and a scalar.
   procedure CvAbsDiffS (Src   : access Cv_Arr;
                         Dst   : access Cv_Arr;
                         S1    : Long_Float;
                         S2    : Long_Float;
                         S3    : Long_Float;
                         S4    : Long_Float);

   -----------------------------------------------------------------------------
   -- Math Operations
   -----------------------------------------------------------------------------
   --     Calculates the magnitude and/or angle of 2d vectors.
   procedure CvCartToPolar (X              : access Cv_Arr;
                            Y              : access Cv_Arr;
                            Magnitude      : access Cv_Arr;
                            Angle          : access Cv_Arr := null;
                            AngleInDegrees : Integer := 0);

   --     Calculates Cartesian coordinates of 2d vectors represented in
   --     polar form.
   procedure CvPolarToCart (Magnitude        : access Cv_Arr;
                            Angle            : access Cv_Arr;
                            X                : access Cv_Arr;
                            Y                : access Cv_Arr;
                            Angle_In_Degrees : Integer := 0);

   --     Raises every array element to a power.
   procedure CvPow (Src   : access Cv_Arr;
                    Dst   : access Cv_Arr;
                    Power : Long_Float);

   --     Calculates the exponent of every array element.
   procedure CvExp (Src : access Cv_Arr;
                    Dst : access Cv_Arr);

   --     Calculates the natural logarithm of every array elements absolute value.
   procedure CvLog (Src : access Cv_Arr;
                    Dst : access Cv_Arr);

   --     Calculates the angle of a 2D vector.
   function CvFastArctan (Y : Float;
                          X : Float)
                          return Float;

   --     Calculates the cubic root
   function CvCbrt (Value : Float)
                    return Float;

   CV_CHECK_RANGE : constant := 1;
   CV_CHECK_QUIET : constant := 2;

   function CvCheckArr (Arr     : Cv_Arr_P;
                        Flags   : Integer := 0;
                        MinVal  : Long_Float := 0.0;
                        Max_Val : Long_Float := 0.0) return Integer;
   function CvCheckArray (Arr     : Cv_Arr_P;
                          Flags   : Integer := 0;
                          MinVal  : Long_Float := 0.0;
                          Max_Val : Long_Float := 0.0) return Integer renames CvCheckArr;

   CV_RAND_UNI    : constant := 0;
   CV_RAND_NORMAL : constant := 1;

   --     Fills an array with random numbers and updates the RNG state.
   procedure CvRandArr (Rng       : Integer_64;
                        Arr       : access Cv_Arr;
                        Dist_Type : Integer;
                        Param1    : Cv_Scalar;
                        Param2    : Cv_Scalar);

   procedure CvRandShuffle (Mat        : Cv_Arr_P;
                            Rng        : access Cv_RNG;
                            IterFactor : Long_Float := 1.0);

   CV_SORT_EVERY_ROW : constant := 0;
   CV_SORT_EVERY_COLUMN : constant := 1;
   CV_SORT_ASCENDING : constant := 0;
   CV_SORT_DESCENDING : constant := 16;

   procedure CvSort (Src    : Cv_Arr_P;
                     Dst    : Cv_Arr_P := null;
                     Idxmat : Cv_Arr_P := null;
                     Flags  : Integer := 0);

   --     Finds the real roots of a cubic equation.
   procedure CvSolveCubic (Coeffs : access Cv_Arr;
                           Roots  : access Cv_Arr);


   -- /* Finds all real and complex roots of a polynomial equation */
   procedure CvSolvePoly (Coeffs  : Cv_Mat_P;
                          Roots2  : Cv_Mat_P;
                          Maxiter : Integer := 20;
                          Fig     : Integer := 100);

   -----------------------------------------------------------------------------
   -- Matrix Operations
   -----------------------------------------------------------------------------
   --     Calculates the cross product of two 3D vectors.
   procedure CvCrossProduct (Src1 : access Cv_Arr;
                             Src2 : access Cv_Arr;
                             Dst  : access Cv_Arr);

   procedure CvMatMulAdd (Src1  : access Cv_Arr;
                          Src2  : access Cv_Arr;
                          Src3  : access Cv_Arr;
                          Dst   : access Cv_Arr);
   pragma Inline (CvMatMulAdd);

   procedure CvMatMul (Src1 : access Cv_Arr;
                       Src2 : access Cv_Arr;
                       Dst  : access Cv_Arr);
   pragma Inline (CvMatMul);

   CV_GEMM_A_T  : constant Integer := 1;
   CV_GEMM_B_T  : constant Integer := 2;
   CV_GEMM_C_T  : constant Integer := 4;

   --     Performs generalized matrix multiplication.
   procedure CvGEMM (Src1  : access Cv_Arr;
                     Src2  : access Cv_Arr;
                     Alpha : Long_Float;
                     Src3  : access Cv_Arr;
                     Beta  : Long_Float;
                     Dst   : access Cv_Arr;
                     TABC  : Integer := 0);

   procedure CvMatMulAddEx (Src1  : access Cv_Arr;
                            Src2  : access Cv_Arr;
                            Src3  : access Cv_Arr;
                            Dst   : access Cv_Arr)
                            renames CvMatMulAdd;

   --     Performs matrix transformation of every array element.
   procedure CvTransform (Src      : access Cv_Arr;
                          Dst      : access Cv_Arr;
                          Transmat : access Cv_Mat;
                          Shiftvec : access Cv_Mat := null);

   procedure CvMatMulAddS (Src      : access Cv_Arr;
                           Dst      : access Cv_Arr;
                           Transmat : access Cv_Mat;
                           Shiftvec : access Cv_Mat := null) renames CvTransform;

   --     Performs perspective matrix transformation of a vector array.
   procedure CvPerspectiveTransform (Src : access Cv_Arr;
                                     Dst : access Cv_Arr;
                                     Mat : access Cv_Arr);

   --     Calculates the product of an array and a transposed array.
   procedure CvMulTransposed (Src       : access Cv_Arr;
                              Dst       : access Cv_Arr;
                              Order     : Integer;
                              Delta_Arr : access Cv_Arr := null;
                              Scale     : Long_Float := 1.0);

   --     Transposes a matrix.
   procedure CvTranspose (Src : access Cv_Arr;
                          Dst : access Cv_Arr);
   procedure CvT (Src : access Cv_Arr;
                  Dst : access Cv_Arr) renames CvTranspose;

   --/* Completes the symmetric matrix from the lower (LtoR=0) or from the upper (LtoR!=0) part */
   procedure CvCompleteSymm (Matrix : Cv_Mat_P;
                             LtoR   : Integer := 0);

   --     Flip a 2D array around vertical, horizontal or both axes.
   procedure CvFlip (Src      : access Cv_Arr;
                     Dst      : access Cv_Arr := null;
                     FlipMode : Integer := 0);

   --     Synonym for Flip.
   procedure CvMirror (Src      : access Cv_Arr;
                       Dst      : access Cv_Arr := null;
                       Flipmode : Integer := 0)
                       renames CvFlip;

   CV_SVD_MODIFY_A : constant Unsigned_32 := 1;
   CV_SVD_U_T   : constant Unsigned_32 := 2;
   CV_SVD_V_T   : constant Unsigned_32 := 4;

   --     Performs singular value decomposition of a real floating-point matrix.
   procedure CvSVD (A     : access Cv_Arr;
                    W     : access Cv_Arr;
                    U     : access Cv_Arr := null;
                    V     : access Cv_Arr := null;
                    Flags : Unsigned_32 := 0);

   --     Performs singular value back substitution.
   procedure CvSVBkSb (W     : access Cv_Arr;
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
   function CvInvert (Src    : access Cv_Arr;
                      Dst    : access Cv_Arr;
                      Method : Integer := CV_LU)
                      return Long_Float;
   function CvInv (Src    : access Cv_Arr;
                   Dst    : access Cv_Arr;
                   Method : Integer := CV_LU)
                   return Long_Float renames CvInvert;

   --     Solves a linear system or least-squares problem.
   function CvSolve (Src1   : access Cv_Arr;
                     Src2   : access Cv_Arr;
                     Dst    : access Cv_Arr;
                     Method : Integer := CV_LU)
                     return Integer;

   --     Returns the determinant of a matrix.
   function CvDet (Mat : access Cv_Arr)
                   return Long_Float;

   --     Returns the trace of a matrix.
   function CvTrace (Mat : access Cv_Arr) return Cv_Scalar;

   --     Computes eigenvalues and eigenvectors of a symmetric matrix.
   procedure CvEigenVV (Mat       : access Cv_Arr;
                        Evects    : access Cv_Arr;
                        Evals     : access Cv_Arr;
                        Eps       : Long_Float := 0.0;
                        Lowindex  : Integer := -1;
                        Highindex : Integer := -1);

   --     Initializes a scaled identity matrix.
   procedure CvSetIdentity (Mat   : access Cv_Arr;
                            Value : Cv_Scalar);

   -- /* Fills matrix with given range of numbers */
   function CvRange (Mat   : Cv_Arr_P;
                     Start : Long_Float;
                     Ende  : Long_Float) return Cv_Arr_P;

   CV_COVAR_SCRAMBLED : constant Unsigned_32 := 0;
   CV_COVAR_NORMAL    : constant Unsigned_32 := 1;
   CV_COVAR_USE_AVG   : constant Unsigned_32 := 2;
   CV_COVAR_SCALE     : constant Unsigned_32 := 4;
   CV_COVAR_ROWS      : constant Unsigned_32 := 8;
   CV_COVAR_COLS      : constant Unsigned_32 := 16;

   --     Calculates covariance matrix of a set of vectors.
   procedure CvCalcCovarMatrix (Vects  : access Cv_Arr;
                                Count  : Integer;
                                CovMat : access Cv_Arr;
                                Avg    : access Cv_Arr;
                                Flags  : Unsigned_32);

   CV_PCA_DATA_AS_ROW : constant := 0;
   CV_PCA_DATA_AS_COL : constant := 1;
   CV_PCA_USE_AVG : constant := 2;

   procedure CvCalcPCA (Data       : Cv_Arr_P;
                        Mean       : Cv_Arr_P;
                        Eigenvals  : Cv_Arr_P;
                        Eigenvects : Cv_Arr_P;
                        Flags      : Integer);

   procedure CvProjectPCA (Data       : Cv_Arr_P;
                           Mean       : Cv_Arr_P;
                           Eigenvects : Cv_Arr_P;
                           Result     : Cv_Arr_P);

   procedure CvBackProjectPCA (Proj       : Cv_Arr_P;
                               Mean       : Cv_Arr_P;
                               Eigenvects : Cv_Arr_P;
                               Result     : Cv_Arr_P);

   --     Calculates the Mahalonobis distance between two vectors.
   function CvMahalanobis (Vec1 : access Cv_Arr;
                           Vec2 : access Cv_Arr;
                           Mat  : access Cv_Arr)
                           return Long_Float;
   function CvMahalonobis (Vec1 : access Cv_Arr;
                           Vec2 : access Cv_Arr;
                           Mat  : access Cv_Arr)
                           return Long_Float renames CvMahalanobis;

   -----------------------------------------------------------------------------
   -- Array Statistics
   -----------------------------------------------------------------------------
   --     Adds up array elements.
   function CvSum (Arr : access Cv_Arr) return Cv_Scalar;

   --     Counts non-zero array elements.
   function CvCountNonZero (Arr : access Cv_Arr)
                            return Integer;

   --     Calculates average (mean) of array elements.
   function CvAvg (Arr  : access Cv_Arr;
                   Mask : access Cv_Arr := null)
                   return Cv_Scalar;

   --     Calculates average (mean) of array elements.
   procedure CvAvgSdv (Arr    : access Cv_Arr;
                       Mean   : access Cv_Scalar;
                       StdDev : access Cv_Scalar;
                       Mask   : access Cv_Arr := null);

   --     Finds global minimum and maximum in array or subarray.
   procedure CvMinMaxLoc (Arr     : access Cv_Arr;
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
   function CvNorm (Arr1      : access Cv_Arr;
                    Arr2      : access Cv_Arr := null;
                    Norm_Type : Unsigned_32 := CV_L2;
                    Mask      : access Cv_Arr := null)
                    return Long_Float;

   procedure CvNormalize (Src      : Cv_Arr_P;
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
   procedure CvReduce (Src : access Cv_Arr;
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
   procedure CvDFT (Src         : access Cv_Arr;
                    Dst         : access Cv_Arr;
                    Flags       : Unsigned_32;
                    NonzeroRows : Integer := 0);
   procedure CvFFT (Src         : access Cv_Arr;
                    Dst         : access Cv_Arr;
                    Flags       : Unsigned_32;
                    NonzeroRows : Integer := 0) renames CvDFT;

   --     Performs per-element multiplication of two Fourier spectrums.
   procedure CvMulSpectrums (Src1  : access Cv_Arr;
                             Src2  : access Cv_Arr;
                             Dst   : access Cv_Arr;
                             Flags : Unsigned_32);

   --     Returns optimal DFT size for a given vector size.
   function CvGetOptimalDFTSize (Size0 : Integer)
                                 return Integer;

   --     Performs a forward or inverse Discrete Cosine transform of a 1D or 2D
   --     floating - point array.
   procedure CvDCT (Src   : access Cv_Arr;
                    Dst   : access Cv_Arr;
                    Flags : Unsigned_32);

   -----------------------------------------------------------------------------
   -- Dynamic data structures
   -----------------------------------------------------------------------------
   --/  * Calculates Length of Sequence Slice (with Support of Negative Indices). *  /
   function CvSliceLength (Slice : Cv_Slice;
                           Seq   : Cv_Seq_P) return Integer;

   --Creates memory storage.
   function CvCreateMemStorage (Blocksize : Integer := 0) return Cv_Mem_Storage_P; -- 0 is about 64k...

   -- Creates child memory storage.
   function CvCreateChildMemStorage (Parent : Cv_Mem_Storage_P) return Cv_Mem_Storage_P;

   -- Releases memory storage.
   procedure CvReleaseMemStorage (Storage : access Cv_Mem_Storage_P);

   -- Clears memory storage.
   procedure CvClearMemStorage (Storage : Cv_Mem_Storage_P);

   -- Saves memory storage position.
   procedure CvSaveMemStoragePos (Storage : Cv_Mem_Storage_P;
                                  Pos     : Cv_Mem_Storage_Pos_P);

   -- Restores memory storage position.
   procedure CvRestoreMemStoragePos (Storage : Cv_Mem_Storage_P;
                                     Pos     : Cv_Mem_Storage_Pos_P);

   -- Allocates a memory buffer in a storage block.
   function CvMemStorageAlloc (Storage : Cv_Mem_Storage_P;
                               Size    : Interfaces.C.Size_T) return Cv_Void_P;

   -- Allocates a memory buffer in a storage block.
   function CvMemStorageAllocString (Storage : Cv_Mem_Storage_P;
                                     Ptr     : String;
                                     Len     : Integer := -1) return Cv_String;

   --Creates a sequence.
   function CvCreateSeq (SeqFlags   : Integer; -- No clue what this could be...
                         HeaderSize : Integer;
                         ElemSize   : Integer;
                         Storage    : Cv_Mem_Storage_P) return Cv_Seq_P;

   -- Sets up sequence block size.
   procedure CvSetSeqBlockSize (Seq        : Cv_Seq_P;
                                DeltaElems : Integer);

   -- Adds an element to the end of a sequence.
   function CvSeqPush (Seq     : Cv_Seq_P;
                       Element : Cv_Void_P := null) return access Character;

   -- Adds an element to the beginning of a sequence.
   function CvSeqPushFront (Seq     : Cv_Seq_P;
                            Element : Cv_Void_P := null) return access Character;

   -- Removes an element from the end of a sequence.
   procedure CvSeqPop (Seq     : Cv_Seq_P;
                       Element : Cv_Void_P := null);

   -- Removes an element from the beginning of a sequence.
   procedure CvSeqPopFront (Seq     : Cv_Seq_P;
                            Element : Cv_Void_P := null);

   -- Back and Front....
   CV_FRONT : constant := 1;
   CV_BACK : constant := 0;

   -- Pushes several elements to either end of a sequence.
   procedure CvSeqPushMulti (Seq      : Cv_Seq_P;
                             Elements : Cv_Void_P; -- maybe not
                             Count    : Integer;
                             InFron   : Integer := 0);

   -- Removes several elements from either end of a sequence.
   procedure CvSeqPopMulti (Seq      : Cv_Seq_P;
                            Elements : Cv_Void_P;
                            Count    : Integer;
                            InFront  : Integer := 0);

   -- Inserts an element in the middle of a sequence.
   function CvSeqInsert (Seq         : Cv_Seq_P;
                         BeforeIndex : Integer;
                         Element     : Cv_Void_P := null) return Cv_Void_P; --return access Character;

   --Removes an element from the middle of a sequence.
   procedure CvSeqRemove (Seq   : Cv_Seq_P;
                          Index : Integer);

   --Clears a sequence.
   procedure CvClearSeq (Seq : Cv_Seq_P);

   -- Returns a pointer to a sequence element according to its index.
   function CvGetSeqElem (Seq   : Cv_Seq_P;
                          Index : Integer) return Cv_Void_P; --return access Character;

   -- Returns the index of a specific sequence element.
   function CvSeqElemIdx (Seq     : Cv_Seq_P;
                          Element : Cv_Void_P;
                          Block   : access Cv_Seq_Block_P := null) return Integer;

   -- Initializes the process of writing data to a sequence.
   procedure CvStartAppendToSeq (Seq    : Cv_Seq_P;
                                 Writer : Cv_Seq_Writer_P);

   -- Creates a new sequence and initializes a writer for it.
   procedure CvStartWriteSeq (SeqFlags   : Integer;
                              HeaderSize : Integer;
                              ElemSize   : Integer;
                              Storage    : Cv_Mem_Storage_P;
                              Writer     : Cv_Seq_Writer_P);

   -- Finishes the process of writing a sequence.
   function CvEndWriteSeq ( Writer : Cv_Seq_Writer_P) return Cv_Seq_P;

   -- Updates sequence headers from the writer.
   procedure CvFlushSeqWriter (Writer : Cv_Seq_Writer_P);

   -- Initializes the process of sequential reading from a sequence
   procedure CvStartReadSeq (Seq       : Cv_Seq_P;
                             Reader    : Cv_Seq_Reader_P;
                             IsReverse : Integer := 0);

   -- Returns the current reader position.
   function CvGetSeqReaderPos (Reader : Cv_Seq_Reader_P) return Integer;

   --Moves the reader to the specified position.
   procedure CvSetSeqReaderPos (Reader     : Cv_Seq_Reader_P;
                                Index      : Integer;
                                IsRelative : Integer := 0);

   -- Copies a sequence to one continuous block of memory.
   function CvCvtSeqToArray (Seq      : Cv_Seq_P;
                             Elements : Cv_Void_P;
                             Slice    : Cv_Slice := CvSlice (0)) return Cv_Void_P;

   --Constructs a sequence header for an array.
   function CvMakeSeqHeaderForArray (SeqType    : Integer;
                                     HeaderSize : Integer;
                                     ElemSize   : Integer;
                                     Elements   : Cv_Void_P;
                                     Seq        : Cv_Seq_P;
                                     Block      : Cv_Seq_Block_P) return Cv_Seq_P;

   -- Makes a separate header for a sequence slice.
   function CvSeqSlice (Seq      : Cv_Seq_P;
                        Slice    : Cv_Slice;
                        Storage  : Cv_Mem_Storage_P := null;
                        CopyData : Integer := 0) return Cv_Seq_P;

   -- Creates a copy of a sequence.
   procedure CvCloneSeq (Seq     : Cv_Seq_P;
                         Storage : Cv_Mem_Storage_P := null);

   -- Removes a sequence slice.
   procedure CvSeqRemoveSlice (Seq   : Cv_Seq_P;
                               Slice : Cv_Slice_P);

   -- Inserts an array in the middle of a sequence.
   procedure CvSeqInsertSlice (Seq         : Cv_Seq_P;
                               BeforeIndex : Integer;
                               FromArr     : Cv_Arr_P);

   type Cv_Cmp_Func is access function (A        : Cv_Void_P;
                                        B        : Cv_Void_P;
                                        Userdata : Cv_Void_P)
                                        return Integer;

   -- Sorts sequence element using the specified comparison function.
   procedure CvSeqSort (Seq      : Cv_Seq_P;
                        Func     : Cv_Cmp_Func;
                        Userdata : Cv_Void_P := null);
   pragma Convention (C, Cv_Cmp_Func);

   -- Searches for an element in a sequence.
   function CvSeqSearch (Seq         : Cv_Seq_P;
                         Elem        : Cv_Void_P;
                         Func        : Cv_Cmp_Func;
                         IsSorted    : Integer;
                         ElemIdx     : access Integer;
                         Userdata    : Cv_Void_P := null) return Cv_Void_P; --return access Character;

   -- Reverses the order of sequence elements.
   procedure CvSeqInvert (Seq : Cv_Seq_P);

   --     Splits a sequence into equivalency classes.
   function CvSeqPartition (Seq      : access Cv_Seq;
                            Storage  : access Cv_Mem_Storage;
                            Labels   : access Cv_Seq_P;
                            Is_Equal : Cv_Cmp_Func;
                            Userdata : Cv_Void_P)
                            return Integer;

   procedure CvChangeSeqBlock (Reader    : Cv_Void_P;
                               Direction : Integer) renames Core.CvChangeSeqBlock;
   procedure CvCreateSeqBlock (Writer : Cv_Seq_Writer_P) renames Core.CvCreateSeqBlock;

   -- Creates an empty set.
   function CvCreateSet ( SetFlags  : Integer;
                         HeaderSize : Integer;
                         ElemSize   : Integer;
                         Storage    : Cv_Mem_Storage_P) return Cv_Set_P;

   -- Occupies a node in the set.
   function CvSetAdd (SetHeader    : Cv_Set_P;
                      Elem         : Cv_Set_Elem_P := null;
                      InsertedElem : access Cv_Set_Elem_P := null) return Integer;

   -- Adds an element to a set (fast variant).
   function CvSetNew (SetHeader : Cv_Set_P) return Cv_Set_Elem_P;

   -- Removes a set element based on its pointer.
   procedure CvSetRemoveByPtr (SetHeader : Cv_Set_P;
                               Elem      : Cv_Void_P);

   -- Removes an element from a set.
   procedure CvSetRemove (SetHEader : Cv_Set_P;
                          Index     : Integer);

   -- Finds a set element by its index.
   function CvGetSetElem (SetHeader : Cv_Set_P;
                          Index     : Integer) return Cv_Set_Elem_P;

   -- Clears a set.
   procedure CvClearSet (SetHeader : Cv_Set_P);

   -- Creates an empty graph.
   function CvCreateGraph (GraphFlags : Integer;
                           HeaderSize : Integer;
                           VtxSize    : Integer;
                           EdgeSize   : Integer;
                           Storage    : Cv_Mem_Storage_P) return Cv_Graph_P;

   -- Adds a vertex to a graph.
   function CvGraphAddVtx (Graph       : Cv_Graph_P;
                           Vtx         : Cv_Graph_Vtx_P := null;
                           InsertedVtx : access Cv_Graph_Vtx_P := null) return Integer;

   -- Removes a vertex from a graph.
   function CvGraphRemoveVtx (Graph : Cv_Graph_P;
                              Index : Integer) return Integer;

   --Removes a vertex from a graph by using its pointer.
   function CvGraphRemoveVtxByPtr (Graph : Cv_Graph_P;
                                   Vtx   : Cv_Graph_Vtx_P) return Integer;

   -- Adds an edge to a graph.
   function CvGraphAddEdge (Graph        : Cv_Graph_P;
                            StartIdx     : Integer;
                            EndIdx       : Integer;
                            Edge         : Cv_Graph_Edge_P := null;
                            InsertedEdge : access Cv_Graph_Edge_P := null) return Integer;

   -- Adds an edge to a graph by using its pointer.
   function CvGraphAddEdgeByPtr (Graph        : Cv_Graph_P;
                                 StartVtx     : Cv_Graph_Vtx_P;
                                 EndVtx       : Cv_Graph_Vtx_P;
                                 Edge         : Cv_Graph_Edge_P := null;
                                 InsertedEdge : access Cv_Graph_Edge_P := null) return Integer;

   -- Removes an edge from a graph.
   procedure CvGraphRemoveEdge (Graph    : Cv_Graph_P;
                                StartIdx : Integer;
                                EndIdx   : Integer);

   --Removes an edge from a graph by using its pointer.
   procedure CvGraphRemoveEdgeByPtr (Graph    : Cv_Graph_P;
                                     StartVtx : Cv_Graph_Vtx_P;
                                     EndVtx   : Cv_Graph_Vtx_P);

   -- Finds an edge in a graph.
   function CvFindGraphEdge (Graph    : Cv_Graph_P;
                             StartIdx : Integer;
                             EndIdx   : Integer) return Cv_Graph_Edge_P;
   function CvGraphFindEdge (Graph    : Cv_Graph_P;
                             StartIdx : Integer;
                             EndIdx   : Integer) return Cv_Graph_Edge_P renames CvFindGraphEdge;

   -- Finds an edge in a graph by using its pointer.
   function CvFindGraphEdgeByPtr (Graph    : Cv_Graph_P;
                                  StartVtx : Cv_Graph_Vtx_P;
                                  EndVtx   : Cv_Graph_Vtx_P) return Cv_Graph_Edge_P;
   function CvGraphFindEdgeByPtr (Graph    : Cv_Graph_P;
                                  StartVtx : Cv_Graph_Vtx_P;
                                  EndVtx   : Cv_Graph_Vtx_P) return Cv_Graph_Edge_P renames CvFindGraphEdgeByPtr;

   --Clears a graph.
   procedure CvClearGraph (Graph : Cv_Graph_P);

   -- Counts the number of edges indicent to the vertex.
   function CvGraphVtxDegree (Graph  : Cv_Graph_P;
                              Vtxldx : Integer) return Integer;

   -- Finds an edge in a graph.
   function CvGraphVtxDegreeByPtr (Graph : Cv_Graph_P;
                                   Vtx   : Cv_Graph_Vtx_P) return Integer;

   -- Finds a graph vertex by using its index.
   function CvGetGraphVtx (Graph  : Cv_Graph_P;
                           VtxIdx : Integer) return Cv_Graph_Vtx_P;

   -- Returns the index of a graph vertex.
   function CvGraphVtxIdx (Graph : Cv_Graph_P;
                           Vtx   : Cv_Graph_Vtx_P) return Integer;

   -- Returns the index of a graph edge.
   function CvGraphEdgeIdx (Graph : Cv_Graph_P;
                            Edge  : Cv_Graph_Edge_P) return Integer;

   --#define cvGraphGetVtxCount( graph ) ((graph)->active_count)
   function CvGraphGetVtxCount (Graph : Cv_Graph_P) return Integer;
   --#define cvGraphGetEdgeCount( graph ) ((graph)->edges->active_count)
   function CvGraphGetEdgeCount (Graph : Cv_Graph_P) return Integer;

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
   function CvCreateGraphScanner (Graph : Cv_Graph_P;
                                  Vtx   : Cv_Graph_Vtx_P := null; --null = start from beginning
                                  Mask  : Integer := CV_GRAPH_ALL_ITEMS) return Cv_Graph_Scanner_P;

   -- Completes the graph traversal procedure.
   procedure CvReleaseGraphScanner (Scanner : access Cv_Graph_Scanner_P);

   -- Executes one or more steps of the graph traversal procedure.
   function CvNextGraphItem (Scanner : Cv_Graph_Scanner_P) return Integer;

   -- Clones a graph.
   procedure CvCloneGraph (Graph   : Cv_Graph_P;
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
   procedure CvLine (Img       : Cv_Arr_P;
                     Pt1       : Cv_Point;
                     Pt2       : Cv_Point;
                     Color     : Cv_Scalar;
                     Thickness : Integer := 1;
                     Line_Type : Integer := 8;
                     Shift     : Integer := 0);

   -- Draws a simple, thick, or filled rectangle.
   procedure CvRectangle (Img       : Cv_Arr_P;
                          Pt1       : Cv_Point;
                          Pt2       : Cv_Point;
                          Color     : Cv_Scalar;
                          Thickness : Integer :=  1;
                          LineType  : Integer := 8;
                          Shift     : Integer := 0);

   -- /* Draws a rectangle specified by a CvRect structure */
   procedure CvRectangleR (Img       : Cv_Arr_P;
                           R         : Cv_Rect;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           LineType  : Integer := 8;
                           Shift     : Integer := 0);

   -- Draws a circle.
   procedure CvCircle (Img       : Cv_Arr_P;
                       Center    : Cv_Point;
                       Radius    : Integer;
                       Color     : Cv_Scalar;
                       Thickness : Integer := 1;
                       LineType  : Integer := 8;
                       Shift     : Integer := 0);

   -- Draws a simple or thick elliptic arc or an fills ellipse sector.
   procedure CvEllipse (Img        : Cv_Arr_P;
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
   procedure CvEllipseBox (Img       : Cv_Arr_P;
                           Box       : Cv_Box_2D;
                           Color     : Cv_Scalar;
                           Thickness : Integer;
                           LineType  : Integer := 8;
                           Shift     : Integer := 0);

   -- Fills a convex polygon.
   procedure CvFillConvexPoly (Img      : Cv_Arr_P;
                               Pts      : Cv_Point_Array;
                               Npts     : Integer;
                               Color    : Cv_Scalar;
                               LineType : Integer := 8;
                               Shift    : Integer := 0);

   -- Fills a polygon's interior.
   procedure CvFillPoly (Img      : Cv_Arr_P;
                         Pts      : Cv_Point_Pointer_Array;
                         Npts     : Cv_32U_Array;
                         Contours : Integer;
                         Color    : Cv_Scalar;
                         LineType : Integer := 8;
                         Shift    : Integer := 0);

   -- Draws simple or thick polygons.
   procedure CvPolyLine (Img       : Cv_Arr_P;
                         Pts       : Cv_Point_Pointer_Array;
                         Npts      : Cv_32U_Array;
                         Contours  : Integer;
                         IsClosed  : Integer;
                         Color     : Cv_Scalar;
                         Thickness : Integer := 1;
                         LineTyoe  : Integer := 8;
                         Shift     : Integer := 0);

   procedure CvDrawRect (Img       : Cv_Arr_P;
                         Pt1       : Cv_Point;
                         Pt2       : Cv_Point;
                         Color     : Cv_Scalar;
                         Thickness : Integer :=  1;
                         LineType  : Integer := 8;
                         Shift     : Integer := 0) renames CvRectangle;

   procedure CvDrawLine (Img       : Cv_Arr_P;
                         Pt1       : Cv_Point;
                         Pt2       : Cv_Point;
                         Color     : Cv_Scalar;
                         Thickness : Integer := 1;
                         Line_Type : Integer := 8;
                         Shift     : Integer := 0) renames CvLine;

   procedure CvDrawCircle (Img       : Cv_Arr_P;
                           Center    : Cv_Point;
                           Radius    : Integer;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           LineType  : Integer := 8;
                           Shift     : Integer := 0) renames CvCircle;

   procedure CvDrawEllipse (Img        : Cv_Arr_P;
                            Center     : Cv_Point;
                            Axes       : Cv_Size;
                            Angle      : Long_Float;
                            StartAngle : Long_Float;
                            EndAngle   : Long_Float;
                            Color      : Cv_Scalar;
                            Thickness  : Integer := 1;
                            LineType   : Integer := 8;
                            Shift      : Integer := 0) renames CvEllipse;

   procedure CvDrawPolyLine (Img       : Cv_Arr_P;
                             Pts       : Cv_Point_Pointer_Array;
                             Npts      : Cv_32U_Array;
                             Contours  : Integer;
                             IsClosed  : Integer;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             LineTyoe  : Integer := 8;
                             Shift     : Integer := 0) renames CvPolyLine;

   -- Clips the line against the image rectangle.
   function CvClipLine (ImgSize : Cv_Size;
                        Pt1     : access Cv_Point;
                        Pt2     : access Cv_Point) return Integer;

   -- Initializes the line iterator.
   function CvInitLineIterator (Image        : Cv_Arr_P;
                                Pt1          : Cv_Point;
                                Pt2          : Cv_Point;
                                LineIterator : access Cv_Line_Iterator;
                                Connectivity : Integer := 8;
                                LeftToRight  : Integer := 0) return Integer;

   procedure CV_NEXT_LINE_POINT (LineIterator : Cv_Line_Iterator_P);

   -- Fonts
   type Cv_Font_Face is (CV_FONT_HERSHEY_SIMPLEX,
                         CV_FONT_HERSHEY_PLAIN,
                         CV_FONT_HERSHEY_DUPLEX,
                         CV_FONT_HERSHEY_COMPLEX,
                         CV_FONT_HERSHEY_TRIPLEX,
                         CV_FONT_HERSHEY_COMPLEX_SMALL,
                         CV_FONT_HERSHEY_SCRIPT_SIMPLEX,
                         CV_FONT_HERSHEY_SCRIPT_COMPLEX);
   for Cv_Font_Face use (CV_FONT_HERSHEY_SIMPLEX        => 0,
                         CV_FONT_HERSHEY_PLAIN          => 1,
                         CV_FONT_HERSHEY_DUPLEX         => 2,
                         CV_FONT_HERSHEY_COMPLEX        => 3,
                         CV_FONT_HERSHEY_TRIPLEX        => 4,
                         CV_FONT_HERSHEY_COMPLEX_SMALL  => 5,
                         CV_FONT_HERSHEY_SCRIPT_SIMPLEX => 6,
                         CV_FONT_HERSHEY_SCRIPT_COMPLEX => 7);

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
   procedure CvInitFont (Font      : access Cv_Font;
                         FontFace  : Cv_Font_Face;
                         Hscale    : Long_Float;
                         Vscale    : Long_Float;
                         Shear     : Long_Float := 0.0;
                         Thickness : Integer := 1;
                         LineType  : Integer := 8);

   function CvFont (Scale     : Long_Float;
                    Thickness : Integer := 1) return Cv_Font;

   procedure CvPutText (Img   : Cv_Arr_P;
                        Text  : String;
                        Org   : Cv_Point;
                        Font  : access Cv_Font;
                        Color : Cv_Scalar);

   -- Retrieves the width and height of a text string.
   procedure CvGetTextSize (TextString : String;
                            Font       : Cv_Font;
                            TextSize   : access Cv_Size;
                            Baseline   : access Integer);

   --     /* Unpacks color value, if arrtype is CV_8UC?, <color> is treated as
   --     packed color value, otherwise the first channels (depending on arrtype)
   --     of destination scalar are set to the same value = <color> */
   function CvColorToScalar (Packed_Color : Long_Float;
                             Arrtype      : Integer) return Cv_Scalar;

   --     /* Returns the polygon points which make up the given ellipse.  The ellipse is define by
   --     the box of size 'axes' rotated 'angle' around the 'center'.  A partial sweep
   --     of the ellipse arc can be done by spcifying arc_start and arc_end to be something
   --     other than 0 and 360, respectively.  The input array 'pts' must be large enough to
   --     hold the result.  The total number of points stored into 'pts' is returned by this
   --     function. */
   function CvEllipse2Poly (Center      : Cv_Point;
                            Axes        : Cv_Size;
                            Angle       : Integer;
                            ArcStart    : Integer;
                            ArcEnd      : Integer;
                            Pts         : Cv_Point_Array;
                            DeltaVal    : Integer) return Integer;

   -- Draws contour outlines or interiors in an image.
   procedure CvDrawContours (Img           : Cv_Arr_P;
                             Contour       : access Cv_Seq;
                             ExternalColor : Cv_Scalar;
                             HoleColor     : Cv_Scalar;
                             MaxLevel      : Integer;
                             Thickness     : Integer := 1;
                             LineType      : Integer := 8);

   --     Performs a look-up table transform of an array.
   procedure CvLUT (Src : access Cv_Arr;
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
   procedure CvInitTreeNodeIterator (TreeIterator : Cv_Tree_Node_Iterator_P;
                                     First        : Cv_Void_P;
                                     MaxLevel     : Integer);

   -- Returns the currently observed node and moves the iterator toward the next node.
   function CvNextTreeNode (TreeIterator : Cv_Tree_Node_Iterator_P) return Cv_Void_P;

   -- Returns the currently observed node and moves the iterator toward the previous node.
   function CvPrevTreeNode (TreeIterator : Cv_Tree_Node_Iterator_P) return Cv_Void_P;

   --Adds a new node to a tree.
   procedure CvInsertNodeIntoTree (Node   : Cv_Void_P;
                                   Parent : Cv_Void_P;
                                   Fram   : Cv_Void_P);

   --/* Removes contour from tree (together with the contour children). */
   procedure CvRemoveNodeFromTree (Node  : Cv_Void_P;
                                   Frame : Cv_Void_P);

   --Gathers all node pointers to a single sequence.
   function CvTreeToNodeSeq (First      : Cv_Void_P;
                             HeaderSize : Integer;
                             Storage    : Cv_Mem_Storage_P) return Cv_Seq_P;

   CV_KMEANS_USE_INITIAL_LABELS : constant := 1;

   function CvKMeans2 (Samples     : access Cv_Arr;
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
   function CvRegisterModule (Module_Info : access Cv_Module_Info)
                              return Integer;

   --     Switches between optimized/non-optimized modes.
   function CvUseOptimized (Onoff : Integer)
                            return Integer;

   --     Retrieves information about registered module(s) and plugins.
   procedure CvGetModuleInfo (Module_Name          : Interfaces.C.Strings.Chars_Ptr;
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
   procedure CvSetMemoryManager (Alloc_Func : Cv_Alloc_Func := null;
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
   procedure CvSetIPLAllocators (Create_Header : Cv_Ipl_Create_Image_Header;
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
   function CvOpenFileStorage (Fílename   : Interfaces.C.Strings.Chars_Ptr;
                               Memstorage : access Cv_Mem_Storage;
                               Flags      : Unsigned_32)
                               return Cv_File_Storage_P;

   --     Releases file storage.
   procedure CvReleaseFileStorage (Fs : access Cv_File_Storage_P);

   --/* returns attribute value or 0 (NULL) if there is no such attribute */
   function CvAttrValue (Attr       : Cv_Attr_List_P;
                         Attr_Name  : Interfaces.C.Strings.Chars_Ptr) return Interfaces.C.Strings.Chars_Ptr;

   --     Starts writing a new structure.
   procedure CvStartWriteStruct (Fs           : access Cv_File_Storage;
                                 Name         : Interfaces.C.Strings.Chars_Ptr;
                                 Struct_Flags : Unsigned_32;
                                 Type_Name    : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr;
                                 Attributes   : Cv_Attr_List := CvAttrList);

   --     Ends the writing of a structure.
   procedure CvEndWriteStruct (Fs : access Cv_File_Storage);

   --     Writes an integer value.
   procedure CvWriteInt (Fs    : access Cv_File_Storage;
                         Name  : Interfaces.C.Strings.Chars_Ptr;
                         Value : Integer);

   --     Writes a floating-point value.
   procedure CvWriteReal (Fs    : access Cv_File_Storage;
                          Name  : Interfaces.C.Strings.Chars_Ptr;
                          Value : Long_Float);

   --     Writes a text string.
   procedure CvWriteString (Fs    : access Cv_File_Storage;
                            Name  : Interfaces.C.Strings.Chars_Ptr;
                            Str   : Interfaces.C.Strings.Chars_Ptr;
                            Quote : Integer := 0);

   --     Writes a comment.
   procedure CvWriteComment (Fs          : access Cv_File_Storage;
                             Comment     : Interfaces.C.Strings.Chars_Ptr;
                             Eol_Comment : Integer);

   --     Writes a user object.
   procedure CvWrite (Fs         : access Cv_File_Storage;
                      Name       : Interfaces.C.Strings.Chars_Ptr;
                      Ptr        : Cv_Void_P;
                      Attributes : Cv_Attr_List := CvAttrList);

   --     Starts the next stream.
   procedure CvStartNextStream (Fs : access Cv_File_Storage);

   --     Writes multiple numbers.
   procedure CvWriteRawData (Fs  : access Cv_File_Storage;
                             Src : Cv_Void_P;
                             Len : Integer;
                             Dt  : Interfaces.C.Strings.Chars_Ptr);

   --     Returns a unique pointer for a given name.
   function CvGetHashedKey (Fs             : access Cv_File_Storage;
                            Name           : String_C;
                            Len            : Integer := -1;
                            Create_Missing : Integer := 0)
                            return Cv_String_Hash_Node_P;

   --     Retrieves one of the top-level nodes of the file storage.
   function CvGetRootFileNode (Fs           : access Cv_File_Storage;
                               Stream_Index : Integer := 0)
                               return Cv_File_Node_P;

   --     Finds a node in a map or file storage.
   function CvGetFileNode (Fs             : access Cv_File_Storage;
                           Map            : access Cv_File_Node;
                           Key            : access Cv_String_Hash_Node;
                           Create_Missing : Integer := 0)
                           return Cv_File_Node_P;

   --     Finds a node in a map or file storage.
   function CvGetFileNodeByName (Fs   : access Cv_File_Storage;
                                 Map  : access Cv_File_Node;
                                 Name : Interfaces.C.Strings.Chars_Ptr)
                                 return Cv_File_Node_P;

   --     Finds a file node and returns its value.
   function CvReadInt (Node          : access Cv_File_Node;
                       Default_Value : Integer := 0)
                       return Integer;

   function CvReadIntByName (Fs            : Cv_File_Storage_P;
                             Map           : Cv_File_Node_P ;
                             Name          : Interfaces.C.Strings.Chars_Ptr;
                             Default_Value : Integer := 0) return Integer;

   --     Retrieves a floating-point value from a file node.
   function CvReadReal (Node          : access Cv_File_Node;
                        Default_Value : Long_Float := 0.0)
                        return Long_Float;

   --     Finds a file node and returns its value.
   function CvReadRealByName (Fs            : access Cv_File_Storage;
                              Map           : access Cv_File_Node;
                              Name          : Interfaces.C.Strings.Chars_Ptr;
                              Default_Value : Long_Float := 0.0)
                              return Long_Float;

   --     Retrieves a text string from a file node.
   function CvReadString (Node          : access Cv_File_Node;
                          Default_Value : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr)
                          return Interfaces.C.Strings.Chars_Ptr;

   --     Finds a file node by its name and returns its value.
   function CvReadStringByName (Fs            : access Cv_File_Storage;
                                Map           : access Cv_File_Node;
                                Name          : Interfaces.C.Strings.Chars_Ptr;
                                Default_Value : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr)
                                return Interfaces.C.Strings.Chars_Ptr;

   --     Decodes an object and returns a pointer to it.
   function CvRead (Fs         : access Cv_File_Storage;
                    Node       : access Cv_File_Node;
                    Attributes : access Cv_Attr_List := null)
                    return Cv_Void_P;

   --     Finds an object by name and decodes it.
   function CvReadByName (Fs         : access Cv_File_Storage;
                          Map        : access Cv_File_Node;
                          Name       : Interfaces.C.Strings.Chars_Ptr;
                          Attributes : access Cv_Attr_List := null)
                          return Cv_Void_P;

   --     Initializes the file node sequence reader.
   procedure CvStartReadRawData (Fs     : access Cv_File_Storage;
                                 Src    : access Cv_File_Node;
                                 Reader : access Cv_Seq_Reader);

   --     Initializes file node sequence reader.
   procedure CvReadRawDataSlice (Fs     : access Cv_File_Storage;
                                 Reader : access Cv_Seq_Reader;
                                 Count  : Integer;
                                 Dst    : Cv_Void_P;
                                 Dt     : Interfaces.C.Strings.Chars_Ptr);

   --     Reads multiple numbers.
   procedure CvReadRawData (Fs  : access Cv_File_Storage;
                            Src : access Cv_File_Node;
                            Dst : Cv_Void_P;
                            Dt  : Interfaces.C.Strings.Chars_Ptr);

   --     Writes a file node to another file storage.
   procedure CvWriteFileNode (Fs            : access Cv_File_Storage;
                              New_Node_Name : Interfaces.C.Strings.Chars_Ptr;
                              Node          : access Cv_File_Node;
                              Embed         : Integer);

   --     Returns the name of a file node.
   function CvGetFileNodeName (Node : access Cv_File_Node)
                               return Interfaces.C.Strings.Chars_Ptr;

   -----------------------------------------------------------------------------
   -- Adding own types
   -----------------------------------------------------------------------------
   --     Registers a new type.
   procedure CvRegisterType (Info : access Cv_Type_Info);

   --     Unregisters the type.
   procedure CvUnregisterType (Type_Name : Interfaces.C.Strings.Chars_Ptr);

   --     Returns the beginning of a type list.
   function CvFirstType return Cv_Type_Info_P;

   --     Finds a type by its name.
   function CvFindType (Type_Name : Interfaces.C.Strings.Chars_Ptr)
                        return Cv_Type_Info_P;

   --     Returns the type of an object.
   function CvTypeOf (Struct_Ptr : Cv_Void_P)
                      return Cv_Type_Info_P;

   --     Releases an object.
   procedure CvRelease (Struct_Ptr : access Cv_Void_P);

   --     Makes a clone of an object.
   function CvClone (Struct_Ptr : Cv_Void_P)
                     return Cv_Void_P;

   --     Saves an object to a file.
   procedure CvSave (Filename   : Interfaces.C.Strings.Chars_Ptr;
                     Struct_Ptr : Cv_Void_P;
                     Name       : Interfaces.C.Strings.Chars_Ptr;
                     Comment    : Interfaces.C.Strings.Chars_Ptr;
                     Attributes : Cv_Attr_List := CvAttrList);

   --     Loads an object from a file.
   function CvLoad (Filename  : String_C;
                    Storage   : access Cv_Mem_Storage := null;
                    Name      : String_C := Null_String_C;
                    Real_Name : access Interfaces.C.Strings.Chars_Ptr := null)
                    return Cv_Void_P;

   -----------------------------------------------------------------------------
   -- Measuring Execution Time
   -----------------------------------------------------------------------------
   --     Returns the number of ticks.
   function CvGetTickCount return Integer_64;

   --     Returns the number of ticks per microsecond.
   function CvGetTickFrequency return Long_Float;

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

   function CvCheckHardwareSupport (Feature : Integer) return Integer;

   -----------------------------------------------------------------------------
   -- Multi-Threading
   -----------------------------------------------------------------------------
   --/* retrieve/set the number of threads used in OpenMP implementations */
   function CvGetNumThreads return Integer;
   procedure CvSetNumThreads ( Threads : Integer := 0);

   --/* get index of the thread being executed */
   function CvGetThreadNum return Integer;
   -----------------------------------------------------------------------------
   -- Error Handling
   -----------------------------------------------------------------------------



   --     Returns the current error status.
   function CvGetErrStatus return Integer;

   --     Sets the error status.
   procedure CvSetErrStatus (Status : Integer);

   CV_ErrModeLeaf   : constant := 0;
   CV_ErrModeParent : constant := 1;
   CV_ErrModeSilent : constant := 2;

   --     Returns the current error mode.
   function CvGetErrMode return Integer;

   --     Sets the error mode.
   function CvSetErrMode (Mode : Integer)
                          return Integer;

   --     Raises an error.
   function CvError (Status    : Integer;
                     Func_Name : Interfaces.C.Strings.Chars_Ptr;
                     Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                     Filename  : Interfaces.C.Strings.Chars_Ptr;
                     Line      : Integer)
                     return Integer;

   --     Returns textual description of an error status code.
   function CvErrorStr (Status : Integer)
                        return Interfaces.C.Strings.Chars_Ptr;

   --/* Retrieves detailed information about the last error occured */
   -- This doesn't work anyway so why do I care! it returns 0 yay!
   function CvGetErrInfo (Errcode_Desc : access Interfaces.C.Strings.Chars_Ptr;
                          Desciption   : access Interfaces.C.Strings.Chars_Ptr;
                          Filename     : access Interfaces.C.Strings.Chars_Ptr;
                          Line         : access Integer) return Integer;

   --   /* Maps IPP error codes to the counterparts from OpenCV */
   function CvErrorFromIppStatus (IppStatus : Integer) return Integer;

   type Cv_Error_Callback is access function (Status    : Integer;
                                              Func_Name : Interfaces.C.Strings.Chars_Ptr;
                                              Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                                              File_Name : Interfaces.C.Strings.Chars_Ptr;
                                              Line      : Integer)
                                              return Integer;
   pragma Convention (C, Cv_Error_Callback);

   --     Sets a new error handler.
   function CvRedirectError (Error_Handler : Cv_Error_Callback;
                             Userdata      : Cv_Void_P;
                             Prev_Userdata : access Cv_Void_P)
                             return Cv_Error_Callback;

   --     Provide standard error handling.
   function CvNulDevReport (Status    : Integer;
                            Func_Name : Interfaces.C.Strings.Chars_Ptr;
                            Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                            File_Name : Interfaces.C.Strings.Chars_Ptr;
                            Line      : Integer;
                            Userdata  : Cv_Void_P)
                            return Integer;

   function CvStdErrReport (Status    : Integer;
                            Func_Name : Interfaces.C.Strings.Chars_Ptr;
                            Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                            File_Name : Interfaces.C.Strings.Chars_Ptr;
                            Line      : Integer;
                            Userdata  : Cv_Void_P) return Integer;

   function CvGuiBoxReport (Status    : Integer;
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
                       Line    : Integer := GNAT.Source_Info.Line) renames OPENCV_ERROR;

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
   procedure CvFree_Wrapper (Ptr : access Cv_Void_P);
   -- Wrapper due to String
   function WCvMemStorageAllocString (Storage : Cv_Mem_Storage_P;
                                      Ptr     : String_C;
                                      Len     : Integer := -1) return Cv_String;

   procedure WcvPutText (Img   : Cv_Arr_P;
                         Text  : String_C;
                         Org   : Cv_Point;
                         Font  : access Cv_Font;
                         Color : Cv_Scalar);

   procedure WCvGetTextSize (TextString : String_C;
                             Font       : Cv_Font;
                             TextSize   : access Cv_Size;
                             Baseline   : access Integer);

   pragma Import (C, CvAlloc, "cvAlloc");
   pragma Import (C, CvFree_Wrapper, "cvFree_");
   pragma Import (C, CvCreateImageHeader, "cvCreateImageHeader");
   pragma Import (C, CvInitImageHeader, "cvInitImageHeader");
   pragma Import (C, CvCreateImage, "cvCreateImage");
   pragma Import (C, CvReleaseImageHeader, "cvReleaseImageHeader");
   pragma Import (C, CvReleaseImage, "cvReleaseImage");
   pragma Import (C, CvCloneImage, "cvCloneImage");
   pragma Import (C, CvSetImageCOI, "cvSetImageCOI");
   pragma Import (C, CvGetImageCOI, "cvGetImageCOI");
   pragma Import (C, CvSetImageROI, "cvSetImageROI");
   pragma Import (C, CvResetImageROI, "cvResetImageROI");
   pragma Import (C, CvGetImageROI, "cvGetImageROI");
   pragma Import (C, CvCreateMatHeader, "cvCreateMatHeader");
   pragma Import (C, CvInitMatHeader, "cvInitMatHeader");
   pragma Import (C, CvCreateMat, "cvCreateMat");
   pragma Import (C, CvReleaseMat, "cvReleaseMat");
   pragma Import (C, CvDecRefData, "cvDecRefData");
   pragma Import (C, CvIncRefData, "cvIncRefData");
   pragma Import (C, CvCloneMat, "cvCloneMat");
   pragma Import (C, CvGetSubRect, "cvGetSubRect");
   pragma Import (C, CvGetRows, "cvGetRows");
   pragma Import (C, CvGetRow, "cvGetRow");
   pragma Import (C, CvGetCols, "cvGetCols");
   pragma Import (C, CvGetCol, "cvGetCol");
   pragma Import (C, CvGetDiag, "cvGetDiag");
   pragma Import (C, CvScalarToRawData, "cvScalarToRawData");
   pragma Import (C, CvRawDataToScalar, "cvRawDataToScalar");
   pragma Import (C, CvCreateMatNDHeader, "cvCreateMatNDHeader");
   pragma Import (C, CvCreateMatND, "cvCreateMatND");
   pragma Import (C, CvInitMatNDHeader, "cvInitMatNDHeader");
   pragma Import (C, CvReleaseMatND, "cvReleaseMatND");
   pragma Import (C, CvCloneMatND, "cvCloneMatND");
   pragma Import (C, CvCreateSparseMat, "cvCreateSparseMat");
   pragma Import (C, CvReleaseSparseMat, "cvReleaseSparseMat");
   pragma Import (C, CvCloneSparseMat, "cvCloneSparseMat");
   pragma Import (C, CvInitSparseMatIterator, "cvInitSparseMatIterator");
   pragma Import (C, CvGetNextSparseNode, "cvGetNextSparseNode");
   pragma Import (C, CvInitNArrayIterator, "cvInitNArrayIterator");
   pragma Import (C, CvNextNArraySlice, "cvNextNArraySlice");
   pragma Import (C, CvGetElemType, "cvGetElemType");
   pragma Import (C, CvGetDims, "cvGetDims");
   pragma Import (C, CvGetDimSize, "cvGetDimSize");
   pragma Import (C, CvSetReal1D, "cvSetReal1D");
   pragma Import (C, CvSetReal2D, "cvSetReal2D");
   pragma Import (C, CvSetReal3D, "cvSetReal3D");
   pragma Import (C, CvSetRealND, "cvSetRealND");
   pragma Import (C, CvPtr1D, "cvPtr1D");
   pragma Import (C, CvPtr2D, "cvPtr2D");
   pragma Import (C, CvPtr3D, "cvPtr3D");
   pragma Import (C, CvPtrND, "cvPtrND");
   pragma Import (C, CvGet1D, "cvGet1D");
   pragma Import (C, CvGet2D, "cvGet2D");
   pragma Import (C, CvGet3D, "cvGet3D");
   pragma Import (C, CvGetND, "cvGetND");
   pragma Import (C, CvGetReal1D, "cvGetReal1D");
   pragma Import (C, CvGetReal2D, "cvGetReal2D");
   pragma Import (C, CvGetReal3D, "cvGetReal3D");
   pragma Import (C, CvGetRealND, "cvGetRealND");
   pragma Import (C, CvSet1D, "CvSet1D");
   pragma Import (C, CvSet2D, "CvSet2D");
   pragma Import (C, CvSet3D, "CvSet3D");
   pragma Import (C, CvSetND, "CvSetND");
   pragma Import (C, CvClearND, "cvClearND");
   pragma Import (C, CvGetMat, "cvGetMat");
   pragma Import (C, CvGetImage, "cvGetImage");
   pragma Import (C, CvReshapeMatND, "cvReshapeMatND");
   pragma Import (C, CvReshape, "cvReshape");
   pragma Import (C, CvRepeat, "cvRepeat");
   pragma Import (C, CvCreateData, "cvCreateData");
   pragma Import (C, CvReleaseData, "cvReleaseData");
   pragma Import (C, CvSetData, "cvSetData");
   pragma Import (C, CvGetRawData, "cvGetRawData");
   pragma Import (C, CvGetSize, "cvGetSize");
   pragma Import (C, CvCopy, "cvCopy");
   pragma Import (C, CvSet, "cvSet");
   pragma Import (C, CvSetZero, "cvSetZero");
   pragma Import (C, CvSplit, "cvSplit");
   pragma Import (C, CvMerge, "cvMerge");
   pragma Import (C, CvMixChannels, "cvMixChannels");
   pragma Import (C, CvConvertScale, "cvConvertScale");
   pragma Import (C, CvConvertScaleAbs, "cvConvertScaleAbs");
   pragma Import (C, CvCheckTermCriteria, "cvCheckTermCriteria");
   pragma Import (C, CvAdd, "cvAdd");
   pragma Import (C, CvAddS, "cvAddS");
   pragma Import (C, CvSub, "cvSub");
   pragma Import (C, CvSubS, "cvSubS");
   pragma Import (C, CvSubRS, "cvSubRS");
   pragma Import (C, CvMul, "cvMul");
   pragma Import (C, CvDiv, "cvDiv");
   pragma Import (C, CvScaleAdd, "cvScaleAdd");
   pragma Import (C, CvAddWeighted, "cvAddWeighted");
   pragma Import (C, CvDotProduct, "cvDotProduct");
   pragma Import (C, CvAnd, "cvAnd");
   pragma Import (C, CvAndS, "cvAndS");
   pragma Import (C, CvOr, "cvOr");
   pragma Import (C, CvOrS, "cvOrS");
   pragma Import (C, CvXor, "cvXor");
   pragma Import (C, CvXorS, "cvXorS");
   pragma Import (C, CvNot, "cvNot");
   pragma Import (C, CvInRange, "cvInRange");
   pragma Import (C, CvInRangeS, "cvInRangeS");
   pragma Import (C, CvCmp, "cvCmp");
   pragma Import (C, CvCmpS, "cvCmpS");
   pragma Import (C, CvMin, "cvMin");
   pragma Import (C, CvMax, "cvMax");
   pragma Import (C, CvMinS, "cvMinS");
   pragma Import (C, CvMaxS, "cvMaxS");
   pragma Import (C, CvAbsDiff, "cvAbsDiff");
   pragma Import (C, CvAbsDiffS, "cvAbsDiffS");
   pragma Import (C, CvCartToPolar, "cvCartToPolar");
   pragma Import (C, CvPolarToCart, "cvPolarToCart");
   pragma Import (C, CvPow, "cvPow");
   pragma Import (C, CvExp, "cvExp");
   pragma Import (C, CvLog, "cvLog");
   pragma Import (C, CvFastArctan, "cvFastArctan");
   pragma Import (C, CvCbrt, "cvCbrt");
   pragma Import (C, CvCheckArr, "cvCheckArr");
   pragma Import (C, CvRandArr, "cvRandArr");
   pragma Import (C, CvRandShuffle, "cvRandShuffle");
   pragma Import (C, CvSort, "cvSort");
   pragma Import (C, CvSolveCubic, "cvSolveCubic");
   pragma Import (C, CvSolvePoly, "cvSolvePoly");
   pragma Import (C, CvCrossProduct, "cvCrossProduct");
   pragma Import (C, CvGEMM, "cvGEMM");
   pragma Import (C, CvTransform, "cvTransform");
   pragma Import (C, CvPerspectiveTransform, "cvPerspectiveTransform");
   pragma Import (C, CvMulTransposed, "cvMulTransposed");
   pragma Import (C, CvTranspose, "cvTranspose");
   pragma Import (C, CvCompleteSymm, "cvCompleteSymm");
   pragma Import (C, CvFlip, "cvFlip");
   pragma Import (C, CvSVD, "cvSVD");
   pragma Import (C, CvSVBkSb, "cvSVBkSb");
   pragma Import (C, CvInvert, "cvInvert");
   pragma Import (C, CvSolve, "cvSolve");
   pragma Import (C, CvDet, "cvDet");
   pragma Import (C, CvTrace, "cvTrace");
   pragma Import (C, CvEigenVV, "cvEigenVV");
   pragma Import (C, CvSetIdentity, "cvSetIdentity");
   pragma Import (C, CvRange, "cvRange");
   pragma Import (C, CvCalcCovarMatrix, "cvCalcCovarMatrix");
   pragma Import (C, CvCalcPCA, "cvCalcPCA");
   pragma Import (C, CvProjectPCA, "cvProjectPCA");
   pragma Import (C, CvBackProjectPCA, "cvBackProjectPCA");
   pragma Import (C, CvMahalanobis, "cvMahalanobis");
   pragma Import (C, CvSum, "cvSum");
   pragma Import (C, CvCountNonZero, "cvCountNonZero");
   pragma Import (C, CvAvg, "cvAvg");
   pragma Import (C, CvAvgSdv, "cvAvgSdv");
   pragma Import (C, CvMinMaxLoc, "cvMinMaxLoc");
   pragma Import (C, CvNorm, "cvNorm");
   pragma Import (C, CvNormalize, "cvNormalize");
   pragma Import (C, CvReduce, "cvReduce");
   pragma Import (C, CvDFT, "cvDFT");
   pragma Import (C, CvMulSpectrums, "cvMulSpectrums");
   pragma Import (C, CvGetOptimalDFTSize, "cvGetOptimalDFTSize");
   pragma Import (C, CvDCT, "cvDCT");
   pragma Import (C, CvSliceLength, "cvSliceLength");
   pragma Import (C, CvCreateMemStorage, "cvCreateMemStorage");
   pragma Import (C, CvCreateChildMemStorage, "cvCreateChildMemStorage");
   pragma Import (C, CvReleaseMemStorage, "cvReleaseMemStorage");
   pragma Import (C, CvClearMemStorage, "cvClearMemStorage");
   pragma Import (C, CvSaveMemStoragePos, "cvSaveMemStoragePos");
   pragma Import (C, CvRestoreMemStoragePos, "cvRestoreMemStoragePos");
   pragma Import (C, CvMemStorageAlloc, "cvMemStorageAlloc");
   pragma Import (C, WCvMemStorageAllocString, "cvMemStorageAllocString");
   pragma Import (C, CvCreateSeq, "cvCreateSeq");
   pragma Import (C, CvSetSeqBlockSize, "cvSetSeqBlockSize");
   pragma Import (C, CvSeqPush, "cvSeqPush");
   pragma Import (C, CvSeqPushFront, "cvSeqPushFront");
   pragma Import (C, CvSeqPop, "cvSeqPop");
   pragma Import (C, CvSeqPopFront, "cvSeqPopFront");
   pragma Import (C, CvSeqPushMulti, "cvSeqPushMulti");
   pragma Import (C, CvSeqPopMulti, "cvSeqPopMulti");
   pragma Import (C, CvSeqInsert, "cvSeqInsert");
   pragma Import (C, CvSeqRemove, "cvSeqRemove");
   pragma Import (C, CvClearSeq, "cvClearSeq");
   pragma Import (C, CvGetSeqElem, "cvGetSeqElem");
   pragma Import (C, CvSeqElemIdx, "cvSeqElemIdx");
   pragma Import (C, CvStartAppendToSeq, "cvStartAppendToSeq");
   pragma Import (C, CvStartWriteSeq, "cvStartWriteSeq");
   pragma Import (C, CvEndWriteSeq, "cvEndWriteSeq");
   pragma Import (C, CvFlushSeqWriter, "cvFlushSeqWriter");
   pragma Import (C, CvStartReadSeq, "cvStartReadSeq");
   pragma Import (C, CvGetSeqReaderPos, "cvGetSeqReaderPos");
   pragma Import (C, CvSetSeqReaderPos, "cvSetSeqReaderPos");
   pragma Import (C, CvCvtSeqToArray, "cvCvtSeqToArray");
   pragma Import (C, CvMakeSeqHeaderForArray, "cvMakeSeqHeaderForArray");
   pragma Import (C, CvSeqSlice, "cvSeqSlice");
   pragma Import (C, CvCloneSeq, "cvCloneSeq");
   pragma Import (C, CvSeqRemoveSlice, "cvSeqRemoveSlice");
   pragma Import (C, CvSeqSort, "cvSeqSort");
   pragma Import (C, CvSeqSearch, "cvSeqSearch");
   pragma Import (C, CvSeqInsertSlice, "cvSeqInsertSlice");
   pragma Import (C, CvSeqInvert, "cvSeqInvert");
   pragma Import (C, CvSeqPartition, "cvSeqPartition");
   pragma Import (C, CvCreateSet, "cvCreateSet");
   pragma Import (C, CvSetAdd, "cvSetAdd");
   pragma Import (C , CvSetNew, "cvSetNew");
   pragma Import (C, CvSetRemoveByPtr, "cvSetRemoveByPtr");
   pragma Import (C, CvSetRemove, "cvSetRemove");
   pragma Import (C, CvGetSetElem, "cvGetSetElem");
   pragma Import (C, CvClearSet, "cvClearSet");
   pragma Import (C, CvCreateGraph, "cvCreateGraph");
   pragma Import (C, CvGraphAddVtx, "cvGraphAddVtx");
   pragma Import (C, CvGraphRemoveVtx, "cvGraphRemoveVtx");
   pragma Import (C, CvGraphRemoveVtxByPtr, "cvGraphRemoveVtxByPtr");
   pragma Import (C, CvGraphAddEdge, "cvGraphAddEdge");
   pragma Import (C, CvGraphAddEdgeByPtr, "cvGraphAddEdgeByPtr");
   pragma Import (C, CvGraphRemoveEdge, "cvGraphRemoveEdge");
   pragma Import (C, CvGraphRemoveEdgeByPtr, "cvGraphRemoveEdgeByPtr");
   pragma Import (C, CvFindGraphEdge, "cvFindGraphEdge");
   pragma Import (C, CvFindGraphEdgeByPtr, "cvFindGraphEdgeByPtr");
   pragma Import (C, CvClearGraph, "cvClearGraph");
   pragma Import (C, CvGraphVtxDegree, "cvGraphVtxDegree");
   pragma Import (C, CvGraphVtxDegreeByPtr, "cvGraphVtxDegreeByPtr");
   pragma Import (C, CvGetGraphVtx, "cvGetSetElem");
   pragma Import (C, CvCreateGraphScanner, "cvCreateGraphScanner");
   pragma Import (C, CvReleaseGraphScanner, "cvReleaseGraphScanner");
   pragma Import (C, CvNextGraphItem, "cvNextGraphItem");
   pragma Import (C, CvCloneGraph, "cvCloneGraph");
   pragma Import (C, CvLine, "cvLine");
   pragma Import (C, CvRectangle, "cvRectangle");
   pragma Import (C, CvRectangleR, "cvRectangleR");
   pragma Import (C, CvCircle, "cvCircle");
   pragma Import (C, CvEllipse, "cvEllipse");
   pragma Import (C, CvEllipseBox, "cvEllipseBox");
   pragma Import (C, CvFillConvexPoly, "cvFillConvexPoly");
   pragma Import (C, CvFillPoly, "cvFillPoly");
   pragma Import (C, CvPolyLine, "cvPolyLine");
   pragma Import (C, CvClipLine, "cvClipLine");
   pragma Import (C, CvInitLineIterator, "cvInitLineIterator");
   pragma Import (C, CvInitFont, "cvInitFont");
   pragma Import (C, CvFont, "cvFont");
   pragma Import (C, WCvPutText, "cvPutText");
   pragma Import (C, WCvGetTextSize, "cvGetTextSize");
   pragma Import (C, CvColorToScalar, "cvColorToScalar");
   pragma Import (C, CvEllipse2Poly, "cvEllipse2Poly");
   pragma Import (C, CvDrawContours, "cvDrawContours");
   pragma Import (C, CvLUT, "cvLUT");
   pragma Import (C, CvInitTreeNodeIterator, "cvInitTreeNodeIterator");
   pragma Import (C, CvNextTreeNode, "cvNextTreeNode");
   pragma Import (C, CvPrevTreeNode, "cvPrevTreeNode");
   pragma Import (C, CvInsertNodeIntoTree, "cvInsertNodeIntoTree");
   pragma Import (C, CvRemoveNodeFromTree, "cvRemoveNodeFromTree");
   pragma Import (C, CvTreeToNodeSeq, "cvTreeToNodeSeq");
   pragma Import (C, CvKMeans2, "cvKMeans2");
   pragma Import (C, CvRegisterModule, "cvRegisterModule");
   pragma Import (C, CvUseOptimized, "cvUseOptimized");
   pragma Import (C, CvGetModuleInfo, "cvGetModuleInfo");
   pragma Import (C, CvSetMemoryManager, "cvSetMemoryManager");
   pragma Import (C, CvSetIPLAllocators, "cvSetIPLAllocators");
   pragma Import (C, CvOpenFileStorage, "cvOpenFileStorage");
   pragma Import (C, CvReleaseFileStorage, "cvReleaseFileStorage");
   pragma Import (C, CvAttrValue, "cvAttrValue");
   pragma Import (C, CvStartWriteStruct, "cvStartWriteStruct");
   pragma Import (C, CvEndWriteStruct, "CvEndWriteStruct");
   pragma Import (C, CvWriteInt, "cvWriteInt");
   pragma Import (C, CvWriteReal, "cvWriteReal");
   pragma Import (C, CvWriteString, "cvWriteString");
   pragma Import (C, CvWriteComment, "cvWriteComment");
   pragma Import (C, CvWrite, "cvWrite");
   pragma Import (C, CvStartNextStream, "cvStartNextStream");
   pragma Import (C, CvWriteRawData, "cvWriteRawData");
   pragma Import (C, CvGetHashedKey, "cvGetHashedKey");
   pragma Import (C, CvGetRootFileNode, "cvGetRootFileNode");
   pragma Import (C, CvGetFileNode, "cvGetFileNode");
   pragma Import (C, CvGetFileNodeByName, "cvGetFileNodeByName");
   pragma Import (C, CvReadInt, "cvReadInt");
   pragma Import (C, CvReadIntByName, "cvReadIntByName");
   pragma Import (C, CvReadReal, "cvReadReal");
   pragma Import (C, CvReadRealByName, "cvReadRealByName");
   pragma Import (C, CvReadString, "cvReadString");
   pragma Import (C, CvReadStringByName, "cvReadStringByName");
   pragma Import (C, CvRead, "cvRead");
   pragma Import (C, CvReadByName, "cvReadByName");
   pragma Import (C, CvStartReadRawData, "cvStartReadRawData");
   pragma Import (C, CvReadRawDataSlice, "cvReadRawDataSlice");
   pragma Import (C, CvReadRawData, "cvReadRawData");
   pragma Import (C, CvWriteFileNode, "cvWriteFileNode");
   pragma Import (C, CvGetFileNodeName, "cvGetFileNodeName");
   pragma Import (C, CvRegisterType, "cvRegisterType");
   pragma Import (C, CvUnregisterType, "cvUnregisterType");
   pragma Import (C, CvFirstType, "cvFirstType");
   pragma Import (C, CvFindType, "cvFindType");
   pragma Import (C, CvTypeOf, "cvTypeOf");
   pragma Import (C, CvRelease, "cvRelease");
   pragma Import (C, CvClone, "cvClone");
   pragma Import (C, CvSave, "cvSave");
   pragma Import (C, CvLoad, "cvLoad");
   pragma Import (C, CvGetTickCount, "cvGetTickCount");
   pragma Import (C, CvGetTickFrequency, "cvGetTickFrequency");
   pragma Import (C, CvCheckHardwareSupport, "cvCheckHardwareSupport");
   pragma Import (C, CvGetNumThreads, "cvGetNumThreads");
   pragma Import (C, CvSetNumThreads, "cvSetNumThreads");
   pragma Import (C, CvGetThreadNum, "cvGetThreadNum");
   pragma Import (C, CvGetErrStatus, "cvGetErrStatus");
   pragma Import (C, CvSetErrStatus, "cvSetErrStatus");
   pragma Import (C, CvGetErrMode, "cvGetErrMode");
   pragma Import (C, CvSetErrMode, "cvSetErrMode");
   pragma Import (C, CvError, "cvError");
   pragma Import (C, CvErrorStr, "cvErrorStr");
   pragma Import (C, CvGetErrInfo, "cvGetErrInfo");
   pragma Import (C, CvErrorFromIppStatus, "cvErrorFromIppStatus");
   pragma Import (C, CvRedirectError, "cvRedirectError");
   pragma Import (C, CvNulDevReport, "cvNulDevReport");
   pragma Import (C, CvStdErrReport, "cvStdErrReport");
   pragma Import (C, CvGuiBoxReport, "cvGuiBoxReport");
end Core.Operations;
