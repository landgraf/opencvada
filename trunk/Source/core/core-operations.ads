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
with Gnat.Source_Info;
with Imgproc;

package Core.Operations is
--

   -----------------------------------------------------------------------------
   -- Array allocation, deallocation, initialization and access to elements
   -----------------------------------------------------------------------------
   --     Allocates a memory buffer.
   function Cv_Alloc (Size : Interfaces.C.Size_T)
                      return Cv_Void_Ptr;

   --     Deallocates a memory buffer.
   procedure Cv_Free (Ptr : access Cv_Void_Ptr);

   --     Creates an image header but does not allocate the image data.
   function Cv_Create_Image_Header (Size    : Cv_Size;
                                    Depth   : Unsigned_32;
                                    Channel : Integer)
                                    return Ipl_Image_Ptr;

   --     Initializes an image header that was previously allocated.
   function Cv_Init_Image_Header (Image    : Ipl_Image_Ptr;
                                  Size     : Cv_Size;
                                  Depth    : Unsigned_32;
                                  Channels : Integer;
                                  Origin   : Integer := 0;
                                  Align    : Integer := 4)
                                  return Ipl_Image_Ptr;

   --     Creates an image header and allocates the image data.
   function Cv_Create_Image (Size     : Cv_Size;
                             Depth    : Unsigned_32;
                             Channels : Integer)
                             return Ipl_Image_Ptr;

   --     Deallocates an image header.
   procedure Cv_Release_Image_Header (Image : access Ipl_Image_Ptr);

   --     Deallocates the image header and the image data.
   procedure Cv_Release_Image (Image : access Ipl_Image_Ptr);

   --     Makes a full copy of an image, including the header, data, and ROI.
   function Cv_Clone_Image (Image : Ipl_Image_Ptr)
                            return Ipl_Image_Ptr;

   --     Sets the channel of interest in an IplImage.
   procedure Cv_Set_Image_Coi (Image : Ipl_Image_Ptr;
                               Coi   : Integer);

   --     Returns the index of the channel of interest.
   function Cv_Get_Image_Coi (Image : Ipl_Image_Ptr)
                              return Integer;

   --     Sets an image Region Of Interest (ROI) for a given rectangle.
   procedure Cv_Set_Image_Roi (Image : Ipl_Image_Ptr;
                               Rect  : Cv_Rect);

   --     Resets the image ROI to include the entire image and releases the
   --     ROI structure.
   procedure Cv_Reset_Image_Roi (Image : Ipl_Image_Ptr);

   --     Returns the image ROI.
   function Cv_Get_Image_Roi (Image : Ipl_Image_Ptr)
                              return Cv_Rect;


   --     Creates a matrix header but does not allocate the matrix data.
   function Cv_Create_Mat_Header (Rows     : Integer;
                                  Cols     : Integer;
                                  Mat_Type : Integer)
                                  return Cv_Mat_Ptr;

   --     Initializes a pre-allocated matrix header.
   function Cv_Init_Mat_Header (Mat   : Cv_Mat_Ptr;
                                Rows  : Integer;
                                Cols  : Integer;
                                Mat_T : Integer;
                                Data  : access Mat_Data := null; -- void*
                                Step  : Unsigned_32 := Cv_Autostep)
                                return Cv_Mat_Ptr;

   --     Creates a matrix header and allocates the matrix data.
   function Cv_Create_Mat (Rows     : Integer;
                           Cols     : Integer;
                           Mat_Type : Unsigned_32)
                           return Cv_Mat_Ptr;

   --     Deallocates a matrix.
   procedure Cv_Release_Mat (Mat : access Cv_Mat_Ptr);

   --     Decrements an array data reference counter.
   procedure Cv_Dec_Ref_Data (Arr : Cv_Arr_Ptr);
   procedure Cv_Dec_Ref_Data (Arr : Cv_Mat_Ptr);
   procedure Cv_Dec_Ref_Data (Arr : Ipl_Image_Ptr);

   --     Increments array data reference counter.
   function Cv_Inc_Ref_Data (Arr : Cv_Arr_Ptr)
                             return Integer;
   function Cv_Inc_Ref_Data (Arr : Cv_Mat_Ptr)
                             return Integer;
   function Cv_Inc_Ref_Data (Arr : Ipl_Image_Ptr)
                             return Integer;

   --     Creates a full matrix copy.
   function Cv_Clone_Mat (Mat : Cv_Mat_Ptr)
                          return Cv_Mat_Ptr;

   --     Returns matrix header corresponding to the rectangular sub-array of
   --     input image or matrix.
   function Cv_Get_Sub_Rect (Arr    : Cv_Arr_Ptr;
                             Submat : Cv_Mat_Ptr;
                             Rect   : Cv_Rect)
                             return Cv_Mat_Ptr;
   function Cv_Get_Sub_Rect (Arr    : Cv_Mat_Ptr;
                             Submat : Cv_Mat_Ptr;
                             Rect   : Cv_Rect)
                             return Cv_Mat_Ptr;
   function Cv_Get_Sub_Rect (Arr    : Ipl_Image_Ptr;
                             Submat : Cv_Mat_Ptr;
                             Rect   : Cv_Rect)
                             return Cv_Mat_Ptr;
   function Cv_Get_Sub_Arr (Arr    : Cv_Arr_Ptr;
                            Submat : Cv_Mat_Ptr;
                            Rect   : Cv_Rect)
                            return Cv_Mat_Ptr renames Cv_Get_Sub_Rect;
   function Cv_Get_Sub_Arr (Arr    : Cv_Mat_Ptr;
                            Submat : Cv_Mat_Ptr;
                            Rect   : Cv_Rect)
                            return Cv_Mat_Ptr renames Cv_Get_Sub_Rect;
   function Cv_Get_Sub_Arr (Arr    : Ipl_Image_Ptr;
                            Submat : Cv_Mat_Ptr;
                            Rect   : Cv_Rect)
                            return Cv_Mat_Ptr renames Cv_Get_Sub_Rect;

   --     Returns row span.
   function Cv_Get_Rows (Arr       : Cv_Arr_Ptr;
                         Submat    : Cv_Mat_Ptr;
                         Start_Row : Integer;
                         End_Row   : Integer;
                         Delta_Row : Integer := 1)
                         return Cv_Mat_Ptr;
   function Cv_Get_Rows (Arr       : Cv_Mat_Ptr;
                         Submat    : Cv_Mat_Ptr;
                         Start_Row : Integer;
                         End_Row   : Integer;
                         Delta_Row : Integer := 1)
                         return Cv_Mat_Ptr;
   function Cv_Get_Rows (Arr       : Ipl_Image_Ptr;
                         Submat    : Cv_Mat_Ptr;
                         Start_Row : Integer;
                         End_Row   : Integer;
                         Delta_Row : Integer := 1)
                         return Cv_Mat_Ptr;

   --     Returns array row.
   function Cv_Get_Row (Arr    : Cv_Arr_Ptr;
                        Submat : Cv_Mat_Ptr;
                        Row    : Integer)
                        return Cv_Mat_Ptr;
   function Cv_Get_Row (Arr    : Cv_Mat_Ptr;
                        Submat : Cv_Mat_Ptr;
                        Row    : Integer)
                        return Cv_Mat_Ptr;
   function Cv_Get_Row (Arr    : Ipl_Image_Ptr;
                        Submat : Cv_Mat_Ptr;
                        Row    : Integer)
                        return Cv_Mat_Ptr;

   --     Returns array column span.
   function Cv_Get_Cols (Arr       : Cv_Arr_Ptr;
                         Submat    : Cv_Mat_Ptr;
                         Start_Col : Integer;
                         End_Col   : Integer)
                         return Cv_Mat_Ptr;
   function Cv_Get_Cols (Arr       : Cv_Mat_Ptr;
                         Submat    : Cv_Mat_Ptr;
                         Start_Col : Integer;
                         End_Col   : Integer)
                         return Cv_Mat_Ptr;
   function Cv_Get_Cols (Arr       : Ipl_Image_Ptr;
                         Submat    : Cv_Mat_Ptr;
                         Start_Col : Integer;
                         End_Col   : Integer)
                         return Cv_Mat_Ptr;

   --     Returns array column.
   function Cv_Get_Col (Arr    : Cv_Arr_Ptr;
                        Submat : Cv_Mat_Ptr;
                        Col    : Integer)
                        return Cv_Mat_Ptr;
   function Cv_Get_Col (Arr    : Cv_Mat_Ptr;
                        Submat : Cv_Mat_Ptr;
                        Col    : Integer)
                        return Cv_Mat_Ptr;
   function Cv_Get_Col (Arr    : Ipl_Image_Ptr;
                        Submat : Cv_Mat_Ptr;
                        Col    : Integer)
                        return Cv_Mat_Ptr;

   --     Returns one of array diagonals.
   function Cv_Get_Diag (Arr    : Cv_Arr_Ptr;
                         Submat : Cv_Mat_Ptr;
                         Diag   : Integer := 0)
                         return Cv_Mat_Ptr;
   function Cv_Get_Diag (Arr    : Cv_Mat_Ptr;
                         Submat : Cv_Mat_Ptr;
                         Diag   : Integer := 0)
                         return Cv_Mat_Ptr;
   function Cv_Get_Diag (Arr    : Ipl_Image_Ptr;
                         Submat : Cv_Mat_Ptr;
                         Diag   : Integer := 0)
                         return Cv_Mat_Ptr;


   --/* low-level scalar <-> raw data conversion functions */
   procedure Cv_Scalar_To_Raw_Data (Scalar     : access Cv_Scalar;
                                    Data       : Cv_Void_Ptr;
                                    Itype      : Integer;
                                    Extendto12 : Integer := 0);

   procedure Cv_Raw_Data_To_Scalar (Data   : Cv_Void_Ptr;
                                    Itype  : Integer;
                                    Scalar : access Cv_Scalar);

   --     Creates a new matrix header but does not allocate the matrix data.
   function Cv_Create_Mat_Nd_Header (Dims     : Integer;
                                     Sizes    : access Integer;
                                     Mat_Type : Integer)
                                     return Cv_Mat_Nd_Ptr;

   --     Creates the header and allocates the data for a multi-dimensional
   --     dense array.
   function Cv_Create_Mat_Nd (Dims     : Integer;
                              Sizes    : access Integer;
                              Mat_Type : Integer)
                              return Cv_Mat_Nd_Ptr;

   --     Initializes a pre-allocated multi-dimensional array header.
   function Cv_Init_Mat_Nd_Header (Mat   : access Cv_Mat_Nd;
                                   Dims  : Integer;
                                   Sizes : Cv_32s_Array;
                                   Mat_T : Integer;
                                   Data  : access Mat_Data := null) -- void*
                                   return Cv_Mat_Nd_Ptr;

   --     Deallocates a multi-dimensional array.
   procedure Cv_Release_Mat_Nd (Mat : access Cv_Mat_Nd_Ptr);

   --     Creates full copy of a multi-dimensional array and returns a pointer
   --     to the copy.
   function Cv_Clone_Mat_Nd (Mat : Cv_Mat_Nd_Ptr)
                             return Cv_Mat_Nd_Ptr;

   --     Creates sparse array.
   function Cv_Create_Sparse_Mat (Dims     : Integer;
                                  Sizes    : access Integer;
                                  Mat_Type : Integer)
                                  return Cv_Sparse_Mat_Ptr;

   --     Deallocates sparse array.
   procedure Cv_Release_Sparse_Mat (Mat : access Cv_Sparse_Mat_Ptr);

   --     Creates full copy of sparse array.
   function Cv_Clone_Sparse_Mat (Mat : Cv_Sparse_Mat_Ptr)
                                 return Cv_Sparse_Mat_Ptr;

   --     Initializes sparse array elements iterator.
   function Cv_Init_Sparse_Mat_Iterator (Mat      : Cv_Sparse_Mat_Ptr;
                                         Mat_Iter : Cv_Sparse_Mat_Iterator_Ptr)
                                         return Cv_Sparse_Node_Ptr;

   --     Returns the next sparse matrix element
   function Cv_Get_Next_Sparse_Node (Mat_Iterator : Cv_Sparse_Mat_Iterator_Ptr)
                                     return Cv_Sparse_Node;

   -----------------------------------------------------------------------------
   -- matrix iterator: used for n-ary operations on dense arrays
   -----------------------------------------------------------------------------


   --     /* initializes iterator that traverses through several arrays simulteneously
   --     (the function together with cvNextArraySlice is used for
   --      N-ari element-wise operations) */
   procedure Cv_Init_N_Array_Iterator (Count         : Integer;
                                       Arrs          : Cv_Arr_Ptr_Array;
                                       Mask          : Cv_Arr_Ptr;
                                       Stubs         : Cv_Mat_Nd_Ptr;
                                       Arrayiterator : Cv_N_Array_Iterator;
                                       Flags         : Integer := 0);
   procedure Cv_Init_N_Array_Iterator (Count         : Integer;
                                       Arrs          : Cv_Mat_Array;
                                       Mask          : Cv_Mat_Ptr;
                                       Stubs         : Cv_Mat_Nd_Ptr;
                                       Arrayiterator : Cv_N_Array_Iterator;
                                       Flags         : Integer := 0);
   procedure Cv_Init_N_Array_Iterator (Count         : Integer;
                                       Arrs          : Ipl_Image_Ptr_Array;
                                       Mask          : Ipl_Image_Ptr;
                                       Stubs         : Cv_Mat_Nd_Ptr;
                                       Arrayiterator : Cv_N_Array_Iterator;
                                       Flags         : Integer := 0);

   --  /* returns zero value if iteration is finished, non-zero (slice length) otherwise */
   function Cv_Next_N_Array_Slice (Array_Iterator : Cv_N_Array_Iterator) return Integer;

   --     Returns type of array elements.
   function Cv_Get_Elem_Type (Arr : Cv_Arr_Ptr)
                              return Integer;
   function Cv_Get_Elem_Type (Arr : Cv_Mat_Ptr)
                              return Integer;
   function Cv_Get_Elem_Type (Arr : Ipl_Image_Ptr)
                              return Integer;

   --     Return number of array dimensions and their sizes.
   function Cv_Get_Dims (Arr   : Cv_Arr_Ptr;
                         Sizes : Cv_32s_Array)
                         return Integer;
   function Cv_Get_Dims (Arr   : Cv_Mat_Ptr;
                         Sizes : Cv_32s_Array)
                         return Integer;
   function Cv_Get_Dims (Arr   : Ipl_Image_Ptr;
                         Sizes : Cv_32s_Array)
                         return Integer;

   --     Return the size of a particular dimension.
   function Cv_Get_Dim_Size (Arr   : Cv_Arr_Ptr;
                             Index : Integer)
                             return Integer;
   function Cv_Get_Dim_Size (Arr   : Cv_Mat_Ptr;
                             Index : Integer)
                             return Integer;
   function Cv_Get_Dim_Size (Arr   : Ipl_Image_Ptr;
                             Index : Integer)
                             return Integer;

   --     Return pointer to a particular array element.
   function Cv_Ptr_1d (Arr   : Cv_Arr_Ptr;
                       Idx0  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;
   function Cv_Ptr_1d (Arr   : Cv_Mat_Ptr;
                       Idx0  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;
   function Cv_Ptr_1d (Arr   : Ipl_Image_Ptr;
                       Idx0  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;

   --     Return pointer to a particular array element.
   function Cv_Ptr_2d (Arr   : Cv_Arr_Ptr;
                       Idx0  : Integer;
                       Idx1  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;
   function Cv_Ptr_2d (Arr   : Cv_Mat_Ptr;
                       Idx0  : Integer;
                       Idx1  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;
   function Cv_Ptr_2d (Arr   : Ipl_Image_Ptr;
                       Idx0  : Integer;
                       Idx1  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;

   --     Return pointer to a particular array element.
   function Cv_Ptr_3d (Arr   : Cv_Arr_Ptr;
                       Idx0  : Integer;
                       Idx1  : Integer;
                       Idx2  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;
   function Cv_Ptr_3d (Arr   : Cv_Mat_Ptr;
                       Idx0  : Integer;
                       Idx1  : Integer;
                       Idx2  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;
   function Cv_Ptr_3d (Arr   : Ipl_Image_Ptr;
                       Idx0  : Integer;
                       Idx1  : Integer;
                       Idx2  : Integer;
                       Mat_T : access Integer := null)
                       return Cv_Void_Ptr;

   --     Return pointer to a particular array element.
   function Cv_Ptr_Nd (Arr             : Cv_Arr_Ptr;
                       Idx             : Cv_32s_Array;
                       Mat_T           : access Integer := null;
                       Create_Node     : Integer := 1;
                       Precalc_Hashval : access Unsigned_32 := null)
                       return Cv_Void_Ptr;
   function Cv_Ptr_Nd (Arr             : Cv_Mat_Ptr;
                       Idx             : Cv_32s_Array;
                       Mat_T           : access Integer := null;
                       Create_Node     : Integer := 1;
                       Precalc_Hashval : access Unsigned_32 := null)
                       return Cv_Void_Ptr;
   function Cv_Ptr_Nd (Arr             : Ipl_Image_Ptr;
                       Idx             : Cv_32s_Array;
                       Mat_T           : access Integer := null;
                       Create_Node     : Integer := 1;
                       Precalc_Hashval : access Unsigned_32 := null)
                       return Cv_Void_Ptr;

   --     Return a specific array element.
   function Cv_Get_1d (Arr  : Cv_Arr_Ptr;
                       Idx0 : Integer)
                       return Cv_Scalar;
   function Cv_Get_1d (Arr  : Cv_Mat_Ptr;
                       Idx0 : Integer)
                       return Cv_Scalar;
   function Cv_Get_1d (Arr  : Ipl_Image_Ptr;
                       Idx0 : Integer)
                       return Cv_Scalar;

   --     Return a specific array element.
   function Cv_Get_2d (Arr  : Cv_Arr_Ptr;
                       Idx0 : Integer;
                       Idx1 : Integer)
                       return Cv_Scalar;
   function Cv_Get_2d (Arr  : Cv_Mat_Ptr;
                       Idx0 : Integer;
                       Idx1 : Integer)
                       return Cv_Scalar;
   function Cv_Get_2d (Arr  : Ipl_Image_Ptr;
                       Idx0 : Integer;
                       Idx1 : Integer)
                       return Cv_Scalar;

   --     Return a specific array element.
   function Cv_Get_3d (Arr  : Cv_Arr_Ptr;
                       Idx0 : Integer;
                       Idx1 : Integer;
                       Idx2 : Integer)
                       return Cv_Scalar;
   function Cv_Get_3d (Arr  : Cv_Mat_Ptr;
                       Idx0 : Integer;
                       Idx1 : Integer;
                       Idx2 : Integer)
                       return Cv_Scalar;
   function Cv_Get_3d (Arr  : Ipl_Image_Ptr;
                       Idx0 : Integer;
                       Idx1 : Integer;
                       Idx2 : Integer)
                       return Cv_Scalar;

   --     Return a specific array element.
   function Cv_Get_Nd (Arr : Cv_Arr_Ptr;
                       Idx : Cv_32s_Array)
                       return Cv_Scalar;
   function Cv_Get_Nd (Arr : Cv_Mat_Ptr;
                       Idx : Cv_32s_Array)
                       return Cv_Scalar;
   function Cv_Get_Nd (Arr : Ipl_Image_Ptr;
                       Idx : Cv_32s_Array)
                       return Cv_Scalar;

   --     Return a specific element of single-channel 1D array.
   function Cv_Get_Real_1d (Arr  : Cv_Arr_Ptr;
                            Idx0 : Integer)
                            return Long_Float;
   function Cv_Get_Real_1d (Arr  : Cv_Mat_Ptr;
                            Idx0 : Integer)
                            return Long_Float;
   function Cv_Get_Real_1d (Arr  : Ipl_Image_Ptr;
                            Idx0 : Integer)
                            return Long_Float;

   --     Return a specific element of single-channel 2D array.
   function Cv_Get_Real_2d (Arr  : Cv_Arr_Ptr;
                            Idx0 : Integer;
                            Idx1 : Integer)
                            return Long_Float;
   function Cv_Get_Real_2d (Arr  : Cv_Mat_Ptr;
                            Idx0 : Integer;
                            Idx1 : Integer)
                            return Long_Float;
   function Cv_Get_Real_2d (Arr  : Ipl_Image_Ptr;
                            Idx0 : Integer;
                            Idx1 : Integer)
                            return Long_Float;

   --     Return a specific element of single-channel array.
   function Cv_Get_Real_3d (Arr  : Cv_Arr_Ptr;
                            Idx0 : Integer;
                            Idx1 : Integer;
                            Idx2 : Integer)
                            return Long_Float;
   function Cv_Get_Real_3d (Arr  : Cv_Mat_Ptr;
                            Idx0 : Integer;
                            Idx1 : Integer;
                            Idx2 : Integer)
                            return Long_Float;
   function Cv_Get_Real_3d (Arr  : Ipl_Image_Ptr;
                            Idx0 : Integer;
                            Idx1 : Integer;
                            Idx2 : Integer)
                            return Long_Float;

   --     Return a specific element of single-channel array.
   function Cv_Get_Real_Nd (Arr  : Cv_Arr_Ptr;
                            Idx  : Cv_32s_Array)
                            return Long_Float;
   function Cv_Get_Real_Nd (Arr  : Cv_Mat_Ptr;
                            Idx  : Cv_32s_Array)
                            return Long_Float;
   function Cv_Get_Real_Nd (Arr  : Ipl_Image_Ptr;
                            Idx  : Cv_32s_Array)
                            return Long_Float;

   --     Change the particular array element.
   procedure Cv_Set_1d (Arr   : Cv_Arr_Ptr;
                        Idx0  : Integer;
                        Value : Cv_Scalar);
   procedure Cv_Set_1d (Arr   : Cv_Mat_Ptr;
                        Idx0  : Integer;
                        Value : Cv_Scalar);
   procedure Cv_Set_1d (Arr   : Ipl_Image_Ptr;
                        Idx0  : Integer;
                        Value : Cv_Scalar);

   --     Change the particular array element.
   procedure Cv_Set_2d (Arr   : Cv_Arr_Ptr;
                        Idx0  : Integer;
                        Idx1  : Integer;
                        Value : Cv_Scalar);
   procedure Cv_Set_2d (Arr   : Cv_Mat_Ptr;
                        Idx0  : Integer;
                        Idx1  : Integer;
                        Value : Cv_Scalar);
   procedure Cv_Set_2d (Arr   : Ipl_Image_Ptr;
                        Idx0  : Integer;
                        Idx1  : Integer;
                        Value : Cv_Scalar);

   --     Change the particular array element.
   procedure Cv_Set_3d (Arr   : Cv_Arr_Ptr;
                        Idx0  : Integer;
                        Idx1  : Integer;
                        Idx2  : Integer;
                        Value : Cv_Scalar);
   procedure Cv_Set_3d (Arr   : Cv_Mat_Ptr;
                        Idx0  : Integer;
                        Idx1  : Integer;
                        Idx2  : Integer;
                        Value : Cv_Scalar);
   procedure Cv_Set_3d (Arr   : Ipl_Image_Ptr;
                        Idx0  : Integer;
                        Idx1  : Integer;
                        Idx2  : Integer;
                        Value : Cv_Scalar);

   --     Change the particular array element.
   procedure Cv_Set_Nd (Arr   : Cv_Arr_Ptr;
                        Idx   : Cv_32s_Array;
                        Value : Cv_Scalar);
   procedure Cv_Set_Nd (Arr   : Cv_Mat_Ptr;
                        Idx   : Cv_32s_Array;
                        Value : Cv_Scalar);
   procedure Cv_Set_Nd (Arr   : Ipl_Image_Ptr;
                        Idx   : Cv_32s_Array;
                        Value : Cv_Scalar);

   --     Change a specific array element.
   procedure Cv_Set_Real_1d (Arr   : Cv_Arr_Ptr;
                             Idx0  : Integer;
                             Value : Long_Float);
   procedure Cv_Set_Real_1d (Arr   : Cv_Mat_Ptr;
                             Idx0  : Integer;
                             Value : Long_Float);
   procedure Cv_Set_Real_1d (Arr   : Ipl_Image_Ptr;
                             Idx0  : Integer;
                             Value : Long_Float);

   --     Change a specific array element.
   procedure Cv_Set_Real_2d (Arr   : Cv_Arr_Ptr;
                             Idx0  : Integer;
                             Idx1  : Integer;
                             Value : Long_Float);
   procedure Cv_Set_Real_2d (Arr   : Cv_Mat_Ptr;
                             Idx0  : Integer;
                             Idx1  : Integer;
                             Value : Long_Float);
   procedure Cv_Set_Real_2d (Arr   : Ipl_Image_Ptr;
                             Idx0  : Integer;
                             Idx1  : Integer;
                             Value : Long_Float);

   --     Change a specific array element.
   procedure Cv_Set_Real_3d (Arr   : Cv_Arr_Ptr;
                             Idx0  : Integer;
                             Idx1  : Integer;
                             Idx2  : Integer;
                             Value : Long_Float);
   procedure Cv_Set_Real_3d (Arr   : Cv_Mat_Ptr;
                             Idx0  : Integer;
                             Idx1  : Integer;
                             Idx2  : Integer;
                             Value : Long_Float);
   procedure Cv_Set_Real_3d (Arr   : Ipl_Image_Ptr;
                             Idx0  : Integer;
                             Idx1  : Integer;
                             Idx2  : Integer;
                             Value : Long_Float);

   --     Change a specific array element.
   procedure Cv_Set_Real_Nd (Arr   : Cv_Arr_Ptr;
                             Idx0  : Cv_32s_Array;
                             Value : Long_Float);
   procedure Cv_Set_Real_Nd (Arr   : Cv_Mat_Ptr;
                             Idx0  : Cv_32s_Array;
                             Value : Long_Float);
   procedure Cv_Set_Real_Nd (Arr   : Ipl_Image_Ptr;
                             Idx0  : Cv_32s_Array;
                             Value : Long_Float);

   --     Clears a specific array element.
   procedure Cv_Clear_Nd (Arr : Cv_Arr_Ptr;
                          Idx : access Integer);
   procedure Cv_Clear_Nd (Arr : Cv_Mat_Ptr;
                          Idx : access Integer);
   procedure Cv_Clear_Nd (Arr : Ipl_Image_Ptr;
                          Idx : access Integer);

   --     Returns matrix header for arbitrary array.
   function Cv_Get_Mat (Arr     : Cv_Arr_Ptr;
                        Header  : access Cv_Mat;
                        Coi     : access Integer := null;
                        Allownd : Integer := 0)
                        return Cv_Mat_Ptr;
   function Cv_Get_Mat (Arr     : Cv_Mat_Ptr;
                        Header  : access Cv_Mat;
                        Coi     : access Integer := null;
                        Allownd : Integer := 0)
                        return Cv_Mat_Ptr;
   function Cv_Get_Mat (Arr     : Ipl_Image_Ptr;
                        Header  : access Cv_Mat;
                        Coi     : access Integer := null;
                        Allownd : Integer := 0)
                        return Cv_Mat_Ptr;

   --     Returns image header for arbitrary array.
   function Cv_Get_Image (Arr          : Cv_Arr_Ptr;
                          Image_Header : Ipl_Image_Ptr)
                          return Ipl_Image_Ptr;
   function Cv_Get_Image (Arr          : Cv_Mat_Ptr;
                          Image_Header : Ipl_Image_Ptr)
                          return Ipl_Image_Ptr;
   function Cv_Get_Image (Arr          : Ipl_Image_Ptr;
                          Image_Header : Ipl_Image_Ptr)
                          return Ipl_Image_Ptr;

   --     Changes the shape of a multi-dimensional array without copying
   --     the data.
   function Cv_Reshape_Mat_Nd (Arr           : Cv_Arr_Ptr;
                               Sizeof_Header : Integer;
                               Header        : Cv_Arr_Ptr;
                               New_Cn        : Integer;
                               New_Dims      : Integer;
                               New_Sizes     : Cv_32s_Array)
                               return Cv_Arr_Ptr;

   -- #define cvReshapeND( arr, header, new_cn, new_dims, new_sizes )   \
   --        cvReshapeMatND( (arr), sizeof(*(header)), (header),         \
   --                        (new_cn), (new_dims), (new_sizes))
   function Cv_Reshape_Nd (Arr      : Cv_Arr_Ptr;
                           Header   : Cv_Arr_Ptr;
                           Newcn    : Integer;
                           Newdims  : Integer;
                           Newsizes : Cv_32s_Array) return Cv_Arr_Ptr;

   --     Changes shape of matrix/image without copying data.
   function Cv_Reshape (Arr      : Cv_Arr_Ptr;
                        Header   : Cv_Mat_Ptr;
                        New_Cn   : Integer;
                        New_Rows : Integer := 0)
                        return Cv_Mat_Ptr;
   function Cv_Reshape (Arr      : Cv_Mat_Ptr;
                        Header   : Cv_Mat_Ptr;
                        New_Cn   : Integer;
                        New_Rows : Integer := 0)
                        return Cv_Mat_Ptr;
   function Cv_Reshape (Arr      : Ipl_Image_Ptr;
                        Header   : Cv_Mat_Ptr;
                        New_Cn   : Integer;
                        New_Rows : Integer := 0)
                        return Cv_Mat_Ptr;

   --     Fill the destination array with repeated copies of the source array.
   procedure Cv_Repeat (Src : Cv_Arr_Ptr;
                        Dst : Cv_Arr_Ptr);
   procedure Cv_Repeat (Src : Cv_Mat_Ptr;
                        Dst : Cv_Mat_Ptr);
   procedure Cv_Repeat (Src : Ipl_Image_Ptr;
                        Dst : Ipl_Image_Ptr);

   --     Allocates array data
   procedure Cv_Create_Data (Arr : Cv_Arr_Ptr);
   procedure Cv_Create_Data (Arr : Cv_Mat_Ptr);
   procedure Cv_Create_Data (Arr : Ipl_Image_Ptr);

   --     Releases array data.
   procedure Cv_Release_Data (Arr : Cv_Arr_Ptr);
   procedure Cv_Release_Data (Arr : Cv_Mat_Ptr);
   procedure Cv_Release_Data (Arr : Ipl_Image_Ptr);

   --     Assigns user data to the array header.
   procedure Cv_Set_Data (Arr  : Cv_Arr_Ptr;
                          Data : Cv_Void_Ptr;
                          Step : Unsigned_32);
   procedure Cv_Set_Data (Arr  : Cv_Mat_Ptr;
                          Data : Cv_Void_Ptr;
                          Step : Unsigned_32);
   procedure Cv_Set_Data (Arr  : Ipl_Image_Ptr;
                          Data : Cv_Void_Ptr;
                          Step : Unsigned_32);

   --     Retrieves low-level information about the array.
   procedure Cv_Get_Raw_Data (Arr      : Cv_Arr_Ptr;
                              Data     : access Cv_Arr_Ptr; -- uchar**
                              Step     : access Integer := null;
                              Roi_Size : access Cv_Size := null);

   --     Returns size of matrix or image ROI.
   function Cv_Get_Size (Arr : Cv_Arr_Ptr)
                         return Cv_Size;
   function Cv_Get_Size (Arr : Cv_Mat_Ptr)
                         return Cv_Size;
   function Cv_Get_Size (Arr : Ipl_Image_Ptr)
                         return Cv_Size;

   --     Copies one array to another.
   procedure Cv_Copy (Src  : Cv_Arr_Ptr;
                      Dst  : Cv_Arr_Ptr;
                      Mask : Cv_Arr_Ptr := null);
   procedure Cv_Copy (Src  : Cv_Mat_Ptr;
                      Dst  : Cv_Mat_Ptr;
                      Mask : Cv_Mat_Ptr := null);
   procedure Cv_Copy (Src  : Ipl_Image_Ptr;
                      Dst  : Ipl_Image_Ptr;
                      Mask : Ipl_Image_Ptr := null);

   --     Sets every element of an array to a given value.
   procedure Cv_Set_All (Arr   : Cv_Arr_Ptr;
                         Value : Cv_Scalar;
                         Mask  : Cv_Arr_Ptr := null);
   procedure Cv_Set_All (Arr   : Cv_Mat_Ptr;
                         Value : Cv_Scalar;
                         Mask  : Cv_Mat_Ptr := null);
   procedure Cv_Set_All (Arr   : Ipl_Image_Ptr;
                         Value : Cv_Scalar;
                         Mask  : Ipl_Image_Ptr := null);

   --     Clears the array.
   procedure Cv_Set_Zero (Arr : Cv_Arr_Ptr);
   procedure Cv_Set_Zero (Arr : Cv_Mat_Ptr);
   procedure Cv_Set_Zero (Arr : Ipl_Image_Ptr);
   procedure Cv_Zero (Arr : Cv_Arr_Ptr) renames Cv_Set_Zero;
   procedure Cv_Zero (Arr : Cv_Mat_Ptr) renames Cv_Set_Zero;
   procedure Cv_Zero (Arr : Ipl_Image_Ptr) renames Cv_Set_Zero;


   --     Divides multi-channel array into several single-channel arrays or
   --     extracts a single channel from the array.
   procedure Cv_Split (Src  : Cv_Arr_Ptr;
                       Dst0 : Cv_Arr_Ptr;
                       Dst1 : Cv_Arr_Ptr;
                       Dst2 : Cv_Arr_Ptr;
                       Dst3 : Cv_Arr_Ptr);
   procedure Cv_Split (Src  : Cv_Mat_Ptr;
                       Dst0 : Cv_Mat_Ptr;
                       Dst1 : Cv_Mat_Ptr;
                       Dst2 : Cv_Mat_Ptr;
                       Dst3 : Cv_Mat_Ptr);
   procedure Cv_Split (Src  : Ipl_Image_Ptr;
                       Dst0 : Ipl_Image_Ptr;
                       Dst1 : Ipl_Image_Ptr;
                       Dst2 : Ipl_Image_Ptr;
                       Dst3 : Ipl_Image_Ptr);

   --     Composes a multi-channel array from several single-channel arrays or
   --     inserts a single channel into the array.
   procedure Cv_Merge (Src0 : Cv_Arr_Ptr;
                       Src1 : Cv_Arr_Ptr;
                       Src2 : Cv_Arr_Ptr;
                       Src3 : Cv_Arr_Ptr;
                       Dst  : Cv_Arr_Ptr);
   procedure Cv_Merge (Src0 : Cv_Mat_Ptr;
                       Src1 : Cv_Mat_Ptr;
                       Src2 : Cv_Mat_Ptr;
                       Src3 : Cv_Mat_Ptr;
                       Dst  : Cv_Mat_Ptr);
   procedure Cv_Merge (Src0 : Ipl_Image_Ptr;
                       Src1 : Ipl_Image_Ptr;
                       Src2 : Ipl_Image_Ptr;
                       Src3 : Ipl_Image_Ptr;
                       Dst  : Ipl_Image_Ptr);

   --     Copies several channels from input arrays to certain channels of
   --     output arrays
   procedure Cv_Mix_Channels (Src        : Cv_Arr_Ptr_Array;
                              Src_Count  : Integer;
                              Dst        : Cv_Arr_Ptr_Array;
                              Dst_Count  : Integer;
                              From_To    : Cv_32s_Array;
                              Pair_Count : Integer);
   procedure Cv_Mix_Channels (Src        : Cv_Mat_Ptr_Array;
                              Src_Count  : Integer;
                              Dst        : Cv_Mat_Ptr_Array;
                              Dst_Count  : Integer;
                              From_To    : Cv_32s_Array;
                              Pair_Count : Integer);
   procedure Cv_Mix_Channels (Src        : Ipl_Image_Ptr_Array;
                              Src_Count  : Integer;
                              Dst        : Ipl_Image_Ptr_Array;
                              Dst_Count  : Integer;
                              From_To    : Cv_32s_Array;
                              Pair_Count : Integer);

   --     Converts one array to another with optional linear transformation.
   procedure Cv_Convert_Scale (Src   : Cv_Arr_Ptr;
                               Dst   : Cv_Arr_Ptr;
                               Scale : Long_Float := 1.0;
                               Shift : Long_Float := 0.0);
   procedure Cv_Convert_Scale (Src   : Cv_Mat_Ptr;
                               Dst   : Cv_Mat_Ptr;
                               Scale : Long_Float := 1.0;
                               Shift : Long_Float := 0.0);
   procedure Cv_Convert_Scale (Src   : Ipl_Image_Ptr;
                               Dst   : Ipl_Image_Ptr;
                               Scale : Long_Float := 1.0;
                               Shift : Long_Float := 0.0);

   procedure Cv_Cvt_Scale (Src   : Cv_Arr_Ptr;
                           Dst   : Cv_Arr_Ptr;
                           Scale : Long_Float := 1.0;
                           Shift : Long_Float := 0.0) renames Cv_Convert_Scale;
   procedure Cv_Cvt_Scale (Src   : Cv_Mat_Ptr;
                           Dst   : Cv_Mat_Ptr;
                           Scale : Long_Float := 1.0;
                           Shift : Long_Float := 0.0) renames Cv_Convert_Scale;
   procedure Cv_Cvt_Scale (Src   : Ipl_Image_Ptr;
                           Dst   : Ipl_Image_Ptr;
                           Scale : Long_Float := 1.0;
                           Shift : Long_Float := 0.0) renames Cv_Convert_Scale;

   procedure Cv_Scale (Src   : Cv_Arr_Ptr;
                       Dst   : Cv_Arr_Ptr;
                       Scale : Long_Float := 1.0;
                       Shift : Long_Float := 0.0) renames Cv_Convert_Scale;
   procedure Cv_Scale (Src   : Cv_Mat_Ptr;
                       Dst   : Cv_Mat_Ptr;
                       Scale : Long_Float := 1.0;
                       Shift : Long_Float := 0.0) renames Cv_Convert_Scale;
   procedure Cv_Scale (Src   : Ipl_Image_Ptr;
                       Dst   : Ipl_Image_Ptr;
                       Scale : Long_Float := 1.0;
                       Shift : Long_Float := 0.0) renames Cv_Convert_Scale;

   procedure Cv_Convert (Src : Cv_Arr_Ptr;
                         Dst : Cv_Arr_Ptr);
   procedure Cv_Convert (Src : Cv_Mat_Ptr;
                         Dst : Cv_Mat_Ptr);
   procedure Cv_Convert (Src : Ipl_Image_Ptr;
                         Dst : Ipl_Image_Ptr);

   --     Converts input array elements to another 8-bit unsigned integer with
   --     optional linear transformation.
   procedure Cv_Convert_Scale_Abs (Src   : Cv_Arr_Ptr;
                                   Dst   : Cv_Arr_Ptr;
                                   Scale : Long_Float := 1.0;
                                   Shift : Long_Float := 0.0);
   procedure Cv_Convert_Scale_Abs (Src   : Cv_Mat_Ptr;
                                   Dst   : Cv_Mat_Ptr;
                                   Scale : Long_Float := 1.0;
                                   Shift : Long_Float := 0.0);
   procedure Cv_Convert_Scale_Abs (Src   : Ipl_Image_Ptr;
                                   Dst   : Ipl_Image_Ptr;
                                   Scale : Long_Float := 1.0;
                                   Shift : Long_Float := 0.0);

   --     Converts input array elements to another 8-bit unsigned integer with
   --     optional linear transformation.
   procedure Cv_Cvt_Scale_Abs (Src   : Cv_Arr_Ptr;
                               Dst   : Cv_Arr_Ptr;
                               Scale : Long_Float := 1.0;
                               Shift : Long_Float := 0.0) renames Cv_Convert_Scale_Abs;
   procedure Cv_Cvt_Scale_Abs (Src   : Cv_Mat_Ptr;
                               Dst   : Cv_Mat_Ptr;
                               Scale : Long_Float := 1.0;
                               Shift : Long_Float := 0.0) renames Cv_Convert_Scale_Abs;
   procedure Cv_Cvt_Scale_Abs (Src   : Ipl_Image_Ptr;
                               Dst   : Ipl_Image_Ptr;
                               Scale : Long_Float := 1.0;
                               Shift : Long_Float := 0.0) renames Cv_Convert_Scale_Abs;

   function Cv_Check_Term_Criteria ( Criteria         : Cv_Term_Criteria;
                                    Default_Eps       : Long_Float;
                                    Default_Max_Iters : Integer ) return Cv_Term_Criteria;

   -----------------------------------------------------------------------------
   -- Arithmetic, logic and comparison operations
   -----------------------------------------------------------------------------
   --     Computes the per-element sum of two arrays.
   procedure Cv_Add (Src1 : Cv_Arr_Ptr;
                     Src2 : Cv_Arr_Ptr;
                     Dst  : Cv_Arr_Ptr;
                     Mask : Cv_Arr_Ptr := null);
   procedure Cv_Add (Src1 : Cv_Mat_Ptr;
                     Src2 : Cv_Mat_Ptr;
                     Dst  : Cv_Mat_Ptr;
                     Mask : Cv_Mat_Ptr := null);
   procedure Cv_Add (Src1 : Ipl_Image_Ptr;
                     Src2 : Ipl_Image_Ptr;
                     Dst  : Ipl_Image_Ptr;
                     Mask : Ipl_Image_Ptr := null);

   --     Computes the sum of an array and a scalar.
   procedure Cv_Add_S (Src   : Cv_Arr_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Cv_Arr_Ptr;
                       Mask  : Cv_Arr_Ptr := null);
   procedure Cv_Add_S (Src   : Cv_Mat_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Cv_Mat_Ptr;
                       Mask  : Cv_Mat_Ptr := null);
   procedure Cv_Add_S (Src   : Ipl_Image_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Ipl_Image_Ptr;
                       Mask  : Ipl_Image_Ptr := null);

   --     Computes the per-element difference between two arrays.
   procedure Cv_Sub (Src1 : Cv_Arr_Ptr;
                     Src2 : Cv_Arr_Ptr;
                     Dst  : Cv_Arr_Ptr;
                     Mask : Cv_Arr_Ptr := null);
   procedure Cv_Sub (Src1 : Cv_Mat_Ptr;
                     Src2 : Cv_Mat_Ptr;
                     Dst  : Cv_Mat_Ptr;
                     Mask : Cv_Mat_Ptr := null);
   procedure Cv_Sub (Src1 : Ipl_Image_Ptr;
                     Src2 : Ipl_Image_Ptr;
                     Dst  : Ipl_Image_Ptr;
                     Mask : Ipl_Image_Ptr := null);
   --     Computes the difference between an array and a scalar.
   procedure Cv_Sub_S (Src   : Cv_Arr_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Cv_Arr_Ptr;
                       Mask  : Cv_Arr_Ptr := null);
   procedure Cv_Sub_S (Src   : Cv_Mat_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Cv_Mat_Ptr;
                       Mask  : Cv_Mat_Ptr := null);
   procedure Cv_Sub_S (Src   : Ipl_Image_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Ipl_Image_Ptr;
                       Mask  : Ipl_Image_Ptr := null);

   --     Computes the difference between a scalar and an array.
   procedure Cv_Sub_Rs (Src   : Cv_Arr_Ptr;
                        Value : Cv_Scalar;
                        Dst   : Cv_Arr_Ptr;
                        Mask  : Cv_Arr_Ptr := null);
   procedure Cv_Sub_Rs (Src   : Cv_Mat_Ptr;
                        Value : Cv_Scalar;
                        Dst   : Cv_Mat_Ptr;
                        Mask  : Cv_Mat_Ptr := null);
   procedure Cv_Sub_Rs (Src   : Ipl_Image_Ptr;
                        Value : Cv_Scalar;
                        Dst   : Ipl_Image_Ptr;
                        Mask  : Ipl_Image_Ptr := null);

   --     Calculates the per-element product of two arrays.
   procedure Cv_Mul (Src1  : Cv_Arr_Ptr;
                     Src2  : Cv_Arr_Ptr;
                     Dst   : Cv_Arr_Ptr;
                     Scale : Long_Float := 1.0);
   procedure Cv_Mul (Src1  : Cv_Mat_Ptr;
                     Src2  : Cv_Mat_Ptr;
                     Dst   : Cv_Mat_Ptr;
                     Scale : Long_Float := 1.0);
   procedure Cv_Mul (Src1  : Ipl_Image_Ptr;
                     Src2  : Ipl_Image_Ptr;
                     Dst   : Ipl_Image_Ptr;
                     Scale : Long_Float := 1.0);

   --     Performs per-element division of two arrays.
   procedure Cv_Div (Src1  : Cv_Arr_Ptr;
                     Src2  : Cv_Arr_Ptr;
                     Dst   : Cv_Arr_Ptr;
                     Scale : Long_Float := 1.0);
   procedure Cv_Div (Src1  : Cv_Mat_Ptr;
                     Src2  : Cv_Mat_Ptr;
                     Dst   : Cv_Mat_Ptr;
                     Scale : Long_Float := 1.0);
   procedure Cv_Div (Src1  : Ipl_Image_Ptr;
                     Src2  : Ipl_Image_Ptr;
                     Dst   : Ipl_Image_Ptr;
                     Scale : Long_Float := 1.0);

   --     Calculates the sum of a scaled array and another array.
   procedure Cv_Scale_Add (Src1  : Cv_Arr_Ptr;
                           Scale : Cv_Scalar;
                           Src2  : Cv_Arr_Ptr;
                           Dst   : Cv_Arr_Ptr);
   procedure Cv_Scale_Add (Src1  : Cv_Mat_Ptr;
                           Scale : Cv_Scalar;
                           Src2  : Cv_Mat_Ptr;
                           Dst   : Cv_Mat_Ptr);
   procedure Cv_Scale_Add (Src1  : Ipl_Image_Ptr;
                           Scale : Cv_Scalar;
                           Src2  : Ipl_Image_Ptr;
                           Dst   : Ipl_Image_Ptr);

   procedure Cv_Axpy (Src1  : Cv_Arr_Ptr;
                      Scale : Long_Float;
                      Src2  : Cv_Arr_Ptr;
                      Dst   : Cv_Arr_Ptr);
   procedure Cv_Axpy (Src1  : Cv_Mat_Ptr;
                      Scale : Long_Float;
                      Src2  : Cv_Mat_Ptr;
                      Dst   : Cv_Mat_Ptr);
   procedure Cv_Axpy (Src1  : Ipl_Image_Ptr;
                      Scale : Long_Float;
                      Src2  : Ipl_Image_Ptr;
                      Dst   : Ipl_Image_Ptr);

   --     Computes the weighted sum of two arrays.
   procedure Cv_Add_Weighted (Src1  : Cv_Arr_Ptr;
                              Alpha : Long_Float;
                              Src2  : Cv_Arr_Ptr;
                              Beta  : Long_Float;
                              Gamma : Long_Float;
                              Dst   : Cv_Arr_Ptr);
   procedure Cv_Add_Weighted (Src1  : Cv_Mat_Ptr;
                              Alpha : Long_Float;
                              Src2  : Cv_Mat_Ptr;
                              Beta  : Long_Float;
                              Gamma : Long_Float;
                              Dst   : Cv_Mat_Ptr);
   procedure Cv_Add_Weighted (Src1  : Ipl_Image_Ptr;
                              Alpha : Long_Float;
                              Src2  : Ipl_Image_Ptr;
                              Beta  : Long_Float;
                              Gamma : Long_Float;
                              Dst   : Ipl_Image_Ptr);

   --     Calculates the dot product of two arrays in Euclidian metrics.
   function Cv_Dot_Product (Src1 : Cv_Arr_Ptr;
                            Src2 : Cv_Arr_Ptr)
                            return Long_Float;
   function Cv_Dot_Product (Src1 : Cv_Mat_Ptr;
                            Src2 : Cv_Mat_Ptr)
                            return Long_Float;
   function Cv_Dot_Product (Src1 : Ipl_Image_Ptr;
                            Src2 : Ipl_Image_Ptr)
                            return Long_Float;

   --     Calculates per-element bit-wise conjunction of two arrays.
   procedure Cv_And (Src1 : Cv_Arr_Ptr;
                     Src2 : Cv_Arr_Ptr;
                     Dst  : Cv_Arr_Ptr;
                     Mask : Cv_Arr_Ptr := null);
   procedure Cv_And (Src1 : Cv_Mat_Ptr;
                     Src2 : Cv_Mat_Ptr;
                     Dst  : Cv_Mat_Ptr;
                     Mask : Cv_Mat_Ptr := null);
   procedure Cv_And (Src1 : Ipl_Image_Ptr;
                     Src2 : Ipl_Image_Ptr;
                     Dst  : Ipl_Image_Ptr;
                     Mask : Ipl_Image_Ptr := null);

   --     Calculates per-element bit-wise conjunction of an array and a scalar.
   procedure Cv_And_S (Src   : Cv_Arr_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Cv_Arr_Ptr;
                       Mask  : Cv_Arr_Ptr := null);
   procedure Cv_And_S (Src   : Cv_Mat_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Cv_Mat_Ptr;
                       Mask  : Cv_Mat_Ptr := null);
   procedure Cv_And_S (Src   : Ipl_Image_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Ipl_Image_Ptr;
                       Mask  : Ipl_Image_Ptr := null);

   --     Calculates per-element bit-wise disjunction of two arrays.
   procedure Cv_Or (Src1 : Cv_Arr_Ptr;
                    Src2 : Cv_Arr_Ptr;
                    Dst  : Cv_Arr_Ptr;
                    Mask : Cv_Arr_Ptr := null);
   procedure Cv_Or (Src1 : Cv_Mat_Ptr;
                    Src2 : Cv_Mat_Ptr;
                    Dst  : Cv_Mat_Ptr;
                    Mask : Cv_Mat_Ptr := null);
   procedure Cv_Or (Src1 : Ipl_Image_Ptr;
                    Src2 : Ipl_Image_Ptr;
                    Dst  : Ipl_Image_Ptr;
                    Mask : Ipl_Image_Ptr := null);

   --     Calculates a per-element bit-wise disjunction of an array and
   --     a scalar.
   procedure Cv_Or_S (Src   : Cv_Arr_Ptr;
                      Value : Cv_Scalar;
                      Dst   : Cv_Arr_Ptr;
                      Mask  : Cv_Arr_Ptr := null);
   procedure Cv_Or_S (Src   : Cv_Mat_Ptr;
                      Value : Cv_Scalar;
                      Dst   : Cv_Mat_Ptr;
                      Mask  : Cv_Mat_Ptr := null);
   procedure Cv_Or_S (Src   : Ipl_Image_Ptr;
                      Value : Cv_Scalar;
                      Dst   : Ipl_Image_Ptr;
                      Mask  : Ipl_Image_Ptr := null);

   --     Performs per-element bit-wise exclusive or operation on two arrays.
   procedure Cv_Xor (Src1 : Cv_Arr_Ptr;
                     Src2 : Cv_Arr_Ptr;
                     Dst  : Cv_Arr_Ptr;
                     Mask : Cv_Arr_Ptr := null);
   procedure Cv_Xor (Src1 : Cv_Mat_Ptr;
                     Src2 : Cv_Mat_Ptr;
                     Dst  : Cv_Mat_Ptr;
                     Mask : Cv_Mat_Ptr := null);
   procedure Cv_Xor (Src1 : Ipl_Image_Ptr;
                     Src2 : Ipl_Image_Ptr;
                     Dst  : Ipl_Image_Ptr;
                     Mask : Ipl_Image_Ptr := null);

   --     Performs per-element bit-wise exclusive or operation on an array
   --     and a scalar.
   procedure Cv_Xor_S (Src   : Cv_Arr_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Cv_Arr_Ptr;
                       Mask  : Cv_Arr_Ptr := null);
   procedure Cv_Xor_S (Src   : Cv_Mat_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Cv_Mat_Ptr;
                       Mask  : Cv_Mat_Ptr := null);
   procedure Cv_Xor_S (Src   : Ipl_Image_Ptr;
                       Value : Cv_Scalar;
                       Dst   : Ipl_Image_Ptr;
                       Mask  : Ipl_Image_Ptr := null);

   --     Performs per-element bit-wise inversion of array elements.
   procedure Cv_Not (Src : Cv_Arr_Ptr;
                     Dst : Cv_Arr_Ptr);
   procedure Cv_Not (Src : Cv_Mat_Ptr;
                     Dst : Cv_Mat_Ptr);
   procedure Cv_Not (Src : Ipl_Image_Ptr;
                     Dst : Ipl_Image_Ptr);

   --     Checks that array elements lie between the elements of two other
   --     arrays.
   procedure Cv_In_Range (Src   : Cv_Arr_Ptr;
                          Lower : Cv_Arr_Ptr;
                          Upper : Cv_Arr_Ptr;
                          Dst   : Cv_Arr_Ptr);
   procedure Cv_In_Range (Src   : Cv_Mat_Ptr;
                          Lower : Cv_Mat_Ptr;
                          Upper : Cv_Mat_Ptr;
                          Dst   : Cv_Mat_Ptr);
   procedure Cv_In_Range (Src   : Ipl_Image_Ptr;
                          Lower : Ipl_Image_Ptr;
                          Upper : Ipl_Image_Ptr;
                          Dst   : Ipl_Image_Ptr);

   --     Checks that array elements lie between two scalars.
   procedure Cv_In_Range_S (Src   : Cv_Arr_Ptr;
                            Lower : Cv_Scalar;
                            Upper : Cv_Scalar;
                            Dst   : Cv_Arr_Ptr);
   procedure Cv_In_Range_S (Src   : Cv_Mat_Ptr;
                            Lower : Cv_Scalar;
                            Upper : Cv_Scalar;
                            Dst   : Cv_Mat_Ptr);
   procedure Cv_In_Range_S (Src   : Ipl_Image_Ptr;
                            Lower : Cv_Scalar;
                            Upper : Cv_Scalar;
                            Dst   : Ipl_Image_Ptr);



   --     Performs per-element comparison of two arrays.
   procedure Cv_Cmp (Src1   : Cv_Arr_Ptr;
                     Src2   : Cv_Arr_Ptr;
                     Dst    : Cv_Arr_Ptr;
                     Cmp_Op : Compare_Op);
   procedure Cv_Cmp (Src1   : Cv_Mat_Ptr;
                     Src2   : Cv_Mat_Ptr;
                     Dst    : Cv_Mat_Ptr;
                     Cmp_Op : Compare_Op);
   procedure Cv_Cmp (Src1   : Ipl_Image_Ptr;
                     Src2   : Ipl_Image_Ptr;
                     Dst    : Ipl_Image_Ptr;
                     Cmp_Op : Compare_Op);


   --     Performs per-element comparison of an array and a scalar.
   procedure Cv_Cmp_S (Src    : Cv_Arr_Ptr;
                       Value  : Long_Float;
                       Dst    : Cv_Arr_Ptr;
                       Cmp_Op : Compare_Op);
   procedure Cv_Cmp_S (Src    : Cv_Mat_Ptr;
                       Value  : Long_Float;
                       Dst    : Cv_Mat_Ptr;
                       Cmp_Op : Compare_Op);
   procedure Cv_Cmp_S (Src    : Ipl_Image_Ptr;
                       Value  : Long_Float;
                       Dst    : Ipl_Image_Ptr;
                       Cmp_Op : Compare_Op);

   --     Finds per-element minimum of two arrays.
   procedure Cv_Min (Src1 : Cv_Arr_Ptr;
                     Src2 : Cv_Arr_Ptr;
                     Dst  : Cv_Arr_Ptr);
   procedure Cv_Min (Src1 : Cv_Mat_Ptr;
                     Src2 : Cv_Mat_Ptr;
                     Dst  : Cv_Mat_Ptr);
   procedure Cv_Min (Src1 : Ipl_Image_Ptr;
                     Src2 : Ipl_Image_Ptr;
                     Dst  : Ipl_Image_Ptr);

   --     Finds per-element maximum of two arrays.
   procedure Cv_Max (Src1 : Cv_Arr_Ptr;
                     Src2 : Cv_Arr_Ptr;
                     Dst  : Cv_Arr_Ptr);
   procedure Cv_Max (Src1 : Cv_Mat_Ptr;
                     Src2 : Cv_Mat_Ptr;
                     Dst  : Cv_Mat_Ptr);
   procedure Cv_Max (Src1 : Ipl_Image_Ptr;
                     Src2 : Ipl_Image_Ptr;
                     Dst  : Ipl_Image_Ptr);

   --     Finds per-element minimum of an array and a scalar.
   procedure Cv_Min_S (Src   : Cv_Arr_Ptr;
                       Value : Long_Float;
                       Dst   : Cv_Arr_Ptr);
   procedure Cv_Min_S (Src   : Cv_Mat_Ptr;
                       Value : Long_Float;
                       Dst   : Cv_Mat_Ptr);
   procedure Cv_Min_S (Src   : Ipl_Image_Ptr;
                       Value : Long_Float;
                       Dst   : Ipl_Image_Ptr);

   --     Finds per-element maximum of array and scalar.
   procedure Cv_Max_S (Src   : Cv_Arr_Ptr;
                       Value : Long_Float;
                       Dst   : Cv_Arr_Ptr);
   procedure Cv_Max_S (Src   : Cv_Mat_Ptr;
                       Value : Long_Float;
                       Dst   : Cv_Mat_Ptr);
   procedure Cv_Max_S (Src   : Ipl_Image_Ptr;
                       Value : Long_Float;
                       Dst   : Ipl_Image_Ptr);

   --     Calculates absolute difference between two arrays.
   procedure Cv_Abs_Diff (Src1 : Cv_Arr_Ptr;
                          Src2 : Cv_Arr_Ptr;
                          Dst  : Cv_Arr_Ptr);
   procedure Cv_Abs_Diff (Src1 : Cv_Mat_Ptr;
                          Src2 : Cv_Mat_Ptr;
                          Dst  : Cv_Mat_Ptr);
   procedure Cv_Abs_Diff (Src1 : Ipl_Image_Ptr;
                          Src2 : Ipl_Image_Ptr;
                          Dst  : Ipl_Image_Ptr);

   --     Calculates absolute difference between an array and a scalar.
   procedure Cv_Abs_Diff_S (Src   : Cv_Arr_Ptr;
                            Dst   : Cv_Arr_Ptr;
                            S1    : Long_Float;
                            S2    : Long_Float;
                            S3    : Long_Float;
                            S4    : Long_Float);
   procedure Cv_Abs_Diff_S (Src   : Cv_Mat_Ptr;
                            Dst   : Cv_Mat_Ptr;
                            S1    : Long_Float;
                            S2    : Long_Float;
                            S3    : Long_Float;
                            S4    : Long_Float);
   procedure Cv_Abs_Diff_S (Src   : Ipl_Image_Ptr;
                            Dst   : Ipl_Image_Ptr;
                            S1    : Long_Float;
                            S2    : Long_Float;
                            S3    : Long_Float;
                            S4    : Long_Float);

   -----------------------------------------------------------------------------
   -- Math Operations
   -----------------------------------------------------------------------------
   --     Calculates the magnitude and/or angle of 2d vectors.
   procedure Cv_Cart_To_Polar (X              : Cv_Arr_Ptr;
                               Y              : Cv_Arr_Ptr;
                               Magnitude      : Cv_Arr_Ptr;
                               Angle          : Cv_Arr_Ptr := null;
                               Angleindegrees : Integer := 0);
   procedure Cv_Cart_To_Polar (X              : Cv_Mat_Ptr;
                               Y              : Cv_Mat_Ptr;
                               Magnitude      : Cv_Mat_Ptr;
                               Angle          : Cv_Mat_Ptr := null;
                               Angleindegrees : Integer := 0);
   procedure Cv_Cart_To_Polar (X              : Ipl_Image_Ptr;
                               Y              : Ipl_Image_Ptr;
                               Magnitude      : Ipl_Image_Ptr;
                               Angle          : Ipl_Image_Ptr := null;
                               Angleindegrees : Integer := 0);

   --     Calculates Cartesian coordinates of 2d vectors represented in
   --     polar form.
   procedure Cv_Polar_To_Cart (Magnitude        : Cv_Arr_Ptr;
                               Angle            : Cv_Arr_Ptr;
                               X                : Cv_Arr_Ptr;
                               Y                : Cv_Arr_Ptr;
                               Angle_In_Degrees : Integer := 0);
   procedure Cv_Polar_To_Cart (Magnitude        : Cv_Mat_Ptr;
                               Angle            : Cv_Mat_Ptr;
                               X                : Cv_Mat_Ptr;
                               Y                : Cv_Mat_Ptr;
                               Angle_In_Degrees : Integer := 0);
   procedure Cv_Polar_To_Cart (Magnitude        : Ipl_Image_Ptr;
                               Angle            : Ipl_Image_Ptr;
                               X                : Ipl_Image_Ptr;
                               Y                : Ipl_Image_Ptr;
                               Angle_In_Degrees : Integer := 0);

   --     Raises every array element to a power.
   procedure Cv_Pow (Src   : Cv_Arr_Ptr;
                     Dst   : Cv_Arr_Ptr;
                     Power : Long_Float);
   procedure Cv_Pow (Src   : Cv_Mat_Ptr;
                     Dst   : Cv_Mat_Ptr;
                     Power : Long_Float);
   procedure Cv_Pow (Src   : Ipl_Image_Ptr;
                     Dst   : Ipl_Image_Ptr;
                     Power : Long_Float);

   --     Calculates the exponent of every array element.
   procedure Cv_Exp (Src : Cv_Arr_Ptr;
                     Dst : Cv_Arr_Ptr);
   procedure Cv_Exp (Src : Cv_Mat_Ptr;
                     Dst : Cv_Mat_Ptr);
   procedure Cv_Exp (Src : Ipl_Image_Ptr;
                     Dst : Ipl_Image_Ptr);

   --     Calculates the natural logarithm of every array elements absolute value.
   procedure Cv_Log (Src : Cv_Arr_Ptr;
                     Dst : Cv_Arr_Ptr);
   procedure Cv_Log (Src : Cv_Mat_Ptr;
                     Dst : Cv_Mat_Ptr);
   procedure Cv_Log (Src : Ipl_Image_Ptr;
                     Dst : Ipl_Image_Ptr);

   --     Calculates the angle of a 2D vector.
   function Cv_Fast_Arctan (Y : Float;
                            X : Float)
                            return Float;

   --     Calculates the cubic root
   function Cv_Cbrt (Value : Float)
                     return Float;



   function Cv_Check_Arr (Arr     : Cv_Arr_Ptr;
                          Flags   : Integer := 0;
                          Minval  : Long_Float := 0.0;
                          Max_Val : Long_Float := 0.0) return Integer;
   function Cv_Check_Arr (Arr     : Cv_Mat_Ptr;
                          Flags   : Integer := 0;
                          Minval  : Long_Float := 0.0;
                          Max_Val : Long_Float := 0.0) return Integer;
   function Cv_Check_Arr (Arr     : Ipl_Image_Ptr;
                          Flags   : Integer := 0;
                          Minval  : Long_Float := 0.0;
                          Max_Val : Long_Float := 0.0) return Integer;

   function Cv_Check_Array (Arr     : Cv_Arr_Ptr;
                            Flags   : Integer := 0;
                            Minval  : Long_Float := 0.0;
                            Max_Val : Long_Float := 0.0) return Integer renames Cv_Check_Arr;
   function Cv_Check_Array (Arr     : Cv_Mat_Ptr;
                            Flags   : Integer := 0;
                            Minval  : Long_Float := 0.0;
                            Max_Val : Long_Float := 0.0) return Integer renames Cv_Check_Arr;
   function Cv_Check_Array (Arr     : Ipl_Image_Ptr;
                            Flags   : Integer := 0;
                            Minval  : Long_Float := 0.0;
                            Max_Val : Long_Float := 0.0) return Integer renames Cv_Check_Arr;

   --     Fills an array with random numbers and updates the RNG state.
   procedure Cv_Rand_Arr (Rng       : Cv_Rng;
                          Arr       : Cv_Arr_Ptr;
                          Dist_Type : Integer;
                          Param1    : Cv_Scalar;
                          Param2    : Cv_Scalar);
   procedure Cv_Rand_Arr (Rng       : Cv_Rng;
                          Arr       : Cv_Mat_Ptr;
                          Dist_Type : Integer;
                          Param1    : Cv_Scalar;
                          Param2    : Cv_Scalar);
   procedure Cv_Rand_Arr (Rng       : Cv_Rng;
                          Arr       : Ipl_Image_Ptr;
                          Dist_Type : Integer;
                          Param1    : Cv_Scalar;
                          Param2    : Cv_Scalar);

   procedure Cv_Rand_Shuffle (Mat        : Cv_Arr_Ptr;
                              Rng        : access Cv_Rng;
                              Iterfactor : Long_Float := 1.0);
   procedure Cv_Rand_Shuffle (Mat        : Cv_Mat_Ptr;
                              Rng        : access Cv_Rng;
                              Iterfactor : Long_Float := 1.0);
   procedure Cv_Rand_Shuffle (Mat        : Ipl_Image_Ptr;
                              Rng        : access Cv_Rng;
                              Iterfactor : Long_Float := 1.0);



   procedure Cv_Sort (Src    : Cv_Arr_Ptr;
                      Dst    : Cv_Arr_Ptr := null;
                      Idxmat : Cv_Arr_Ptr := null;
                      Flags  : Integer := 0);
   procedure Cv_Sort (Src    : Cv_Mat_Ptr;
                      Dst    : Cv_Mat_Ptr := null;
                      Idxmat : Cv_Mat_Ptr := null;
                      Flags  : Integer := 0);
   procedure Cv_Sort (Src    : Ipl_Image_Ptr;
                      Dst    : Ipl_Image_Ptr := null;
                      Idxmat : Ipl_Image_Ptr := null;
                      Flags  : Integer := 0);

   --     Finds the real roots of a cubic equation.
   procedure Cv_Solve_Cubic (Coeffs : Cv_Arr_Ptr;
                             Roots  : Cv_Arr_Ptr);
   procedure Cv_Solve_Cubic (Coeffs : Cv_Mat_Ptr;
                             Roots  : Cv_Mat_Ptr);
   procedure Cv_Solve_Cubic (Coeffs : Ipl_Image_Ptr;
                             Roots  : Ipl_Image_Ptr);


   -- /* Finds all real and complex roots of a polynomial equation */
   procedure Cv_Solve_Poly (Coeffs  : Cv_Mat_Ptr;
                            Roots2  : Cv_Mat_Ptr;
                            Maxiter : Integer := 20;
                            Fig     : Integer := 100);

   -----------------------------------------------------------------------------
   -- Matrix Operations
   -----------------------------------------------------------------------------
   --     Calculates the cross product of two 3D vectors.
   procedure Cv_Cross_Product (Src1 : Cv_Arr_Ptr;
                               Src2 : Cv_Arr_Ptr;
                               Dst  : Cv_Arr_Ptr);
   procedure Cv_Cross_Product (Src1 : Cv_Mat_Ptr;
                               Src2 : Cv_Mat_Ptr;
                               Dst  : Cv_Mat_Ptr);
   procedure Cv_Cross_Product (Src1 : Ipl_Image_Ptr;
                               Src2 : Ipl_Image_Ptr;
                               Dst  : Ipl_Image_Ptr);

   procedure Cv_Mat_Mul_Add (Src1  : Cv_Arr_Ptr;
                             Src2  : Cv_Arr_Ptr;
                             Src3  : Cv_Arr_Ptr;
                             Dst   : Cv_Arr_Ptr);
   procedure Cv_Mat_Mul_Add (Src1  : Cv_Mat_Ptr;
                             Src2  : Cv_Mat_Ptr;
                             Src3  : Cv_Mat_Ptr;
                             Dst   : Cv_Mat_Ptr);
   procedure Cv_Mat_Mul_Add (Src1  : Ipl_Image_Ptr;
                             Src2  : Ipl_Image_Ptr;
                             Src3  : Ipl_Image_Ptr;
                             Dst   : Ipl_Image_Ptr);

   procedure Cv_Mat_Mul (Src1 : Cv_Arr_Ptr;
                         Src2 : Cv_Arr_Ptr;
                         Dst  : Cv_Arr_Ptr);
   procedure Cv_Mat_Mul (Src1 : Cv_Mat_Ptr;
                         Src2 : Cv_Mat_Ptr;
                         Dst  : Cv_Mat_Ptr);
   procedure Cv_Mat_Mul (Src1 : Ipl_Image_Ptr;
                         Src2 : Ipl_Image_Ptr;
                         Dst  : Ipl_Image_Ptr);

   --     Performs generalized matrix multiplication.
   procedure Cv_Gemm (Src1  : Cv_Arr_Ptr;
                      Src2  : Cv_Arr_Ptr;
                      Alpha : Long_Float;
                      Src3  : Cv_Arr_Ptr;
                      Beta  : Long_Float;
                      Dst   : Cv_Arr_Ptr;
                      Tabc  : Integer := 0);
   procedure Cv_Gemm (Src1  : Cv_Mat_Ptr;
                      Src2  : Cv_Mat_Ptr;
                      Alpha : Long_Float;
                      Src3  : Cv_Mat_Ptr;
                      Beta  : Long_Float;
                      Dst   : Cv_Mat_Ptr;
                      Tabc  : Integer := 0);
   procedure Cv_Gemm (Src1  : Ipl_Image_Ptr;
                      Src2  : Ipl_Image_Ptr;
                      Alpha : Long_Float;
                      Src3  : Ipl_Image_Ptr;
                      Beta  : Long_Float;
                      Dst   : Ipl_Image_Ptr;
                      Tabc  : Integer := 0);

   procedure Cv_Mat_Mul_Add_Ex (Src1  : Cv_Arr_Ptr;
                                Src2  : Cv_Arr_Ptr;
                                Src3  : Cv_Arr_Ptr;
                                Dst   : Cv_Arr_Ptr)
                                renames Cv_Mat_Mul_Add;
   procedure Cv_Mat_Mul_Add_Ex (Src1  : Cv_Mat_Ptr;
                                Src2  : Cv_Mat_Ptr;
                                Src3  : Cv_Mat_Ptr;
                                Dst   : Cv_Mat_Ptr)
                                renames Cv_Mat_Mul_Add;
   procedure Cv_Mat_Mul_Add_Ex (Src1  : Ipl_Image_Ptr;
                                Src2  : Ipl_Image_Ptr;
                                Src3  : Ipl_Image_Ptr;
                                Dst   : Ipl_Image_Ptr)
                                renames Cv_Mat_Mul_Add;

   --     Performs matrix transformation of every array element.
   procedure Cv_Transform (Src      : Cv_Arr_Ptr;
                           Dst      : Cv_Arr_Ptr;
                           Transmat : access Cv_Mat;
                           Shiftvec : access Cv_Mat := null);
   procedure Cv_Transform (Src      : Cv_Mat_Ptr;
                           Dst      : Cv_Mat_Ptr;
                           Transmat : access Cv_Mat;
                           Shiftvec : access Cv_Mat := null);
   procedure Cv_Transform (Src      : Ipl_Image_Ptr;
                           Dst      : Ipl_Image_Ptr;
                           Transmat : access Cv_Mat;
                           Shiftvec : access Cv_Mat := null);

   procedure Cv_Mat_Mul_Add_S (Src      : Cv_Arr_Ptr;
                               Dst      : Cv_Arr_Ptr;
                               Transmat : access Cv_Mat;
                               Shiftvec : access Cv_Mat := null) renames Cv_Transform;
   procedure Cv_Mat_Mul_Add_S (Src      : Cv_Mat_Ptr;
                               Dst      : Cv_Mat_Ptr;
                               Transmat : access Cv_Mat;
                               Shiftvec : access Cv_Mat := null) renames Cv_Transform;
   procedure Cv_Mat_Mul_Add_S (Src      : Ipl_Image_Ptr;
                               Dst      : Ipl_Image_Ptr;
                               Transmat : access Cv_Mat;
                               Shiftvec : access Cv_Mat := null) renames Cv_Transform;

   --     Performs perspective matrix transformation of a vector array.
   procedure Cv_Perspective_Transform (Src : Cv_Arr_Ptr;
                                       Dst : Cv_Arr_Ptr;
                                       Mat : Cv_Arr_Ptr);
   procedure Cv_Perspective_Transform (Src : Cv_Mat_Ptr;
                                       Dst : Cv_Mat_Ptr;
                                       Mat : Cv_Mat_Ptr);
   procedure Cv_Perspective_Transform (Src : Ipl_Image_Ptr;
                                       Dst : Ipl_Image_Ptr;
                                       Mat : Ipl_Image_Ptr);

   --     Calculates the product of an array and a transposed array.
   procedure Cv_Mul_Transposed (Src       : Cv_Arr_Ptr;
                                Dst       : Cv_Arr_Ptr;
                                Order     : Integer;
                                Delta_Arr : Cv_Arr_Ptr := null;
                                Scale     : Long_Float := 1.0);
   procedure Cv_Mul_Transposed (Src       : Cv_Mat_Ptr;
                                Dst       : Cv_Mat_Ptr;
                                Order     : Integer;
                                Delta_Arr : Cv_Mat_Ptr := null;
                                Scale     : Long_Float := 1.0);
   procedure Cv_Mul_Transposed (Src       : Ipl_Image_Ptr;
                                Dst       : Ipl_Image_Ptr;
                                Order     : Integer;
                                Delta_Arr : Ipl_Image_Ptr := null;
                                Scale     : Long_Float := 1.0);

   --     Transposes a matrix.
   procedure Cv_Transpose (Src : Cv_Arr_Ptr;
                           Dst : Cv_Arr_Ptr);
   procedure Cv_Transpose (Src : Cv_Mat_Ptr;
                           Dst : Cv_Mat_Ptr);
   procedure Cv_Transpose (Src : Ipl_Image_Ptr;
                           Dst : Ipl_Image_Ptr);

   procedure Cv_T (Src : Cv_Arr_Ptr;
                   Dst : Cv_Arr_Ptr) renames Cv_Transpose;
   procedure Cv_T (Src : Cv_Mat_Ptr;
                   Dst : Cv_Mat_Ptr) renames Cv_Transpose;
   procedure Cv_T (Src : Ipl_Image_Ptr;
                   Dst : Ipl_Image_Ptr) renames Cv_Transpose;

   --/* Completes the symmetric matrix from the lower (LtoR=0) or from the upper (LtoR!=0) part */
   procedure Cv_Complete_Symm (Matrix : Cv_Mat_Ptr;
                               Ltor   : Integer := 0);

   --     Flip a 2D array around vertical, horizontal or both axes.
   procedure Cv_Flip (Src      : Cv_Arr_Ptr;
                      Dst      : Cv_Arr_Ptr := null;
                      Flipmode : Integer := 0);
   procedure Cv_Flip (Src      : Cv_Mat_Ptr;
                      Dst      : Cv_Mat_Ptr := null;
                      Flipmode : Integer := 0);
   procedure Cv_Flip (Src      : Ipl_Image_Ptr;
                      Dst      : Ipl_Image_Ptr := null;
                      Flipmode : Integer := 0);

   --     Synonym for Flip.
   procedure Cv_Mirror (Src      : Cv_Arr_Ptr;
                        Dst      : Cv_Arr_Ptr := null;
                        Flipmode : Integer := 0)
                        renames Cv_Flip;
   procedure Cv_Mirror (Src      : Cv_Mat_Ptr;
                        Dst      : Cv_Mat_Ptr := null;
                        Flipmode : Integer := 0)
                        renames Cv_Flip;
   procedure Cv_Mirror (Src      : Ipl_Image_Ptr;
                        Dst      : Ipl_Image_Ptr := null;
                        Flipmode : Integer := 0)
                        renames Cv_Flip;



   --     Performs singular value decomposition of a real floating-point matrix.
   procedure Cv_Svdecomp (A     : Cv_Arr_Ptr;
                          W     : Cv_Arr_Ptr;
                          U     : Cv_Arr_Ptr := null;
                          V     : Cv_Arr_Ptr := null;
                          Flags : Unsigned_32 := 0);
   procedure Cv_Svdecomp (A     : Cv_Mat_Ptr;
                          W     : Cv_Mat_Ptr;
                          U     : Cv_Mat_Ptr := null;
                          V     : Cv_Arr_Ptr := null;
                          Flags : Unsigned_32 := 0);
   procedure Cv_Svdecomp (A     : Ipl_Image_Ptr;
                          W     : Ipl_Image_Ptr;
                          U     : Ipl_Image_Ptr := null;
                          V     : Ipl_Image_Ptr := null;
                          Flags : Unsigned_32 := 0);

   --     Performs singular value back substitution.
   procedure Cv_Svbksb (W     : Cv_Arr_Ptr;
                        U     : Cv_Arr_Ptr;
                        V     : Cv_Arr_Ptr;
                        B     : Cv_Arr_Ptr;
                        X     : Cv_Arr_Ptr;
                        Flags : Unsigned_32);
   procedure Cv_Svbksb (W     : Cv_Mat_Ptr;
                        U     : Cv_Mat_Ptr;
                        V     : Cv_Mat_Ptr;
                        B     : Cv_Mat_Ptr;
                        X     : Cv_Mat_Ptr;
                        Flags : Unsigned_32);
   procedure Cv_Svbksb (W     : Ipl_Image_Ptr;
                        U     : Ipl_Image_Ptr;
                        V     : Ipl_Image_Ptr;
                        B     : Ipl_Image_Ptr;
                        X     : Ipl_Image_Ptr;
                        Flags : Unsigned_32);



   --     Finds the inverse or pseudo-inverse of a matrix.
   function Cv_Invert (Src    : Cv_Arr_Ptr;
                       Dst    : Cv_Arr_Ptr;
                       Method : Integer := Cv_Lu)
                       return Long_Float;
   function Cv_Invert (Src    : Cv_Mat_Ptr;
                       Dst    : Cv_Mat_Ptr;
                       Method : Integer := Cv_Lu)
                       return Long_Float;
   function Cv_Invert (Src    : Ipl_Image_Ptr;
                       Dst    : Ipl_Image_Ptr;
                       Method : Integer := Cv_Lu)
                       return Long_Float;
   function Cv_Inv (Src    : Cv_Arr_Ptr;
                    Dst    : Cv_Arr_Ptr;
                    Method : Integer := Cv_Lu)
                    return Long_Float renames Cv_Invert;
   function Cv_Inv (Src    : Cv_Mat_Ptr;
                    Dst    : Cv_Mat_Ptr;
                    Method : Integer := Cv_Lu)
                    return Long_Float renames Cv_Invert;
   function Cv_Inv (Src    : Ipl_Image_Ptr;
                    Dst    : Ipl_Image_Ptr;
                    Method : Integer := Cv_Lu)
                    return Long_Float renames Cv_Invert;

   --     Solves a linear system or least-squares problem.
   function Cv_Solve (Src1   : Cv_Arr_Ptr;
                      Src2   : Cv_Arr_Ptr;
                      Dst    : Cv_Arr_Ptr;
                      Method : Integer := Cv_Lu)
                      return Integer;
   function Cv_Solve (Src1   : Cv_Mat_Ptr;
                      Src2   : Cv_Mat_Ptr;
                      Dst    : Cv_Mat_Ptr;
                      Method : Integer := Cv_Lu)
                      return Integer;
   function Cv_Solve (Src1   : Ipl_Image_Ptr;
                      Src2   : Ipl_Image_Ptr;
                      Dst    : Ipl_Image_Ptr;
                      Method : Integer := Cv_Lu)
                      return Integer;

   --     Returns the determinant of a matrix.
   function Cv_Det (Mat : Cv_Arr_Ptr)
                    return Long_Float;
   function Cv_Det (Mat : Cv_Mat_Ptr)
                    return Long_Float;
   function Cv_Det (Mat : Ipl_Image_Ptr)
                    return Long_Float;

   --     Returns the trace of a matrix.
   function Cv_Trace (Mat : Cv_Arr_Ptr) return Cv_Scalar;
   function Cv_Trace (Mat : Cv_Mat_Ptr) return Cv_Scalar;
   function Cv_Trace (Mat : Ipl_Image_Ptr) return Cv_Scalar;

   --     Computes eigenvalues and eigenvectors of a symmetric matrix.
   procedure Cv_Eigen_Vv (Mat       : Cv_Arr_Ptr;
                          Evects    : Cv_Arr_Ptr;
                          Evals     : Cv_Arr_Ptr;
                          Eps       : Long_Float := 0.0;
                          Lowindex  : Integer := -1;
                          Highindex : Integer := -1);
   procedure Cv_Eigen_Vv (Mat       : Cv_Mat_Ptr;
                          Evects    : Cv_Mat_Ptr;
                          Evals     : Cv_Mat_Ptr;
                          Eps       : Long_Float := 0.0;
                          Lowindex  : Integer := -1;
                          Highindex : Integer := -1);
   procedure Cv_Eigen_Vv (Mat       : Ipl_Image_Ptr;
                          Evects    : Ipl_Image_Ptr;
                          Evals     : Ipl_Image_Ptr;
                          Eps       : Long_Float := 0.0;
                          Lowindex  : Integer := -1;
                          Highindex : Integer := -1);

   --     Initializes a scaled identity matrix.
   procedure Cv_Set_Identity (Mat   : Cv_Arr_Ptr;
                              Value : Cv_Scalar);
   procedure Cv_Set_Identity (Mat   : Cv_Mat_Ptr;
                              Value : Cv_Scalar);
   procedure Cv_Set_Identity (Mat   : Ipl_Image_Ptr;
                              Value : Cv_Scalar);

   -- /* Fills matrix with given range of numbers */
   function Cv_Range (Mat   : Cv_Arr_Ptr;
                      Start : Long_Float;
                      Ende  : Long_Float) return Cv_Arr_Ptr;
   function Cv_Range (Mat   : Cv_Mat_Ptr;
                      Start : Long_Float;
                      Ende  : Long_Float) return Cv_Arr_Ptr;
   function Cv_Range (Mat   : Ipl_Image_Ptr;
                      Start : Long_Float;
                      Ende  : Long_Float) return Cv_Arr_Ptr;

   --     Calculates covariance matrix of a set of vectors.
   procedure Cv_Calc_Covar_Matrix (Vects  : Cv_Arr_Ptr;
                                   Count  : Integer;
                                   Covmat : Cv_Arr_Ptr;
                                   Avg    : Cv_Arr_Ptr;
                                   Flags  : Unsigned_32);
   procedure Cv_Calc_Covar_Matrix (Vects  : Cv_Mat_Ptr;
                                   Count  : Integer;
                                   Covmat : Cv_Mat_Ptr;
                                   Avg    : Cv_Mat_Ptr;
                                   Flags  : Unsigned_32);
   procedure Cv_Calc_Covar_Matrix (Vects  : Ipl_Image_Ptr;
                                   Count  : Integer;
                                   Covmat : Ipl_Image_Ptr;
                                   Avg    : Ipl_Image_Ptr;
                                   Flags  : Unsigned_32);

   procedure Cv_Calc_Pca (Data       : Cv_Arr_Ptr;
                          Mean       : Cv_Arr_Ptr;
                          Eigenvals  : Cv_Arr_Ptr;
                          Eigenvects : Cv_Arr_Ptr;
                          Flags      : Integer);
   procedure Cv_Calc_Pca (Data       : Cv_Mat_Ptr;
                          Mean       : Cv_Mat_Ptr;
                          Eigenvals  : Cv_Mat_Ptr;
                          Eigenvects : Cv_Mat_Ptr;
                          Flags      : Integer);
   procedure Cv_Calc_Pca (Data       : Ipl_Image_Ptr;
                          Mean       : Ipl_Image_Ptr;
                          Eigenvals  : Ipl_Image_Ptr;
                          Eigenvects : Ipl_Image_Ptr;
                          Flags      : Integer);

   procedure Cv_Project_Pca (Data       : Cv_Arr_Ptr;
                             Mean       : Cv_Arr_Ptr;
                             Eigenvects : Cv_Arr_Ptr;
                             Result     : Cv_Arr_Ptr);
   procedure Cv_Project_Pca (Data       : Cv_Mat_Ptr;
                             Mean       : Cv_Mat_Ptr;
                             Eigenvects : Cv_Mat_Ptr;
                             Result     : Cv_Mat_Ptr);
   procedure Cv_Project_Pca (Data       : Ipl_Image_Ptr;
                             Mean       : Ipl_Image_Ptr;
                             Eigenvects : Ipl_Image_Ptr;
                             Result     : Ipl_Image_Ptr);

   procedure Cv_Back_Project_Pca (Proj       : Cv_Arr_Ptr;
                                  Mean       : Cv_Arr_Ptr;
                                  Eigenvects : Cv_Arr_Ptr;
                                  Result     : Cv_Arr_Ptr);
   procedure Cv_Back_Project_Pca (Proj       : Cv_Mat_Ptr;
                                  Mean       : Cv_Mat_Ptr;
                                  Eigenvects : Cv_Mat_Ptr;
                                  Result     : Cv_Mat_Ptr);
   procedure Cv_Back_Project_Pca (Proj       : Ipl_Image_Ptr;
                                  Mean       : Ipl_Image_Ptr;
                                  Eigenvects : Ipl_Image_Ptr;
                                  Result     : Ipl_Image_Ptr);

   --     Calculates the Mahalonobis distance between two vectors.
   function Cv_Mahalanobis (Vec1 : Cv_Arr_Ptr;
                            Vec2 : Cv_Arr_Ptr;
                            Mat  : Cv_Arr_Ptr)
                            return Long_Float;
   function Cv_Mahalanobis (Vec1 : Cv_Mat_Ptr;
                            Vec2 : Cv_Mat_Ptr;
                            Mat  : Cv_Mat_Ptr)
                            return Long_Float;
   function Cv_Mahalanobis (Vec1 : Ipl_Image_Ptr;
                            Vec2 : Ipl_Image_Ptr;
                            Mat  : Ipl_Image_Ptr)
                            return Long_Float;
   function Cv_Mahalonobis (Vec1 : Cv_Arr_Ptr;
                            Vec2 : Cv_Arr_Ptr;
                            Mat  : Cv_Arr_Ptr)
                            return Long_Float renames Cv_Mahalanobis;
   function Cv_Mahalonobis (Vec1 : Cv_Mat_Ptr;
                            Vec2 : Cv_Mat_Ptr;
                            Mat  : Cv_Mat_Ptr)
                            return Long_Float renames Cv_Mahalanobis;
   function Cv_Mahalonobis (Vec1 : Ipl_Image_Ptr;
                            Vec2 : Ipl_Image_Ptr;
                            Mat  : Ipl_Image_Ptr)
                            return Long_Float renames Cv_Mahalanobis;

   -----------------------------------------------------------------------------
   -- Array Statistics
   -----------------------------------------------------------------------------
   --     Adds up array elements.
   function Cv_Sum (Arr : Cv_Arr_Ptr) return Cv_Scalar;
   function Cv_Sum (Arr : Cv_Mat_Ptr) return Cv_Scalar;
   function Cv_Sum (Arr : Ipl_Image_Ptr) return Cv_Scalar;

   --     Counts non-zero array elements.
   function Cv_Count_Non_Zero (Arr : Cv_Arr_Ptr)
                               return Integer;
   function Cv_Count_Non_Zero (Arr : Cv_Mat_Ptr)
                               return Integer;
   function Cv_Count_Non_Zero (Arr : Ipl_Image_Ptr)
                               return Integer;

   --     Calculates average (mean) of array elements.
   function Cv_Avg (Arr  : Cv_Arr_Ptr;
                    Mask : Cv_Arr_Ptr := null)
                    return Cv_Scalar;
   function Cv_Avg (Arr  : Cv_Mat_Ptr;
                    Mask : Cv_Mat_Ptr := null)
                    return Cv_Scalar;
   function Cv_Avg (Arr  : Ipl_Image_Ptr;
                    Mask : Ipl_Image_Ptr := null)
                    return Cv_Scalar;

   --     Calculates average (mean) of array elements.
   procedure Cv_Avg_Sdv (Arr    : Cv_Arr_Ptr;
                         Mean   : access Cv_Scalar;
                         Stddev : access Cv_Scalar;
                         Mask   : Cv_Arr_Ptr := null);
   procedure Cv_Avg_Sdv (Arr    : Cv_Mat_Ptr;
                         Mean   : access Cv_Scalar;
                         Stddev : access Cv_Scalar;
                         Mask   : Cv_Mat_Ptr := null);
   procedure Cv_Avg_Sdv (Arr    : Ipl_Image_Ptr;
                         Mean   : access Cv_Scalar;
                         Stddev : access Cv_Scalar;
                         Mask   : Ipl_Image_Ptr := null);

   --     Finds global minimum and maximum in array or subarray.
   procedure Cv_Min_Max_Loc (Arr     : Cv_Arr_Ptr;
                             Min_Val : access Long_Float;
                             Max_Val : access Long_Float;
                             Min_Loc : access Cv_Point := null;
                             Max_Loc : access Cv_Point := null;
                             Mask    : Cv_Arr_Ptr := null);
   procedure Cv_Min_Max_Loc (Arr     : Cv_Mat_Ptr;
                             Min_Val : access Long_Float;
                             Max_Val : access Long_Float;
                             Min_Loc : access Cv_Point := null;
                             Max_Loc : access Cv_Point := null;
                             Mask    : Cv_Mat_Ptr := null);
   procedure Cv_Min_Max_Loc (Arr     : Ipl_Image_Ptr;
                             Min_Val : access Long_Float;
                             Max_Val : access Long_Float;
                             Min_Loc : access Cv_Point := null;
                             Max_Loc : access Cv_Point := null;
                             Mask    : Ipl_Image_Ptr := null);

   --     Calculates absolute array norm, absolute difference norm, or relative difference norm.
   function Cv_Norm (Arr1      : Cv_Arr_Ptr;
                     Arr2      : Cv_Arr_Ptr := null;
                     Norm_Type : Unsigned_32 := Cv_L2;
                     Mask      : Cv_Arr_Ptr := null)
                     return Long_Float;
   function Cv_Norm (Arr1      : Cv_Mat_Ptr;
                     Arr2      : Cv_Mat_Ptr := null;
                     Norm_Type : Unsigned_32 := Cv_L2;
                     Mask      : Cv_Mat_Ptr := null)
                     return Long_Float;
   function Cv_Norm (Arr1      : Ipl_Image_Ptr;
                     Arr2      : Ipl_Image_Ptr := null;
                     Norm_Type : Unsigned_32 := Cv_L2;
                     Mask      : Ipl_Image_Ptr := null)
                     return Long_Float;

   procedure Cv_Normalize (Src      : Cv_Arr_Ptr;
                           Dst      : Cv_Arr_Ptr;
                           A        : Long_Float := 1.0;
                           B        : Long_Float := 0.0;
                           Normtype : Integer := Integer (Cv_L2);
                           Mask     : Cv_Arr_Ptr := null);
   procedure Cv_Normalize (Src      : Cv_Mat_Ptr;
                           Dst      : Cv_Mat_Ptr;
                           A        : Long_Float := 1.0;
                           B        : Long_Float := 0.0;
                           Normtype : Integer := Integer (Cv_L2);
                           Mask     : Cv_Mat_Ptr := null);
   procedure Cv_Normalize (Src      : Ipl_Image_Ptr;
                           Dst      : Ipl_Image_Ptr;
                           A        : Long_Float := 1.0;
                           B        : Long_Float := 0.0;
                           Normtype : Integer := Integer (Cv_L2);
                           Mask     : Ipl_Image_Ptr := null);



   --     Reduces a matrix to a vector.
   procedure Cv_Reduce (Src : Cv_Arr_Ptr;
                        Dst : Cv_Arr_Ptr;
                        Dim : Integer := -1;
                        Op  : Integer := Cv_Reduce_Sum);
   procedure Cv_Reduce (Src : Cv_Mat_Ptr;
                        Dst : Cv_Mat_Ptr;
                        Dim : Integer := -1;
                        Op  : Integer := Cv_Reduce_Sum);
   procedure Cv_Reduce (Src : Ipl_Image_Ptr;
                        Dst : Ipl_Image_Ptr;
                        Dim : Integer := -1;
                        Op  : Integer := Cv_Reduce_Sum);

   -----------------------------------------------------------------------------
   -- Discrete Linear Transforms and Related Functions
   -----------------------------------------------------------------------------

   --     Performs a forward or inverse Discrete Fourier transform of a 1D or 2D
   --     floating - point array.
   procedure Cv_Dft (Src         : Cv_Arr_Ptr;
                     Dst         : Cv_Arr_Ptr;
                     Flags       : Unsigned_32;
                     Nonzerorows : Integer := 0);
   procedure Cv_Dft (Src         : Cv_Mat_Ptr;
                     Dst         : Cv_Mat_Ptr;
                     Flags       : Unsigned_32;
                     Nonzerorows : Integer := 0);
   procedure Cv_Dft (Src         : Ipl_Image_Ptr;
                     Dst         : Ipl_Image_Ptr;
                     Flags       : Unsigned_32;
                     Nonzerorows : Integer := 0);
   procedure Cv_Fft (Src         : Cv_Arr_Ptr;
                     Dst         : Cv_Arr_Ptr;
                     Flags       : Unsigned_32;
                     Nonzerorows : Integer := 0) renames Cv_Dft;
   procedure Cv_Fft (Src         : Cv_Mat_Ptr;
                     Dst         : Cv_Mat_Ptr;
                     Flags       : Unsigned_32;
                     Nonzerorows : Integer := 0) renames Cv_Dft;
   procedure Cv_Fft (Src         : Ipl_Image_Ptr;
                     Dst         : Ipl_Image_Ptr;
                     Flags       : Unsigned_32;
                     Nonzerorows : Integer := 0) renames Cv_Dft;

   --     Performs per-element multiplication of two Fourier spectrums.
   procedure Cv_Mul_Spectrums (Src1  : Cv_Arr_Ptr;
                               Src2  : Cv_Arr_Ptr;
                               Dst   : Cv_Arr_Ptr;
                               Flags : Unsigned_32);
   procedure Cv_Mul_Spectrums (Src1  : Cv_Mat_Ptr;
                               Src2  : Cv_Mat_Ptr;
                               Dst   : Cv_Mat_Ptr;
                               Flags : Unsigned_32);
   procedure Cv_Mul_Spectrums (Src1  : Ipl_Image_Ptr;
                               Src2  : Ipl_Image_Ptr;
                               Dst   : Ipl_Image_Ptr;
                               Flags : Unsigned_32);

   --     Returns optimal DFT size for a given vector size.
   function Cv_Get_Optimal_Dft_Size (Size0 : Integer)
                                     return Integer;

   --     Performs a forward or inverse Discrete Cosine transform of a 1D or 2D
   --     floating - point array.
   procedure Cv_Dct (Src   : Cv_Arr_Ptr;
                     Dst   : Cv_Arr_Ptr;
                     Flags : Unsigned_32);
   procedure Cv_Dct (Src   : Cv_Mat_Ptr;
                     Dst   : Cv_Mat_Ptr;
                     Flags : Unsigned_32);
   procedure Cv_Dct (Src   : Ipl_Image_Ptr;
                     Dst   : Ipl_Image_Ptr;
                     Flags : Unsigned_32);

   -----------------------------------------------------------------------------
   -- Dynamic data structures
   -----------------------------------------------------------------------------
   --/  * Calculates Length of Sequence Slice (with Support of Negative Indices). *  /
   function Cv_Slice_Length (Slice : Cv_Slice;
                             Seq   : Cv_Seq_Ptr) return Integer;

   --Creates memory storage.
   function Cv_Create_Mem_Storage (Blocksize : Integer := 0) return Cv_Mem_Storage_Ptr; -- 0 is about 64k...

   -- Creates child memory storage.
   function Cv_Create_Child_Mem_Storage (Parent : Cv_Mem_Storage_Ptr) return Cv_Mem_Storage_Ptr;

   -- Releases memory storage.
   procedure Cv_Release_Mem_Storage (Storage : access Cv_Mem_Storage_Ptr);

   -- Clears memory storage.
   procedure Cv_Clear_Mem_Storage (Storage : Cv_Mem_Storage_Ptr);

   -- Saves memory storage position.
   procedure Cv_Save_Mem_Storage_Pos (Storage : Cv_Mem_Storage_Ptr;
                                      Pos     : Cv_Mem_Storage_Pos_Ptr);

   -- Restores memory storage position.
   procedure Cv_Restore_Mem_Storage_Pos (Storage : Cv_Mem_Storage_Ptr;
                                         Pos     : Cv_Mem_Storage_Pos_Ptr);

   -- Allocates a memory buffer in a storage block.
   function Cv_Mem_Storage_Alloc (Storage : Cv_Mem_Storage_Ptr;
                                  Size    : Interfaces.C.Size_T) return Cv_Void_Ptr;

   -- Allocates a memory buffer in a storage block.
   function Cv_Mem_Storage_Alloc_String (Storage : Cv_Mem_Storage_Ptr;
                                         Ptr     : String;
                                         Len     : Integer := -1) return Cv_String;

   --Creates a sequence.
   function Cv_Create_Seq (Seqflags   : Integer; -- No clue what this could be...
                           Headersize : Integer;
                           Elemsize   : Integer;
                           Storage    : Cv_Mem_Storage_Ptr) return Cv_Seq_Ptr;

   -- Sets up sequence block size.
   procedure Cv_Set_Seq_Block_Size (Seq        : Cv_Seq_Ptr;
                                    Deltaelems : Integer);

   -- Adds an element to the end of a sequence.
   function Cv_Seq_Push (Seq     : Cv_Seq_Ptr;
                         Element : Cv_Void_Ptr := null) return Cv_Void_Ptr;

   -- Adds an element to the beginning of a sequence.
   function Cv_Seq_Push_Front (Seq     : Cv_Seq_Ptr;
                               Element : Cv_Void_Ptr := null) return Cv_Void_Ptr;

   -- Removes an element from the end of a sequence.
   procedure Cv_Seq_Pop (Seq     : Cv_Seq_Ptr;
                         Element : Cv_Void_Ptr := null);

   -- Removes an element from the beginning of a sequence.
   procedure Cv_Seq_Pop_Front (Seq     : Cv_Seq_Ptr;
                               Element : Cv_Void_Ptr := null);



   -- Pushes several elements to either end of a sequence.
   procedure Cv_Seq_Push_Multi (Seq      : Cv_Seq_Ptr;
                                Elements : Cv_Void_Ptr; -- maybe not
                                Count    : Integer;
                                Infron   : Integer := 0);

   -- Removes several elements from either end of a sequence.
   procedure Cv_Seq_Pop_Multi (Seq      : Cv_Seq_Ptr;
                               Elements : Cv_Void_Ptr;
                               Count    : Integer;
                               Infront  : Integer := 0);

   -- Inserts an element in the middle of a sequence.
   function Cv_Seq_Insert (Seq         : Cv_Seq_Ptr;
                           Beforeindex : Integer;
                           Element     : Cv_Void_Ptr := null) return Cv_Void_Ptr; --return access Character;

   --Removes an element from the middle of a sequence.
   procedure Cv_Seq_Remove (Seq   : Cv_Seq_Ptr;
                            Index : Integer);

   --Clears a sequence.
   procedure Cv_Clear_Seq (Seq : Cv_Seq_Ptr);

   -- Returns a pointer to a sequence element according to its index.
   function Cv_Get_Seq_Elem (Seq   : Cv_Seq_Ptr;
                             Index : Integer) return Cv_Void_Ptr; --return access Character;

   -- Returns the index of a specific sequence element.
   function Cv_Seq_Elem_Idx (Seq     : Cv_Seq_Ptr;
                             Element : Cv_Void_Ptr;
                             Block   : access Cv_Seq_Block_Ptr := null) return Integer;

   -- Initializes the process of writing data to a sequence.
   procedure Cv_Start_Append_To_Seq (Seq    : Cv_Seq_Ptr;
                                     Writer : Cv_Seq_Writer_Ptr);

   -- Creates a new sequence and initializes a writer for it.
   procedure Cv_Start_Write_Seq (Seqflags   : Integer;
                                 Headersize : Integer;
                                 Elemsize   : Integer;
                                 Storage    : Cv_Mem_Storage_Ptr;
                                 Writer     : Cv_Seq_Writer_Ptr);

   -- Finishes the process of writing a sequence.
   function Cv_End_Write_Seq ( Writer : Cv_Seq_Writer_Ptr) return Cv_Seq_Ptr;

   -- Updates sequence headers from the writer.
   procedure Cv_Flush_Seq_Writer (Writer : Cv_Seq_Writer_Ptr);

   -- Initializes the process of sequential reading from a sequence
   procedure Cv_Start_Read_Seq (Seq       : Cv_Seq_Ptr;
                                Reader    : Cv_Seq_Reader_Ptr;
                                Isreverse : Integer := 0);

   -- Returns the current reader position.
   function Cv_Get_Seq_Reader_Pos (Reader : Cv_Seq_Reader_Ptr) return Integer;

   --Moves the reader to the specified position.
   procedure Cv_Set_Seq_Reader_Pos (Reader     : Cv_Seq_Reader_Ptr;
                                    Index      : Integer;
                                    Isrelative : Integer := 0);

   -- Copies a sequence to one continuous block of memory.
   function Cv_Cvt_Seq_To_Array (Seq      : Cv_Seq_Ptr;
                                 Elements : Cv_Void_Ptr;
                                 Slice    : Cv_Slice := Cv_Create_Slice (0)) return Cv_Void_Ptr;

   --Constructs a sequence header for an array.
   function Cv_Make_Seq_Header_For_Array (Seqtype    : Integer;
                                          Headersize : Integer;
                                          Elemsize   : Integer;
                                          Elements   : Cv_Void_Ptr;
                                          Seq        : Cv_Seq_Ptr;
                                          Block      : Cv_Seq_Block_Ptr) return Cv_Seq_Ptr;

   -- Makes a separate header for a sequence slice.
   function Cv_Seq_Slice (Seq      : Cv_Seq_Ptr;
                          Slice    : Cv_Slice;
                          Storage  : Cv_Mem_Storage_Ptr := null;
                          Copydata : Integer := 0) return Cv_Seq_Ptr;

   -- Creates a copy of a sequence.
   procedure Cv_Clone_Seq (Seq     : Cv_Seq_Ptr;
                           Storage : Cv_Mem_Storage_Ptr := null);

   -- Removes a sequence slice.
   procedure Cv_Seq_Remove_Slice (Seq   : Cv_Seq_Ptr;
                                  Slice : Cv_Slice_Ptr);

   -- Inserts an array in the middle of a sequence.
   procedure Cv_Seq_Insert_Slice (Seq          : Cv_Seq_Ptr;
                                  Before_Index : Integer;
                                  From_Arr     : Cv_Arr_Ptr);
   procedure Cv_Seq_Insert_Slice (Seq          : Cv_Seq_Ptr;
                                  Before_Index : Integer;
                                  From_Arr     : Cv_Mat_Ptr);
   procedure Cv_Seq_Insert_Slice (Seq          : Cv_Seq_Ptr;
                                  Before_Index : Integer;
                                  From_Arr     : Ipl_Image_Ptr);

   -- Sorts sequence element using the specified comparison function.
   procedure Cv_Seq_Sort (Seq      : Cv_Seq_Ptr;
                          Func     : Cv_Cmp_Func;
                          Userdata : Cv_Void_Ptr := null);


   -- Searches for an element in a sequence.
   function Cv_Seq_Search (Seq         : Cv_Seq_Ptr;
                           Elem        : Cv_Void_Ptr;
                           Func        : Cv_Cmp_Func;
                           Issorted    : Integer;
                           Elemidx     : access Integer;
                           Userdata    : Cv_Void_Ptr := null) return Cv_Void_Ptr; --return access Character;

   -- Reverses the order of sequence elements.
   procedure Cv_Seq_Invert (Seq : Cv_Seq_Ptr);

   --     Splits a sequence into equivalency classes.
   function Cv_Seq_Partition (Seq      : Cv_Seq_Ptr;
                              Storage  : Cv_Mem_Storage_Ptr;
                              Labels   : access Cv_Seq_Ptr;
                              Is_Equal : Cv_Cmp_Func;
                              Userdata : Cv_Void_Ptr)
                              return Integer;

   procedure Cv_Change_Seq_Block (Reader    : Cv_Void_Ptr;
                                  Direction : Integer) renames Core.Cv_Change_Seq_Block;
   procedure Cv_Create_Seq_Block (Writer : Cv_Seq_Writer_Ptr) renames Core.Cv_Create_Seq_Block;

   -- Creates an empty set.
   function Cv_Create_Set ( Setflags  : Integer;
                           Headersize : Integer;
                           Elemsize   : Integer;
                           Storage    : Cv_Mem_Storage_Ptr) return Cv_Set_Ptr;

   -- Occupies a node in the set.
   function Cv_Set_Add (Setheader    : Cv_Set_Ptr;
                        Elem         : Cv_Set_Elem_Ptr := null;
                        Insertedelem : access Cv_Set_Elem_Ptr := null) return Integer;

   -- Adds an element to a set (fast variant).
   function Cv_Set_New (Setheader : Cv_Set_Ptr) return Cv_Set_Elem_Ptr;

   -- Removes a set element based on its pointer.
   procedure Cv_Set_Remove_By_Ptr (Setheader : Cv_Set_Ptr;
                                   Elem      : Cv_Void_Ptr);

   -- Removes an element from a set.
   procedure Cv_Set_Remove (Setheader : Cv_Set_Ptr;
                            Index     : Integer);

   -- Finds a set element by its index.
   function Cv_Get_Set_Elem (Setheader : Cv_Set_Ptr;
                             Index     : Integer) return Cv_Set_Elem_Ptr;

   -- Clears a set.
   procedure Cv_Clear_Set (Setheader : Cv_Set_Ptr);

   -- Creates an empty graph.
   function Cv_Create_Graph (Graphflags : Integer;
                             Headersize : Integer;
                             Vtxsize    : Integer;
                             Edgesize   : Integer;
                             Storage    : Cv_Mem_Storage_Ptr) return Cv_Graph_Ptr;

   -- Adds a vertex to a graph.
   function Cv_Graph_Add_Vtx (Graph       : Cv_Graph_Ptr;
                              Vtx         : Cv_Graph_Vtx_Ptr := null;
                              Insertedvtx : access Cv_Graph_Vtx_Ptr := null) return Integer;

   -- Removes a vertex from a graph.
   function Cv_Graph_Remove_Vtx (Graph : Cv_Graph_Ptr;
                                 Index : Integer) return Integer;

   --Removes a vertex from a graph by using its pointer.
   function Cv_Graph_Remove_Vtx_By_Ptr (Graph : Cv_Graph_Ptr;
                                        Vtx   : Cv_Graph_Vtx_Ptr) return Integer;

   -- Adds an edge to a graph.
   function Cv_Graph_Add_Edge (Graph        : Cv_Graph_Ptr;
                               Startidx     : Integer;
                               Endidx       : Integer;
                               Edge         : Cv_Graph_Edge_Ptr := null;
                               Insertededge : access Cv_Graph_Edge_Ptr := null) return Integer;

   -- Adds an edge to a graph by using its pointer.
   function Cv_Graph_Add_Edge_By_Ptr (Graph        : Cv_Graph_Ptr;
                                      Startvtx     : Cv_Graph_Vtx_Ptr;
                                      Endvtx       : Cv_Graph_Vtx_Ptr;
                                      Edge         : Cv_Graph_Edge_Ptr := null;
                                      Insertededge : access Cv_Graph_Edge_Ptr := null) return Integer;

   -- Removes an edge from a graph.
   procedure Cv_Graph_Remove_Edge (Graph    : Cv_Graph_Ptr;
                                   Startidx : Integer;
                                   Endidx   : Integer);

   --Removes an edge from a graph by using its pointer.
   procedure Cv_Graph_Remove_Edge_By_Ptr (Graph    : Cv_Graph_Ptr;
                                          Startvtx : Cv_Graph_Vtx_Ptr;
                                          Endvtx   : Cv_Graph_Vtx_Ptr);

   -- Finds an edge in a graph.
   function Cv_Find_Graph_Edge (Graph    : Cv_Graph_Ptr;
                                Startidx : Integer;
                                Endidx   : Integer) return Cv_Graph_Edge_Ptr;
   function Cv_Graph_Find_Edge (Graph    : Cv_Graph_Ptr;
                                Startidx : Integer;
                                Endidx   : Integer) return Cv_Graph_Edge_Ptr renames Cv_Find_Graph_Edge;

   -- Finds an edge in a graph by using its pointer.
   function Cv_Find_Graph_Edge_By_Ptr (Graph    : Cv_Graph_Ptr;
                                       Startvtx : Cv_Graph_Vtx_Ptr;
                                       Endvtx   : Cv_Graph_Vtx_Ptr) return Cv_Graph_Edge_Ptr;
   function Cv_Graph_Find_Edge_By_Ptr (Graph    : Cv_Graph_Ptr;
                                       Startvtx : Cv_Graph_Vtx_Ptr;
                                       Endvtx   : Cv_Graph_Vtx_Ptr) return Cv_Graph_Edge_Ptr renames Cv_Find_Graph_Edge_By_Ptr;

   --Clears a graph.
   procedure Cv_Clear_Graph (Graph : Cv_Graph_Ptr);

   -- Counts the number of edges indicent to the vertex.
   function Cv_Graph_Vtx_Degree (Graph  : Cv_Graph_Ptr;
                                 Vtxldx : Integer) return Integer;

   -- Finds an edge in a graph.
   function Cv_Graph_Vtx_Degree_By_Ptr (Graph : Cv_Graph_Ptr;
                                        Vtx   : Cv_Graph_Vtx_Ptr) return Integer;

   -- Finds a graph vertex by using its index.
   function Cv_Get_Graph_Vtx (Graph  : Cv_Graph_Ptr;
                              Vtxidx : Integer) return Cv_Graph_Vtx_Ptr;

   -- Returns the index of a graph vertex.
   function Cv_Graph_Vtx_Idx (Graph : Cv_Graph_Ptr;
                              Vtx   : Cv_Graph_Vtx_Ptr) return Integer;

   -- Returns the index of a graph edge.
   function Cv_Graph_Edge_Idx (Graph : Cv_Graph_Ptr;
                               Edge  : Cv_Graph_Edge_Ptr) return Integer;

   --#define cvGraphGetVtxCount( graph ) ((graph)->active_count)
   function Cv_Graph_Get_Vtx_Count (Graph : Cv_Graph_Ptr) return Integer;
   --#define cvGraphGetEdgeCount( graph ) ((graph)->edges->active_count)
   function Cv_Graph_Get_Edge_Count (Graph : Cv_Graph_Ptr) return Integer;

   --     #define  CV_IS_GRAPH_VERTEX_VISITED(vtx) \
   --      (((CvGraphVtx*)(vtx))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
   function Cv_Is_Graph_Vertex_Visisted (Vtx : Cv_Graph_Vtx_Ptr) return Integer;

   --   #define  CV_IS_GRAPH_EDGE_VISITED(edge) \
   --      (((CvGraphEdge*)(edge))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
   function Cv_Is_Graph_Edge_Visited (Edge : Cv_Graph_Edge_Ptr) return Integer;



   -- Creates structure for depth-first graph traversal.
   function Cv_Create_Graph_Scanner (Graph : Cv_Graph_Ptr;
                                     Vtx   : Cv_Graph_Vtx_Ptr := null; --null = start from beginning
                                     Mask  : Integer := Cv_Graph_All_Items) return Cv_Graph_Scanner_Ptr;

   -- Completes the graph traversal procedure.
   procedure Cv_Release_Graph_Scanner (Scanner : access Cv_Graph_Scanner_Ptr);

   -- Executes one or more steps of the graph traversal procedure.
   function Cv_Next_Graph_Item (Scanner : Cv_Graph_Scanner_Ptr) return Integer;

   -- Clones a graph.
   procedure Cv_Clone_Graph (Graph   : Cv_Graph_Ptr;
                             Storage : Cv_Mem_Storage_Ptr);

   -----------------------------------------------------------------------------
   -- DrawingCV_RGB
   -----------------------------------------------------------------------------
   -- Creates a Cv_Scalar color from RGB values
   function Cv_Rgb (R : Integer;
                    G : Integer;
                    B : Integer) return Cv_Scalar;

   -- Draws a line segment connecting two points.
   procedure Cv_Line (Img       : Cv_Arr_Ptr;
                      Pt1       : Cv_Point;
                      Pt2       : Cv_Point;
                      Color     : Cv_Scalar;
                      Thickness : Integer := 1;
                      Line_Type : Integer := 8;
                      Shift     : Integer := 0);
   procedure Cv_Line (Img       : Cv_Mat_Ptr;
                      Pt1       : Cv_Point;
                      Pt2       : Cv_Point;
                      Color     : Cv_Scalar;
                      Thickness : Integer := 1;
                      Line_Type : Integer := 8;
                      Shift     : Integer := 0);
   procedure Cv_Line (Img       : Ipl_Image_Ptr;
                      Pt1       : Cv_Point;
                      Pt2       : Cv_Point;
                      Color     : Cv_Scalar;
                      Thickness : Integer := 1;
                      Line_Type : Integer := 8;
                      Shift     : Integer := 0);

   -- Draws a simple, thick, or filled rectangle.
   procedure Cv_Rectangle (Img       : Cv_Arr_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer :=  1;
                           Linetype  : Integer := 8;
                           Shift     : Integer := 0);
   procedure Cv_Rectangle (Img       : Cv_Mat_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer :=  1;
                           Linetype  : Integer := 8;
                           Shift     : Integer := 0);
   procedure Cv_Rectangle (Img       : Ipl_Image_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer :=  1;
                           Linetype  : Integer := 8;
                           Shift     : Integer := 0);


   -- /* Draws a rectangle specified by a CvRect structure */
   procedure Cv_Rectangle_R (Img       : Cv_Arr_Ptr;
                             R         : Cv_Rect;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0);
   procedure Cv_Rectangle_R (Img       : Cv_Mat_Ptr;
                             R         : Cv_Rect;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0);
   procedure Cv_Rectangle_R (Img       : Ipl_Image_Ptr;
                             R         : Cv_Rect;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0);

   -- Draws a circle.
   procedure Cv_Circle (Img       : Cv_Arr_Ptr;
                        Center    : Cv_Point;
                        Radius    : Integer;
                        Color     : Cv_Scalar;
                        Thickness : Integer := 1;
                        Linetype  : Integer := 8;
                        Shift     : Integer := 0);
   procedure Cv_Circle (Img       : Cv_Mat_Ptr;
                        Center    : Cv_Point;
                        Radius    : Integer;
                        Color     : Cv_Scalar;
                        Thickness : Integer := 1;
                        Linetype  : Integer := 8;
                        Shift     : Integer := 0);
   procedure Cv_Circle (Img       : Ipl_Image_Ptr;
                        Center    : Cv_Point;
                        Radius    : Integer;
                        Color     : Cv_Scalar;
                        Thickness : Integer := 1;
                        Linetype  : Integer := 8;
                        Shift     : Integer := 0);

   -- Draws a simple or thick elliptic arc or an fills ellipse sector.
   procedure Cv_Ellipse (Img        : Cv_Arr_Ptr;
                         Center     : Cv_Point;
                         Axes       : Cv_Size;
                         Angle      : Long_Float;
                         Startangle : Long_Float;
                         Endangle   : Long_Float;
                         Color      : Cv_Scalar;
                         Thickness  : Integer := 1;
                         Linetype   : Integer := 8;
                         Shift      : Integer := 0);
   procedure Cv_Ellipse (Img        : Cv_Mat_Ptr;
                         Center     : Cv_Point;
                         Axes       : Cv_Size;
                         Angle      : Long_Float;
                         Startangle : Long_Float;
                         Endangle   : Long_Float;
                         Color      : Cv_Scalar;
                         Thickness  : Integer := 1;
                         Linetype   : Integer := 8;
                         Shift      : Integer := 0);
   procedure Cv_Ellipse (Img        : Ipl_Image_Ptr;
                         Center     : Cv_Point;
                         Axes       : Cv_Size;
                         Angle      : Long_Float;
                         Startangle : Long_Float;
                         Endangle   : Long_Float;
                         Color      : Cv_Scalar;
                         Thickness  : Integer := 1;
                         Linetype   : Integer := 8;
                         Shift      : Integer := 0);

   -- Draws a simple or thick elliptic arc or fills an ellipse sector.
   procedure Cv_Ellipse_Box (Img       : Cv_Arr_Ptr;
                             Box       : Cv_Box_2d;
                             Color     : Cv_Scalar;
                             Thickness : Integer;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0);
   procedure Cv_Ellipse_Box (Img       : Cv_Mat_Ptr;
                             Box       : Cv_Box_2d;
                             Color     : Cv_Scalar;
                             Thickness : Integer;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0);
   procedure Cv_Ellipse_Box (Img       : Ipl_Image_Ptr;
                             Box       : Cv_Box_2d;
                             Color     : Cv_Scalar;
                             Thickness : Integer;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0);

   -- Fills a convex polygon.
   procedure Cv_Fill_Convex_Poly (Img      : Cv_Arr_Ptr;
                                  Pts      : Cv_Point_Array;
                                  Npts     : Integer;
                                  Color    : Cv_Scalar;
                                  Linetype : Integer := 8;
                                  Shift    : Integer := 0);
   procedure Cv_Fill_Convex_Poly (Img      : Cv_Mat_Ptr;
                                  Pts      : Cv_Point_Array;
                                  Npts     : Integer;
                                  Color    : Cv_Scalar;
                                  Linetype : Integer := 8;
                                  Shift    : Integer := 0);
   procedure Cv_Fill_Convex_Poly (Img      : Ipl_Image_Ptr;
                                  Pts      : Cv_Point_Array;
                                  Npts     : Integer;
                                  Color    : Cv_Scalar;
                                  Linetype : Integer := 8;
                                  Shift    : Integer := 0);

   -- Fills a polygon's interior.
   procedure Cv_Fill_Poly (Img      : Cv_Arr_Ptr;
                           Pts      : Cv_Point_Pointer_Array;
                           Npts     : Cv_32u_Array;
                           Contours : Integer;
                           Color    : Cv_Scalar;
                           Linetype : Integer := 8;
                           Shift    : Integer := 0);
   procedure Cv_Fill_Poly (Img      : Cv_Mat_Ptr;
                           Pts      : Cv_Point_Pointer_Array;
                           Npts     : Cv_32u_Array;
                           Contours : Integer;
                           Color    : Cv_Scalar;
                           Linetype : Integer := 8;
                           Shift    : Integer := 0);
   procedure Cv_Fill_Poly (Img      : Ipl_Image_Ptr;
                           Pts      : Cv_Point_Pointer_Array;
                           Npts     : Cv_32u_Array;
                           Contours : Integer;
                           Color    : Cv_Scalar;
                           Linetype : Integer := 8;
                           Shift    : Integer := 0);

   -- Draws simple or thick polygons.
   procedure Cv_Poly_Line (Img       : Cv_Arr_Ptr;
                           Pts       : Cv_Point_Pointer_Array;
                           Npts      : Cv_32u_Array;
                           Contours  : Integer;
                           Isclosed  : Integer;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           Linetyoe  : Integer := 8;
                           Shift     : Integer := 0);
   procedure Cv_Poly_Line (Img       : Cv_Mat_Ptr;
                           Pts       : Cv_Point_Pointer_Array;
                           Npts      : Cv_32u_Array;
                           Contours  : Integer;
                           Isclosed  : Integer;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           Linetyoe  : Integer := 8;
                           Shift     : Integer := 0);
   procedure Cv_Poly_Line (Img       : Ipl_Image_Ptr;
                           Pts       : Cv_Point_Pointer_Array;
                           Npts      : Cv_32u_Array;
                           Contours  : Integer;
                           Isclosed  : Integer;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           Linetyoe  : Integer := 8;
                           Shift     : Integer := 0);

   procedure Cv_Draw_Rect (Img       : Cv_Arr_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer :=  1;
                           Linetype  : Integer := 8;
                           Shift     : Integer := 0) renames Cv_Rectangle;
   procedure Cv_Draw_Rect (Img       : Cv_Mat_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer :=  1;
                           Linetype  : Integer := 8;
                           Shift     : Integer := 0) renames Cv_Rectangle;
   procedure Cv_Draw_Rect (Img       : Ipl_Image_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer :=  1;
                           Linetype  : Integer := 8;
                           Shift     : Integer := 0) renames Cv_Rectangle;

   procedure Cv_Draw_Line (Img       : Cv_Arr_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           Line_Type : Integer := 8;
                           Shift     : Integer := 0) renames Cv_Line;
   procedure Cv_Draw_Line (Img       : Cv_Mat_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           Line_Type : Integer := 8;
                           Shift     : Integer := 0) renames Cv_Line;
   procedure Cv_Draw_Line (Img       : Ipl_Image_Ptr;
                           Pt1       : Cv_Point;
                           Pt2       : Cv_Point;
                           Color     : Cv_Scalar;
                           Thickness : Integer := 1;
                           Line_Type : Integer := 8;
                           Shift     : Integer := 0) renames Cv_Line;

   procedure Cv_Draw_Circle (Img       : Cv_Arr_Ptr;
                             Center    : Cv_Point;
                             Radius    : Integer;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0) renames Cv_Circle;
   procedure Cv_Draw_Circle (Img       : Cv_Mat_Ptr;
                             Center    : Cv_Point;
                             Radius    : Integer;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0) renames Cv_Circle;
   procedure Cv_Draw_Circle (Img       : Ipl_Image_Ptr;
                             Center    : Cv_Point;
                             Radius    : Integer;
                             Color     : Cv_Scalar;
                             Thickness : Integer := 1;
                             Linetype  : Integer := 8;
                             Shift     : Integer := 0) renames Cv_Circle;

   procedure Cv_Draw_Ellipse (Img        : Cv_Arr_Ptr;
                              Center     : Cv_Point;
                              Axes       : Cv_Size;
                              Angle      : Long_Float;
                              Startangle : Long_Float;
                              Endangle   : Long_Float;
                              Color      : Cv_Scalar;
                              Thickness  : Integer := 1;
                              Linetype   : Integer := 8;
                              Shift      : Integer := 0) renames Cv_Ellipse;
   procedure Cv_Draw_Ellipse (Img        : Cv_Mat_Ptr;
                              Center     : Cv_Point;
                              Axes       : Cv_Size;
                              Angle      : Long_Float;
                              Startangle : Long_Float;
                              Endangle   : Long_Float;
                              Color      : Cv_Scalar;
                              Thickness  : Integer := 1;
                              Linetype   : Integer := 8;
                              Shift      : Integer := 0) renames Cv_Ellipse;
   procedure Cv_Draw_Ellipse (Img        : Ipl_Image_Ptr;
                              Center     : Cv_Point;
                              Axes       : Cv_Size;
                              Angle      : Long_Float;
                              Startangle : Long_Float;
                              Endangle   : Long_Float;
                              Color      : Cv_Scalar;
                              Thickness  : Integer := 1;
                              Linetype   : Integer := 8;
                              Shift      : Integer := 0) renames Cv_Ellipse;

   procedure Cv_Draw_Poly_Line (Img       : Cv_Arr_Ptr;
                                Pts       : Cv_Point_Pointer_Array;
                                Npts      : Cv_32u_Array;
                                Contours  : Integer;
                                Isclosed  : Integer;
                                Color     : Cv_Scalar;
                                Thickness : Integer := 1;
                                Linetyoe  : Integer := 8;
                                Shift     : Integer := 0) renames Cv_Poly_Line;
   procedure Cv_Draw_Poly_Line (Img       : Cv_Mat_Ptr;
                                Pts       : Cv_Point_Pointer_Array;
                                Npts      : Cv_32u_Array;
                                Contours  : Integer;
                                Isclosed  : Integer;
                                Color     : Cv_Scalar;
                                Thickness : Integer := 1;
                                Linetyoe  : Integer := 8;
                                Shift     : Integer := 0) renames Cv_Poly_Line;
   procedure Cv_Draw_Poly_Line (Img       : Ipl_Image_Ptr;
                                Pts       : Cv_Point_Pointer_Array;
                                Npts      : Cv_32u_Array;
                                Contours  : Integer;
                                Isclosed  : Integer;
                                Color     : Cv_Scalar;
                                Thickness : Integer := 1;
                                Linetyoe  : Integer := 8;
                                Shift     : Integer := 0) renames Cv_Poly_Line;

   -- Clips the line against the image rectangle.
   function Cv_Clip_Line (Imgsize : Cv_Size;
                          Pt1     : access Cv_Point;
                          Pt2     : access Cv_Point) return Integer;

   -- Initializes the line iterator.
   function Cv_Init_Line_Iterator (Image          : Cv_Arr_Ptr;
                                   Pt1            : Cv_Point;
                                   Pt2            : Cv_Point;
                                   Line_Iterator  : access Cv_Line_Iterator;
                                   Connectivity   : Integer := 8;
                                   Left_To_Right  : Integer := 0) return Integer;
   function Cv_Init_Line_Iterator (Image          : Cv_Mat_Ptr;
                                   Pt1            : Cv_Point;
                                   Pt2            : Cv_Point;
                                   Line_Iterator  : access Cv_Line_Iterator;
                                   Connectivity   : Integer := 8;
                                   Left_To_Right  : Integer := 0) return Integer;
   function Cv_Init_Line_Iterator (Image          : Ipl_Image_Ptr;
                                   Pt1            : Cv_Point;
                                   Pt2            : Cv_Point;
                                   Line_Iterator  : access Cv_Line_Iterator;
                                   Connectivity   : Integer := 8;
                                   Left_To_Right  : Integer := 0) return Integer;

   procedure Cv_Next_Line_Point (Lineiterator : Cv_Line_Iterator);

   -- Fonts



   -- Initializes font structure.
   procedure Cv_Init_Font (Font      : access Cv_Font;
                           Fontface  : Cv_Font_Face;
                           Hscale    : Long_Float;
                           Vscale    : Long_Float;
                           Shear     : Long_Float := 0.0;
                           Thickness : Integer := 1;
                           Linetype  : Integer := 8);

   function Cv_Create_Font (Scale     : Long_Float;
                            Thickness : Integer := 1) return Cv_Font;

   procedure Cv_Put_Text (Img   : Cv_Arr_Ptr;
                          Text  : String;
                          Org   : Cv_Point;
                          Font  : Cv_Font_Ptr;
                          Color : Cv_Scalar);
   procedure Cv_Put_Text (Img   : Cv_Mat_Ptr;
                          Text  : String;
                          Org   : Cv_Point;
                          Font  : access Cv_Font;
                          Color : Cv_Scalar);
   procedure Cv_Put_Text (Img   : Ipl_Image_Ptr;
                          Text  : String;
                          Org   : Cv_Point;
                          Font  : access Cv_Font;
                          Color : Cv_Scalar);

   -- Retrieves the width and height of a text string.
   procedure Cv_Get_Text_Size (Textstring : String;
                               Font       : Cv_Font;
                               Textsize   : access Cv_Size;
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
                                Arcstart    : Integer;
                                Arcend      : Integer;
                                Pts         : Cv_Point_Array;
                                Deltaval    : Integer) return Integer;

   -- Draws contour outlines or interiors in an image.
   procedure Cv_Draw_Contours (Img           : Cv_Arr_Ptr;
                               Contour       : Cv_Seq_Ptr;
                               Externalcolor : Cv_Scalar;
                               Holecolor     : Cv_Scalar;
                               Maxlevel      : Integer;
                               Thickness     : Integer := 1;
                               Linetype      : Integer := 8;
                               Offset        : Cv_Point := Cv_Create_Point (0, 0));
   procedure Cv_Draw_Contours (Img           : Cv_Mat_Ptr;
                               Contour       : Cv_Seq_Ptr;
                               Externalcolor : Cv_Scalar;
                               Holecolor     : Cv_Scalar;
                               Maxlevel      : Integer;
                               Thickness     : Integer := 1;
                               Linetype      : Integer := 8;
                               Offset        : Cv_Point := Cv_Create_Point (0, 0));
   procedure Cv_Draw_Contours (Img           : Ipl_Image_Ptr;
                               Contour       : Cv_Seq_Ptr;
                               Externalcolor : Cv_Scalar;
                               Holecolor     : Cv_Scalar;
                               Maxlevel      : Integer;
                               Thickness     : Integer := 1;
                               Linetype      : Integer := 8;
                               Offset        : Cv_Point := Cv_Create_Point (0, 0));

   --     Performs a look-up table transform of an array.
   procedure Cv_Lut (Src : Cv_Arr_Ptr;
                     Dst : Cv_Arr_Ptr;
                     Lut : Cv_Arr_Ptr);
   procedure Cv_Lut (Src : Cv_Mat_Ptr;
                     Dst : Cv_Mat_Ptr;
                     Lut : Cv_Mat_Ptr);
   procedure Cv_Lut (Src : Ipl_Image_Ptr;
                     Dst : Ipl_Image_Ptr;
                     Lut : Ipl_Image_Ptr);

   -----------------------------------------------------------------------------
   -- Iteration through the sequence tree
   -----------------------------------------------------------------------------
   -- Initializes the tree node iterator.
   procedure Cv_Init_Tree_Node_Iterator (Treeiterator : Cv_Tree_Node_Iterator_Ptr;
                                         First        : Cv_Void_Ptr;
                                         Maxlevel     : Integer);

   -- Returns the currently observed node and moves the iterator toward the next node.
   function Cv_Next_Tree_Node (Treeiterator : Cv_Tree_Node_Iterator_Ptr) return Cv_Void_Ptr;

   -- Returns the currently observed node and moves the iterator toward the previous node.
   function Cv_Prev_Tree_Node (Treeiterator : Cv_Tree_Node_Iterator_Ptr) return Cv_Void_Ptr;

   --Adds a new node to a tree.
   procedure Cv_Insert_Node_Into_Tree (Node   : Cv_Void_Ptr;
                                       Parent : Cv_Void_Ptr;
                                       Fram   : Cv_Void_Ptr);

   --/* Removes contour from tree (together with the contour children). */
   procedure Cv_Remove_Node_From_Tree (Node  : Cv_Void_Ptr;
                                       Frame : Cv_Void_Ptr);

   --Gathers all node pointers to a single sequence.
   function Cv_Tree_To_Node_Seq (First      : Cv_Void_Ptr;
                                 Headersize : Integer;
                                 Storage    : Cv_Mem_Storage_Ptr) return Cv_Seq_Ptr;

   function Cv_K_Means2 (Samples     : Cv_Arr_Ptr;
                         Labels      : Cv_Arr_Ptr;
                         Termcrit    : Cv_Term_Criteria;
                         Attempts    : Integer := 1;
                         Rng         : Integer_64 := 0;
                         Flags       : Unsigned_32 := 0;
                         Centers     : Cv_Arr_Ptr := null;
                         Compactness : access Long_Float := null) return Integer;
   function Cv_K_Means2 (Samples     : Cv_Mat_Ptr;
                         Labels      : Cv_Mat_Ptr;
                         Termcrit    : Cv_Term_Criteria;
                         Attempts    : Integer := 1;
                         Rng         : Integer_64 := 0;
                         Flags       : Unsigned_32 := 0;
                         Centers     : Cv_Mat_Ptr := null;
                         Compactness : access Long_Float := null) return Integer;
   function Cv_K_Means2 (Samples     : Ipl_Image_Ptr;
                         Labels      : Ipl_Image_Ptr;
                         Termcrit    : Cv_Term_Criteria;
                         Attempts    : Integer := 1;
                         Rng         : Integer_64 := 0;
                         Flags       : Unsigned_32 := 0;
                         Centers     : Ipl_Image_Ptr := null;
                         Compactness : access Long_Float := null) return Integer;
   -----------------------------------------------------------------------------
   -- System Functions
   -----------------------------------------------------------------------------
   --     Registers another module.
   function Cv_Register_Module (Module_Info : Cv_Module_Info_Ptr)
                                return Integer;

   --     Switches between optimized/non-optimized modes.
   function Cv_Use_Optimized (Onoff : Integer)
                              return Integer;

   --     Retrieves information about registered module(s) and plugins.
   procedure Cv_Get_Module_Info (Module_Name          : Interfaces.C.Strings.Chars_Ptr;
                                 Version              : access Interfaces.C.Strings.Chars_Ptr;
                                 Loaded_Addon_Plugins : access Interfaces.C.Strings.Chars_Ptr);



   --     Accesses custom/default memory managing functions.
   procedure Cv_Set_Memory_Manager (Alloc_Func : Cv_Alloc_Func := null;
                                    Free_Func  : Cv_Free_Func := null;
                                    Userdata   : Cv_Void_Ptr := null);



   --     Switches to IPL functions for image allocation/deallocation.
   procedure Cv_Set_Ipl_Allocators (Create_Header : Cv_Ipl_Create_Image_Header;
                                    Allocate_Data : Cv_Ipl_Allocate_Image_Data;
                                    Deallocate    : Cv_Ipl_Deallocate;
                                    Create_Roi    : Cv_Ipl_Create_Roi;
                                    Clone_Image   : Cv_Ipl_Clone_Image);

   -- N/A
   procedure Cv_Turn_On_Ipl_Compatibility;

   -----------------------------------------------------------------------------
   -- Data Persistence
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- High-level functions
   -----------------------------------------------------------------------------
   --     Opens file storage for reading or writing data.
   function Cv_Open_File_Storage (Fílename   : Interfaces.C.Strings.Chars_Ptr;
                                  Memstorage : Cv_Mem_Storage_Ptr;
                                  Flags      : Unsigned_32)
                                  return Cv_File_Storage_Ptr;

   --     Releases file storage.
   procedure Cv_Release_File_Storage (Fs : access Cv_File_Storage_Ptr);

   --/* returns attribute value or 0 (NULL) if there is no such attribute */
   function Cv_Attr_Value (Attr       : Cv_Attr_List_Ptr;
                           Attr_Name  : Interfaces.C.Strings.Chars_Ptr) return Interfaces.C.Strings.Chars_Ptr;

   --     Starts writing a new structure.
   procedure Cv_Start_Write_Struct (Fs           : Cv_File_Storage_Ptr;
                                    Name         : Interfaces.C.Strings.Chars_Ptr;
                                    Struct_Flags : Unsigned_32;
                                    Type_Name    : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr;
                                    Attributes   : Cv_Attr_List := Cv_Create_Attr_List);

   --     Ends the writing of a structure.
   procedure Cv_End_Write_Struct (Fs : Cv_File_Storage_Ptr);

   --     Writes an integer value.
   procedure Cv_Write_Int (Fs    : Cv_File_Storage_Ptr;
                           Name  : Interfaces.C.Strings.Chars_Ptr;
                           Value : Integer);

   --     Writes a floating-point value.
   procedure Cv_Write_Real (Fs    : Cv_File_Storage_Ptr;
                            Name  : Interfaces.C.Strings.Chars_Ptr;
                            Value : Long_Float);

   --     Writes a text string.
   procedure Cv_Write_String (Fs    : Cv_File_Storage_Ptr;
                              Name  : Interfaces.C.Strings.Chars_Ptr;
                              Str   : Interfaces.C.Strings.Chars_Ptr;
                              Quote : Integer := 0);

   --     Writes a comment.
   procedure Cv_Write_Comment (Fs          : Cv_File_Storage_Ptr;
                               Comment     : Interfaces.C.Strings.Chars_Ptr;
                               Eol_Comment : Integer);

   --     Writes a user object.
   procedure Cv_Write (Fs         : Cv_File_Storage_Ptr;
                       Name       : Interfaces.C.Strings.Chars_Ptr;
                       Ptr        : Cv_Void_Ptr;
                       Attributes : Cv_Attr_List := Cv_Create_Attr_List);

   --     Starts the next stream.
   procedure Cv_Start_Next_Stream (Fs : Cv_File_Storage_Ptr);

   --     Writes multiple numbers.
   procedure Cv_Write_Raw_Data (Fs  : Cv_File_Storage_Ptr;
                                Src : Cv_Void_Ptr;
                                Len : Integer;
                                Dt  : Interfaces.C.Strings.Chars_Ptr);

   --     Returns a unique pointer for a given name.
   function Cv_Get_Hashed_Key (Fs             : Cv_File_Storage_Ptr;
                               Name           : String_C;
                               Len            : Integer := -1;
                               Create_Missing : Integer := 0)
                               return Cv_String_Hash_Node_Ptr;

   --     Retrieves one of the top-level nodes of the file storage.
   function Cv_Get_Root_File_Node (Fs           : Cv_File_Storage_Ptr;
                                   Stream_Index : Integer := 0)
                                   return Cv_File_Node_Ptr;

   --     Finds a node in a map or file storage.
   function Cv_Get_File_Node (Fs             : Cv_File_Storage_Ptr;
                              Map            : Cv_File_Node_Ptr;
                              Key            : Cv_String_Hash_Node_Ptr;
                              Create_Missing : Integer := 0)
                              return Cv_File_Node_Ptr;

   --     Finds a node in a map or file storage.
   function Cv_Get_File_Node_By_Name (Fs   : Cv_File_Storage_Ptr;
                                      Map  : Cv_File_Node_Ptr;
                                      Name : Interfaces.C.Strings.Chars_Ptr)
                                      return Cv_File_Node_Ptr;

   --     Finds a file node and returns its value.
   function Cv_Read_Int (Node          : Cv_File_Node_Ptr;
                         Default_Value : Integer := 0)
                         return Integer;

   function Cv_Read_Int_By_Name (Fs            : Cv_File_Storage_Ptr;
                                 Map           : Cv_File_Node_Ptr ;
                                 Name          : Interfaces.C.Strings.Chars_Ptr;
                                 Default_Value : Integer := 0) return Integer;

   --     Retrieves a floating-point value from a file node.
   function Cv_Read_Real (Node          : Cv_File_Node_Ptr;
                          Default_Value : Long_Float := 0.0)
                          return Long_Float;

   --     Finds a file node and returns its value.
   function Cv_Read_Real_By_Name (Fs            : Cv_File_Storage_Ptr;
                                  Map           : Cv_File_Node_Ptr;
                                  Name          : Interfaces.C.Strings.Chars_Ptr;
                                  Default_Value : Long_Float := 0.0)
                                  return Long_Float;

   --     Retrieves a text string from a file node.
   function Cv_Read_String (Node          : Cv_File_Node_Ptr;
                            Default_Value : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr)
                            return Interfaces.C.Strings.Chars_Ptr;

   --     Finds a file node by its name and returns its value.
   function Cv_Read_String_By_Name (Fs            : Cv_File_Storage_Ptr;
                                    Map           : Cv_File_Node_Ptr;
                                    Name          : Interfaces.C.Strings.Chars_Ptr;
                                    Default_Value : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.Null_Ptr)
                                    return Interfaces.C.Strings.Chars_Ptr;

   --     Decodes an object and returns a pointer to it.
   function Cv_Read (Fs         : Cv_File_Storage_Ptr;
                     Node       : Cv_File_Node_Ptr;
                     Attributes : access Cv_Attr_List := null)
                     return Cv_Void_Ptr;

   --     Finds an object by name and decodes it.
   function Cv_Read_By_Name (Fs         : Cv_File_Storage_Ptr;
                             Map        : Cv_File_Node_Ptr;
                             Name       : Interfaces.C.Strings.Chars_Ptr;
                             Attributes : access Cv_Attr_List := null)
                             return Cv_Void_Ptr;

   --     Initializes the file node sequence reader.
   procedure Cv_Start_Read_Raw_Data (Fs     : Cv_File_Storage_Ptr;
                                     Src    : Cv_File_Node_Ptr;
                                     Reader : Cv_Seq_Reader_Ptr);

   --     Initializes file node sequence reader.
   procedure Cv_Read_Raw_Data_Slice (Fs     : Cv_File_Storage_Ptr;
                                     Reader : Cv_Seq_Reader_Ptr;
                                     Count  : Integer;
                                     Dst    : Cv_Void_Ptr;
                                     Dt     : Interfaces.C.Strings.Chars_Ptr);

   --     Reads multiple numbers.
   procedure Cv_Read_Raw_Data (Fs  : Cv_File_Storage_Ptr;
                               Src : Cv_File_Node_Ptr;
                               Dst : Cv_Void_Ptr;
                               Dt  : Interfaces.C.Strings.Chars_Ptr);

   --     Writes a file node to another file storage.
   procedure Cv_Write_File_Node (Fs            : Cv_File_Storage_Ptr;
                                 New_Node_Name : Interfaces.C.Strings.Chars_Ptr;
                                 Node          : Cv_File_Node_Ptr;
                                 Embed         : Integer);

   --     Returns the name of a file node.
   function Cv_Get_File_Node_Name (Node : Cv_File_Node_Ptr)
                                   return Interfaces.C.Strings.Chars_Ptr;

   -----------------------------------------------------------------------------
   -- Adding own types
   -----------------------------------------------------------------------------
   --     Registers a new type.
   procedure Cv_Register_Type (Info : Cv_Type_Info_Ptr);

   --     Unregisters the type.
   procedure Cv_Unregister_Type (Type_Name : Interfaces.C.Strings.Chars_Ptr);

   --     Returns the beginning of a type list.
   function Cv_First_Type return Cv_Type_Info_Ptr;

   --     Finds a type by its name.
   function Cv_Find_Type (Type_Name : Interfaces.C.Strings.Chars_Ptr)
                          return Cv_Type_Info_Ptr;

   --     Returns the type of an object.
   function Cv_Type_Of (Struct_Ptr : Cv_Void_Ptr)
                        return Cv_Type_Info_Ptr;

   --     Releases an object.
   procedure Cv_Release (Struct_Ptr : access Cv_Void_Ptr);

   --     Makes a clone of an object.
   function Cv_Clone (Struct_Ptr : Cv_Void_Ptr)
                      return Cv_Void_Ptr;

   --     Saves an object to a file.
   procedure Cv_Save (Filename   : String;
                      Struct_Ptr : Cv_Void_Ptr;
                      Name       : String;
                      Comment    : String;
                      Attributes : Cv_Attr_List := Cv_Create_Attr_List);

   --     Loads an object from a file.
   function Cv_Load (Filename  : String;
                     Storage   : Cv_Mem_Storage_Ptr := null;
                     Name      : String := "";
                     Real_Name : access String := null)
                     return Cv_Void_Ptr;

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
   function Cv_Error_From_Ipp_Status (Ippstatus : Integer) return Integer;



   --     Sets a new error handler.
   function Cv_Redirect_Error (Error_Handler : Cv_Error_Callback;
                               Userdata      : Cv_Void_Ptr;
                               Prev_Userdata : access Cv_Void_Ptr)
                               return Cv_Error_Callback;

   --     Provide standard error handling.
   function Cv_Nul_Dev_Report (Status    : Integer;
                               Func_Name : Interfaces.C.Strings.Chars_Ptr;
                               Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                               File_Name : Interfaces.C.Strings.Chars_Ptr;
                               Line      : Integer;
                               Userdata  : Cv_Void_Ptr)
                               return Integer;

   function Cv_Std_Err_Report (Status    : Integer;
                               Func_Name : Interfaces.C.Strings.Chars_Ptr;
                               Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                               File_Name : Interfaces.C.Strings.Chars_Ptr;
                               Line      : Integer;
                               Userdata  : Cv_Void_Ptr) return Integer;

   function Cv_Gui_Box_Report (Status    : Integer;
                               Func_Name : Interfaces.C.Strings.Chars_Ptr;
                               Err_Msg   : Interfaces.C.Strings.Chars_Ptr;
                               File_Name : Interfaces.C.Strings.Chars_Ptr;
                               Line      : Integer;
                               Userdata  : Cv_Void_Ptr) return Integer;

   procedure Opencv_Error (Status  : Integer;
                           Func    : String := Gnat.Source_Info.Enclosing_Entity;
                           Context : String := Gnat.Source_Info.Enclosing_Entity;
                           File    : String := Gnat.Source_Info.File;
                           Line    : Integer := Gnat.Source_Info.Line);

   -- This function is a rename...
   procedure Cv_Error (Status  : Integer;
                       Func    : String := Gnat.Source_Info.Enclosing_Entity;
                       Context : String := Gnat.Source_Info.Enclosing_Entity;
                       File    : String := Gnat.Source_Info.File;
                       Line    : Integer := Gnat.Source_Info.Line);

   -- Don't use this it doesn't work.
   procedure Opencv_Errchk (Func    : String := Gnat.Source_Info.Enclosing_Entity;
                            Context : String := Gnat.Source_Info.Enclosing_Entity;
                            File    : String := Gnat.Source_Info.File;
                            Line    : Integer := Gnat.Source_Info.Line);

   procedure Opencv_Assert (Expression : Boolean;
                            Func       : String := Gnat.Source_Info.Enclosing_Entity;
                            Context    : String := Gnat.Source_Info.Enclosing_Entity;
                            File       : String := Gnat.Source_Info.File;
                            Line       : Integer := Gnat.Source_Info.Line);

   --(cvSetErrStatus(CV_StsOk))
   procedure Opencv_Rsterr;

   procedure Opencv_Call;

   procedure Cv_Error_From_Code (Status  : Integer;
                                 Func    : String := Gnat.Source_Info.Enclosing_Entity;
                                 Context : String := Gnat.Source_Info.Enclosing_Entity;
                                 File    : String := Gnat.Source_Info.File;
                                 Line    : Integer := Gnat.Source_Info.Line) renames Opencv_Error;

   procedure Cv_Check (Func    : String := Gnat.Source_Info.Enclosing_Entity;
                       Context : String := Gnat.Source_Info.Enclosing_Entity;
                       File    : String := Gnat.Source_Info.File;
                       Line    : Integer := Gnat.Source_Info.Line);

   procedure Cv_Call renames Opencv_Call;

   procedure Cv_Assert (Expression : Boolean;
                        Func       : String := Gnat.Source_Info.Enclosing_Entity;
                        Context    : String := Gnat.Source_Info.Enclosing_Entity;
                        File       : String := Gnat.Source_Info.File;
                        Line       : Integer := Gnat.Source_Info.Line) renames Opencv_Assert;
   -----------------------------------------------------------------------------
   -- Private
   -----------------------------------------------------------------------------
private
   procedure Cv_Free_Wrapper (Ptr : access Cv_Void_Ptr);
   -- Wrapper due to String
   function W_Cv_Mem_Storage_Alloc_String (Storage : Cv_Mem_Storage_Ptr;
                                           Ptr     : String_C;
                                           Len     : Integer := -1) return Cv_String;

   procedure W_Cv_Put_Text (Img   : Cv_Arr_Ptr;
                            Text  : String_C;
                            Org   : Cv_Point;
                            Font  : access Cv_Font;
                            Color : Cv_Scalar);
   procedure W_Cv_Put_Text (Img   : Cv_Mat_Ptr;
                            Text  : String_C;
                            Org   : Cv_Point;
                            Font  : access Cv_Font;
                            Color : Cv_Scalar);
   procedure W_Cv_Put_Text (Img   : Ipl_Image_Ptr;
                            Text  : String_C;
                            Org   : Cv_Point;
                            Font  : access Cv_Font;
                            Color : Cv_Scalar);

   procedure W_Cv_Get_Text_Size (Textstring : String_C;
                                 Font       : Cv_Font;
                                 Textsize   : access Cv_Size;
                                 Baseline   : access Integer);

   procedure W_Cv_Save (Filename   : Chars_Ptr;
                        Struct_Ptr : Cv_Void_Ptr;
                        Name       : Chars_Ptr;
                        Comment    : Chars_Ptr;
                        Attributes : Cv_Attr_List := Cv_Create_Attr_List);

   function W_Cv_Load (Filename  : Chars_Ptr;
                       Storage   : Cv_Mem_Storage_Ptr := null;
                       Name      : Chars_Ptr := Null_Ptr;
                       Real_Name : access Interfaces.C.Strings.Chars_Ptr := null)
                       return Cv_Void_Ptr;

   pragma Import (C, Cv_Alloc, "cvAlloc");
   pragma Import (C, Cv_Free_Wrapper, "cvFree_");
   pragma Import (C, Cv_Create_Image_Header, "cvCreateImageHeader");
   pragma Import (C, Cv_Init_Image_Header, "cvInitImageHeader");
   pragma Import (C, Cv_Create_Image, "cvCreateImage");
   pragma Import (C, Cv_Release_Image_Header, "cvReleaseImageHeader");
   pragma Import (C, Cv_Release_Image, "cvReleaseImage");
   pragma Import (C, Cv_Clone_Image, "cvCloneImage");
   pragma Import (C, Cv_Set_Image_Coi, "cvSetImageCOI");
   pragma Import (C, Cv_Get_Image_Coi, "cvGetImageCOI");
   pragma Import (C, Cv_Set_Image_Roi, "cvSetImageROI");
   pragma Import (C, Cv_Reset_Image_Roi, "cvResetImageROI");
   pragma Import (C, Cv_Get_Image_Roi, "cvGetImageROI");
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
   pragma Import (C, Cv_Create_Mat_Nd_Header, "cvCreateMatNDHeader");
   pragma Import (C, Cv_Create_Mat_Nd, "cvCreateMatND");
   pragma Import (C, Cv_Init_Mat_Nd_Header, "cvInitMatNDHeader");
   pragma Import (C, Cv_Release_Mat_Nd, "cvReleaseMatND");
   pragma Import (C, Cv_Clone_Mat_Nd, "cvCloneMatND");
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
   pragma Import (C, Cv_Set_Real_1d, "cvSetReal1D");
   pragma Import (C, Cv_Set_Real_2d, "cvSetReal2D");
   pragma Import (C, Cv_Set_Real_3d, "cvSetReal3D");
   pragma Import (C, Cv_Set_Real_Nd, "cvSetRealND");
   pragma Import (C, Cv_Ptr_1d, "cvPtr1D");
   pragma Import (C, Cv_Ptr_2d, "cvPtr2D");
   pragma Import (C, Cv_Ptr_3d, "cvPtr3D");
   pragma Import (C, Cv_Ptr_Nd, "cvPtrND");
   pragma Import (C, Cv_Get_1d, "cvGet1D");
   pragma Import (C, Cv_Get_2d, "cvGet2D");
   pragma Import (C, Cv_Get_3d, "cvGet3D");
   pragma Import (C, Cv_Get_Nd, "cvGetND");
   pragma Import (C, Cv_Get_Real_1d, "cvGetReal1D");
   pragma Import (C, Cv_Get_Real_2d, "cvGetReal2D");
   pragma Import (C, Cv_Get_Real_3d, "cvGetReal3D");
   pragma Import (C, Cv_Get_Real_Nd, "cvGetRealND");
   pragma Import (C, Cv_Set_1d, "CvSet1D");
   pragma Import (C, Cv_Set_2d, "CvSet2D");
   pragma Import (C, Cv_Set_3d, "CvSet3D");
   pragma Import (C, Cv_Set_Nd, "CvSetND");
   pragma Import (C, Cv_Clear_Nd, "cvClearND");
   pragma Import (C, Cv_Get_Mat, "cvGetMat");
   pragma Import (C, Cv_Get_Image, "cvGetImage");
   pragma Import (C, Cv_Reshape_Mat_Nd, "cvReshapeMatND");
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
   pragma Import (C, Cv_Sub_Rs, "cvSubRS");
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
   pragma Import (C, Cv_Gemm, "cvGEMM");
   pragma Import (C, Cv_Transform, "cvTransform");
   pragma Import (C, Cv_Perspective_Transform, "cvPerspectiveTransform");
   pragma Import (C, Cv_Mul_Transposed, "cvMulTransposed");
   pragma Import (C, Cv_Transpose, "cvTranspose");
   pragma Import (C, Cv_Complete_Symm, "cvCompleteSymm");
   pragma Import (C, Cv_Flip, "cvFlip");
   pragma Import (C, Cv_Svdecomp, "cvSVD");
   pragma Import (C, Cv_Svbksb, "cvSVBkSb");
   pragma Import (C, Cv_Invert, "cvInvert");
   pragma Import (C, Cv_Solve, "cvSolve");
   pragma Import (C, Cv_Det, "cvDet");
   pragma Import (C, Cv_Trace, "cvTrace");
   pragma Import (C, Cv_Eigen_Vv, "cvEigenVV");
   pragma Import (C, Cv_Set_Identity, "cvSetIdentity");
   pragma Import (C, Cv_Range, "cvRange");
   pragma Import (C, Cv_Calc_Covar_Matrix, "cvCalcCovarMatrix");
   pragma Import (C, Cv_Calc_Pca, "cvCalcPCA");
   pragma Import (C, Cv_Project_Pca, "cvProjectPCA");
   pragma Import (C, Cv_Back_Project_Pca, "cvBackProjectPCA");
   pragma Import (C, Cv_Mahalanobis, "cvMahalanobis");
   pragma Import (C, Cv_Sum, "cvSum");
   pragma Import (C, Cv_Count_Non_Zero, "cvCountNonZero");
   pragma Import (C, Cv_Avg, "cvAvg");
   pragma Import (C, Cv_Avg_Sdv, "cvAvgSdv");
   pragma Import (C, Cv_Min_Max_Loc, "cvMinMaxLoc");
   pragma Import (C, Cv_Norm, "cvNorm");
   pragma Import (C, Cv_Normalize, "cvNormalize");
   pragma Import (C, Cv_Reduce, "cvReduce");
   pragma Import (C, Cv_Dft, "cvDFT");
   pragma Import (C, Cv_Mul_Spectrums, "cvMulSpectrums");
   pragma Import (C, Cv_Get_Optimal_Dft_Size, "cvGetOptimalDFTSize");
   pragma Import (C, Cv_Dct, "cvDCT");
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
   pragma Import (C, Cv_Lut, "cvLUT");
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
   pragma Import (C, Cv_Set_Ipl_Allocators, "cvSetIPLAllocators");
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
   pragma Import (C, W_Cv_Save, "cvSave");
   pragma Import (C, W_Cv_Load, "cvLoad");
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

   pragma Import (C, Cv_Next_Line_Point, "Cv_Next_Line_Point");
end Core.Operations;
