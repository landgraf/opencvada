with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Ada.Text_IO; use Ada.Text_IO;

generic
   type Element_T is private;
package Core.Sparse_Mat is
   type Element_Ptr is access all Element_T;
   type Element_Array is array (Integer range <>) of aliased Element_T;
   type Element_Array_Ptr is access all Element_Array;

   pragma Warnings (Off);
   Dummy : Element_T;
   package Cv_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Element_T, Element_Array, Dummy);
   subtype Cv_Pointer is Cv_Pointer_Pkg.Pointer;
   pragma Warnings (On);

   type Cv_Mat_Type is (Cv_Mat_Pointer);
   type Mat_Data (Option : Cv_Mat_Type := Cv_Mat_Pointer) is record
      case Option is
         when Cv_Mat_Pointer =>
            Pointer : aliased Cv_Pointer;
      end case;
   end record;
   pragma Unchecked_Union (Mat_Data);
   pragma Convention (C_Pass_By_Copy, Mat_Data);
   type Mat_Data_Ptr is access all Mat_Data;
   pragma Convention (C, Mat_Data_Ptr);

   type Cv_Sparse_Mat is record
      Mat_Type     : Unsigned_32;
      Dims         : Integer;
      Refcount     : access Integer;
      Hdr_Refcount : Integer;

      Heap         : Cv_Set_Ptr;
      Hashtable    : Cv_Pointer;
      Hashsize     : Integer;
      Valoffset    : Integer;
      Idxoffset    : Integer;
      Size         : Cv_32s_Array (0 .. Cv_Max_Dim - 1);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Sparse_Mat);
   type Cv_Sparse_Mat_Ptr is access all Cv_Sparse_Mat;

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

   function Cv_Create_Sparse_Mat (Dims     : Integer;
                                  Sizes    : Cv_32s_Pointer;
                                  Mat_Type : Unsigned_32)
                                  return Cv_Sparse_Mat_Ptr;

   function Cv_Clone_Sparse_Mat (Src : Cv_Sparse_Mat_Ptr)
                                 return Cv_Sparse_Mat_Ptr;

   function Cv_Init_Sparse_Mat_Iterator (Mat      : Cv_Sparse_Mat_Ptr;
                                         Iterator : Cv_Sparse_Mat_Iterator_Ptr)
                                         return Cv_Sparse_Node_Ptr;

   function Cv_Node_Val (Mat  : Cv_Sparse_Mat_Ptr;
                         Node : Cv_Sparse_Node_Ptr)
                         return Element_Ptr;


   function Cv_Node_Idx (Mat  : Cv_Sparse_Mat_Ptr;
                         Node : Cv_Sparse_Node_Ptr)
                         return Cv_32s_Pointer;

   function To_Arr_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Arr_Ptr,
                                                        Source => Cv_Sparse_Mat_Ptr);

   function To_Arr_Ptr (Source : Element_Array_Ptr)
                        return Cv_Arr_Ptr;

   function To_Void_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Void_Ptr,
                                                         Source => Cv_Sparse_Mat_Ptr);

   function To_Void_Ptr (Source : Element_Array_Ptr)
                         return Cv_Void_Ptr;

   function To_Sparse_Mat_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Sparse_Mat_Ptr,
                                                               Source => Cv_Arr_Ptr);

   function To_Sparse_Mat_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Sparse_Mat_Ptr,
                                                               Source => Cv_Void_Ptr);

   function To_Sparse_Mat_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Sparse_Mat_Ptr,
                                                               Source => Core.Cv_Sparse_Mat_Ptr);

   function To_Sparse_Mat_Ptr is new Ada.Unchecked_Conversion (Target => Core.Cv_Sparse_Mat_Ptr,
                                                               Source => Cv_Sparse_Mat_Ptr);

   function To_Sparse_Mat is new Ada.Unchecked_Conversion (Target => Cv_Sparse_Mat,
                                                           Source => Core.Cv_Sparse_Mat);

   function To_Sparse_Mat is new Ada.Unchecked_Conversion (Target => Core.Cv_Sparse_Mat,
                                                           Source => Cv_Sparse_Mat);
private
   function To_Arr_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Arr_Ptr,
                                                        Source => Cv_Pointer);

   function To_Void_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Void_Ptr,
                                                         Source => Cv_Pointer);

   pragma Import (C, Cv_Create_Sparse_Mat, "cvCreateSparseMat");
   pragma Import (C, Cv_Clone_Sparse_Mat, "cvCloneSparseMat");
   pragma Import (C, Cv_Init_Sparse_Mat_Iterator, "cvInitSparseMatIterator");
   pragma Import (C, Cv_Node_Val, "CvNodeVal_wrap");
   pragma Import (C, Cv_Node_Idx, "CvNodeIdx_wrap");
end Core.Sparse_Mat;
