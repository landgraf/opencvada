with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Ada.Text_IO; use Ada.Text_IO;

generic
   type Element_T is private;
package Core.Mat_Nd is
   type Element_Ptr is access all Element_T;
   type Element_Array is array (Integer range <>) of aliased Element_T;
   type Element_Array_Ptr is access all Element_Array;

   pragma Warnings (Off);
   Dummy : Element_T;
   package Cv_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Element_T, Element_Array, Dummy);
   subtype Cv_Pointer is Cv_Pointer_Pkg.Pointer;
   pragma Warnings (On);

   type Mat_Dimensions is record
      Size : Integer;
      Step : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Mat_Dimensions);

   type Mat_Dimensions_Array is array (Integer range 0 .. Cv_Max_Dim - 1) of aliased Mat_Dimensions;

   type Mat_Sizes is array (Integer range <>) of aliased Integer;
   type Mat_Sizes_Ptr is access all Mat_Sizes;

   type Cv_Mat_ND is record
      Mat_Type     : Unsigned_32;
      Dims         : Integer;

      Refcount     : access Integer;
      Hdr_Refcount : Integer;

      Data         : Cv_Pointer;
      Dim          : Mat_Dimensions_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Mat_ND);
   type Cv_Mat_ND_Ptr is access all Cv_Mat_ND;
   pragma Convention (C, Cv_Mat_ND_Ptr);

--     type Cv_N_Array_Iterator is record
--
--     end record;

   function Cv_Init_Mat_ND_Header (Mat      : Cv_Mat_ND_Ptr;
                                   Dims     : Integer;
                                   Sizes    : Mat_Sizes_Ptr;
                                   Mat_Type : Unsigned_32;
                                   Data     : Element_Array_Ptr := null)
                                   return Cv_Mat_ND_Ptr;

   function Cv_Create_Mat_ND (Dims     : Integer;
                              Sizes    : Mat_Sizes_Ptr;
                              Mat_Type : Unsigned_32)
                              return Cv_Mat_ND_Ptr;

   function Cv_Clone_Mat_ND (Mat : Cv_Mat_ND_Ptr)
                             return Cv_Mat_ND_Ptr;

   function Cv_Get_Mat_ND (Arr    : Cv_Arr_Ptr;
                           Mat_ND : Cv_Mat_ND_Ptr;
                           Coi    : access Integer)
                           return Cv_Mat_ND_Ptr;

--     function Cv_Init_N_Array_Iterator (Count : Integer;
--                                        Arrs  : Cv_Arr_Ptr_Array_Ptr;
--                                        Stubs : Cv_Mat_ND;
--                                        Iterator :

   function To_Arr_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Arr_Ptr,
                                                        Source => Cv_Mat_ND_Ptr);

   function To_Arr_Ptr (Source : Element_Array_Ptr)
                        return Cv_Arr_Ptr;

   function To_Void_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Void_Ptr,
                                                         Source => Cv_Mat_ND_Ptr);

   function To_Void_Ptr (Source : Element_Array_Ptr)
                         return Cv_Void_Ptr;

   function To_Mat_ND_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Mat_ND_Ptr,
                                                           Source => Cv_Arr_Ptr);

   function To_Mat_ND_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Mat_ND_Ptr,
                                                           Source => Cv_Void_Ptr);

   function To_Mat_ND_Ptr is new Ada.Unchecked_Conversion (Target => Core.Cv_Mat_ND_Ptr,
                                                           Source => Cv_Mat_ND_Ptr);

   function To_Mat_ND_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Mat_ND_Ptr,
                                                           Source => Core.Cv_Mat_ND_Ptr);

   function To_Mat_ND is new Ada.Unchecked_Conversion (Target => Cv_Mat_ND,
                                                       Source => Core.Cv_Mat_ND);

   function To_Mat_ND is new Ada.Unchecked_Conversion (Target => Core.Cv_Mat_ND,
                                                       Source => Cv_Mat_ND);
private
   package Mat_Sizes_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Integer, Mat_Sizes, 0);
   subtype Mat_Sizes_Pointer is Mat_Sizes_Pointer_Pkg.Pointer;

   function To_Arr_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Arr_Ptr,
                                                        Source => Cv_Pointer);

   function To_Void_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Void_Ptr,
                                                         Source => Cv_Pointer);

   pragma Import (C, Cv_Clone_Mat_ND, "cvCloneMatND");
   pragma Import (C, Cv_Get_Mat_ND, "cvGetMatND");
end Core.Mat_Nd;
