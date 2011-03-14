with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Ada.Text_IO; use Ada.Text_IO;

generic
   type Element_T is private;
package Core.Mat is
   type Element_Ptr is access all Element_T;
   type Element_Array is array (Integer range <>) of aliased Element_T;
   type Element_Array_Ptr is access all Element_Array;

   pragma Warnings (Off); -- "Dummy" may be referenced before it has a value
   Dummy : Element_T;

   package Cv_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Element_T, Element_Array, Dummy);
   pragma Warnings (On);

   use type Cv_Pointer_Pkg.Pointer;
   subtype Cv_Pointer is Cv_Pointer_Pkg.Pointer;

   type Cv_Mat is record
      Mat_Type     : Unsigned_32;
      Step         : Integer;
      Refcount     : access Integer := null;
      Hdr_Refcount : Integer := 0;
      Data         : aliased Cv_Pointer;
      Rows         : Integer;
      Cols         : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Mat);
   type Cv_Mat_Ptr is access all Cv_Mat;
   pragma Convention (C, Cv_Mat_Ptr);

   function Cv_Create_Mat (Rows     : Integer;
                           Cols     : Integer;
                           Depth    : Integer;
                           Channels : Integer;
                           Data     : Element_Array_Ptr := null)
                           return Cv_Mat_Ptr;

   function Cv_Create_Mat (Rows     : Integer;
                           Cols     : Integer;
                           Depth    : Integer;
                           Channels : Integer;
                           Data     : Element_Array)
                           return Cv_Mat_Ptr;

   function Cv_Create_Mat_Header (Rows     : Integer;
                                  Cols     : Integer;
                                  Mat_Type : Unsigned_32)
                                  return Cv_Mat_Ptr;

   function Cv_Init_Mat_Header (Arr      : Cv_Mat_Ptr;
                                Rows     : Integer;
                                Cols     : Integer;
                                Mat_Type : Unsigned_32;
                                Data     : Element_Array_Ptr;
                                Step     : Integer)
                                return Cv_Mat_Ptr;

   function Cv_Clone_Mat (Src : Cv_Mat_Ptr)
                          return Cv_Mat_Ptr;

   function Cv_Number_Of_Elements (Mat : Cv_Mat_Ptr)
                                   return Integer;

   function Cv_Get_Mat_Data (Mat : Cv_Mat_Ptr)
                             return Element_Array;

   function To_Arr_Ptr is new Ada.Unchecked_Conversion    (Target => Cv_Arr_Ptr,
                                                           Source => Cv_Mat_Ptr);

   function To_Arr_Ptr (Source : Element_Array_Ptr)
                        return Cv_Arr_Ptr;

   function To_Void_Ptr is new Ada.Unchecked_Conversion   (Target => Cv_Void_Ptr,
                                                           Source => Cv_Mat_Ptr);

   function To_Void_Ptr (Source : Element_Array_Ptr)
                         return Cv_Void_Ptr;

   function To_Mat_Ptr is new Ada.Unchecked_Conversion  (Target => Cv_Mat_Ptr,
                                                         Source => Cv_Arr_Ptr);

   function To_Mat_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Mat_Ptr,
                                                        Source => Cv_Void_Ptr);

   function To_Mat_Ptr is new Ada.Unchecked_Conversion (Target => Core.Cv_Mat_Ptr,
                                                        Source => Cv_Mat_Ptr);

   function To_Mat_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Mat_Ptr,
                                                        Source => Core.Cv_Mat_Ptr);

   procedure Cv_Release_Mat (Mat : in out Cv_Mat_Ptr;
                             Arr : access Element_Array_Ptr := null);

   procedure Cv_Release_Element_Array is new Ada.Unchecked_Deallocation (Object => Element_Array,
                                                                         Name   => Element_Array_Ptr);
private
   type Cv_Pointer_Ptr is access all Cv_Pointer;
   pragma Convention (C, Cv_Pointer_Ptr);

   function To_Arr_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Arr_Ptr,
                                                        Source => Cv_Pointer);

   function To_Void_Ptr is new Ada.Unchecked_Conversion (Target => Cv_Void_Ptr,
                                                         Source => Cv_Pointer);

   procedure Deallocate_Mat is new Ada.Unchecked_Deallocation (Object => Cv_Mat,
                                                               Name   => Cv_Mat_Ptr);

   procedure Deallocate_Mat_Data is new Ada.Unchecked_Deallocation (Object => Cv_Pointer,
                                                                    Name   => Cv_Pointer_Ptr);

   function To_Void is new Ada.Unchecked_Conversion (Target => Cv_Void_Ptr,
                                                     Source => Cv_Pointer);

   function Malloc (Size : Interfaces.C.Size_T) return System.Address;
   pragma Import (C, Malloc, "malloc");
   procedure Free (Ptr : System.Address);
   pragma Import (C, Free, "free");

   pragma Import (C, Cv_Create_Mat_Header, "cvCreateMatHeader");
   pragma Import (C, Cv_Clone_Mat, "cvCloneMat");
end Core.Mat;
