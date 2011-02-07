with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Ada.Text_IO; use Ada.Text_IO;

generic
   type Element_T is private;
package Core.Mat is
   type Element_P is access all Element_T;
   type Element_Array is array (Integer range <>) of aliased Element_T;
   type Element_Array_P is access all Element_Array;

   Dummy : Element_T;



   pragma Warnings (Off); -- "Dummy" may be referenced before it has a value
   package Cv_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Element_T, Element_Array, Dummy);
   pragma Warnings (On);
   use type Cv_Pointer_Pkg.Pointer;
   subtype Cv_Pointer is Cv_Pointer_Pkg.Pointer;

--     type Mat_Data is private;
   type Cv_Mat_Type is (Cv_Mat_Pointer);
   type Mat_Data (Option : Cv_Mat_Type := Cv_Mat_Pointer) is record
      case Option is
         when Cv_Mat_Pointer =>
            Pointer : aliased Cv_Pointer;
      end case;
   end record;
   pragma Unchecked_Union (Mat_Data);
   pragma Convention (C_Pass_By_Copy, Mat_Data);
   type Mat_Data_P is access Mat_Data;
   pragma Convention (C, Mat_Data_P);

   type Cv_Mat is record
      Mat_Type     : Unsigned_32;
      Step         : Integer;
      Refcount     : access Integer := null;
      Hdr_Refcount : Integer := 0;
      Data         : aliased Mat_Data;
      Rows         : Integer;
      Cols         : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Mat);
   type Cv_Mat_P is access Cv_Mat;
   pragma Convention (C, Cv_Mat_P);

   function Cv_Create_Mat (Rows     : Integer;
                           Cols     : Integer;
                           Depth    : Integer;
                           Channels : Integer;
                           Data     : access Element_Array := null)
                           return Cv_Mat_P;

   function Cv_Number_Of_Elements (Mat : Cv_Mat_P)
                                   return Integer;

   function Cv_Get_Mat_Data (Mat : Cv_Mat_P)
                             return Element_Array;

--     procedure Deallocate (Mat : out Cv_Mat_P);

   function To_Arr is new Ada.Unchecked_Conversion    (Target => Cv_Arr_P,
                                                       Source => Cv_Mat_P);
   function From_Arr is new Ada.Unchecked_Conversion  (Target => Cv_Mat_P,
                                                       Source => Cv_Arr_P);
   function To_Void is new Ada.Unchecked_Conversion   (Target => Cv_Void_P,
                                                       Source => Cv_Mat_P);
   function From_Void is new Ada.Unchecked_Conversion (Target => Cv_Mat_P,
                                                       Source => Cv_Void_P);

--     procedure Cv_Release_Mat (Mat : in out Cv_Mat_P);

   procedure Cv_Release_Mat (Mat : in out Cv_Mat_P;
                             Arr : access Element_Array_P := null);

   procedure Cv_Release_Element_Array is new Ada.Unchecked_Deallocation (Object => Element_Array,
                                                                         Name   => Element_Array_P);
private
   type Cv_Pointer_P is access all Cv_Pointer;
   pragma Convention (C, Cv_Pointer_P);

   procedure Deallocate_Mat is new Ada.Unchecked_Deallocation (Object => Cv_Mat,
                                                               Name   => Cv_Mat_P);

   procedure Deallocate_Mat_Data is new Ada.Unchecked_Deallocation (Object => Cv_Pointer,
                                                                    Name   => Cv_Pointer_P);

   function To_Void is new Ada.Unchecked_Conversion (Target => Cv_Void_P,
                                                     Source => Cv_Pointer);

   function Malloc (Size : Interfaces.C.Size_T) return System.Address;
   pragma Import (C, Malloc, "malloc");
   procedure Free (Ptr : System.Address);
   pragma Import (C, Free, "free");

--     pragma Import (C, Cv_Release_Mat, "cvReleaseMat");
end Core.Mat;
