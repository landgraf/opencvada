package body Core.Mat is
   function Cv_Create_Mat (Rows     : Integer;
                           Cols     : Integer;
                           Depth    : Integer;
                           Channels : Integer;
                           Data     : Element_Array_Ptr := null)
                           return Cv_Mat_Ptr is

      function Cv_Create_Mat_I (Rows   : Integer;
                                Cols   : Integer;
                                M_Type : Unsigned_32;
                                Data   : Cv_Void_Ptr)
                                return Cv_Mat;
      pragma Import (C, Cv_Create_Mat_I, "CvMat_wrap");

      Pointer : Cv_Pointer := null;
      Mat     : constant Cv_Mat_Ptr := new Cv_Mat;
   begin
      if Data = null then
         Pointer := null;
      else
         Pointer := Data.all (Data.all'First)'Access;
      end if;

      Mat.all := Cv_Create_Mat_I (Rows, Cols, Cv_Make_Type (Depth, Channels), To_Void (Pointer));

      return Mat;
   end Cv_Create_Mat;

   function Cv_Create_Mat (Rows     : Integer;
                           Cols     : Integer;
                           Depth    : Integer;
                           Channels : Integer;
                           Data     : Element_Array)
                           return Cv_Mat_Ptr is
      function Cv_Create_Mat_I (Rows   : Integer;
                                Cols   : Integer;
                                M_Type : Unsigned_32;
                                Data   : Element_Array)
                                return Cv_Mat;
      pragma Import (C, Cv_Create_Mat_I, "CvMat_wrap");
      Mat : constant Cv_Mat_Ptr := new Cv_Mat;
   begin
      Mat.all := Cv_Create_Mat_I (Rows, Cols, Cv_Make_Type (Depth, Channels), Data);
      return Mat;
   end Cv_Create_Mat;

   function Cv_Init_Mat_Header (Arr      : Cv_Mat_Ptr;
                                Rows     : Integer;
                                Cols     : Integer;
                                Mat_Type : Unsigned_32;
                                Data     : Element_Array_Ptr;
                                Step     : Integer)
                                return Cv_Mat_Ptr is
      function Cv_Init_Mat_Header_Wrap (Arr      : Cv_Mat_Ptr;
                                        Rows     : Integer;
                                        Cols     : Integer;
                                        Mat_Type : Unsigned_32;
                                        Data     : Cv_Pointer;
                                        Step     : Integer)
                                        return Cv_Mat_Ptr;
      pragma Import (C, Cv_Init_Mat_Header_Wrap, "cvInitMatHeader");

      Ptr : constant Cv_Pointer := Data.all (Data'First)'Access;
   begin
      return Cv_Init_Mat_Header_Wrap (Arr, Rows, Cols, Mat_Type, Ptr, Step);
   end Cv_Init_Mat_Header;

   function Cv_Number_Of_Elements (Mat : Cv_Mat_Ptr)
                                   return Integer is
   begin
      return Mat.all.Cols * Mat.all.Rows * Integer (Cv_Mat_Cn (Mat.all.Mat_Type));
   end Cv_Number_Of_Elements;

   function Cv_Get_Mat_Data (Mat : Cv_Mat_Ptr)
                             return Element_Array is
      Channels : constant Integer := Integer (Cv_Mat_Cn (Mat.all.Mat_Type));
      Data     : Element_Array (0 .. Mat.all.Cols * Mat.all.Rows * Channels - 1);
   begin
      Data := Cv_Pointer_Pkg.Value (Mat.all.Data, Ptrdiff_T (Mat.all.Cols * Mat.all.Rows * Channels));
      return Data;
   end Cv_Get_Mat_Data;

   procedure Cv_Release_Mat (Mat : in out Cv_Mat_Ptr;
                             Arr : access Element_Array_Ptr := null) is
   begin
      if Mat.all.Data /= null then
         Free (Mat.all.Data.all'Address);
      end if;

      if Mat.all.Refcount /= null then
         Free (Mat.all.Refcount.all'Address);
      end if;

      Free (Mat.all'Address);
      Mat := null;

      if Arr /= null then
         Cv_Release_Element_Array (Arr.all);
         Arr.all := null;
      end if;
   end Cv_Release_Mat;

   function To_Arr_Ptr (Source : Element_Array_Ptr)
                        return Cv_Arr_Ptr is
      Ptr : constant Cv_Pointer := Source.all (Source.all'First)'Access;
   begin
      return To_Arr_Ptr (Ptr);
   end To_Arr_Ptr;

   function To_Void_Ptr (Source : Element_Array_Ptr)
                         return Cv_Void_Ptr is
      Ptr : constant Cv_Pointer := Source.all (Source.all'First)'Access;
   begin
      return To_Void_Ptr (Ptr);
   end To_Void_Ptr;
end Core.Mat;
