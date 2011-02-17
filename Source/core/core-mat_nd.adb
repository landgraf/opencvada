package body Core.Mat_ND is
   function Cv_Init_Mat_ND_Header (Mat      : Cv_Mat_ND_Ptr;
                                   Dims     : Integer;
                                   Sizes    : Mat_Sizes_Ptr;
                                   Mat_Type : Unsigned_32;
                                   Data     : Element_Array_Ptr := null)
                                   return Cv_Mat_ND_Ptr is

      function Cv_Init_Mat_ND_Header_Wrap (Mat      : Cv_Mat_ND_Ptr;
                                           Dims     : Integer;
                                           Sizes    : Mat_Sizes_Pointer;
                                           Mat_Type : Unsigned_32;
                                           Data     : Cv_Pointer)
                                           return Cv_Mat_ND_Ptr;
      pragma Import (C, Cv_Init_Mat_ND_Header_Wrap, "cvInitMatNDHeaderWrap");

      Data_Ptr : constant Cv_Pointer := Data.all (Data.all'First)'Access;
      Size_Ptr : constant Mat_Sizes_Pointer := Sizes.all (Sizes.all'First)'Access;
   begin
      return Cv_Init_Mat_ND_Header_Wrap (Mat, Dims, Size_Ptr, Mat_Type, Data_Ptr);
   end Cv_Init_Mat_ND_Header;

   function Cv_Create_Mat_ND (Dims     : Integer;
                              Sizes    : Mat_Sizes_Ptr;
                              Mat_Type : Unsigned_32)
                              return Cv_Mat_ND_Ptr is

      function Cv_Create_Mat_ND_Wrap (Dims     : Integer;
                                      Sizes    : Mat_Sizes_Pointer;
                                      Mat_Type : Unsigned_32)
                                      return Cv_Mat_ND_Ptr;
      pragma Import (C, Cv_Create_Mat_ND_Wrap, "cvCreateMatNDWrap");

      Size_Ptr : constant Mat_Sizes_Pointer := Sizes.all (Sizes.all'First)'Access;
   begin
      return Cv_Create_Mat_ND_Wrap (Dims, Size_Ptr, Mat_Type);
   end Cv_Create_Mat_ND;

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
end Core.Mat_ND;
