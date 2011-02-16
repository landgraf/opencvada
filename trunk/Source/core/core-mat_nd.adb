package body Core.Mat_ND is
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
