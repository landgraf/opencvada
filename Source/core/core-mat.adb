package body Core.Mat is
   function Cv_Create_Mat (Rows     : Integer;
                           Cols     : Integer;
                           Depth    : Integer;
                           Channels : Integer;
                           Data     : access Element_Array := null)
                           return Cv_Mat_P is

      function Cv_Create_Mat_I (Rows   : Integer;
                                Cols   : Integer;
                                M_Type : Unsigned_32;
                                Data   : Cv_Void_P)
                                return Cv_Mat;
      pragma Import (C, Cv_Create_Mat_I, "CvMat_wrap");

      Pointer : Cv_Pointer := null;
      Mat     : constant Cv_Mat_P := new Cv_Mat;
   begin
      if Data = null then
         Pointer := null;
      else
         Pointer := Data.all (0)'Access;
      end if;

      Mat.all := Cv_Create_Mat_I (Rows, Cols, Cv_Make_Type (Depth, Channels), To_Void (Pointer));

      return Mat;
   end Cv_Create_Mat;

   function Cv_Number_Of_Elements (Mat : Cv_Mat_P)
                                   return Integer is
   begin
      return Mat.all.Cols * Mat.all.Rows * Integer (Cv_Mat_Cn (Mat.all.Mat_Type));
   end Cv_Number_Of_Elements;

   function Cv_Get_Mat_Data (Mat : Cv_Mat_P)
                             return Element_Array is
      Channels : constant Integer := Integer (Cv_Mat_Cn (Mat.all.Mat_Type));
      Data     : Element_Array (0 .. Mat.all.Cols * Mat.all.Rows * Channels - 1);
   begin
      Data := Cv_Pointer_Pkg.Value (Mat.all.Data.Pointer, Ptrdiff_T (Mat.all.Cols * Mat.all.Rows * Channels));
      return Data;
   end Cv_Get_Mat_Data;

   procedure Cv_Release_Mat (Mat : in out Cv_Mat_P;
                             Arr : access Element_Array_P := null) is
   begin
      if Mat.all.Data.Pointer /= null then
--           Put_Line ("Calling free");
         Free (Mat.all.Data.Pointer.all'Address);
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

--     procedure Deallocate (Mat : out Cv_Mat_P) is
--     begin
--        if Mat.all.Data.Pointer /= null then
--           Deallocate_Mat_Data (Mat.all.Data.Pointer'Access);
--        end if;
--        Deallocate_Mat (Mat);
--     end Deallocate;
end Core.Mat;
