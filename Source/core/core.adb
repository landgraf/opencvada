with Ada.Text_Io; use Ada.Text_Io;
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
-- types_c.adb - types_c.h
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Core is

   function Cv_Node_Type (Flags : Unsigned_32)
                          return Unsigned_32 is
   begin
      return (Flags and Cv_Node_Type_Mask);
   end Cv_Node_Type;

   function Cv_Node_Is_Int (Flags : Unsigned_32)
                            return Boolean is
   begin
      if Cv_Node_Type (Flags) = Cv_Node_Int then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_Int;

   function Cv_Node_Is_Real (Flags : Unsigned_32)
                             return Boolean is
   begin
      if Cv_Node_Type (Flags) = Cv_Node_Real then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_Real;

   function Cv_Node_Is_String (Flags : Unsigned_32)
                               return Boolean is
   begin
      if Cv_Node_Type (Flags) = Cv_Node_String then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_String;

   function Cv_Node_Is_Seq (Flags : Unsigned_32)
                            return Boolean is
   begin
      if Cv_Node_Type (Flags) = Cv_Node_Seq then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_Seq;

   function Cv_Node_Is_Map (Flags : Unsigned_32)
                            return Boolean is
   begin
      if Cv_Node_Type (Flags) = Cv_Node_Map then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_Map;

   function Cv_Node_Is_Collection (Flags : Unsigned_32)
                                   return Boolean is
   begin
      if Cv_Node_Type (Flags) >= Cv_Node_Seq then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_Collection;

   function Cv_Node_Is_Flow (Flags : Unsigned_32)
                             return Boolean is
   begin
      if (Flags and Cv_Node_Flow) /= 0 then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_Flow;

   function Cv_Node_Is_Empty (Flags : Unsigned_32)
                              return Boolean is
   begin
      if (Flags and Cv_Node_Empty) /= 0 then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_Empty;

   function Cv_Node_Is_User (Flags : Unsigned_32)
                             return Boolean is
   begin
      if (Flags and Cv_Node_User) /= 0 then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Is_User;

   function Cv_Node_Has_Name (Flags : Unsigned_32)
                              return Boolean is
   begin
      if (Flags and Cv_Node_Named) /= 0 then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Has_Name;

   function Cv_Node_Seq_Is_Simple (Seq : access Cv_Seq)
                                   return Boolean is
   begin
      if (Seq.all.Flags and Cv_Node_Seq_Simple) /= 0 then
         return True;
      else
         return False;
      end if;
   end Cv_Node_Seq_Is_Simple;

   function Cv_Create_Attr_List (Attr : Cv_String_Pointer := null;
                                 Next : Cv_Attr_List_P := null)
                                 return Cv_Attr_List is
      L : Cv_Attr_List;
   begin
      L.Attr := Attr;
      L.Next := Next;
      return L;
   end Cv_Create_Attr_List;

   procedure Cv_Write_Seq_Elem_Var ( Elem_Ptr : Cv_Arr_Pointer;
                                    Writer   : Cv_Seq_Writer_P ) is
   begin
      if (Writer.all.Ptr - Writer.all.Block_Max) >= 0 then
         Cv_Create_Seq_Block (Writer);
      end if;
      Memcpy (Writer.all.Ptr'Address, Elem_Ptr'Address, System.Crtl.Size_T (Writer.all.Seq.all.Elem_Size));
      Writer.all.Ptr := Writer.all.Ptr + Ptrdiff_T (Writer.all.Seq.all.Elem_Size);
   end Cv_Write_Seq_Elem_Var;

--     procedure Cv_Next_Seq_Elem (Elem_Size : Integer;
--                                 Reader    : Cv_Seq_Reader_P) is
--     begin
--        Reader.all.Ptr := Reader.all.Ptr + Ptrdiff_T (Elem_Size);
--        if (Reader.all.Ptr - Reader.all.Blockmax) >= 0 then
--           Cv_Change_Seq_Block (To_Void (Reader), 1);
--        end if;
--     end Cv_Next_Seq_Elem;

   procedure Cv_Next_Seq_Elem (Elem_Size : Integer;
                               Reader    : Cv_Chain_Pt_Reader_P) is
   begin
      Reader.all.Ptr := Reader.all.Ptr + Ptrdiff_T (Elem_Size);
      if (Reader.all.Ptr - Reader.all.Blockmax) >= 0 then
         Cv_Change_Seq_Block (To_Void (Reader), 1);
      end if;
   end Cv_Next_Seq_Elem;

   procedure Cv_Prev_Seq_Elem ( Elem_Size : Integer;
                               Reader    : Cv_Seq_Reader_P ) is
   begin
      Reader.all.Ptr := Reader.all.Ptr - Ptrdiff_T (Elem_Size);
      if (Reader.all.Blockmin - Reader.all.Ptr) > 0 then
         Cv_Change_Seq_Block (To_Void (Reader), -1);
      end if;
   end Cv_Prev_Seq_Elem;

   procedure Cv_Read_Seq_Elem ( Elem  : Cv_Arr_Pointer;
                               Reader : Cv_Seq_Reader_P ) is
   begin
      Memcpy (Elem'Address, Reader.all.Ptr'Address, System.Crtl.Size_T (Reader.all.Seq.all.Elem_Size));
      Cv_Next_Seq_Elem (Reader.all.Seq.all.Elem_Size, Reader);
   end Cv_Read_Seq_Elem;

   procedure Cv_Read_Seq_Elem ( Elem  : Unsigned_8;
                               Reader : Cv_Chain_Pt_Reader_P ) is
   begin
      Memcpy (Elem'Address, Reader.all.Ptr'Address, System.Crtl.Size_T (Reader.all.Seq.all.Elem_Size));
      Cv_Next_Seq_Elem (Reader.all.Seq.all.Elem_Size, Reader);
   end Cv_Read_Seq_Elem;

   procedure Cv_Rev_Read_Seq_Elem ( Elem  : Cv_Arr_Pointer;
                                   Reader : Cv_Seq_Reader_P ) is
   begin
      Memcpy (Elem'Address, Reader.all.Ptr'Address, System.Crtl.Size_T (Reader.all.Seq.all.Elem_Size));
      Cv_Prev_Seq_Elem (Reader.all.Seq.all.Elem_Size, Reader);
   end Cv_Rev_Read_Seq_Elem;


   procedure Cv_Read_Chain_Point ( Pt    : out Cv_Point;
                                  Reader : Cv_Chain_Pt_Reader_P ) is
   begin
      Pt := Reader.all.Pt;
      if not (Reader.all.Ptr = null) then
         Cv_Read_Seq_Elem (Reader.all.Code, Reader);
         if ((Reader.all.Code and not (Unsigned_8 (7))) = 0) then
            Reader.all.Pt.X := Reader.all.Pt.X + Integer (Reader.all.Deltas (Integer (Reader.all.Code), 1));
            Reader.all.Pt.Y := Reader.all.Pt.Y + Integer (Reader.all.Deltas (Integer (Reader.all.Code), 2));
         end if;
      end if;

   end Cv_Read_Chain_Point;

   function Cv_Current_Point ( Reader : Cv_Chain_Pt_Reader_P ) return Cv_Point_P is
   begin
      return From_Arr (Value (Reader.all.Ptr) (1));
   end Cv_Current_Point;

   function Cv_Prev_Point ( Reader : Cv_Chain_Pt_Reader_P) return Cv_Point_P is
   begin
      return From_Arr (Value (Reader.all.Prevelem) (1));
   end Cv_Prev_Point;

   procedure Cv_Read_Edge ( Pt1   : out Cv_Point_P;
                           Pt2    : out Cv_Point_P;
                           Reader : Cv_Chain_Pt_Reader_P ) is
   begin
      Pt1 := Cv_Prev_Point (Reader);
      Pt2 := Cv_Current_Point (Reader);
      Reader.all.Prevelem := Reader.all.Ptr;
      Cv_Next_Seq_Elem (Cv_Point'Size / 8, Reader);
   end Cv_Read_Edge;

   function Cv_Next_Graph_Edge ( Edge  : Cv_Graph_Edge_P;
                                Vertex : Cv_Graph_Vtx_P ) return Cv_Graph_Edge_P is
   begin
      if (Edge.all.Vtx (1) = Vertex) then
         return Edge.all.Next (1);
      elsif (Edge.all.Vtx (2) = Vertex) then
         return Edge.all.Next (2);
      end if;
      return null;
   end Cv_Next_Graph_Edge;

   function Cv_Is_Storage (Storage : Cv_Mem_Storage_P) return Integer is
   begin
      if not (Storage = null) then
         if (Unsigned_32 (Storage.all.Signature) and Cv_Magic_Mask) = Cv_Storage_Magic_Val then
            return 1;
         end if;
      end if;
      return 0;
   end Cv_Is_Storage;

   function Cv_Is_Set_Elem (Ptr : Cv_Set_Elem_P) return Integer is
   begin
      if Ptr.all.Flags >= 0 then
         return 1;
      end if;
      return 0;
   end Cv_Is_Set_Elem;

   --#define CV_IS_SEQ(seq) \
   --    ((seq) != NULL && (((CvSeq*)(seq))->flags & CV_MAGIC_MASK) == CV_SEQ_MAGIC_VAL)
   function Cv_Is_Seq (Seq : Cv_Seq_P) return Integer is
   begin
      if not (Seq = null) then
         if (Seq.all.Flags and Cv_Magic_Mask) = Cv_Seq_Magic_Val then
            return 1;
         end if;
      end if;
      return 0;
   end Cv_Is_Seq;

   --#define CV_IS_SET(set) \
   --((set) != NULL && (((CvSeq*)(set))->flags & CV_MAGIC_MASK) == CV_SET_MAGIC_VAL)
   function Cv_Is_Set (Set : Cv_Seq_P) return Integer is
   begin
      if not (Set = null) then
         if (Set.all.Flags and Cv_Magic_Mask) = Cv_Set_Magic_Val then
            return 1;
         end if;
      end if;
      return 0;
   end Cv_Is_Set;

   function Cv_Seq_Eltype_Point return Unsigned_32 is  --/* (x,y) */ -- used to be Integer
   begin
      return Cv_Maketype (Cv_32s, 2);  --/* (x,y) */
   end Cv_Seq_Eltype_Point;

   function Cv_Seq_Eltype_Code return Unsigned_32 is -- used to be Integer
   begin
      return Cv_Maketype (Cv_8u, 1);
   end Cv_Seq_Eltype_Code;

   function Cv_Seq_Eltype_Index return Unsigned_32 is -- used to be Integer
   begin
      return Cv_Maketype (Cv_32s, 1);  --/* #(x,y) */
   end Cv_Seq_Eltype_Index;

   function Cv_Seq_Eltype_Point3d return Unsigned_32 is
   begin
      return Cv_Maketype (Cv_32f, 3);  --/* (x,y,z)  */ -- used to be Integer
   end Cv_Seq_Eltype_Point3d;

   function Cv_Seq_Point_Set return Integer is
   begin
      return Integer (Cv_Seq_Kind_Generic or Unsigned_32 (Cv_Seq_Eltype_Point));
   end Cv_Seq_Point_Set;

   function Cv_Seq_Point3d_Set return Integer is
   begin
      return Integer (Cv_Seq_Kind_Generic or Unsigned_32 (Cv_Seq_Eltype_Point3d));
   end Cv_Seq_Point3d_Set;

   function Cv_Seq_Polyline return Integer is
   begin
      return Integer (Cv_Seq_Kind_Curve or Unsigned_32 (Cv_Seq_Eltype_Point));
   end Cv_Seq_Polyline;

   function Cv_Seq_Polygon return Integer is
   begin
      return Integer (Cv_Seq_Flag_Closed or Unsigned_32 (Cv_Seq_Polyline));
   end Cv_Seq_Polygon;

   function Cv_Seq_Simple_Polygon return Integer is
   begin
      return Integer (Cv_Seq_Flag_Simple or Unsigned_32 (Cv_Seq_Polygon  ));
   end Cv_Seq_Simple_Polygon;

   function Cv_Seq_Chain return Integer is
   begin
      return Integer (Cv_Seq_Kind_Curve or Unsigned_32 (Cv_Seq_Eltype_Code));
   end Cv_Seq_Chain;

   function Cv_Seq_Chain_Contour return Integer is
   begin
      return Integer (Cv_Seq_Flag_Closed or Unsigned_32 (Cv_Seq_Chain));
   end Cv_Seq_Chain_Contour;

   function Cv_Seq_Polygon_Tree return Integer is
   begin
      return Integer (Cv_Seq_Kind_Bin_Tree or Unsigned_32 (Cv_Seq_Eltype_Trian_Atr));
   end Cv_Seq_Polygon_Tree;

   function Cv_Seq_Connected_Comp return Integer is
   begin
      return Integer (Cv_Seq_Kind_Generic or Unsigned_32 (Cv_Seq_Eltype_Connected_Comp));
   end Cv_Seq_Connected_Comp;

   function Cv_Seq_Index return Integer is
   begin
      return Integer (Cv_Seq_Kind_Generic or Unsigned_32 (Cv_Seq_Eltype_Index));
   end Cv_Seq_Index;

   function  Cv_Seq_Eltype ( Seq : Cv_Seq_P ) return Unsigned_32 is
   begin
      return Seq.all.Flags and Cv_Seq_Eltype_Mask;
   end Cv_Seq_Eltype;

   function Cv_Seq_Kind ( Seq : Cv_Seq_P ) return Integer is
   begin
      return Integer (Seq.all.Flags and Cv_Seq_Kind_Mask );
   end Cv_Seq_Kind;

   function Cv_Is_Seq_Index ( Seq : Cv_Seq_P) return Integer is
   begin
      if ((Cv_Seq_Eltype (Seq) = Cv_Seq_Eltype_Index) and
            (Cv_Seq_Kind (Seq) = Cv_Seq_Kind_Generic)) then
         return 1;
      else
         return 0;
      end if;
   end Cv_Is_Seq_Index;

   function Cv_Is_Seq_Curve ( Seq : Cv_Seq_P) return Integer is
   begin
      if (Cv_Seq_Kind (Seq) = Cv_Seq_Kind_Curve) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Curve;

   function Cv_Is_Seq_Closed ( Seq : Cv_Seq_P) return Integer is
   begin
      if not ((Seq.all.Flags and Cv_Seq_Flag_Closed) = 0) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Closed;

   function Cv_Is_Seq_Convex ( Seq : Cv_Seq_P)   return Integer is
   begin
      if not ((Seq.all.Flags and Cv_Seq_Flag_Convex) = 0) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Convex;

   function Cv_Is_Seq_Hole ( Seq : Cv_Seq_P) return Integer is
   begin
      if not ((Seq.all.Flags and  Cv_Seq_Flag_Hole) = 0) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Hole;

   function Cv_Is_Seq_Simple ( Seq : Cv_Seq_P) return Integer is
   begin
      if (not ((Seq.all.Flags and Cv_Seq_Flag_Simple) = 0) or Cv_Is_Seq_Convex (Seq) = 1) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Simple;

   function Cv_Is_Seq_Point_Set ( Seq : Cv_Seq_P) return Integer is
   begin
      if ((Cv_Seq_Eltype (Seq) = Cv_Make_Type (Cv_32s, 2)) or (Cv_Seq_Eltype (Seq) = Cv_Make_Type (Cv_32f, 2))) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Point_Set;

   function Cv_Is_Seq_Point_Subset ( Seq : Cv_Seq_P) return Integer is
   begin
      if (Cv_Is_Seq_Index ( Seq ) = 1) or (Cv_Seq_Eltype (Seq) = Cv_Seq_Eltype_Ppoint) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Point_Subset;

   function Cv_Is_Seq_Polyline ( Seq : Cv_Seq_P ) return Integer is
   begin
      if (Cv_Seq_Kind (Seq) = Cv_Seq_Kind_Curve) and (Cv_Is_Seq_Point_Set (Seq) = 1) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Polyline;

   function Cv_Is_Seq_Polygon ( Seq : Cv_Seq_P) return Integer is
   begin
      if (Cv_Is_Seq_Polyline (Seq) = 1) and (Cv_Is_Seq_Closed (Seq) = 1) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Polygon;

   function Cv_Is_Seq_Chain ( Seq : Cv_Seq_P) return Integer is
   begin
      if (Cv_Seq_Kind (Seq) = Cv_Seq_Kind_Curve) and (Seq.all.Elem_Size = 1) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Chain;

   function Cv_Is_Seq_Contour ( Seq : Cv_Seq_P) return Integer is
   begin
      if (Cv_Is_Seq_Closed (Seq) = 1 )and ((Cv_Is_Seq_Polyline (Seq) = 1) or (Cv_Is_Seq_Chain (Seq) = 1)) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Contour;

   function Cv_Is_Seq_Chain_Contour ( Seq  : Cv_Seq_P ) return Integer is
   begin
      if (Cv_Is_Seq_Chain ( Seq ) = 1) and (Cv_Is_Seq_Closed ( Seq ) = 1) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Chain_Contour;

   function Cv_Is_Seq_Polygon_Tree ( Seq  : Cv_Seq_P) return Integer is
   begin
      if (Cv_Seq_Eltype (Seq) =  Cv_Seq_Eltype_Trian_Atr) and (Cv_Seq_Kind ( Seq ) =  Cv_Seq_Kind_Bin_Tree ) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Seq_Polygon_Tree;

   function Cv_Is_Graph ( Seq : Cv_Seq_P) return Integer is
   begin
      if (Cv_Is_Set (Seq) = 1) and (Cv_Seq_Kind (Seq) = Cv_Seq_Kind_Graph) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Graph;
   function Cv_Is_Graph_Oriented ( Seq : Cv_Seq_P) return Integer is
   begin
      if not ((Seq.all.Flags and Cv_Graph_Flag_Oriented) = 0) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Graph_Oriented;

   function Cv_Is_Subdiv2d ( Seq : Cv_Seq_P) return Integer is
   begin
      if (Cv_Is_Set (Seq) = 1) and (Cv_Seq_Kind (Seq) = Cv_Seq_Kind_Subdiv2d) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Subdiv2d;


   function Cv_Create_Slice (Start_Index : Integer;
                     End_Index   : Integer := Cv_Whole_Seq_End_Index)
                     return Cv_Slice is
      Slice : Cv_Slice;
   begin
      Slice.Start_Index := Start_Index;
      Slice.End_Index := End_Index;
      return Slice;
   end Cv_Create_Slice;

   function Cv_Create_Term_Criteria (T_Type  : Integer; Max_Iter : Integer;
                                     Epsilon : Long_Float) return Cv_Term_Criteria is
      T : Cv_Term_Criteria;
   begin
      T.Term_Type := T_Type;
      T.Max_Iter := Max_Iter;
      T.Epsilon := Epsilon;

      return T;
   end Cv_Create_Term_Criteria;


   function Cv_Is_Hist (Hist : Cv_Histogram_P) return Integer is
   begin
      if not (Hist = null) then
         if ((Unsigned_32 (Hist.all.Histtype) and Cv_Magic_Mask) = Cv_Hist_Magic_Val) and not (Hist.all.Bins = null) then
            return 1;
         end if;
      end if;
      return 0;
   end Cv_Is_Hist;

   function Cv_Is_Uniform_Hist (Hist : Cv_Histogram_P) return Integer is
   begin
      if not ((Unsigned_32 (Hist.all.Histtype) and Cv_Hist_Uniform_Flag) = 0) then
         return 1;
      end if;
      return 0;
   end Cv_Is_Uniform_Hist;

   function Cv_Is_Sparse_Hist (Hist : Cv_Histogram_P) return Integer is
   begin
      return Cv_Is_Sparse_Mat (From_Arr (Hist.all.Bins));
   end Cv_Is_Sparse_Hist;

   function Cv_Hist_Has_Ranges (Hist : Cv_Histogram_P) return Integer is
   begin
      if not ((Unsigned_32 (Hist.all.Histtype) and Cv_Hist_Ranges_Flag) = 0 ) then
         return 1;
      end if;
      return 0;
   end Cv_Hist_Has_Ranges;


   function Ipl_To_Cv_Depth (Depth : Unsigned_32) return Integer is
      Temp : Unsigned_32 := 0;
   begin
      if (Unsigned_32 (Depth) and Ipl_Depth_Sign) > 0 then
         Temp := 20;
      end if;
      return Integer ((Shift_Right (
        Unsigned_32 (Cv_8u) +
          Shift_Left (Unsigned_32 (Cv_16u), 4)  +
          Shift_Left (Unsigned_32 (Cv_32f), 8)  +
          Shift_Left (Unsigned_32 (Cv_64f), 16) +
          Shift_Left (Unsigned_32 (Cv_8s), 20)  +
          Shift_Left (Unsigned_32 (Cv_16s), 24) +
          Shift_Left (Unsigned_32 (Cv_32s), 28),
        Integer (Shift_Right (Unsigned_32 (Depth) and Unsigned_32 (16#F0#),
          2))) +
          Temp) and Unsigned_32 (15));
   end Ipl_To_Cv_Depth;


   function Cv_Is_Mat_Hdr (Mat : Cv_Mat_P) return Integer is
   begin
      if not (Mat = null) then
         if (Unsigned_32 (Mat.all.Mat_Type) and Unsigned_32 (Cv_Magic_Mask)) = Cv_Mat_Magic_Val then
            if (Mat.all.Cols > 0) and (Mat.all.Rows > 0) then
               return 1;
            end if;
         else
            Put_Line ("CV_IS_MAT_HDR: Mat magic val invalid, is" & Unsigned_32'Image (Unsigned_32 (Mat.all.Mat_Type) and Unsigned_32 (Cv_Magic_Mask)));
            Put_Line ("CV_IS_MAT_HDR: Should be" & Cv_Mat_Magic_Val'Img);
         end if;
      end if;
      return 0;
   end Cv_Is_Mat_Hdr;


   function Cv_Is_Mask_Arr (Mat : Cv_Mat_P) return Integer is
   begin
      if (Unsigned_32 (Mat.all.Mat_Type) and (Cv_Mat_Type_Mask and (not Unsigned_32 (Cv_Make_Type (Cv_8s, 1))))) = 0 then
         return 1;
      else
         return 0;
      end if;

   end Cv_Is_Mask_Arr;

   function Cv_Are_Types_Eq (Mat1 : Cv_Mat_P;
                             Mat2 : Cv_Mat_P) return Integer is
   begin
      if ((Unsigned_32 (Mat1.all.Mat_Type) xor Unsigned_32 (Mat2.all.Mat_Type)) and Cv_Mat_Type_Mask) = 0 then
         return 1;
      end if;
      return 0;
   end Cv_Are_Types_Eq;

   function Cv_Are_Cns_Eq (Mat1 : Cv_Mat_P;
                           Mat2 : Cv_Mat_P) return Integer is
   begin
      if ((Unsigned_32 (Mat1.all.Mat_Type) xor Unsigned_32 (Mat2.all.Mat_Type)) and Unsigned_32 (Cv_Mat_Cn_Mask)) = 0 then
         return 1;
      end if;
      return 0;
   end Cv_Are_Cns_Eq;

   function Cv_Are_Depths_Eq (Mat1 : Cv_Mat_P;
                              Mat2 : Cv_Mat_P) return Integer is
   begin
      if ((Unsigned_32 (Mat1.all.Mat_Type) xor Unsigned_32 (Mat2.all.Mat_Type)) and Unsigned_32 (Cv_Mat_Depth_Mask)) = 0 then
         return 1;
      end if;
      return 0;
   end Cv_Are_Depths_Eq;

   function Cv_Are_Sizes_Eq (Mat1 : Cv_Mat_P;
                             Mat2 : Cv_Mat_P) return Integer is
   begin
      if ((Mat1.all.Rows = Mat2.all.Rows) and (Mat1.all.Cols = Mat2.all.Cols)) then
         return 1;
      end if;
      return 0;
   end Cv_Are_Sizes_Eq;

   function Cv_Is_Mat_Const (Mat : Cv_Mat_P) return Integer is
   begin
      if (Unsigned_32 (Mat.all.Rows) or Unsigned_32 (Mat.all.Cols)) = 1 then
         return 1;
      end if;
      return 0;
   end Cv_Is_Mat_Const;



   function Cv_Mat_Elem_Ptr (Mat : Cv_Mat_P;
                             Row : Integer;
                             Col : Integer) return Cv_8u_Pointer is
   begin
      return Cv_Mat_Elem_Ptr_Fast (Mat, Row, Col, Cv_Elem_Size (Mat.all.Mat_Type));
   end Cv_Mat_Elem_Ptr;

--     function Cv_Mat_Elem (Mat      : Cv_Mat_P;
--                           Elemtype : Unsigned_32; -- used to be Integer
--                           Row      : Integer;
--                           Col      : Integer) return Cv_8u_Pointer is
--     begin
--        return Cv_Mat_Elem_Ptr_Fast (Mat, Row, Col, Elemtype);
--     end Cv_Mat_Elem;


   function Cv_Is_Matnd_Hdr (Mat : Cv_Mat_Nd_P) return Integer is
   begin
      if not (Mat = null) then
         if ( Unsigned_32 (Mat.all.Mat_Type) and Cv_Magic_Mask) = Cv_Matnd_Magic_Val then
            return 1;
         end if;
      end if;
      return 0;
   end Cv_Is_Matnd_Hdr;


   function Cv_Is_Sparse_Mat_Hdr (Mat : Cv_Sparse_Mat_P) return Integer is
   begin
      if not (Mat = null) then
         if (Unsigned_32 (Mat.all.Mat_Type) and Cv_Magic_Mask) = Cv_Sparse_Mat_Magic_Val then
            return 1;
         end if;
      end if;
      return 0;
   end Cv_Is_Sparse_Mat_Hdr;

   --     function CvMat (Rows   : Integer;
   --                     Cols   : Integer;
   --                     M_Type : Unsigned_32;
   --                     Data   : Mat_Data_P)
   --                     return Cv_Mat is
   --        Mat      : Cv_Mat;
   --        Mat_Type : Unsigned_32;
   --     begin
   --        Mat_Type := CV_MAT_TYPE (M_Type);
   --
   --        Mat.Mat_Type := CV_MAT_MAGIC_VAL or CV_MAT_CONT_FLAG or Unsigned_32 (Mat_Type);
   --        Put_Line ("Mat_Type:" & Unsigned_32'Image (Mat_Type));
   --        Put_Line ("mat.Mat_Type:" & Mat.Mat_Type'Img);
   --        Put_Line ("CV_MAT_MAGIC_VAL:" & Cv_Mat_Magic_Val'Img);
   --        Put_Line ("CV_MAT_CONT_FLAG:" & Cv_Mat_Cont_Flag'Img);
   --  --        Mat.Mat_Type := CV_MAT_MAGIC_VAL or Unsigned_32 (Mat_Type);
   --        Mat.Cols := Cols;
   --        Mat.Rows := Rows;
   --        Mat.Step := Mat.Cols * Integer(CV_ELEM_SIZE (Mat_Type));
   --        if not (Data = null) then
   --           Put_Line ("Data not null, adding...");
   --           Mat.Data := Data.all;
   --        end if;
   --        Mat.Refcount := null;
   --        Mat.Hdr_Refcount := 0;
   --
   --        return Mat;
   --     end CvMat;

   function Cv_Mat_Cn_Mask return Unsigned_32 is
   begin
      return Shift_Left ((Cv_Cn_Max - 1), Integer (Cv_Cn_Shift));
   end Cv_Mat_Cn_Mask;

   function Cv_Mat_Cn (Flags : Unsigned_32) return Unsigned_32 is -- used to be Integer
   begin
      return Shift_Left (Unsigned_32 (Flags) and Unsigned_32 (Cv_Mat_Cn_Mask), Integer (Cv_Cn_Shift)) + 1;
   end Cv_Mat_Cn;

   function Cv_Elem_Size_1 (E_Type : Unsigned_32) return Unsigned_32 is -- used to be Integer
   begin
      -- ((((sizeof(size_t)<<28)|0x8442211) >> CV_MAT_DEPTH(type)*4) & 15)
      return Shift_Right (
        (Shift_Left (
        Unsigned_32 (Interfaces.C.Size_T'Size), 28) or Unsigned_32 (16#8442211#)),
        Integer (Cv_Mat_Depth (E_Type) * 4)) and Unsigned_32 (15);
   end Cv_Elem_Size_1;

   function Cv_Elem_Size (E_Type : Unsigned_32) return Unsigned_32 is -- used to be Integer
   begin
      -- (CV_MAT_CN(type) << ((((sizeof(size_t)/4+1)*16384|0x3a50) >> CV_MAT_DEPTH(type)*2) & 3))
      return Shift_Left (Unsigned_32 (Cv_Mat_Cn (E_Type)),
        Integer (Shift_Right ((Unsigned_32 ((Interfaces.C.Size_T'Size / 32 + 1) * 16384) or Unsigned_32 (16#3a50#)),
          Integer ((Cv_Mat_Depth (E_Type) * 2))) and Unsigned_32 (3)));
   end Cv_Elem_Size;

   function Cv_Is_Mat_Cont (Flags : Integer) return Boolean is
   begin
      if (Unsigned_32 (Flags) and Cv_Mat_Cont_Flag) /= 0 then
         return True;
      else
         return False;
      end if;
   end Cv_Is_Mat_Cont;

   function Cv_Is_Temp_Mat (Flags : Integer) return Boolean is
   begin
      if (Unsigned_32 (Flags) and Cv_Mat_Temp_Flag) /= 0 then
         return True;
      else
         return False;
      end if;
   end Cv_Is_Temp_Mat;

   function Cv_Mat_Type (Flags : Unsigned_32) return Unsigned_32 is
   begin
      return Flags and Cv_Mat_Type_Mask;
   end Cv_Mat_Type;

   function Cv_Maketype (Depth : Integer; Cn : Integer) return Unsigned_32 is
   begin
      return Cv_Mat_Depth (Unsigned_32 (Depth)) + Shift_Left (Unsigned_32 (Cn - 1), Integer (Cv_Cn_Shift));
   end Cv_Maketype;

   function Cv_Mat_Depth (M_Type : Unsigned_32) return Unsigned_32 is
   begin
      return M_Type and Cv_Mat_Depth_Mask;
   end Cv_Mat_Depth;


   function Cv_Mat_Depth_Mask return Unsigned_32 is
   begin
      return Cv_Depth_Max - 1;
   end Cv_Mat_Depth_Mask;

   function Cv_Is_Image_Hdr (Img : Ipl_Image_P) return Integer is
   begin
      if not (Img = null) then
         if (Img.all.N_Size = Ipl_Image_Magic_Val) then
            return 1;
         end if;
      end if;
      return 0;
   end Cv_Is_Image_Hdr;

   function Cv_Is_Image (Img : Ipl_Image_P) return Integer is
      use Core.Cv_8u_Pointer_Pkg;
   begin
      if Cv_Is_Image_Hdr (Img) = 1 then
         if not (Img.all.Image_Data = null) then
            return 1;
         end if;
      end if;
      return 0;
   end Cv_Is_Image;


   function Cv_Inv_Sqrt (Value : Float)
                       return Float is
   begin
      return (1.0 / Value_Functions.Sqrt (Value));
   end Cv_Inv_Sqrt;

   function Cv_Sqrt (Value : Float)
                    return Float is
   begin
      return Value_Functions.Sqrt (Value);
   end Cv_Sqrt;


   function Cv_Create_Point (X : Integer; Y : Integer) return Cv_Point is
      Point : Cv_Point;
   begin
      Point.X := X;
      Point.Y := Y;

      return Point;
   end Cv_Create_Point;

   function Cv_Create_Point_2d_32f (X : Long_Float; Y : Long_Float)
                                    return Cv_Point_2d_32f is
      Point : Cv_Point_2d_32f;
   begin
      Point.X := Float (X);
      Point.Y := Float (Y);

      return Point;
   end Cv_Create_Point_2d_32f;

   function Cv_Point_To_32f (Point : Cv_Point) return Cv_Point_2d_32f is
      P : Cv_Point_2d_32f;
   begin
      P.X := Float (Point.X);
      P.Y := Float (Point.Y);

      return P;
   end Cv_Point_To_32f;

   function Cv_Point_From_32f (Point : Cv_Point_2d_32f) return Cv_Point is
      P : Cv_Point;
   begin
      P.X := Integer (Point.X);
      P.Y := Integer (Point.Y);

      return P;
   end Cv_Point_From_32f;

   function Cv_Create_Point_3d_32f (X : Long_Float; Y : Long_Float;
                                    Z : Long_Float) return Cv_Point_3d_32f is
      Point : Cv_Point_3d_32f;
   begin
      Point.X := Float (X);
      Point.Y := Float (Y);
      Point.Z := Float (Z);

      return Point;
   end Cv_Create_Point_3d_32f;

   function Cv_Create_Point_2d_64f (X : Long_Float; Y : Long_Float)
                          return Cv_Point_2d_64f is
      Point : Cv_Point_2d_64f;
   begin
      Point.X := X;
      Point.Y := Y;

      return Point;
   end Cv_Create_Point_2d_64f;

   function Cv_Create_Point_3d_64f (X : Long_Float; Y : Long_Float;
                                    Z : Long_Float) return Cv_Point_3d_64f is
      Point : Cv_Point_3d_64f;
   begin
      Point.X := X;
      Point.Y := Y;
      Point.Z := Z;

      return Point;
   end Cv_Create_Point_3d_64f;


   function Cv_Create_Scalar (V0 : Long_Float; V1 : Long_Float := 0.0;
                              V2 : Long_Float := 0.0; V3 : Long_Float := 0.0)
                              return Cv_Scalar is
      Scalar : Cv_Scalar;
   begin
      Scalar.Val := (V0, V1, V2, V3);
      return Scalar;
   end Cv_Create_Scalar;

   function Cv_Real_Scalar (V0 : Long_Float) return Cv_Scalar is
      Scalar : Cv_Scalar;
   begin
      Scalar.Val := (V0, 0.0, 0.0, 0.0);
      return Scalar;
   end Cv_Real_Scalar;

   function Cv_Scalar_All (V0123 : Long_Float) return Cv_Scalar is
      Scalar : Cv_Scalar;
   begin
      Scalar.Val := (V0123, V0123, V0123, V0123);
      return Scalar;
   end Cv_Scalar_All;


   function Cv_Create_Size (Width : Integer; Height : Integer) return Cv_Size is
      Size : Cv_Size;
   begin
      Size.Width := Width;
      Size.Height := Height;
      return Size;
   end Cv_Create_Size;

   function Cv_Create_Size_2d_32f (Width : Float; Height : Float)
                                   return Cv_Size_2d_32f is
      Size : Cv_Size_2d_32f;
   begin
      Size.Width := Width;
      Size.Height := Height;
      return Size;
   end Cv_Create_Size_2d_32f;

   -- Cv_Rect ------------------------------------------------------------------
   -----------------------------------------------------------------------------
   function Cv_Create_Rect (X : Integer; Y : Integer; Width : Integer; Height : Integer)
                            return Cv_Rect is
      Rect : Cv_Rect;
   begin
      Rect.X := X;
      Rect.Y := Y;
      Rect.Width := Width;
      Rect.Height := Height;

      return Rect;
   end Cv_Create_Rect;

   function Cv_Rect_To_Roi (Rect : Cv_Rect; Coi : Integer) return Ipl_Roi is
      Roi : Ipl_Roi;
   begin
      Roi.X_Offset := Rect.X;
      Roi.Y_Offset := Rect.Y;
      Roi.Width := Rect.Width;
      Roi.Height := Rect.Height;
      Roi.Coi := Coi;

      return Roi;
   end Cv_Rect_To_Roi;

   function Cv_Roi_To_Rect (Roi : Ipl_Roi) return Cv_Rect is
   begin
      return Cv_Create_Rect (Roi.X_Offset, Roi.Y_Offset, Roi.Width, Roi.Height);
   end Cv_Roi_To_Rect;



   function "+" (Right : Ipl_Image_P) return Cv_Arr_P is
   begin
      return Image_To_Arr (Right);
   end "+";

   function Cv_Is_Mat (Mat : Cv_Mat_P) return Integer is
      use Core.Cv_8u_Pointer_Pkg;
   begin
      if not ( Mat = null) then
         if (Cv_Is_Mat_Hdr (Mat) > 0) and Mat.all.Data.Cv_8u /= null then
            return 1;
         elsif Cv_Is_Mat_Hdr (Mat) = 0 then
            Put_Line ("CV_IS_MAT: Header for Mat invalid");
         elsif Mat.all.Data.Cv_8u = null then
            Put_Line ("CV_IS_MAT: Data in Mat is (null)");
         end if;
      else
         Put_Line ("CV_IS_MAT: Mat is (null)");
      end if;
      return 0;
   end Cv_Is_Mat;


   function Cv_Mat_Elem_Ptr_Fast (Mat      : Cv_Mat_P;
                                  Row      : Integer;
                                  Col      : Integer;
                                  Pix_Size : Unsigned_32) return Cv_8u_Pointer is
      use Core.Cv_8u_Pointer_Pkg;
   begin
      return Mat.all.Data.Cv_8u + Interfaces.C.Ptrdiff_T (Mat.all.Step * (Row) + Integer (Pix_Size) * (Col));
   end Cv_Mat_Elem_Ptr_Fast;

   function "+" (Right : String) return String_C is
   begin
      return String_C (Right & Ascii.Nul);
   end "+";

   -----------------------------------------------------------------------------
   -- Inline functions
   -----------------------------------------------------------------------------
   function Cv_Round (Value : Long_Float) return Integer is
   begin
      return Integer (Long_Float'Rounding (Value));
   end Cv_Round;

   function Cv_Round (Value : Float) return Integer is
   begin
      return Integer (Float'Rounding (Value));
   end Cv_Round;


   -----------------------------------------------------------------------------
   -- 2d array functions
   -----------------------------------------------------------------------------
   function To_2d_Pointer (Src : access Cv_8u_2d_Array)
                           return Cv_8u_Pointer_Array is
      D        : aliased Cv_8u_2d_Array := Src.all;
      Dst      : Cv_8u_Pointer_Array (D'Range);
   begin
      Dst := (others => null);
      for I in Integer range D'Range loop
         Dst (I) := D (I, D'First (2))'Unchecked_Access;
      end loop;
      return Dst;
   end To_2d_Pointer;

   function To_2d_Pointer (Src : access Cv_8s_2d_Array)
                           return Cv_8s_Pointer_Array is
      D        : aliased Cv_8s_2d_Array := Src.all;
      Dst      : Cv_8s_Pointer_Array (D'Range);
   begin
      Dst := (others => null);
      for I in Integer range D'Range loop
         Dst (I) := D (I, D'First (2))'Unchecked_Access;
      end loop;
      return Dst;
   end To_2d_Pointer;

   function To_2d_Pointer (Src : access Cv_16u_2d_Array)
                           return Cv_16u_Pointer_Array is
      D        : aliased Cv_16u_2d_Array := Src.all;
      Dst      : Cv_16u_Pointer_Array (D'Range);
   begin
      Dst := (others => null);
      for I in Integer range D'Range loop
         Dst (I) := D (I, D'First (2))'Unchecked_Access;
      end loop;
      return Dst;
   end To_2d_Pointer;

   function To_2d_Pointer (Src : access Cv_16s_2d_Array)
                           return Cv_16s_Pointer_Array is
      D        : aliased Cv_16s_2d_Array := Src.all;
      Dst      : Cv_16s_Pointer_Array (D'Range);
   begin
      Dst := (others => null);
      for I in Integer range D'Range loop
         Dst (I) := D (I, D'First (2))'Unchecked_Access;
      end loop;
      return Dst;
   end To_2d_Pointer;

   function To_2d_Pointer (Src : access Cv_32s_2d_Array)
                           return Cv_32s_Pointer_Array is
      D        : aliased Cv_32s_2d_Array := Src.all;
      Dst      : Cv_32s_Pointer_Array (D'Range);
   begin
      Dst := (others => null);
      for I in Integer range D'Range loop
         Dst (I) := D (I, D'First (2))'Unchecked_Access;
      end loop;
      return Dst;
   end To_2d_Pointer;

   function To_2d_Pointer (Src : access Cv_32f_2d_Array)
                           return Cv_32f_Pointer_Array is
      D        : aliased Cv_32f_2d_Array := Src.all;
      Dst      : Cv_32f_Pointer_Array (D'Range); --:= (null,null,null,null); --:= (others => null);
   begin
      Dst := (others => null);
      for I in Integer range D'Range loop
         Dst (I) := D (I, D'First (2))'Unchecked_Access;
      end loop;
      return Dst;
   end To_2d_Pointer;

   function To_2d_Pointer (Src : access Cv_64f_2d_Array)
                           return Cv_64f_Pointer_Array is
      D        : aliased Cv_64f_2d_Array := Src.all;
      Dst      : Cv_64f_Pointer_Array (D'Range);
   begin
      Dst := (others => null);
      for I in Integer range D'Range loop
         Dst (I) := D (I, D'First (2))'Unchecked_Access;
      end loop;
      return Dst;
   end To_2d_Pointer;

   function To_2d_Pointer (Src : access Cv_Point_2d_Array)
                           return Cv_Point_Pointer_Array is
      D        : aliased Cv_Point_2d_Array := Src.all;
      Dst      : Cv_Point_Pointer_Array (D'Range);
   begin
      Dst := (others => null);
      for I in Integer range D'Range loop
         Dst (I) := D (I, D'First (2))'Unchecked_Access;
      end loop;
      return Dst;
   end To_2d_Pointer;
end Core;
