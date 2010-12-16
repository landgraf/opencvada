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

   function CV_NODE_TYPE (Flags : Unsigned_32)
                          return Unsigned_32 is
   begin
      return (Flags and Cv_Node_Type_Mask);
   end CV_NODE_TYPE;

   function CV_NODE_IS_INT (Flags : Unsigned_32)
                            return Boolean is
   begin
      if Cv_Node_Type (Flags) = CV_NODE_INT then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_INT;

   function CV_NODE_IS_REAL (Flags : Unsigned_32)
                             return Boolean is
   begin
      if Cv_Node_Type (Flags) = CV_NODE_REAL then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_REAL;

   function CV_NODE_IS_STRING (Flags : Unsigned_32)
                               return Boolean is
   begin
      if Cv_Node_Type (Flags) = CV_NODE_STRING then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_STRING;

   function CV_NODE_IS_SEQ (Flags : Unsigned_32)
                            return Boolean is
   begin
      if Cv_Node_Type (Flags) = CV_NODE_SEQ then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_SEQ;

   function CV_NODE_IS_MAP (Flags : Unsigned_32)
                            return Boolean is
   begin
      if Cv_Node_Type (Flags) = CV_NODE_MAP then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_MAP;

   function CV_NODE_IS_COLLECTION (Flags : Unsigned_32)
                                   return Boolean is
   begin
      if Cv_Node_Type (Flags) >= CV_NODE_SEQ then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_COLLECTION;

   function CV_NODE_IS_FLOW (Flags : Unsigned_32)
                             return Boolean is
   begin
      if (Flags and CV_NODE_FLOW) /= 0 then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_FLOW;

   function CV_NODE_IS_EMPTY (Flags : Unsigned_32)
                              return Boolean is
   begin
      if (Flags and CV_NODE_EMPTY) /= 0 then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_EMPTY;

   function CV_NODE_IS_USER (Flags : Unsigned_32)
                             return Boolean is
   begin
      if (Flags and CV_NODE_USER) /= 0 then
         return True;
      else
         return False;
      end if;
   end CV_NODE_IS_USER;

   function CV_NODE_HAS_NAME (Flags : Unsigned_32)
                              return Boolean is
   begin
      if (Flags and CV_NODE_NAMED) /= 0 then
         return True;
      else
         return False;
      end if;
   end CV_NODE_HAS_NAME;

   function CV_NODE_SEQ_IS_SIMPLE (Seq : access Cv_Seq)
                                   return Boolean is
   begin
      if (Seq.all.Flags and CV_NODE_SEQ_SIMPLE) /= 0 then
         return True;
      else
         return False;
      end if;
   end CV_NODE_SEQ_IS_SIMPLE;

   function CvAttrList (Attr : Cv_String_Pointer := null;
                        Next : Cv_Attr_List_P := null)
                        return Cv_Attr_List is
      L : Cv_Attr_List;
   begin
      L.Attr := Attr;
      L.Next := Next;
      return L;
   end CvAttrList;

   procedure CV_WRITE_SEQ_ELEM_VAR ( Elem_Ptr : Cv_Arr_Pointer;
                                    Writer   : Cv_Seq_Writer_P ) is
   begin
      if (Writer.all.Ptr - Writer.all.Block_Max) >= 0 then
         CvCreateSeqBlock (Writer);
      end if;
      Memcpy (Writer.all.Ptr'Address, Elem_Ptr'Address, System.CRTL.Size_T (Writer.all.Seq.all.Elem_Size));
      Writer.all.Ptr := Writer.all.Ptr + Ptrdiff_T (Writer.all.Seq.all.Elem_Size);
   end CV_WRITE_SEQ_ELEM_VAR;

   procedure CV_NEXT_SEQ_ELEM (Elem_Size : Integer;
                               Reader    : Cv_Seq_Reader_P) is
   begin
      Reader.all.Ptr := Reader.all.Ptr + Ptrdiff_T (Elem_Size);
      if (Reader.all.Ptr - Reader.all.BlockMax) >= 0 then
         CvChangeSeqBlock (To_Void (Reader), 1);
      end if;
   end CV_NEXT_SEQ_ELEM;

   procedure CV_NEXT_SEQ_ELEM (Elem_Size : Integer;
                               Reader    : Cv_Chain_Pt_Reader_P) is
   begin
      Reader.all.Ptr := Reader.all.Ptr + Ptrdiff_T (Elem_Size);
      if (Reader.all.Ptr - Reader.all.BlockMax) >= 0 then
         CvChangeSeqBlock (To_Void (Reader), 1);
      end if;
   end CV_NEXT_SEQ_ELEM;

   procedure CV_PREV_SEQ_ELEM ( Elem_Size : Integer;
                               Reader    : Cv_Seq_Reader_P ) is
   begin
      Reader.all.Ptr := Reader.all.Ptr - Ptrdiff_T (Elem_Size);
      if (Reader.all.BlockMin - Reader.all.Ptr) > 0 then
         CvChangeSeqBLock (To_Void (Reader), -1);
      end if;
   end CV_PREV_SEQ_ELEM;

   procedure CV_READ_SEQ_ELEM ( Elem  : Cv_Arr_Pointer;
                               Reader : Cv_Seq_Reader_P ) is
   begin
      Memcpy (Elem'Address, Reader.all.Ptr'Address, System.Crtl.Size_T (Reader.all.Seq.all.Elem_Size));
      CV_NEXT_SEQ_ELEM (Reader.all.Seq.all.Elem_Size, Reader);
   end CV_READ_SEQ_ELEM;

   procedure CV_READ_SEQ_ELEM ( Elem  : Unsigned_8;
                               Reader : Cv_Chain_Pt_Reader_P ) is
   begin
      Memcpy (Elem'Address, Reader.all.Ptr'Address, System.Crtl.Size_T (Reader.all.Seq.all.Elem_Size));
      CV_NEXT_SEQ_ELEM (Reader.all.Seq.all.Elem_Size, Reader);
   end CV_READ_SEQ_ELEM;

   procedure CV_REV_READ_SEQ_ELEM ( Elem  : Cv_Arr_Pointer;
                                   Reader : Cv_Seq_Reader_P ) is
   begin
      Memcpy (Elem'Address, Reader.all.Ptr'Address, System.CRTL.Size_T (Reader.all.Seq.all.Elem_Size));
      CV_PREV_SEQ_ELEM (Reader.all.Seq.all.Elem_Size, Reader);
   end CV_REV_READ_SEQ_ELEM;


   procedure CV_READ_CHAIN_POINT ( Pt    : out Cv_Point;
                                  Reader : Cv_Chain_Pt_Reader_P ) is
   begin
      Pt := Reader.all.Pt;
      if not (Reader.all.Ptr = null) then
         CV_READ_SEQ_ELEM (Reader.all.Code, Reader);
         if ((Reader.all.Code and not (Unsigned_8 (7))) = 0) then
            Reader.all.Pt.X := Reader.all.Pt.X + Integer (Reader.all.Deltas (Integer (Reader.all.Code), 1));
            Reader.all.Pt.Y := Reader.all.Pt.Y + Integer (Reader.all.Deltas (Integer (Reader.all.Code), 2));
         end if;
      end if;

   end CV_READ_CHAIN_POINT;

   function CV_CURRENT_POINT ( Reader : Cv_Chain_Pt_Reader_P ) return Cv_Point_P is
   begin
      return From_Arr (Value (Reader.all.Ptr) (1));
   end CV_CURRENT_POINT;

   function CV_PREV_POINT ( Reader : Cv_Chain_Pt_Reader_P) return Cv_Point_P is
   begin
      return From_Arr (Value (Reader.all.PrevElem) (1));
   end CV_PREV_POINT;

   procedure CV_READ_EDGE ( Pt1   : out Cv_Point_P;
                           Pt2    : out Cv_Point_P;
                           Reader : Cv_Chain_Pt_Reader_P ) is
   begin
      Pt1 := CV_PREV_POINT (Reader);
      Pt2 := CV_CURRENT_POINT (Reader);
      Reader.all.PrevElem := Reader.all.Ptr;
      CV_NEXT_SEQ_ELEM (Cv_Point'Size / 8, Reader);
   end CV_READ_EDGE;

   function CV_NEXT_GRAPH_EDGE ( Edge  : Cv_Graph_Edge_P;
                                Vertex : Cv_Graph_Vtx_P ) return Cv_Graph_Edge_P is
   begin
      if (Edge.all.Vtx (1) = Vertex) then
         return Edge.all.Next (1);
      elsif (Edge.all.Vtx (2) = Vertex) then
         return Edge.all.Next (2);
      end if;
      return null;
   end CV_NEXT_GRAPH_EDGE;

   function CV_IS_STORAGE (Storage : Cv_Mem_Storage_P) return Integer is
   begin
      if not (Storage = null) then
         if (Unsigned_32 (Storage.all.Signature) and CV_MAGIC_MASK) = CV_STORAGE_MAGIC_VAL then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_STORAGE;

   function CV_IS_SET_ELEM (Ptr : CV_Set_Elem_P) return Integer is
   begin
      if Ptr.all.Flags >= 0 then
         return 1;
      end if;
      return 0;
   end CV_IS_SET_ELEM;

   --#define CV_IS_SEQ(seq) \
   --    ((seq) != NULL && (((CvSeq*)(seq))->flags & CV_MAGIC_MASK) == CV_SEQ_MAGIC_VAL)
   function CV_IS_SEQ (Seq : Cv_Seq_P) return Integer is
   begin
      if not (Seq = null) then
         if (Seq.all.Flags and CV_MAGIC_MASK) = CV_SEQ_MAGIC_VAL then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_SEQ;

   --#define CV_IS_SET(set) \
   --((set) != NULL && (((CvSeq*)(set))->flags & CV_MAGIC_MASK) == CV_SET_MAGIC_VAL)
   function CV_IS_SET (Set : Cv_Seq_P) return Integer is
   begin
      if not (Set = null) then
         if (Set.all.Flags and CV_MAGIC_MASK) = CV_SET_MAGIC_VAL then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_SET;

   function CV_SEQ_ELTYPE_POINT return Integer is  --/* (x,y) */
   begin
      return CV_MAKETYPE (CV_32S, 2);  --/* (x,y) */
   end CV_SEQ_ELTYPE_POINT;

   function CV_SEQ_ELTYPE_CODE return Integer is
   begin
      return CV_MAKETYPE (CV_8U, 1);
   end CV_SEQ_ELTYPE_CODE;

   function CV_SEQ_ELTYPE_INDEX return Integer is
   begin
      return CV_MAKETYPE (CV_32S, 1);  --/* #(x,y) */
   end CV_SEQ_ELTYPE_INDEX;

   function CV_SEQ_ELTYPE_POINT3D return Integer is
   begin
      return CV_MAKETYPE (CV_32F, 3);  --/* (x,y,z)  */
   end CV_SEQ_ELTYPE_POINT3D;

   function CV_SEQ_POINT_SET return Integer is
   begin
      return Integer (CV_SEQ_KIND_GENERIC or Unsigned_32 (CV_SEQ_ELTYPE_POINT));
   end CV_SEQ_POINT_SET;

   function CV_SEQ_POINT3D_SET return Integer is
   begin
      return Integer (CV_SEQ_KIND_GENERIC or Unsigned_32 (CV_SEQ_ELTYPE_POINT3D));
   end CV_SEQ_POINT3D_SET;

   function CV_SEQ_POLYLINE return Integer is
   begin
      return Integer (CV_SEQ_KIND_CURVE or Unsigned_32 (CV_SEQ_ELTYPE_POINT));
   end CV_SEQ_POLYLINE;

   function CV_SEQ_POLYGON return Integer is
   begin
      return Integer (CV_SEQ_FLAG_CLOSED or Unsigned_32 (CV_SEQ_POLYLINE));
   end CV_SEQ_POLYGON;

   function CV_SEQ_SIMPLE_POLYGON return Integer is
   begin
      return Integer (CV_SEQ_FLAG_SIMPLE or Unsigned_32 (CV_SEQ_POLYGON  ));
   end CV_SEQ_SIMPLE_POLYGON;

   function CV_SEQ_CHAIN return Integer is
   begin
      return Integer (CV_SEQ_KIND_CURVE or Unsigned_32 (CV_SEQ_ELTYPE_CODE));
   end CV_SEQ_CHAIN;

   function CV_SEQ_CHAIN_CONTOUR return Integer is
   begin
      return Integer (CV_SEQ_FLAG_CLOSED or Unsigned_32 (CV_SEQ_CHAIN));
   end CV_SEQ_CHAIN_CONTOUR;

   function CV_SEQ_POLYGON_TREE return Integer is
   begin
      return Integer (CV_SEQ_KIND_BIN_TREE or Unsigned_32 (CV_SEQ_ELTYPE_TRIAN_ATR));
   end CV_SEQ_POLYGON_TREE;

   function CV_SEQ_CONNECTED_COMP return Integer is
   begin
      return Integer (CV_SEQ_KIND_GENERIC or Unsigned_32 (CV_SEQ_ELTYPE_CONNECTED_COMP));
   end CV_SEQ_CONNECTED_COMP;

   function CV_SEQ_INDEX return Integer is
   begin
      return Integer (CV_SEQ_KIND_GENERIC or Unsigned_32 (CV_SEQ_ELTYPE_INDEX));
   end CV_SEQ_INDEX;

   function  CV_SEQ_ELTYPE ( Seq : Cv_Seq_P ) return Integer is
   begin
      return Integer (Seq.all.Flags and CV_SEQ_ELTYPE_MASK);
   end CV_SEQ_ELTYPE;

   function CV_SEQ_KIND ( Seq : Cv_Seq_P ) return Integer is
   begin
      return Integer (Seq.all.Flags and CV_SEQ_KIND_MASK );
   end CV_SEQ_KIND;

   function CV_IS_SEQ_INDEX ( Seq : Cv_Seq_P) return Integer is
   begin
      if ((CV_SEQ_ELTYPE (Seq) = CV_SEQ_ELTYPE_INDEX) and
            (CV_SEQ_KIND (Seq) = CV_SEQ_KIND_GENERIC)) then
         return 1;
      else
         return 0;
      end if;
   end CV_IS_SEQ_INDEX;

   function CV_IS_SEQ_CURVE ( Seq : Cv_Seq_P) return Integer is
   begin
      if (CV_SEQ_KIND (Seq) = CV_SEQ_KIND_CURVE) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_CURVE;

   function CV_IS_SEQ_CLOSED ( Seq : Cv_Seq_P) return Integer is
   begin
      if not ((Seq.all.Flags and CV_SEQ_FLAG_CLOSED) = 0) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_CLOSED;

   function CV_IS_SEQ_CONVEX ( Seq : Cv_Seq_P)   return Integer is
   begin
      if not ((Seq.all.Flags and CV_SEQ_FLAG_CONVEX) = 0) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_CONVEX;

   function CV_IS_SEQ_HOLE ( Seq : Cv_Seq_P) return Integer is
   begin
      if not ((Seq.all.Flags and  CV_SEQ_FLAG_HOLE) = 0) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_HOLE;

   function CV_IS_SEQ_SIMPLE ( Seq : Cv_Seq_P) return Integer is
   begin
      if (not ((Seq.all.Flags and CV_SEQ_FLAG_SIMPLE) = 0) or CV_IS_SEQ_CONVEX (Seq) = 1) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_SIMPLE;

   function CV_IS_SEQ_POINT_SET ( Seq : CV_Seq_P) return Integer is
   begin
      if ((CV_SEQ_ELTYPE (Seq) = CV_MAKE_TYPE (CV_32S, 2)) or (CV_SEQ_ELTYPE (Seq) = CV_MAKE_TYPE (CV_32F, 2))) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_POINT_SET;

   function CV_IS_SEQ_POINT_SUBSET ( Seq : Cv_Seq_P) return Integer is
   begin
      if (CV_IS_SEQ_INDEX ( Seq ) = 1) or (CV_SEQ_ELTYPE (Seq) = CV_SEQ_ELTYPE_PPOINT) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_POINT_SUBSET;

   function CV_IS_SEQ_POLYLINE ( Seq : Cv_Seq_P ) return Integer is
   begin
      if (CV_SEQ_KIND (Seq) = CV_SEQ_KIND_CURVE) and (CV_IS_SEQ_POINT_SET (Seq) = 1) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_POLYLINE;

   function CV_IS_SEQ_POLYGON ( Seq : Cv_Seq_P) return Integer is
   begin
      if (CV_IS_SEQ_POLYLINE (Seq) = 1) and (CV_IS_SEQ_CLOSED (Seq) = 1) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_POLYGON;

   function CV_IS_SEQ_CHAIN ( Seq : Cv_Seq_P) return Integer is
   begin
      if (CV_SEQ_KIND (Seq) = CV_SEQ_KIND_CURVE) and (Seq.all.Elem_Size = 1) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_CHAIN;

   function CV_IS_SEQ_CONTOUR ( Seq : Cv_Seq_P) return Integer is
   begin
      if (CV_IS_SEQ_CLOSED (Seq) = 1 )and ((CV_IS_SEQ_POLYLINE (Seq) = 1) or (CV_IS_SEQ_CHAIN (Seq) = 1)) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_CONTOUR;

   function CV_IS_SEQ_CHAIN_CONTOUR ( Seq  : Cv_Seq_P ) return Integer is
   begin
      if (CV_IS_SEQ_CHAIN ( Seq ) = 1) and (CV_IS_SEQ_CLOSED ( Seq ) = 1) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_CHAIN_CONTOUR;

   function CV_IS_SEQ_POLYGON_TREE ( Seq  : Cv_Seq_P) return Integer is
   begin
      if (CV_SEQ_ELTYPE (Seq) =  CV_SEQ_ELTYPE_TRIAN_ATR) and (CV_SEQ_KIND ( Seq ) =  CV_SEQ_KIND_BIN_TREE ) then
         return 1;
      end if;
      return 0;
   end CV_IS_SEQ_POLYGON_TREE;

   function CV_IS_GRAPH ( Seq : Cv_Seq_P) return Integer is
   begin
      if (CV_IS_SET (Seq) = 1) and (CV_SEQ_KIND (Seq) = CV_SEQ_KIND_GRAPH) then
         return 1;
      end if;
      return 0;
   end CV_IS_GRAPH;
   function CV_IS_GRAPH_ORIENTED ( Seq : Cv_Seq_P) return Integer is
   begin
      if not ((Seq.all.Flags and CV_GRAPH_FLAG_ORIENTED) = 0) then
         return 1;
      end if;
      return 0;
   end CV_IS_GRAPH_ORIENTED;

   function CV_IS_SUBDIV2D ( Seq : Cv_Seq_P) return Integer is
   begin
      if (CV_IS_SET (Seq) = 1) and (CV_SEQ_KIND (Seq) = CV_SEQ_KIND_SUBDIV2D) then
         return 1;
      end if;
      return 0;
   end CV_IS_SUBDIV2D;


   function CvSlice (Start_Index : Integer;
                     End_Index   : Integer := CV_WHOLE_SEQ_END_INDEX)
                     return Cv_Slice is
      Slice : Cv_Slice;
   begin
      Slice.Start_Index := Start_Index;
      Slice.End_Index := End_Index;
      return Slice;
   end CvSlice;

   function CvTermCriteria (T_Type  : Integer; Max_Iter : Integer;
                            Epsilon : Long_Float) return Cv_Term_Criteria is
      T : Cv_Term_Criteria;
   begin
      T.Term_Type := T_Type;
      T.Max_Iter := Max_Iter;
      T.Epsilon := Epsilon;

      return T;
   end CvTermCriteria;


   function CV_IS_HIST (Hist : Cv_Histogram_P) return Integer is
   begin
      if not (Hist = null) then
         if ((Unsigned_32 (Hist.all.HistType) and CV_MAGIC_MASK) = CV_HIST_MAGIC_VAL) and not (Hist.all.Bins = null) then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_HIST;

   function CV_IS_UNIFORM_HIST (Hist : Cv_Histogram_P) return Integer is
   begin
      if not ((Unsigned_32 (Hist.all.HistType) and CV_HIST_UNIFORM_FLAG) = 0) then
         return 1;
      end if;
      return 0;
   end CV_IS_UNIFORM_HIST;

   function CV_IS_SPARSE_HIST (Hist : Cv_Histogram_P) return Integer is
   begin
      return CV_IS_SPARSE_MAT (From_Arr (Hist.all.Bins));
   end CV_IS_SPARSE_HIST;

   function CV_HIST_HAS_RANGES (Hist : Cv_Histogram_P) return Integer is
   begin
      if not ((Unsigned_32 (Hist.all.HistType) and CV_HIST_RANGES_FLAG) = 0 ) then
         return 1;
      end if;
      return 0;
   end CV_HIST_HAS_RANGES;


   function IPL2CV_DEPTH (Depth : Unsigned_32) return Integer is
      Temp : Unsigned_32 := 0;
   begin
      if (Unsigned_32 (Depth) and IPL_DEPTH_SIGN) > 0 then
         Temp := 20;
      end if;
      return Integer ((Shift_Right (
        Unsigned_32 (CV_8U) +
          Shift_Left (Unsigned_32 (CV_16U), 4)  +
          Shift_Left (Unsigned_32 (CV_32F), 8)  +
          Shift_Left (Unsigned_32 (CV_64F), 16) +
          Shift_Left (Unsigned_32 (CV_8S), 20)  +
          Shift_Left (Unsigned_32 (CV_16S), 24) +
          Shift_Left (Unsigned_32 (CV_32S), 28),
        Integer (Shift_Right (Unsigned_32 (Depth) and Unsigned_32 (16#F0#),
          2))) +
          Temp) and Unsigned_32 (15));
   end IPL2CV_DEPTH;


   function CV_IS_MAT_HDR (Mat : Cv_Mat_P) return Integer is
   begin
      if not (Mat = null) then
         if (Unsigned_32 (Mat.all.Mat_Type) and Unsigned_32 (CV_MAGIC_MASK)) = CV_MAT_MAGIC_VAL then
            if (Mat.all.Cols > 0) and (Mat.all.Rows > 0) then
               return 1;
            end if;
         end if;
      end if;
      return 0;
   end CV_IS_MAT_HDR;


   function CV_IS_MASK_ARR (Mat : Cv_Mat_P) return Integer is
   begin
      if (Unsigned_32 (Mat.all.Mat_Type) and (CV_MAT_TYPE_MASK and (not Unsigned_32 (Cv_Make_Type (CV_8S, 1))))) = 0 then
         return 1;
      else
         return 0;
      end if;

   end CV_IS_MASK_ARR;

   function CV_ARE_TYPES_EQ (Mat1 : Cv_Mat_P;
                             Mat2 : Cv_Mat_P) return Integer is
   begin
      if ((Unsigned_32 (Mat1.all.Mat_Type) xor Unsigned_32 (Mat2.all.Mat_Type)) and CV_MAT_TYPE_MASK) = 0 then
         return 1;
      end if;
      return 0;
   end CV_ARE_TYPES_EQ;

   function CV_ARE_CNS_EQ (Mat1 : Cv_Mat_P;
                           Mat2 : Cv_Mat_P) return Integer is
   begin
      if ((Unsigned_32 (Mat1.all.Mat_Type) xor Unsigned_32 (Mat2.all.Mat_Type)) and Unsigned_32 (CV_MAT_CN_MASK)) = 0 then
         return 1;
      end if;
      return 0;
   end CV_ARE_CNS_EQ;

   function CV_ARE_DEPTHS_EQ (Mat1 : Cv_Mat_P;
                              Mat2 : Cv_Mat_P) return Integer is
   begin
      if ((Unsigned_32 (Mat1.all.Mat_Type) xor Unsigned_32 (Mat2.all.Mat_Type)) and Unsigned_32 (CV_MAT_DEPTH_MASK)) = 0 then
         return 1;
      end if;
      return 0;
   end CV_ARE_DEPTHS_EQ;

   function CV_ARE_SIZES_EQ (Mat1 : Cv_Mat_P;
                             Mat2 : Cv_Mat_P) return Integer is
   begin
      if ((Mat1.all.Rows = Mat2.all.Rows) and (Mat1.all.Cols = Mat2.all.Cols)) then
         return 1;
      end if;
      return 0;
   end CV_ARE_SIZES_EQ;

   function CV_IS_MAT_CONST (Mat : Cv_Mat_P) return Integer is
   begin
      if (Unsigned_32 (Mat.all.Rows) or Unsigned_32 (Mat.all.Cols)) = 1 then
         return 1;
      end if;
      return 0;
   end CV_IS_MAT_CONST;



   function CV_MAT_ELEM_PTR (Mat : Cv_Mat_P;
                             Row : Integer;
                             Col : Integer) return Cv_8u_Pointer is
   begin
      return CV_MAT_ELEM_PTR_FAST (Mat, Row, Col, CV_ELEM_SIZE (Mat.all.Mat_Type));
   end CV_MAT_ELEM_PTR;

   function CV_MAT_ELEM (Mat      : Cv_Mat_P;
                         Elemtype : Integer;
                         Row      : Integer;
                         Col      : Integer) return Cv_8u_Pointer is
   begin
      return CV_MAT_ELEM_PTR_FAST (Mat, Row, Col, Elemtype);
   end CV_MAT_ELEM;


   function CV_IS_MATND_HDR (Mat : Cv_Mat_ND_P) return Integer is
   begin
      if not (Mat = null) then
         if ( Unsigned_32 (Mat.all.Mat_Type) and CV_MAGIC_MASK) = CV_MATND_MAGIC_VAL then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_MATND_HDR;


   function CV_IS_SPARSE_MAT_HDR (Mat : Cv_Sparse_Mat_P) return Integer is
   begin
      if not (Mat = null) then
         if (Unsigned_32 (Mat.all.Mat_Type) and CV_MAGIC_MASK) = CV_SPARSE_MAT_MAGIC_VAL then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_SPARSE_MAT_HDR;

   function CvMat (Rows   : Integer;
                   Cols   : Integer;
                   M_Type : Integer;
                   Data   : Mat_Data)
                   return Cv_Mat is
      Mat      : Cv_Mat;
      Mat_Type : Integer;
   begin
      Mat_Type := CV_MAT_TYPE (M_Type);

      Mat.Mat_Type := Integer (CV_MAT_MAGIC_VAL or CV_MAT_CONT_FLAG or Unsigned_32 (Mat_Type));
      Mat.Cols := Cols;
      Mat.Rows := Rows;
      Mat.Step := Mat.Cols * CV_ELEM_SIZE (Mat_Type);
      Mat.Data := Data;
      Mat.Refcount := null;
      Mat.Hdr_Refcount := 0;

      return Mat;
   end CvMat;

   function CV_MAT_CN_MASK return Integer is
   begin
      return Integer (Shift_Left ((Unsigned_32 (CV_CN_MAX - 1)), CV_CN_SHIFT));
   end CV_MAT_CN_MASK;

   function CV_MAT_CN (Flags : Integer) return Integer is
   begin
      return Integer (Shift_Left (Unsigned_32 (Flags) and Unsigned_32 (CV_MAT_CN_MASK), CV_CN_SHIFT)) + 1;
   end CV_MAT_CN;

   function CV_ELEM_SIZE_1 (E_Type : Integer) return Integer is
   begin
      -- ((((sizeof(size_t)<<28)|0x8442211) >> CV_MAT_DEPTH(type)*4) & 15)
      return Integer (Shift_Right (
        (Shift_Left (
        Unsigned_32 (Interfaces.C.Size_T'Size), 28) or Unsigned_32 (16#8442211#)),
        CV_MAT_DEPTH (E_Type) * 4) and Unsigned_32 (15));
   end CV_ELEM_SIZE_1;

   function CV_ELEM_SIZE (E_Type : Integer) return Integer is
   begin
      -- (CV_MAT_CN(type) << ((((sizeof(size_t)/4+1)*16384|0x3a50) >> CV_MAT_DEPTH(type)*2) & 3))
      return Integer (Shift_Left (Unsigned_32 (CV_MAT_CN (E_Type)),
        Integer (Shift_Right ((Unsigned_32 ((Interfaces.C.Size_T'Size / 32 + 1) * 16384) or Unsigned_32 (16#3a50#)),
          (CV_MAT_DEPTH (E_Type) * 2)) and Unsigned_32 (3))));
   end CV_ELEM_SIZE;

   function CV_IS_MAT_CONT (Flags : Integer) return Boolean is
   begin
      if (Unsigned_32 (Flags) and CV_MAT_CONT_FLAG) /= 0 then
         return True;
      else
         return False;
      end if;
   end CV_IS_MAT_CONT;

   function CV_IS_TEMP_MAT (Flags : Integer) return Boolean is
   begin
      if (Unsigned_32 (Flags) and CV_MAT_TEMP_FLAG) /= 0 then
         return True;
      else
         return False;
      end if;
   end CV_IS_TEMP_MAT;

   function CV_MAT_TYPE (Flags : Integer) return Integer is
   begin
      return Integer (Unsigned_32 (Flags) and Unsigned_32 (CV_MAT_TYPE_MASK));
   end CV_MAT_TYPE;

   function CV_MAKETYPE (Depth : Integer; Cn : Integer) return Integer is
   begin
      return CV_MAT_DEPTH (Depth) + Integer (Shift_Left (Unsigned_32 (Cn - 1), CV_CN_SHIFT));
   end CV_MAKETYPE;

   function CV_MAT_DEPTH (M_Type : Integer) return Integer is
   begin
      return Integer (Unsigned_32 (M_Type) and Unsigned_32 (CV_MAT_DEPTH_MASK));
   end CV_MAT_DEPTH;


   function CV_MAT_DEPTH_MASK return Integer is
   begin
      return CV_DEPTH_MAX - 1;
   end CV_MAT_DEPTH_MASK;

   function CV_IS_IMAGE_HDR (Img : Ipl_Image_P) return Integer is
   begin
      if not (Img = null) then
         if (Img.all.N_Size = IPL_IMAGE_MAGIC_VAL) then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_IMAGE_HDR;

   function CV_IS_IMAGE (Img : IPL_IMAGE_P) return Integer is
   begin
      if CV_IS_IMAGE_HDR (Img) = 1 then
         if not (Img.all.Image_Data = Null_Ptr) then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_IMAGE;


   function CvInvSqrt (Value : Float)
                       return Float is
   begin
      return (1.0 / Value_Functions.Sqrt (Value));
   end CvInvSqrt;

   function CvSqrt (Value : Float)
                    return Float is
   begin
      return Value_Functions.Sqrt (Value);
   end CvSqrt;


   function CvPoint (X : Integer; Y : Integer) return Cv_Point is
      Point : Cv_Point;
   begin
      Point.X := X;
      Point.Y := Y;

      return Point;
   end CvPoint;

   function CvPoint2D32f (X : Long_Float; Y : Long_Float)
                          return Cv_Point_2D_32f is
      Point : Cv_Point_2D_32f;
   begin
      Point.X := Float (X);
      Point.Y := Float (Y);

      return Point;
   end CvPoint2D32f;

   function CvPointTo32f (Point : Cv_Point) return Cv_Point_2D_32f is
      P : Cv_Point_2D_32f;
   begin
      P.X := Float (Point.X);
      P.Y := Float (Point.Y);

      return P;
   end CvPointTo32f;

   function CvPointFrom32f (Point : Cv_Point_2D_32f) return Cv_Point is
      P : Cv_Point;
   begin
      P.X := Integer (Point.X);
      P.Y := Integer (Point.Y);

      return P;
   end CvPointFrom32f;

   function CvPoint3D32f (X : Long_Float; Y : Long_Float;
                          Z : Long_Float) return Cv_Point_3D_32f is
      Point : Cv_Point_3D_32f;
   begin
      Point.X := Float (X);
      Point.Y := Float (Y);
      Point.Z := Float (Z);

      return Point;
   end CvPoint3D32f;

   function CvPoint2D64f (X : Long_Float; Y : Long_Float)
                          return Cv_Point_2D_64f is
      Point : Cv_Point_2d_64f;
   begin
      Point.X := X;
      Point.Y := Y;

      return Point;
   end CvPoint2D64f;

   function CvPoint3D64f (X : Long_Float; Y : Long_Float;
                          Z : Long_Float) return Cv_Point_3D_64f is
      Point : Cv_Point_3D_64f;
   begin
      Point.X := X;
      Point.Y := Y;
      Point.Z := Z;

      return Point;
   end CvPoint3D64f;


   function CvScalar (V0 : Long_Float; V1 : Long_Float := 0.0;
                      V2 : Long_Float := 0.0; V3 : Long_Float := 0.0)
                      return Cv_Scalar is
      Scalar : Cv_Scalar;
   begin
      Scalar.Val := (V0, V1, V2, V3);
      return Scalar;
   end CvScalar;

   function CvRealScalar (V0 : Long_Float) return Cv_Scalar is
      Scalar : Cv_Scalar;
   begin
      Scalar.Val := (V0, 0.0, 0.0, 0.0);
      return Scalar;
   end CvRealScalar;

   function CvScalarAll (V0123 : Long_Float) return Cv_Scalar is
      Scalar : Cv_Scalar;
   begin
      Scalar.Val := (V0123, V0123, V0123, V0123);
      return Scalar;
   end CvScalarAll;


   function CvSize (Width : Integer; Height : Integer) return Cv_Size is
      Size : Cv_Size;
   begin
      Size.Width := Width;
      Size.Height := Height;
      return Size;
   end CvSize;

   function CvSize2d32f (Width : Float; Height : Float)
                         return Cv_Size_2d_32f is
      Size : Cv_Size_2d_32f;
   begin
      Size.Width := Width;
      Size.Height := Height;
      return Size;
   end CvSize2d32f;

   -- Cv_Rect ------------------------------------------------------------------
   -----------------------------------------------------------------------------
   function CvRect (X : Integer; Y : Integer; Width : Integer; Height : Integer)
                    return Cv_Rect is
      Rect : Cv_Rect;
   begin
      Rect.X := X;
      Rect.Y := Y;
      Rect.Width := Width;
      Rect.Height := Height;

      return Rect;
   end CvRect;

   function CvRectToROI (Rect : Cv_Rect; Coi : Integer) return Ipl_ROI is
      Roi : Ipl_ROI;
   begin
      Roi.X_Offset := Rect.X;
      Roi.Y_Offset := Rect.Y;
      Roi.Width := Rect.Width;
      Roi.Height := Rect.Height;
      Roi.Coi := Coi;

      return Roi;
   end CvRectToROI;

   function CvROIToRect (Roi : Ipl_ROI) return Cv_Rect is
   begin
      return CvRect (Roi.X_Offset, Roi.Y_Offset, Roi.Width, Roi.Height);
   end CvROIToRect;



   function "+" (Right : Ipl_Image_P) return Cv_Arr_P is
   begin
      return Image_To_Arr (Right);
   end "+";

   function CV_IS_MAT (Mat : Cv_Mat_P) return Integer is
   begin
      if not ( MAt = null) then
         if (CV_IS_MAT_HDR (Mat) = 1) and Mat.all.Data.Cv_8u /= null then
            return 1;
         end if;
      end if;
      return 0;
   end CV_IS_MAT;


   function CV_MAT_ELEM_PTR_FAST (Mat      : Cv_Mat_P;
                                  Row      : Integer;
                                  Col      : Integer;
                                  Pix_Size : Integer) return Cv_8u_Pointer is
   begin
      return Mat.all.Data.Cv_8u + Interfaces.C.Ptrdiff_T (Mat.all.Step * (Row) + (Pix_Size) * (Col));
   end CV_MAT_ELEM_PTR_FAST;

   function "+" (Right : String) return String_C is
   begin
      return String_C (Right & ASCII.NUL);
   end "+";

   -----------------------------------------------------------------------------
   -- Inline functions
   -----------------------------------------------------------------------------
   function CvRound (Value : Long_Float) return Integer is
   begin
      return Integer (Long_Float'Rounding (Value));
   end CvRound;

   function CvRound (Value : Float) return Integer is
   begin
      return Integer (Float'Rounding (Value));
   end CvRound;


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
      D        : aliased Cv_64F_2d_Array := Src.all;
      Dst      : Cv_64F_Pointer_Array (D'Range);
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
