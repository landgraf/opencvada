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
-- core_c.adb - core.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------
package body Core.Core_C is
-- CvFree
   procedure CvFree (Ptr : access Cv_Void_P) is
      Temp_Ptr : access Cv_Void_P := Ptr;
   begin
      CvFree_Wrapper (Temp_Ptr);
      Temp_Ptr := null;
   end CvFree;

   -- CvReshapeMatND wrapper?
   function CvReshapeND (Arr      : Cv_Arr_P;
                         Header   : Cv_Arr_P;
                         NewCn    : Integer;
                         NewDims  : Integer;
                         NewSizes : Cv_32s_Array) return Cv_Arr_P is
   begin
      return Cv_Arr_P (CvReshapeMatND (Arr, Header'Size, Header, NewCn, NewDims, NewSizes));
   end CvReshapeND;

   procedure CvConvert (Src : Cv_Arr_P;
                        Dst : Cv_Arr_P) is
   begin
      CvConvertScale (Src, Dst, 1.0, 0.0);
   end CvConvert;

   -- wrapper to cvScaleAdd
   procedure CvAXPY (Src1  : Cv_Arr_P;
                     Scale : Long_Float;
                     Src2  : Cv_Arr_P;
                     Dst   : Cv_Arr_P) is
   begin
      CvScaleAdd (Src1, CvRealScalar (Scale), Src2, Dst);
   end CvAXPY;

   procedure CvMatMulAdd (Src1  : access Cv_Arr;
                          Src2  : access Cv_Arr;
                          Src3  : access Cv_Arr;
                          Dst   : access Cv_Arr) is
   begin
      CvGEMM (Src1, Src2, 1.0, Src3, 1.0, Dst, 0);
   end CvMatMulAdd;

   procedure CvMatMul (Src1 : access Cv_Arr;
                       Src2 : access Cv_Arr;
                       Dst  : access Cv_Arr) is
   begin
      CvMatMulAdd (Src1, Src2, null, Dst);
   end CvMatMul;

      -- Allocates a memory buffer in a storage block.
   function CvMemStorageAllocString (Storage : Cv_Mem_Storage_P;
                                     Ptr     : String;
                                     Len     : Integer := -1) return Cv_String is
   begin
      return WCvMemStorageAllocString (Storage => Storage,
                                       Ptr     => +Ptr,
                                       Len     => Len);
   end CvMemStorageAllocString;

   -- Returns the index of a graph vertex.
   function CvGraphVtxIdx (Graph : Cv_Graph_P;
                           Vtx   : Cv_Graph_Vtx_P) return Integer is
   begin
      return Integer(Unsigned_32(Vtx.all.Flags) and CV_SET_ELEM_IDX_MASK);
   end CvGraphVtxIdx;

      -- Returns the index of a graph edge.
   function CvGraphEdgeIdx (Graph : Cv_Graph_P;
                            Edge  : Cv_Graph_Edge_P) return Integer is
   begin
      return Integer(Unsigned_32(Edge.all.Flags) and CV_SET_ELEM_IDX_MASK);
   end CvGraphEdgeIdx;

      --#define cvGraphGetVtxCount( graph ) ((graph)->active_count)
   function CvGraphGetVtxCount (Graph : Cv_Graph_P) return Integer is
   begin
      return Graph.all.Active_Count;
   end CvGraphGetVtxCount;

   --#define cvGraphGetEdgeCount( graph ) ((graph)->edges->active_count)
   function CvGraphGetEdgeCount (Graph : Cv_Graph_P) return Integer is
   begin
      return Graph.all.Edges.all.Active_Count;
   end CvGraphGetEdgeCount;

      --     #define  CV_IS_GRAPH_VERTEX_VISITED(vtx) \
   --      (((CvGraphVtx*)(vtx))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
   function CV_IS_GRAPH_VERTEX_VISISTED (Vtx : Cv_Graph_Vtx_P) return Integer is
   begin
      return Integer (Unsigned_32 (Vtx.all.Flags) and Unsigned_32 (CV_GRAPH_ITEM_VISITED_FLAG));
   end CV_IS_GRAPH_VERTEX_VISISTED;

   --   #define  CV_IS_GRAPH_EDGE_VISITED(edge) \
   --      (((CvGraphEdge*)(edge))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
   function CV_IS_GRAPH_EDGE_VISITED (Edge : Cv_Graph_Edge_P) return Integer is
   begin
      return Integer (Unsigned_32 (Edge.all.Flags) and Unsigned_32 (CV_GRAPH_ITEM_VISITED_FLAG));
   end CV_IS_GRAPH_EDGE_VISITED;

      -- Creates a Cv_Scalar color from RGB values
   function CV_RGB (R : Integer;
                    G : Integer;
                    B : Integer) return Cv_Scalar is
   begin
      return CvScalar (Long_Float (B),
                       Long_Float (G),
                       Long_Float (R),
                       0.0);
   end CV_RGB;

   procedure CV_NEXT_LINE_POINT (LineIterator : Cv_Line_Iterator_P) is
      LineIterator_temp : Cv_Line_Iterator_P := LineIterator;
      Line_Iterator_Mask : Integer;
      Diff               : Ptrdiff_T ;

      use Types_C.C_Point_Arr;
   begin
      if (LineIterator.Err < 0) then
         Line_Iterator_Mask := -1;
      else
         Line_Iterator_Mask := 0;
      end if;

      LineIterator_Temp.all.Err := LineIterator.all.Err + LineIterator.all.Minus_Delta + Integer (Unsigned_32 (LineIterator.all.Plus_Delta) and Unsigned_32 (Line_Iterator_Mask));

      diff :=  ptrdiff_t (LineIterator.all.Minus_Step + Integer (Unsigned_32 (LineIterator.all.Plus_Step) and Unsigned_32 (Line_Iterator_Mask))) ;
      LineIterator_temp.Ptr := C_Point_Arr.Pointer(LineIterator.all.Ptr + Diff);

   end CV_NEXT_LINE_POINT;

   -- Draws a text string.
   procedure CvPutText (Img : Cv_Arr_P;
                        Text : String;
                        Org  : Cv_Point;
                        Font : access Cv_Font;
                        Color : Cv_Scalar) is
   begin
      WCvPutText (Img, +Text, Org, Font, Color);
   end CvPutText;

      -- Retrieves the width and height of a text string.
   procedure CvGetTextSize (TextString : String;
                            Font       : Cv_Font;
                            TextSize   : access Cv_Size;
                            Baseline   : access Integer) is
   begin
      WCvGetTextSize (+TextString, Font, Textsize, Baseline);
   end CvGetTextSize;

   procedure CV_TURN_ON_IPL_COMPATIBILITY is
   begin
      null; -- We dont have this...
   end CV_TURN_ON_IPL_COMPATIBILITY;

   -- use this for func: Enclosing_Entity
   procedure OPENCV_ERROR (Status  : Integer;
                           Func    : String := GNAT.Source_Info.Enclosing_Entity;
                           Context : String := GNAT.Source_Info.Enclosing_Entity;
                           File    : String := GNAT.Source_Info.File;
                           Line    : Integer := GNAT.Source_Info.Line) is
      Retval : Integer;
   begin
      Retval := CvError (Status,
                                                  Interfaces.C.Strings.New_String (Func),
                                                  Interfaces.C.Strings.New_String (Context),
                                                  Interfaces.C.Strings.New_String (File),
                                                  Line);
   end OPENCV_ERROR;

   procedure OPENCV_ERRCHK (Func : String := GNAT.Source_Info.Enclosing_Entity;
                            Context : String := GNAT.Source_Info.Enclosing_Entity;
                            File    : String := GNAT.Source_Info.File;
                            Line    : Integer := GNAT.Source_Info.Line) is
   begin
      if (CvGetErrStatus >= 0) then
         OPENCV_ERROR (Integer(CV_StsBackTrace),
                       (Func),
                       (Context),
                       File,
                       Line);
      end if;
   end OPENCV_ERRCHK;

   procedure OPENCV_ASSERT (Expression : Boolean;
                            Func       : String := GNAT.Source_Info.Enclosing_Entity;
                            Context    : String := GNAT.Source_Info.Enclosing_Entity;
                            File       : String := GNAT.Source_Info.File;
                            Line       : Integer := GNAT.Source_Info.Line) is
   begin
      if not Expression then
         OPENCV_ERROR (Integer(CV_StsInternal),
                       Func,
                       Context,
                       File,
                       Line);
      end if;
   end OPENCV_ASSERT;

   --(cvSetErrStatus(CV_StsOk))
   procedure OPENCV_RSTERR is
   begin
      CvSetErrStatus (Integer(CV_StsOk));
   end OPENCV_RSTERR;

   procedure CV_CHECK (Func : String := GNAT.Source_Info.Enclosing_Entity;
                       Context : String := GNAT.Source_Info.Enclosing_Entity;
                       File    : String := GNAT.Source_Info.File;
                       Line    : Integer := GNAT.Source_Info.Line) is
   begin
      if CvGetErrStatus < 0 then
         CV_ERROR (Integer(CV_StsBackTrace), "Inner function failed.");
      end if;
   end CV_CHECK;

   procedure OPENCV_CALL is
   begin
      null;
   end OPENCV_CALL;
end Core.Core_C;
