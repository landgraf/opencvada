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
package body Core.Operations is
-- CvFree
   procedure Cv_Free (Ptr : access Cv_Void_Ptr) is
      Temp_Ptr : constant access Cv_Void_Ptr := Ptr;
   begin
      Cv_Free_Wrapper (Temp_Ptr);
      Temp_Ptr.all := null;
   end Cv_Free;

   -- CvReshapeMatND wrapper?
   function Cv_Reshape_Nd (Arr      : Cv_Arr_Ptr;
                           Header   : Cv_Arr_Ptr;
                           Newcn    : Integer;
                           Newdims  : Integer;
                           Newsizes : Cv_32s_Array) return Cv_Arr_Ptr is
   begin
      return Cv_Arr_Ptr (Cv_Reshape_Mat_Nd (Arr, Header'Size / 8, Header, Newcn, Newdims, Newsizes));
   end Cv_Reshape_Nd;

   procedure Cv_Convert (Src : Cv_Arr_Ptr;
                         Dst : Cv_Arr_Ptr) is
   begin
      Cv_Convert_Scale (Src, Dst, 1.0, 0.0);
   end Cv_Convert;
   procedure Cv_Convert (Src : Cv_Mat_Ptr;
                         Dst : Cv_Mat_Ptr) is
   begin
      Cv_Convert_Scale (Src, Dst, 1.0, 0.0);
   end Cv_Convert;
   procedure Cv_Convert (Src : Ipl_Image_Ptr;
                         Dst : Ipl_Image_Ptr) is
   begin
      Cv_Convert_Scale (Src, Dst, 1.0, 0.0);
   end Cv_Convert;

   -- wrapper to cvScaleAdd
   procedure Cv_Axpy (Src1  : Cv_Arr_Ptr;
                      Scale : Long_Float;
                      Src2  : Cv_Arr_Ptr;
                      Dst   : Cv_Arr_Ptr) is
   begin
      Cv_Scale_Add (Src1, Cv_Real_Scalar (Scale), Src2, Dst);
   end Cv_Axpy;
   procedure Cv_Axpy (Src1  : Cv_Mat_Ptr;
                      Scale : Long_Float;
                      Src2  : Cv_Mat_Ptr;
                      Dst   : Cv_Mat_Ptr) is
   begin
      Cv_Scale_Add (Src1, Cv_Real_Scalar (Scale), Src2, Dst);
   end Cv_Axpy;
      procedure Cv_Axpy (Src1  : Ipl_Image_Ptr;
                      Scale : Long_Float;
                      Src2  : Ipl_Image_Ptr;
                      Dst   : Ipl_Image_Ptr) is
   begin
      Cv_Scale_Add (Src1, Cv_Real_Scalar (Scale), Src2, Dst);
   end Cv_Axpy;

   procedure Cv_Mat_Mul_Add (Src1  : Cv_Arr_Ptr;
                             Src2  : Cv_Arr_Ptr;
                             Src3  : Cv_Arr_Ptr;
                             Dst   : Cv_Arr_Ptr) is
   begin
      Cv_Gemm (Src1, Src2, 1.0, Src3, 1.0, Dst, 0);
   end Cv_Mat_Mul_Add;
   procedure Cv_Mat_Mul_Add (Src1  : Cv_Mat_Ptr;
                             Src2  : Cv_Mat_Ptr;
                             Src3  : Cv_Mat_Ptr;
                             Dst   : Cv_Mat_Ptr) is
   begin
      Cv_Gemm (Src1, Src2, 1.0, Src3, 1.0, Dst, 0);
   end Cv_Mat_Mul_Add;
   procedure Cv_Mat_Mul_Add (Src1  : Ipl_Image_Ptr;
                             Src2  : Ipl_Image_Ptr;
                             Src3  : Ipl_Image_Ptr;
                             Dst   : Ipl_Image_Ptr) is
   begin
      Cv_Gemm (Src1, Src2, 1.0, Src3, 1.0, Dst, 0);
   end Cv_Mat_Mul_Add;


   procedure Cv_Mat_Mul (Src1 : Cv_Arr_Ptr;
                         Src2 : Cv_Arr_Ptr;
                         Dst  : Cv_Arr_Ptr) is
   begin
      Cv_Mat_Mul_Add (Src1, Src2, null, Dst);
   end Cv_Mat_Mul;
   procedure Cv_Mat_Mul (Src1 : Cv_Mat_Ptr;
                         Src2 : Cv_Mat_Ptr;
                         Dst  : Cv_Mat_Ptr) is
   begin
      Cv_Mat_Mul_Add (Src1, Src2, null, Dst);
   end Cv_Mat_Mul;
   procedure Cv_Mat_Mul (Src1 : Ipl_Image_Ptr;
                         Src2 : Ipl_Image_Ptr;
                         Dst  : Ipl_Image_Ptr) is
   begin
      Cv_Mat_Mul_Add (Src1, Src2, null, Dst);
   end Cv_Mat_Mul;

   -- Allocates a memory buffer in a storage block.
   function Cv_Mem_Storage_Alloc_String (Storage : Cv_Mem_Storage_Ptr;
                                         Ptr     : String;
                                         Len     : Integer := -1) return Cv_String is
   begin
      return W_Cv_Mem_Storage_Alloc_String (Storage => Storage,
                                            Ptr     => +Ptr,
                                            Len     => Len);
   end Cv_Mem_Storage_Alloc_String;

   -- Returns the index of a graph vertex.
   function Cv_Graph_Vtx_Idx (Graph : Cv_Graph_Ptr;
                              Vtx   : Cv_Graph_Vtx_Ptr) return Integer is
      pragma Unreferenced (Graph);
   begin
      return Integer (Unsigned_32 (Vtx.all.Flags) and Cv_Set_Elem_Idx_Mask);
   end Cv_Graph_Vtx_Idx;

   -- Returns the index of a graph edge.
   function Cv_Graph_Edge_Idx (Graph : Cv_Graph_Ptr;
                               Edge  : Cv_Graph_Edge_Ptr) return Integer is
      pragma Unreferenced (Graph);
   begin
      return Integer (Unsigned_32 (Edge.all.Flags) and Cv_Set_Elem_Idx_Mask);
   end Cv_Graph_Edge_Idx;

   --#define cvGraphGetVtxCount( graph ) ((graph)->active_count)
   function Cv_Graph_Get_Vtx_Count (Graph : Cv_Graph_Ptr) return Integer is
   begin
      return Graph.all.Active_Count;
   end Cv_Graph_Get_Vtx_Count;

   --#define cvGraphGetEdgeCount( graph ) ((graph)->edges->active_count)
   function Cv_Graph_Get_Edge_Count (Graph : Cv_Graph_Ptr) return Integer is
   begin
      return Graph.all.Edges.all.Active_Count;
   end Cv_Graph_Get_Edge_Count;

   --     #define  CV_IS_GRAPH_VERTEX_VISITED(vtx) \
   --      (((CvGraphVtx*)(vtx))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
   function Cv_Is_Graph_Vertex_Visisted (Vtx : Cv_Graph_Vtx_Ptr) return Integer is
   begin
      return Integer (Unsigned_32 (Vtx.all.Flags) and Unsigned_32 (Cv_Graph_Item_Visited_Flag));
   end Cv_Is_Graph_Vertex_Visisted;

   --   #define  CV_IS_GRAPH_EDGE_VISITED(edge) \
   --      (((CvGraphEdge*)(edge))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
   function Cv_Is_Graph_Edge_Visited (Edge : Cv_Graph_Edge_Ptr) return Integer is
   begin
      return Integer (Unsigned_32 (Edge.all.Flags) and Unsigned_32 (Cv_Graph_Item_Visited_Flag));
   end Cv_Is_Graph_Edge_Visited;

   -- Creates a Cv_Scalar color from RGB values
   function Cv_Rgb (R : Integer;
                    G : Integer;
                    B : Integer) return Cv_Scalar is
   begin
      return Cv_Create_Scalar (Long_Float (B),
                               Long_Float (G),
                               Long_Float (R),
                               0.0);
   end Cv_Rgb;

   -- Draws a text string.
   procedure Cv_Put_Text (Img   : Cv_Arr_Ptr;
                          Text  : String;
                          Org   : Cv_Point;
                          Font  : Cv_Font_Ptr;
                          Color : Cv_Scalar) is
   begin
      W_Cv_Put_Text (Img, +Text, Org, Font, Color);
   end Cv_Put_Text;
   procedure Cv_Put_Text (Img   : Cv_Mat_Ptr;
                          Text  : String;
                          Org   : Cv_Point;
                          Font  : access Cv_Font;
                          Color : Cv_Scalar) is
   begin
      W_Cv_Put_Text (Img, +Text, Org, Font, Color);
   end Cv_Put_Text;
      procedure Cv_Put_Text (Img   : Ipl_Image_Ptr;
                          Text  : String;
                          Org   : Cv_Point;
                          Font  : access Cv_Font;
                          Color : Cv_Scalar) is
   begin
      W_Cv_Put_Text (Img, +Text, Org, Font, Color);
   end Cv_Put_Text;

   -- Retrieves the width and height of a text string.
   procedure Cv_Get_Text_Size (Textstring : String;
                               Font       : Cv_Font;
                               Textsize   : access Cv_Size;
                               Baseline   : access Integer) is
   begin
      W_Cv_Get_Text_Size (+Textstring, Font, Textsize, Baseline);
   end Cv_Get_Text_Size;

   procedure Cv_Turn_On_Ipl_Compatibility is
   begin
      null; -- We dont have this...
   end Cv_Turn_On_Ipl_Compatibility;

   -- use this for func: Enclosing_Entity
   procedure Opencv_Error (Status  : Integer;
                           Func    : String := Gnat.Source_Info.Enclosing_Entity;
                           Context : String := Gnat.Source_Info.Enclosing_Entity;
                           File    : String := Gnat.Source_Info.File;
                           Line    : Integer := Gnat.Source_Info.Line) is
      Retval : Integer;
      pragma Unreferenced (Retval);
   begin
      Retval := Cv_Error (Status,
                          Interfaces.C.Strings.New_String (Func),
                          Interfaces.C.Strings.New_String (Context),
                          Interfaces.C.Strings.New_String (File),
                          Line);
   end Opencv_Error;

   procedure Opencv_Errchk (Func    : String := Gnat.Source_Info.Enclosing_Entity;
                            Context : String := Gnat.Source_Info.Enclosing_Entity;
                            File    : String := Gnat.Source_Info.File;
                            Line    : Integer := Gnat.Source_Info.Line) is
   begin
      if (Cv_Get_Err_Status >= 0) then
         Opencv_Error (Integer (Cv_Stsbacktrace),
                       (Func),
                       (Context),
                       File,
                       Line);
      end if;
   end Opencv_Errchk;

   procedure Opencv_Assert (Expression : Boolean;
                            Func       : String := Gnat.Source_Info.Enclosing_Entity;
                            Context    : String := Gnat.Source_Info.Enclosing_Entity;
                            File       : String := Gnat.Source_Info.File;
                            Line       : Integer := Gnat.Source_Info.Line) is
   begin
      if not Expression then
         Opencv_Error (Integer (Cv_Stsinternal),
                       Func,
                       Context,
                       File,
                       Line);
      end if;
   end Opencv_Assert;

   --(cvSetErrStatus(CV_StsOk))
   procedure Opencv_Rsterr is
   begin
      Cv_Set_Err_Status (Integer (Cv_Stsok));
   end Opencv_Rsterr;

   procedure Cv_Check (Func    : String := Gnat.Source_Info.Enclosing_Entity;
                       Context : String := Gnat.Source_Info.Enclosing_Entity;
                       File    : String := Gnat.Source_Info.File;
                       Line    : Integer := Gnat.Source_Info.Line) is
      pragma Unreferenced (Func, Context, File, Line);
   begin
      if Cv_Get_Err_Status < 0 then
         Cv_Error (Integer (Cv_Stsbacktrace), "Inner function failed.");
      end if;
   end Cv_Check;

   procedure Opencv_Call is
   begin
      null;
   end Opencv_Call;
end Core.Operations;
