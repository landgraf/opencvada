----
with Imgproc, Imgproc.operations; use Imgproc, Imgproc.operations;
with Core, Core.Operations; use Core, Core.Operations;
with Highgui; use Highgui;
with Ada;
with Ada.Unchecked_Conversion;

procedure Delaunay is
   function Init_Delaunay (Storage : Cv_Mem_Storage_P;
                           Rect    : Cv_Rect) return Cv_Subdiv_2D_P is
      Subdiv : constant Cv_Subdiv_2d_P := CvCreateSubdiv2d (Cv_Seq_Kind_Subdiv2d,
                                                            Cv_Subdiv_2d'Size / 8,
                                                            Cv_Subdiv_2d_Point'Size / 8,
                                                            Cv_Quad_Edge_2d'Size / 8,
                                                            Storage);
   begin
      CvInitSubdivDelaunay2d (Subdiv, Rect);
      return Subdiv;
   end Init_Delaunay;

   procedure Draw_Subdiv_Point (Img : Ipl_Image_P;
                                Fp  : Cv_Point_2d_32f;
                                Color : Cv_Scalar) is
   begin
      CvCircle (+Img, CvPoint (CvRound (Long_Float(Fp.X)), CvRound (Long_Float(Fp.Y))), 3, Color, Cv_Filled, 8, 0);
   end Draw_Subdiv_Point;

   procedure Draw_Subdiv_Edge (Img : Ipl_Image_P;
                               Edge : Cv_Subdiv_2d_Edge;
                               Color : Cv_Scalar) is
      Org_Pt, Dst_Pt : Cv_Subdiv_2d_Point_P;
      Org, Dst       : Cv_Point_2d_32F;
      Iorg, Idst     : Cv_Point;
   begin
      Org_Pt := CvSubdiv2dEdgeOrg (Edge);
      Dst_Pt := CvSubdiv2dEdgedst (Edge);

      if not (Dst_Pt = null) and not (Org_Pt = null) then
         Org := Org_Pt.all.Pt;
         Dst := Dst_Pt.all.Pt;

         IOrg := CvPoint (Cvround (Org.X), CvRound (Org.Y));
         IDst := CvPoint (CvRound (Dst.X), CvRound (Org.Y));

         CvLine (+Img, Iorg, Idst, Color, 1, Cv_Aa, 0);
      end if;
   end Draw_Subdiv_Edge;

   procedure Draw_Subdiv (Img : Ipl_Image_P;
                          Subdiv : Cv_Subdiv_2d_P;
                          Delaunay_Color : Cv_Scalar;
                          Voronoi_Color  : Cv_Scalar) is
      Reader : aliased Cv_Seq_Reader;
      Total  : constant Integer := Subdiv.all.Edges.all.Total;
      Elem_Size : constant Integer := Subdiv.all.Edges.all.Elem_Size;
      Edge      : Cv_Quad_Edge_2d_P := null;

      ---------
      -- Conversions
      ---------
      function From_Arr is
        new Ada.Unchecked_Conversion (Source => Cv_Arr_P,
                                      Target => Imgproc.Cv_Quad_Edge_2d_P);

      function Quad_To_Subdiv is
        new Ada.Unchecked_Conversion (Source => Cv_Quad_Edge_2d_P,
                                      Target => Cv_Subdiv_2d_Edge);
   begin
      CvStartReadSeq (To_Seq (Subdiv.all.Edges), Reader'Unchecked_Access, 0);

      for I in Integer range 0 .. Total - 1
      loop
         Edge := From_Arr (Value (Reader.Ptr) (0));
         -- skipping an if here
         -- if( CV_IS_SET_ELEM( edge ))
         --{
         Draw_Subdiv_Edge (Img, Quad_To_Subdiv(From_Arr (Value (Reader.Ptr + 1) (0))), Voronoi_Color);
         Draw_Subdiv_Edge (Img, Quad_To_Subdiv (Edge), Delaunay_Color);
         --}
         CV_NEXT_SEQ_ELEM( elem_size, Reader'Unchecked_Access );
      end loop;
   end Draw_Subdiv;

   procedure Locate_Point (Subdiv       : Cv_Subdiv_2D_P;
                           Fp           : Cv_Point_2d_32F;
                           Img          : Ipl_Image_P;
                           Active_Color : Cv_Scalar) is
      E  : aliased Cv_Subdiv_2d_Edge;
      E0 : aliased Cv_Subdiv_2d_Edge := 0;
      P  : aliased Cv_Subdiv_2d_Point_P := null;
      Ret : Cv_Subdiv_2D_Point_Location;
   begin
      Ret := CvSubdiv2DLocate (Subdiv, Fp, E0'Unchecked_Access, P'Access);
      if not (E0 = 0) then
         E := E0;
         loop
            Draw_Subdiv_Edge (Img, E, Active_Color);
            E := CvSubdiv2DGetEdge (E, CV_NEXT_AROUND_LEFT);
            exit when not (E = E0);
         end loop;
      end if;
      Draw_Subdiv_Point (Img, Fp, Active_Color);
   end Locate_Point;

   procedure Draw_Subdiv_Facet (Img  : Ipl_Image_P;
                                Edge : Cv_Subdiv_2d_Edge) is
      T : Cv_Subdiv_2D_Edge := Edge;

      function Get_Count return Integer is
         Temp_Count : Integer := 0;
      begin
         loop
            Temp_Count := Temp_Count + 1;
            T := CvSubdiv2dGetEdge (T, CV_NEXT_AROUND_LEFT);
            exit when not ( T = Edge);
         end loop;
         return Temp_Count;
      end Get_Count;


      Count : Integer := Get_Count;
      Buf   : aliased Cv_Point_Array (0 .. Count - 1) := (others => (0, 0));
      N : Integer;
      Pt : Cv_Subdiv_2d_Point_P;
   begin
      T := Edge;
      for I in Integer range Buf'Range
      loop
         Pt := CvSubdiv2dEdgeOrg (T);
         exit when Pt = null;
         Buf (I) := CvPoint (CvRound (Pt.all.Pt.X), CvRound (Pt.all.Pt.Y));
         T := CvSubdiv2dGetEdge (T, CV_NEXT_AROUND_LEFT);
         N := I;
      end loop;

      if (N + 1 = Count) then
         Pt := CvSubdiv2dEdgedst (CvSubdiv2dRotateEdge (Edge, 1));
         CvFillConvexPoly (+Img, Buf, Count, CV_RGB (43, 169, 102), CV_AA, 0);
         CvPolyLine(+Img,To_2d_Pointer(Cv_Point_2d_Array(Buf)'access),Cv_32u_Array(Count),1,1,Cv_Rgb(0,0,0),1,Cv_Aa,0);
      end if;
   end Draw_Subdiv_Facet;
begin
   null;
end Delaunay;
