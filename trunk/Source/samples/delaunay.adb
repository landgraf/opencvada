----
with Imgproc, Imgproc.operations; use Imgproc, Imgproc.operations;
with Core, Core.Operations; use Core, Core.Operations;
with Highgui; use Highgui;
with Ada;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
procedure Delaunay is

   subtype Ran is Integer range 5 .. 600 - 5;
   package Random_Num is new Ada.Numerics.Discrete_Random (Ran);
   use Random_Num;



   G : Generator;

   procedure Help is
   begin
      New_Line;
      Put ("This program demostrates iterative construction of");
      New_Line;
      Put ("delaunay triangulation and voronoi tesselation.");
      New_Line;
      Put ("It draws a random set of points in an image and then delaunay triangulates them.");
      New_Line;
      Put ("Call:");
      New_Line;
      Put ("./delaunay");
      New_Line;
      Put ("This program builds the traingulation interactively, you may stop this process by");
      New_Line;
      Put ("hitting any key.");
      New_Line;
   end Help;

   function Init_Delaunay (Storage : Cv_Mem_Storage_Ptr;
                           Rect    : Cv_Rect) return Cv_Subdiv_2d_Ptr is
      Subdiv : constant Cv_Subdiv_2d_Ptr := Cv_Create_Subdiv_2d (Cv_Seq_Kind_Subdiv2d,
                                                            Cv_Subdiv_2d'Size / 8,
                                                            Cv_Subdiv_2d_Point'Size / 8,
                                                            Cv_Quad_Edge_2d'Size / 8,
                                                            Storage);
   begin
      Cv_Init_Subdiv_Delaunay_2d (Subdiv, Rect);
      return Subdiv;
   end Init_Delaunay;

   procedure Draw_Subdiv_Point (Img : Ipl_Image_Ptr;
                                Fp  : Cv_Point_2d_32f;
                                Color : Cv_Scalar) is
   begin
      Cv_Circle (+Img, Cv_Create_Point (Cv_Round (Long_Float (Fp.X)), Cv_Round (Long_Float (Fp.Y))), 3, Color, Cv_Filled, 8, 0);
   end Draw_Subdiv_Point;

   procedure Draw_Subdiv_Edge (Img : Ipl_Image_Ptr;
                               Edge : Cv_Subdiv_2d_Edge;
                               Color : Cv_Scalar) is
      Org_Pt, Dst_Pt : Cv_Subdiv_2d_Point_Ptr;
      Org, Dst       : Cv_Point_2d_32F;
      Iorg, Idst     : Cv_Point;
   begin
      --Put_Line("DRAW_SUBDIV_EDGE: " & Edge'Img);
      Org_Pt := Cv_Subdiv_2d_Edge_Org (Edge);
      Dst_Pt := Cv_Subdiv_2d_Edge_Dst (Edge);

--        Put_Line ("Color:" & Color.Val(2)'img);

      if not (Dst_Pt = null) and not (Org_Pt = null) then
         Org := Org_Pt.all.Pt;
         Dst := Dst_Pt.all.Pt;

         IOrg := Cv_Create_Point (Cv_Round (Org.X), Cv_Round (Org.Y));
         IDst := Cv_Create_Point (Cv_Round (Dst.X), Cv_Round (Dst.Y));

--           Put_Line ("CvLine org:" & Org.X'Img & Iorg.Y'Img);
--           Put_Line ("CvLine dst:" & Idst.X'Img & Idst.Y'Img);
         Cv_Line (+Img, Iorg, Idst, Color, 1, Cv_Aa, 0);
      else
         null;--Put_Line("Error");
      end if;
   end Draw_Subdiv_Edge;

   procedure Draw_Subdiv (Img : Ipl_Image_Ptr;
                          Subdiv : Cv_Subdiv_2d_Ptr;
                          Delaunay_Color : Cv_Scalar;
                          Voronoi_Color  : Cv_Scalar) is
      Reader : aliased Cv_Seq_Reader;
      Total  : constant Integer := Subdiv.all.Edges.all.Total;
      Elem_Size : constant Integer := Subdiv.all.Edges.all.Elem_Size;
      Edge      : Cv_Quad_Edge_2d_Ptr := null;

      ---------
      -- Conversions
      ---------
      function From_Arr is
        new Ada.Unchecked_Conversion (Source => Cv_Arr_Pointer,
                                      Target => Imgproc.Cv_Quad_Edge_2d_Ptr);

      function Quad_To_Subdiv is
        new Ada.Unchecked_Conversion (Source => Cv_Quad_Edge_2d_Ptr,
                                      Target => Cv_Subdiv_2d_Edge);
--        Temp      : Cv_Arr_Pointer;
      Edge1     : Cv_Quad_Edge_2d_Ptr;
      use Core.Cv_Arr_Pointer_Pkg;
   begin
      Cv_Start_Read_Seq (To_Seq (Subdiv.all.Edges), Reader'Unchecked_Access, 0);

      for I in Integer range 0 .. Total-1
      loop
--             Put("draw_subdiv " & I'Img);
         Edge := From_Arr (Reader.Ptr);
         Edge1 := From_Arr(Reader.Ptr + 1);
         if( Edge.all.Flags >= 0 ) then
            Draw_Subdiv_Edge (Img, Quad_To_Subdiv (Edge1), Voronoi_Color);
            Draw_Subdiv_Edge (Img, Quad_To_Subdiv (Edge), Delaunay_Color);
         else
            --              Put("skipping");
            null;
         end if;

         Cv_Next_Seq_Elem( elem_size, Reader'Unchecked_Access );
      end loop;
   end Draw_Subdiv;

   procedure Locate_Point (Subdiv       : Cv_Subdiv_2d_Ptr;
                           Fp           : Cv_Point_2d_32F;
                           Img          : Ipl_Image_Ptr;
                           Active_Color : Cv_Scalar) is
      E  : aliased Cv_Subdiv_2d_Edge;
      E0 : aliased Cv_Subdiv_2d_Edge := 0;
      P  : aliased Cv_Subdiv_2d_Point_Ptr := null;
      Ret : Cv_Subdiv_2D_Point_Location;
   begin
      Ret := Cv_Subdiv_2d_Locate (Subdiv, Fp, E0'Unchecked_Access, P'Access);
      if E0 > 0 then
         E := E0;
         loop
            Draw_Subdiv_Edge (Img, E, Active_Color);
            E := Cv_Subdiv_2d_Get_Edge (E, CV_NEXT_AROUND_LEFT);
            exit when (E = E0);
         end loop;
      end if;
      Draw_Subdiv_Point (Img, Fp, Active_Color);
   end Locate_Point;

   procedure Draw_Subdiv_Facet (Img  : Ipl_Image_Ptr;
                                Edge : Cv_Subdiv_2d_Edge) is
      T : Cv_Subdiv_2D_Edge := Edge;

      function Get_Count return Integer is
         Temp_Count : Integer := 0;
      begin
         loop
            Temp_Count := Temp_Count + 1;
            T := Cv_Subdiv_2d_Get_Edge (T, CV_NEXT_AROUND_LEFT);
--              Put_Line("T="& T'Img);
            exit when ( T = Edge);
         end loop;
         return Temp_Count;
      end Get_Count;


      Count : constant Integer := Get_Count;
      Count_Arr : Cv_32U_Array(0 .. 0);
      Buf   : aliased Cv_Point_Array (0 .. Count - 1);
      Buf_2d : Cv_Point_Pointer_Array (0 .. 0);
      N : Integer;
      Pt : Cv_Subdiv_2d_Point_Ptr;
   begin
      T := Edge;
      for I in Integer range Buf'Range
      loop
         Pt := Cv_Subdiv_2d_Edge_Org (T);
         exit when Pt = null;
         Buf (I) := Cv_Create_Point (Cv_Round (Pt.all.Pt.X), Cv_Round (Pt.all.Pt.Y));
         T := Cv_Subdiv_2d_Get_Edge (T, Cv_Next_Around_Left);
         N := I;
      end loop;

      if (N + 1 = Count) then
         Pt := Cv_Subdiv_2d_Edge_Dst (Cv_Subdiv_2d_Rotate_Edge (Edge, 1));
         Cv_Fill_Convex_Poly (+Img, Buf, Count, CV_RGB (Random (G) mod 255, Random (G) mod 255, Random (G) mod 255), CV_AA, 0);
--           Put_Line("count : " & Count'Img);
         Buf_2d (0) := Buf (0)'unchecked_access;
         Count_Arr(0) := Unsigned_32(Count);
         Cv_Poly_Line (+Img, Buf_2d, Count_Arr, 1, 1, Cv_Rgb (0, 0, 0), 1, Cv_Aa, 0);
      end if;
   end Draw_Subdiv_Facet;

   procedure Paint_Voronoi (Subdiv : Cv_Subdiv_2d_Ptr;
                            Img    : Ipl_Image_Ptr) is
      Reader : aliased Cv_Seq_Reader;
      Total  : constant Integer := Subdiv.all.Edges.all.Total;
      Elem_Size : constant Integer := Subdiv.all.Edges.all.Elem_Size;

      Edge      : Cv_Quad_Edge_2d_Ptr;
      E         : Cv_Subdiv_2d_Edge;

      function Quad_To_Subdiv is
        new Ada.Unchecked_Conversion (Source => Cv_Quad_Edge_2d_Ptr,
                                      Target => Cv_Subdiv_2d_Edge);

      function From_Arr is
        new Ada.Unchecked_Conversion (Source => Cv_Arr_Pointer,
                                      Target => Imgproc.Cv_Quad_Edge_2d_Ptr);

   begin
      Cv_Calc_Subdiv_Voronoi_2d (Subdiv);
      Cv_Start_Read_Seq (To_Seq (Subdiv.all.Edges), Reader'Unchecked_Access, 0);

      for I in Integer range 0  .. Total - 1
      loop
--           Put_Line("test" & I'Img);
         Edge := From_Arr (Reader.Ptr);

         if( Edge.all.Flags >= 0 ) then
            E := Quad_To_Subdiv (Edge);
            -- left
            Draw_Subdiv_Facet (Img, Cv_Subdiv_2d_Rotate_Edge (E, 1));
            -- right
            Draw_Subdiv_Facet (Img, Cv_Subdiv_2d_Rotate_Edge (E, 3));
         end if;
         Cv_Next_Seq_Elem (Elem_Size, Reader'Unchecked_Access);
      end loop;
   end Paint_Voronoi;

   procedure Run is
      Win                                                            : constant String := "Source";
      Rect                                                           : constant Cv_Rect := (0, 0, 600, 600);
      Storage                                                        : aliased Cv_Mem_Storage_Ptr;
      Subdiv                                                         : Cv_Subdiv_2d_Ptr;
      Img                                                            : aliased Ipl_Image_Ptr;
      Active_Facet_Color, Delaunay_Color, Voronoi_Color, Bkgnd_Color : Cv_Scalar;


      Ret : Integer;
      S_Ret : Cv_Subdiv_2d_Point_Ptr;
      Fp : Cv_Point_2d_32f;
   begin
      Reset (G);
      Active_Facet_Color := CV_RGB ( 255, 0, 0 );
      Delaunay_Color  := CV_RGB ( 0, 0, 0);
      Voronoi_Color := CV_RGB (0, 180, 0);
      Bkgnd_Color := CV_RGB (255, 255, 255);
      Img := Cv_Create_Image (Cv_Create_Size(Rect.Width, Rect.Height), 8, 3);
      Cv_Set_All (+Img, Bkgnd_Color, null);

      Ret := Cv_Named_Window (Win, 1);

      Storage := Cv_Create_Mem_Storage (0);
      Subdiv := Init_Delaunay (Storage, Rect);
--        Put_Line ("Delaunay triangulation will be build now interactively.");
--        Put_Line ("To stop the process, press any key");
--        New_Line;

      for I in Integer range 0 .. 50
      loop
         Fp := (Float (Random (G)),Float (Random (G)));
         Locate_Point (Subdiv, Fp, Img, Active_Facet_Color);
         Cv_Show_Image (Win, +Img);

         exit when Cv_Wait_Key (10) = Ascii.Esc;

--           Put_Line("fp: " & Fp.X'Img & Fp.Y'Img);

         S_Ret := Cv_Subdiv_Delaunay_2d_Insert (Subdiv, Fp);
--           Put_Line("s_ret: " & S_Ret.all.Id'Img);
         Cv_Calc_Subdiv_Voronoi_2d (Subdiv);
         Cv_Set_All (+Img, Bkgnd_Color, null);
         Draw_Subdiv (Img, Subdiv, Delaunay_Color, Voronoi_Color);
         Cv_Show_Image (Win, +Img);

         exit when Cv_Wait_Key (10) = Ascii.Esc;
      end loop;

--        Put("Here we are");

      Cv_Set_All(+Img, Bkgnd_Color, null);
      Paint_Voronoi (Subdiv, Img);
      Cv_Show_Image (Win, +Img);

--        if CvWaitKey (0) = Ascii.Esc then
         Cv_Release_Mem_Storage (Storage'Access);
         Cv_Release_Image (Image => Img'Access);
         Cv_Destroy_Window (Win);
--        end if;

   end Run;
begin
   Help;
   Run;
end Delaunay;
