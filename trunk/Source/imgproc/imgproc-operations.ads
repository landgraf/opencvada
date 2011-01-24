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
-- imgproc_c.ads - imgproc_c.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

with Legacy;

package Imgproc.Operations is
--

   -----------------------------------------------------------------------------
   -- Background statistics accumulation
   -----------------------------------------------------------------------------

   -- Adds image to accumulator
   procedure Cv_Acc (Image : Cv_Arr_P;
                     Sum   : Cv_Arr_P;
                     Mask  : Cv_Arr_P := null);

   -- Adds the square of the source image to the accumulator.
   procedure Cv_Square_Acc (Image : Cv_Arr_P;
                            Sqsum : Cv_Arr_P;
                            Mask  : Cv_Arr_P := null);

   -- Adds the product of two input images to the accumulator.
   procedure Cv_Multiply_Acc (Image1 : Cv_Arr_P;
                              Image2 : Cv_Arr_P;
                              Acc    : Cv_Arr_P;
                              Mask   : Cv_Arr_P := null);

   -- Updates the running average.
   procedure Cv_Running_Avg (Image : Cv_Arr_P;
                             Acc   : Cv_Arr_P;
                             Alpha : Long_Float;
                             Mask  : Cv_Arr_P := null);

   -----------------------------------------------------------------------------
   -- Image Processing
   -----------------------------------------------------------------------------

   -- Copies source 2D array inside of the larger destination array and
   -- makes a border of the specified type (IPL_BORDER_*) around the copied area.
   procedure Cv_Copy_Make_Border (Src         : Cv_Arr_P;
                                  Dst         : Cv_Arr_P;
                                  Offset      : Cv_Point;
                                  Border      : Border_Type;
                                  Value       : Cv_Scalar);

   -- Smoothes array (removes noise)
   procedure Cv_Smooth (Src        : Cv_Arr_P;
                        Dst        : Cv_Arr_P;
                        Smoothtype : Smooth_Type;
                        Param1     : Integer := 3;
                        Param2     : Integer := 0;
                        Param3     : Long_Float := 0.0;
                        Param4     : Long_Float := 0.0);

   -- Convolves the image with the kernel
   procedure Cv_Filter_2d (Src    : Cv_Arr_P;
                           Dst    : Cv_Arr_P;
                           Kernel : Cv_Mat_P;
                           Anchor : Cv_Point := (-1, -1));

   -- Finds integral image: SUM(X,Y) = sum(x<X,y<Y)I(x,y)
   procedure Cv_Integral (Image     : Cv_Arr_P;
                          Sum       : Cv_Arr_P;
                          Sqsum     : Cv_Arr_P := null;
                          Titledsum : Cv_Arr_P := null);

   -- Smoothes the input image with gaussian kernel and then down-samples it.
   -- dst_width = floor(src_width/2)[+1],
   -- dst_height = floor(src_height/2)[+1]
   procedure Cv_Pyr_Down (Src    : Cv_Arr_P;
                          Dst    : Cv_Arr_P;
                          Filter : Pyr_Filter := Cv_Gaussian_5x5);

   -- Up-samples image and smoothes the result with gaussian kernel.
   -- dst_width = src_width*2,
   -- dst_height = src_height*2
   procedure Cv_Pyr_Up (Src    : access Cv_Arr_P;
                        Dst    : access Cv_Arr;
                        Filter : Pyr_Filter := Cv_Gaussian_5x5);

   -- Builds pyramid for an image
   function Cv_Create_Pyramid (Img          : Cv_Arr_P;
                               Extra_Layers : Integer;
                               Rate         : Long_Float;
                               Layer_Sizes  : Cv_Size_Pointer := null;
                               Bufarr       : Cv_Arr_P := null;
                               Calc         : Integer := 1;
                               Filter       : Pyr_Filter := Cv_Gaussian_5x5)
                               return Cv_Mat_P_Pointer;

   -- Releases pyramid
   procedure Cv_Release_Pyramid (Pyramid      : access Cv_Mat_P_Pointer;
                                 Extra_Layers : Integer);

   -- Splits color or grayscale image into multiple connected components
   -- of nearly the same color/brightness using modification of Burt algorithm.
   -- comp with contain a pointer to sequence (CvSeq)
   -- of connected components (CvConnectedComp)
   procedure Cv_Pyr_Segmentation (Src        : Ipl_Image_P;
                                  Dst        : Ipl_Image_P;
                                  Storage    : Cv_Mem_Storage_P;
                                  Comp       : Cv_Seq_P_Array;
                                  Level      : Integer;
                                  Threshold1 : Long_Float;
                                  Threshold2 : Long_Float);

   -- Does meanshift image segmentation
   procedure Cv_Pyr_Mean_Shift_Filtering (Src       : Cv_Arr_P;
                                          Dst       : Cv_Arr_P;
                                          Sp        : Long_Float;
                                          Sr        : Long_Float;
                                          Max_Level : Integer := 1;
                                          Termcrit  : Cv_Term_Criteria := Cv_Create_Term_Criteria (Cv_Termcrit_Iter + Cv_Termcrit_Eps, 5, 1.0));

   -- Segments image using seed markers
   procedure Cv_Watershed (Image   : Cv_Arr_P;
                           Markers : Cv_Arr_P);

   -- Inpaints the selected region in the image.
   procedure Cv_Inpaint (Src           : Cv_Arr_P;
                         Mask          : Cv_Arr_P;
                         Dst           : Cv_Arr_P;
                         Inpaintradius : Long_Float;
                         Flags         : Integer);

   -- Calculates the first, second, third or mxied image derivatives using an extended Sobel operator.
   procedure Cv_Sobel (Src          : Cv_Arr_P;
                       Dst          : Cv_Arr_P;
                       Xorder       : Integer;
                       Yorder       : Integer;
                       Aperturesize : Integer := 3);

   -- Calculates the image Laplacian: (d2/dx + d2/dy)I
   procedure Cv_Laplace (Src          : Cv_Arr_P;
                         Dst          : Cv_Arr_P;
                         Aperturesize : Integer);

   -- Converts an image from one color space to another.
   procedure Cv_Cvt_Color (Src  : Cv_Arr_P;
                           Dst  : Cv_Arr_P;
                           Code : Color_Conversion);

   -- Resizes an image.
   procedure Cv_Resize (Src          : Cv_Arr_P;
                        Dst          : Cv_Arr_P;
                        Interplation : Cv_Inter := Cv_Inter_Linear);

   --Applies an affine transformation to an image.
   procedure Cv_Warp_Affine (Src       : Cv_Arr_P;
                             Dst       : Cv_Arr_P;
                             Mapmatrix : Cv_Mat_P;
                             Flags     : Integer := Cv_Inter'Pos (Cv_Inter_Linear) + Cv_Warp'Pos (Cv_Warp_Fill_Outliers);
                             Fillval   : Cv_Scalar := Cv_Scalar_All (0.0));

   -- Calculates the affine transform from 3 corresponding points.
   function Cv_Get_Affine_Transform (Src       : Cv_Point_2d_32f_Array;
                                     Dst       : Cv_Point_2d_32f_Array;
                                     Mapmatrix : Cv_Mat_P) return Cv_Mat_P;

   -- Calculates the affine matrix of 2d rotation.
   function Cv_2d_Rotation_Matrix (Center    : Cv_Point_2d_32f;
                                   Angle     : Long_Float;
                                   Scale     : Long_Float;
                                   Mapmatrix : Cv_Mat_P) return Cv_Mat_P;

   --Applies a perspective transformation to an image.
   procedure Cv_Warp_Perspective (Src       : Cv_Arr_P;
                                  Dst       : Cv_Arr_P;
                                  Mapmatrix : Cv_Mat_P;
                                  Flags     : Integer := Cv_Inter'Pos (Cv_Inter_Linear) + Cv_Warp'Pos (Cv_Warp_Fill_Outliers);
                                  Fillval   : Cv_Scalar := Cv_Scalar_All (0.0));

   -- Calculates the perspective transform from 4 corresponding points.
   function Cv_Get_Perspective_Transform (Src       : Cv_Point_2d_32f_Array;
                                          Dst       : Cv_Point_2d_32f_Array;
                                          Mapmatrix : Cv_Mat_P) return Cv_Mat_P;

   -- Applies a generic geometrical transformation to the image.
   procedure Cv_Remap (Src     : Cv_Arr_P;
                       Dst     : Cv_Arr_P;
                       Mapx    : Cv_Arr_P;
                       Mapy    : Cv_Arr_P;
                       Flags   : Integer := Cv_Inter'Pos (Cv_Inter_Linear) + Cv_Warp'Pos (Cv_Warp_Fill_Outliers);
                       Fillval : Cv_Scalar := Cv_Scalar_All (0.0));

   -- Converts mapx & mapy from floating-point to integer formats for cvRemap
   procedure Cv_Convert_Maps (Mapx     : Cv_Arr_P;
                              Mapy     : Cv_Arr_P;
                              Mapxy    : Cv_Arr_P;
                              Mapalpha : Cv_Arr_P);

   -- Remaps an image to log-polar space.
   procedure Cv_Log_Polar (Src        : Cv_Arr_P;
                           Dst        : Cv_Arr_P;
                           Center     : Cv_Point_2d_32f;
                           --X          : Long_Float;
                           --Y          : Long_Float;
                           M          : Long_Float;
                           Flags      : Integer := Integer (Cv_Inter_Linear) + Integer (Cv_Warp_Fill_Outliers));
--                             Flags      : Integer := Cv_Inter'Pos (Cv_Inter_Linear) + Cv_Warp'Pos (Cv_Warp_Fill_Outliers));

   -- Performs forward or inverse linear-polar image transform
   procedure Cv_Linear_Polar (Src        : Cv_Arr_P;
                              Dst        : Cv_Arr_P;
                              Center     : Cv_Point_2d_32f;
                              --                              X          : Long_Float;
                              --                              Y          : Long_Float;
                              Max_Radius : Long_Float;
                              Flags      : Integer := Integer (Cv_Inter_Linear) + Integer (Cv_Warp_Fill_Outliers));
--                                Flags      : Integer := Cv_Inter'Pos (Cv_Inter_Linear) + Cv_Warp'Pos (Cv_Warp_Fill_Outliers));

   -- Transforms an image to compensate for lens distortion.
   procedure Cv_Undistort2 (Src             : Cv_Arr_P;
                            Dst             : Cv_Arr_P;
                            Cameramatrix    : Cv_Mat_P;
                            Distcoefs       : Cv_Mat_P;
                            Newcameramatrix : Cv_Mat_P);

   -- Computes an undistortion map.
   procedure Cv_Init_Undistort_Map (Cameramatrix : Cv_Mat_P;
                                    Distcoeffs   : Cv_Mat_P;
                                    Map1         : Cv_Arr_P;
                                    Map2         : Cv_Arr_P);

   -- Computes the undistortion and rectification transformation map.
   procedure Cv_Init_Undistort_Rectify_Map (Cameramatrix    : Cv_Mat_P;
                                            Distcoeffs      : Cv_Mat_P;
                                            R               : Cv_Mat_P;
                                            Newcameramatrix : Cv_Mat_P;
                                            Map1            : Cv_Arr_P;
                                            Map2            : Cv_Arr_P);

   -- Computes the ideal point coordinates from the observed point coordinates.
   procedure Cv_Undistort_Points (Src          : Cv_Mat_P;
                                  Dst          : Cv_Mat_P;
                                  Cameramatrix : Cv_Mat_P;
                                  Distcoeffs   : Cv_Mat_P;
                                  R            : Cv_Mat_P := null;
                                  P            : Cv_Mat_P := null);

   -- creates structuring element used for morphological operations
   function Cv_Create_Structuring_Element_Ex (Cols     : Integer;
                                              Rows     : Integer;
                                              Anchorx  : Integer;
                                              Anchory  : Integer;
                                              Shape    : Structuring_Shape;
                                              Values   : access Integer) return Ipl_Conv_Kernel_P;

   -- releases structuring element
   procedure Cv_Release_Structuring_Element (Element : access Ipl_Conv_Kernel_P);

   -- erodes input image (applies minimum filter) one or more times.
   -- if element pointer is NULL, 3x3 rectangular element is used
   procedure Cv_Erode (Src        : Cv_Arr_P;
                       Dst        : Cv_Arr_P;
                       Element    : Ipl_Conv_Kernel_P := null;
                       Iterations : Integer := 1);

   -- dilates input image (applies maximum filter) one or more times.
   -- If element pointer is NULL, 3x3 rectangular element is used
   procedure Cv_Dilate (Src        : Cv_Arr_P;
                        Dst        : Cv_Arr_P;
                        Element    : Ipl_Conv_Kernel_P := null;
                        Iterations : Integer := 1);

   -- Performs complex morphological transformation
   procedure Cv_Morphology_Ex (Src        : Cv_Arr_P;
                               Dst        : Cv_Arr_P;
                               Temp       : Cv_Arr_P;
                               Element    : Ipl_Conv_Kernel_P;
                               Operation  : Morph_Operation;
                               Iterations : Integer);

   -- Calculates all of the moments up to the third order of a polygon or rasterized shape.
   procedure Cv_Moments (Arr     : Cv_Arr_P;
                         Moments : Cv_Moments_P;
                         Binary  : Integer := 0);

   -- Retrieves the spatial moment from the moment state structure.
   function Cv_Get_Spatial_Moment (Moments : Cv_Moments_P;
                                   Xorder  : Integer;
                                   Yorder  : Integer) return Long_Float;

   -- Retrieves the central moment from the moment state structure.
   function Cv_Get_Central_Moment (Moments : Cv_Moments_P;
                                   Xorder  : Integer;
                                   Yorder  : Integer) return Long_Float;

   -- Retrieves the normalized central moment from the moment state structure.
   function Cv_Get_Normalized_Central_Moment (Moments : Cv_Moments_P;
                                              Xorder  : Integer;
                                              Yorder  : Integer) return Long_Float;

   -- Calculates the seven Hu invariants.
   procedure Cv_Get_Hu_Moments (Moments : Cv_Moments_P;
                                Hu      : Cv_Hu_Moments_P);

   -----------------------------------------------------------------------------
   -- Data sampling
   -----------------------------------------------------------------------------
   -- Fetches pixels that belong to the specified line segment and stores them to the buffer.
   -- Returns the number of retrieved points.
   function Cv_Sample_Line (Image        : Cv_Arr_P;
                            Pt1          : Cv_Point;
                            Pt2          : Cv_Point;
                            Buffer       : Cv_Void_P;
                            Connectivity : Integer := 8) return Integer;

   -- Retrieves the rectangular image region with specified center from the input array.
   -- dst(x,y) <- src(x + center.x - dst_width/2, y + center.y - dst_height/2).
   -- Values of pixels with fractional coordinates are retrieved using bilinear interpolation
   procedure Cv_Get_Rect_Sub_Pix (Src    : Cv_Arr_P;
                                  Dst    : Cv_Arr_P;
                                  Center : Cv_Point_2d_32f);

   --  Retrieves quadrangle from the input array.
   --   matrixarr = ( a11  a12 | b1 )   dst(x,y) <- src(A[x y]' + b)
   --               ( a21  a22 | b2 )   (bilinear interpolation is used to retrieve pixels
   --                                   with fractional coordinates)
   procedure Cv_Get_Quadrangle_Sub_Pix (Src       : Cv_Arr_P;
                                        Dst       : Cv_Arr_P;
                                        Mapmatrix : Cv_Mat_P);

   -- Measures similarity between template and overlapped windows in the source image
   -- and fills the resultant image with the measurements
   procedure Cv_Match_Template (Image  : Cv_Arr_P;
                                Templ  : Cv_Arr_P;
                                Result : Cv_Arr_P;
                                Method : Integer);

   --     Computes the minimal work distance between two weighted point configurations.
   function Cv_Calc_Emd2 (Signature1    : access Cv_Arr;
                          Signature2    : access Cv_Arr;
                          Distance_Type : Integer;
                          Distance_Func : Cv_Distance_Function := null;
                          Cost_Matrix   : access Cv_Arr := null;
                          Flow          : access Cv_Arr := null;
                          Lower_Bound   : access Float := null;
                          Userdata      : Cv_Void_P := null)
                          return Float;

   -----------------------------------------------------------------------------
   -- Contours retrieving
   -----------------------------------------------------------------------------

   -- Retrieves outer and optionally inner boundaries of white (non-zero) connected
   -- components in the black (zero) background
   function Cv_Find_Contours (Image        : Cv_Arr_P;
                              Storage      : Cv_Mem_Storage_P;
                              Firstcontour : access Cv_Seq_P;
                              Headersize   : Integer := Cv_Contour'Size;
                              Mode         : Cv_Retr := Cv_Retr_List;
                              Method       : Cv_Chain_Enum := Cv_Chain_Approx_Simple;
                              Offset       : Cv_Point := Cv_Create_Point (0, 0)) return Integer;

   -- Initializes the contour scanning process.
   function Cv_Start_Find_Contours (Image      : Cv_Arr_P;
                                    Storage    : Cv_Mem_Storage_P;
                                    Headersize : Integer := Cv_Contour'Size;
                                    Mode       : Cv_Retr := Cv_Retr_List;
                                    Method     : Cv_Chain_Enum := Cv_Chain_Approx_Simple;
                                    Offset     : Cv_Point := Cv_Create_Point (0, 0)) return Cv_Contour_Scanner;

   -- Finds the next contour in the image.
   function Cv_Find_Next_Contour (Scanner : Cv_Contour_Scanner) return Cv_Seq_P;

   -- Replaces a retrieved contour.
   procedure Cv_Substitute_Contour (Scanner    : Cv_Contour_Scanner;
                                    Newcontour : Cv_Seq_P);

   -- Finishes the scanning process.
   function Cv_End_Find_Contours (Scanner : Cv_Contour_Scanner_P) return Cv_Seq_P;

   --     Approximates Freeman chain with a polygonal curve.
   function Cv_Approx_Chains (Src_Seq           : access Cv_Seq;
                              Storage           : access Cv_Mem_Storage;
                              Method            : Cv_Chain_Enum := Cv_Chain_Approx_Simple;
                              Parameter         : Long_Float := 0.0;
                              Minimal_Perimeter : Integer := 0;
                              Recursive         : Integer := 0)
                              return Cv_Seq_P;

   -- Initalizes Freeman chain reader.
   -- The reader is used to iteratively get coordinates of all the chain points.
   -- If the Freeman codes should be read as is, a simple sequence reader should be used
   procedure Cv_Start_Read_Chain_Points (Chain  : Cv_Chain_P;
                                         Reader : Cv_Chain_Pt_Reader_P);

   -- Gets the next chain point.
   function Cv_Read_Chain_Point (Reader : Cv_Chain_Pt_Reader_P) return Cv_Point;

   -----------------------------------------------------------------------------
   -- Planar subdivisions
   -----------------------------------------------------------------------------
   -- Initializes Delaunay triangulation
   procedure Cv_Init_Subdiv_Delaunay_2d (Subdiv : Cv_Subdiv_2d_P;
                                         Rect   : Cv_Rect);

   -- Creates new subdivision
   function Cv_Create_Subdiv_2d (Subdiv_Type   : Integer;
                                 Header_Size   : Integer;
                                 Vtx_Size      : Integer;
                                 Quadedge_Size : Integer;
                                 Storage       : Cv_Mem_Storage_P) return Cv_Subdiv_2d_P;

   -----------------------------------------------------------------------------
   -- high-level subdivision functions
   -----------------------------------------------------------------------------

   -- Creates an empty Delaunay triangulation.
   function Cv_Create_Subdiv_Delaunay_2d (Rect    : Cv_Rect;
                                          Storage : Cv_Mem_Storage_P) return Cv_Subdiv_2d_P;

   --Inserts a single point into a Delaunay triangulation.
   function Cv_Subdiv_Delaunay_2d_Insert (Subdiv : Cv_Subdiv_2d_P;
                                          Pt     : Cv_Point_2d_32f) return Cv_Subdiv_2d_Point_P;

   -- Returns the location of a point within a Delaunay triangulation.
   function Cv_Subdiv_2d_Locate (Subdiv : Cv_Subdiv_2d_P;
                                 Pt     : Cv_Point_2d_32f;
                                 Edge   : Cv_Subdiv_2d_Edge_P;
                                 Vertex : access Cv_Subdiv_2d_Point_P := null) return Cv_Subdiv_2d_Point_Location;

   -- Calculates the coordinates of Voronoi diagram cells.
   procedure Cv_Calc_Subdiv_Voronoi_2d (Subdiv : Cv_Subdiv_2d_P);

   -- Removes all virtual points.
   procedure Cv_Clear_Subdiv_Voronoi_2d (Subdiv : Cv_Subdiv_2d_P);

   --Finds the closest subdivision vertex to the given point.
   function Cv_Find_Nearest_Point_2d (Subdiv : Cv_Subdiv_2d_P;
                                      Pt     : Cv_Point_2d_32f) return Cv_Subdiv_2d_Point_P;

   -----------------------------------------------------------------------------
   -- Basic quad-edge navigation and operations
   -----------------------------------------------------------------------------
   -- Returns next edge around the edge origin
   function Cv_Subdiv_2d_Next_Edge (Edge : Cv_Subdiv_2d_Edge) return Cv_Subdiv_2d_Edge;

   -- Returns another edge of the same quad-edge.
   function Cv_Subdiv_2d_Rotate_Edge (Edge   : Cv_Subdiv_2d_Edge;
                                      Rotate : Integer) return Cv_Subdiv_2d_Edge;

   function Cv_Subdiv_2d_Sym_Edge (Edge : Cv_Subdiv_2d_Edge) return Cv_Subdiv_2d_Edge;

   function Cv_Subdiv_2d_Get_Edge (Edge      : Cv_Subdiv_2d_Edge;
                                   Edge_Type : Cv_Next_Edge_Type) return Cv_Subdiv_2d_Edge;

   function Cv_Subdiv_2d_Edge_Org (Edege : Cv_Subdiv_2d_Edge) return Cv_Subdiv_2d_Point_P;

   -- Returns the edge destination.
   function Cv_Subdiv_2d_Edge_Dst (Edge : Cv_Subdiv_2d_Edge) return Cv_Subdiv_2d_Point_P;

   function Cv_Triangle_Area (A : Cv_Point_2d_32f;
                              B : Cv_Point_2d_32f;
                              C : Cv_Point_2d_32f) return Long_Float;

   -----------------------------------------------------------------------------
   -- Contour Processing and Shape Analysis
   -----------------------------------------------------------------------------
   --     Approximates polygonal curve with the specified precision.
   function Cv_Approx_Poly (Src_Seq     : Cv_Void_P;
                            Header_Size : Integer;
                            Storage     : access Cv_Mem_Storage;
                            Method      : Integer;
                            Parameter   : Long_Float;
                            Parameter2  : Integer := 0)
                            return Cv_Seq_P;

   --     Calculates the contour perimeter or the curve length.
   function Cv_Arc_Length (Curve     : Cv_Void_P;
                           Slice     : Cv_Slice := Cv_Create_Slice (0);
                           Is_Closed : Integer := -1)
                           return Long_Float;

   function Cv_Contour_Perimeter (Curve : Cv_Void_P)
                                  return Long_Float;

   -- Calculates contour boundning rectangle (update=1) or
   -- just retrieves pre-calculated rectangle (update=0)
   function Cv_Bounding_Rect (Points : access Cv_Arr;
                              Update : Integer := 0)
                              return Cv_Rect;

   --     Calculates the area of a whole contour or a contour section.
   function Cv_Contour_Area (Contour : access Cv_Arr;
                             Slice   : Cv_Slice := Cv_Create_Slice (0))
                             return Long_Float;

   -- Finds the circumscribed rectangle of minimal area for a given 2D point set.
   function Cv_Min_Area_Rect2 (Points  : Cv_Arr_P;
                               Storage : Cv_Mem_Storage_P := null) return Cv_Box_2d;

   -- Finds the circumscribed circle of minimal area for a given 2D point set.

   function Cv_Min_Enclosing_Circle (Points : Cv_Arr_P;
                                     Center : access Cv_Point_2d_32f;
                                     Radius : access Float) return Integer;

   -- Compares two shapes.
   function Cv_Match_Shapes (Object1   : Cv_Void_P;
                             Object2   : Cv_Void_P;
                             Method    : Integer;
                             Parameter : Long_Float := 0.0) return Long_Float;

   -- Finds the convex hull of a point set.
   function Cv_Convex_Hull2 (Input       : Cv_Arr_P;
                             Storage     : Cv_Void_P := null;
                             Orientation : Integer := Cv_Clockwise;
                             Returpoints : Integer := 0) return Cv_Seq_P;

   --     Tests contour convexity.
   function Cv_Check_Contour_Convexity (Contour : access Cv_Arr)
                                        return Integer;

   -- Finds the convexity defects of a contour.
   function Cv_Convexity_Defects (Contour    : Cv_Arr_P;
                                  Convexhull : Cv_Arr_P) return Cv_Seq_P;

   -- Fits an ellipse around a set of 2D points.
   function Cv_Fit_Ellipse2 (Points : Cv_Arr_P) -- might be Cv_Point_Arr)
                             return Cv_Box_2d;

   -- Finds minimum rectangle containing two given rectangles
   function Cv_Max_Rect (Rect1 : access Cv_Rect;
                         Rect2 : access Cv_Rect)
                         return Cv_Rect;

   -- Finds coordinates of the box vertices
   procedure Cv_Box_Points (Box : Cv_Box_2d;
                            Pt  : out Legacy.Cv_Point_2d_32f_Array_4);

   -- Initializes sequence header for a matrix (column or row vector) of points -
   -- a wrapper for cvMakeSeqHeaderForArray (it does not initialize bounding rectangle!!!)
   function Cv_Point_Seq_From_Mat (Seqkind       : Integer;
                                   Mat           : Cv_Mat_P;
                                   Contourheader : Cv_Contour_P;
                                   Block         : Cv_Seq_Block_P) return Cv_Seq_P;

   -- Checks whether the point is inside polygon, outside, on an edge (at a vertex).
   -- Returns positive, negative or zero value, correspondingly.
   -- Optionally, measures a signed distance between
   -- the point and the nearest polygon edge (measure_dist=1)
   function Cv_Point_Polygon_Test (Contour     : Cv_Arr_P;
                                   Pt          : Cv_Point_2d_32f;
                                   Measuredist : Integer) return Long_Float;

   -----------------------------------------------------------------------------
   -- Histogram functions
   -----------------------------------------------------------------------------
   --Creates a histogram.
   function Cv_Create_Hist (Dims     : Integer;
                            Sizes    : Cv_32s_Array;
                            Histtype : Integer;
                            Ranges   : Cv_32f_Pointer_Array := Cv_32f_Array_Null;
                            Uniform  : Integer := 1) return Cv_Histogram_P;

   --Sets the bounds of the histogram bins.
   procedure Cv_Set_Hist_Bin_Ranges (Hist     : Cv_Histogram_P;
                                     Ranges   : Cv_32f_Pointer_Array;
                                     Uniforrm : Integer);

   -- Makes a histogram out of an array.
   procedure Cv_Make_Hist_Header_For_Array (Dims    : Integer;
                                            Sizes   : Cv_32s_Array;
                                            Hist    : Cv_Histogram_P;
                                            Data    : Cv_32f_Array;
                                            Ranges  : Cv_32f_Pointer_Array; -- fix me
                                            Uniform : Integer := 1);

   --Releases the histogram.
   procedure Cv_Release_Hist (Hist : access Cv_Histogram_P);

   -- Clears the histogram.
   procedure Cv_Clear_Hist (Hist : Cv_Histogram_P);

   -- Finds the minimum and maximum histogram bins.
   procedure Cv_Get_Min_Max_Hist_Value (Hist     : Cv_Histogram_P;
                                        Minvalue : access Float;
                                        Maxvalue : access Float;
                                        Minidx   : Cv_32s_Array := Cv_32s_Array_Null;
                                        Maxidx   : Cv_32s_Array := Cv_32s_Array_Null);

   --Normalizes the histogram.
   procedure Cv_Normalize_Hist (Hist   : Cv_Histogram_P;
                                Factor : Long_Float);

   -- Thresholds the histogram.
   procedure Cv_Thresh_Hist (Hist      : Cv_Histogram_P;
                             Threshold : Long_Float);

   --Compares two dense histograms.
   function Cv_Compare_Hist (Hist1  : Cv_Histogram_P;
                             Hist2  : Cv_Histogram_P;
                             Method : Hist_Compare_Method) return Long_Float;

   -- Copies a histogram.
   procedure Cv_Copy_Hist (Src : Cv_Histogram_P;
                           Dst : access Cv_Histogram_P);

   -- Calculates bayesian probabilistic histograms
   -- (each or src and dst is an array of <number> histograms
   procedure Cv_Calc_Bayesian_Prob (Src   : Cv_Histogram_P_Pointer;
                                    Count : Integer;
                                    Dst   : Cv_Histogram_P_Pointer);

   -- Calculates array histogram
   procedure Cv_Calc_Arr_Hist (Arr        : C_Cv_Arr_P_Ptr;
                               Hist       : Cv_Histogram_P;
                               Accumulate : Integer := 0;
                               Mask       : Cv_Arr_P := null);

   -- Calculates the histogram of image(s).
   procedure Cv_Calc_Hist (Image      : Cv_Arr_P_Array;
                           Hist       : Cv_Histogram_P;
                           Accumulate : Integer := 0;
                           Mask       : Cv_Arr_P := null);

   --Calculates the back projection.
   procedure Cv_Calc_Arr_Back_Project (Image       : Cv_Arr_P_Array;
                                       Backproject : Cv_Arr_P;
                                       Hist        : Cv_Histogram_P);

   --Calculates the back projection.
   procedure Cv_Calc_Back_Project (Image       : Cv_Arr_P_Array;
                                   Backproject : Cv_Arr_P;
                                   Hist        : Cv_Histogram_P) renames Cv_Calc_Arr_Back_Project;

   --Locates a template within an image by using a histogram comparison.
   procedure Cv_Calc_Arr_Back_Project_Patch (Images    : Cv_Arr_P_Array;
                                             Dst       : Cv_Arr_P;
                                             Patchsize : Cv_Size;
                                             Hist      : Cv_Histogram_P;
                                             Method    : Hist_Compare_Method;
                                             Factor    : Float);

   --Locates a template within an image by using a histogram comparison.
   procedure Cv_Calc_Back_Project_Patch (Images    : Cv_Arr_P_Array;
                                         Dst       : Cv_Arr_P;
                                         Patchsize : Cv_Size;
                                         Hist      : Cv_Histogram_P;
                                         Method    : Hist_Compare_Method;
                                         Factor    : Float) renames Cv_Calc_Arr_Back_Project_Patch;

   --Divides one histogram by another.
   procedure Cv_Calc_Prob_Density (Hist1   : Cv_Histogram_P;
                                   Hist2   : Cv_Histogram_P;
                                   Dsthist : Cv_Histogram_P;
                                   Scale   : Long_Float := 255.0);

   -- equalizes histogram of 8-bit single-channel image
   procedure Cv_Equalize_Hist (Src : Cv_Arr_P;
                               Dst : Cv_Arr_P);

   -- Calculates the distance to the closest zero pixel for all non-zero pixels of the source image.
   procedure Cv_Dist_Transform (Src          : Cv_Arr_P;
                                Dst          : Cv_Arr_P;
                                Distancetype : Integer := Cv_Dist_L2;
                                Masksize     : Integer := 3;
                                Mask         : access Float := null;
                                Labels       : Cv_Arr_P := null);

   -- Applies a fixed-level threshold to array elements.
   procedure Cv_Threshold (Src           : Cv_Arr_P;
                           Dst           : Cv_Arr_P;
                           Threshold     : Long_Float;
                           Maxvalue      : Long_Float;
                           Thresholdtype : Threshold_Type);

   -- Applies adaptive threshold to grayscale image.
   -- The two parameters for methods CV_ADAPTIVE_THRESH_MEAN_C and
   -- CV_ADAPTIVE_THRESH_GAUSSIAN_C are:
   -- neighborhood size (3, 5, 7 etc.),
   -- and a constant subtracted from mean (...,-3,-2,-1,0,1,2,3,...)
   procedure Cv_Adaptive_Threshold (Src             : Cv_Arr_P;
                                    Dst             : Cv_Arr_P;
                                    Maxvalue        : Long_Float;
                                    Adaptivemethod  : Adaptive_Method := Cv_Adaptive_Thresh_Mean_C;
                                    Thresholdtype   : Threshold_Type := Cv_Thresh_Binary;
                                    Blocksize       : Integer := 3;
                                    Param1          : Long_Float := 5.0);

   -- Fills the connected component until the color difference gets large enough
   procedure Cv_Flood_Fill (Image     : Cv_Arr_P;
                            Seedpoint : Cv_Point;
                            Newvalue  : Cv_Scalar;
                            Lodiff    : Cv_Scalar := Cv_Scalar_All (0.0);
                            Updiff    : Cv_Scalar := Cv_Scalar_All (0.0);
                            Comp      : Cv_Connected_Comp_P := null;
                            Flags     : Integer := 4;
                            Mask      : Cv_Arr_P := null);

   -----------------------------------------------------------------------------
   -- Feature detection
   -----------------------------------------------------------------------------
   -- Runs canny edge detector
   procedure Cv_Canny ( Image        : Cv_Arr_P;
                       Edges         : Cv_Arr_P;
                       Threshold1    : Long_Float;
                       Threshold2    : Long_Float;
                       Aperture_Size : Integer := 3);

   -- Calculates constraint image for corner detection
   -- Dx^2 * Dyy + Dxx * Dy^2 - 2 * Dx * Dy * Dxy.
   -- Applying threshold to the result gives coordinates of corners
   procedure Cv_Pre_Corner_Detect ( Image       : Cv_Arr_P;
                                   Corners      : Cv_Arr_P;
                                   Aperturesize : Integer := 3);

   -- Calculates eigenvalues and eigenvectors of image blocks for corner detection.
   procedure Cv_Corner_Eigen_Vals_And_Vecs (Image         : Cv_Arr_P;
                                            Eigenvv       : Cv_Arr_P;
                                            Blocksize     : Integer;
                                            Aperture_Size : Integer := 3);

   -- Calculates the minimal eigenvalue of gradient matrices for corner detection.
   procedure Cv_Corner_Min_Eigen_Val (Image         : Cv_Arr_P;
                                      Eigenval      : Cv_Arr_P;
                                      Blocksize     : Integer;
                                      Aperture_Size : Integer := 3);

   --Harris edge detector.
   procedure Cv_Corner_Harris (Image         : Cv_Arr_P;
                               Harris_Dst    : Cv_Arr_P;
                               Blocksize     : Integer;
                               Aperture_Size : Integer := 3;
                               K             : Long_Float := 0.04);

   --Refines the corner locations.
   procedure Cv_Find_Corner_Sub_Pix (Image    : Cv_Arr_P;
                                     Corners  : Cv_Point_2d_32f; -- fixa
                                     Count    : Integer;
                                     Win      : Cv_Size;
                                     Zerozone : Cv_Size;
                                     Criteria : Cv_Term_Criteria);

   -- Determines strong corners on an image.
   procedure Cv_Good_Features_To_Track (Image        : Cv_Arr_P;
                                        Eigimage     : Cv_Arr_P;
                                        Tempimage    : Cv_Arr_P;
                                        Corners      : Cv_Point_2d_32f_Array;
                                        Cornercount  : access Integer;
                                        Qualitylevel : Long_Float;
                                        Mindistance  : Long_Float;
                                        Mask         : Cv_Arr_P := null;
                                        Blocksize    : Integer := 3;
                                        Useharris    : Integer := 0;
                                        K            : Long_Float := 0.04);

   --Finds lines in a binary image using a Hough transform.
   function Cv_Hough_Lines2 (Image     : Cv_Arr_P;
                             Storage   : Cv_Void_P;
                             Method    : Integer;
                             Rho       : Long_Float;
                             Theta     : Long_Float;
                             Threshold : Integer;
                             Param1    : Long_Float := 0.0;
                             Param2    : Long_Float := 0.0) return Cv_Seq_P;

   -- Finds circles in the image
   function Cv_Hough_Circles (Image          : Cv_Arr_P;
                              Circle_Storage : Cv_Void_P;
                              Method         : Integer;
                              Dp             : Long_Float;
                              Min_Dist       : Long_Float;
                              Param1         : Long_Float := 100.0;
                              Param2         : Long_Float := 100.0;
                              Min_Radis      : Integer := 0;
                              Max_Radius     : Integer := 0)
                              return Cv_Seq_P;

   -- Fits a line to a 2D or 3D point set.
   procedure Cv_Fit_Line (Points   : Cv_Arr_P;
                          Disttype : Integer;
                          Param    : Long_Float;
                          Reps     : Long_Float;
                          Aepes    : Long_Float;
                          Line     : Cv_32f_Array);

   -- Constructs kd-tree from set of feature descriptors
   function Cv_Create_Kd_Tree (Desc : Cv_Mat_P)
                               return Cv_Feature_Tree_P;

   -- Constructs spill-tree from set of feature descriptors
   function Cv_Create_Spill_Tree (Raw_Data : Cv_Mat_P;
                                  Naive    : Integer := 50;
                                  Rho      : Long_Float := 0.7;
                                  Tau      : Long_Float := 0.1)
                                  return Cv_Feature_Tree_P;

   -- Release feature tree
   procedure Cv_Release_Feature_Tree (Tr : Cv_Feature_Tree_P);

   -- Searches feature tree for k nearest neighbors of given reference points,
   -- searching (in case of kd-tree/bbf) at most emax leaves.
   procedure Cv_Find_Features (Tr           : Cv_Feature_Tree_P;
                               Query_Points : Cv_Mat_P;
                               Indices      : Cv_Mat_P;
                               Dist         : Cv_Mat_P;
                               K            : Integer;
                               Emax         : Integer := 20);

   -- Search feature tree for all points that are inlier to given rect region.
   -- Only implemented for kd trees
   function Cv_Find_Features_Boxed (Tr          : Cv_Feature_Tree_P;
                                    Bounds_Min  : Cv_Mat_P;
                                    Bounds_Max  : Cv_Mat_P;
                                    Out_Indices : Cv_Mat_P)
                                    return Integer;

   -- Construct a Locality Sensitive Hash (LSH) table, for indexing d-dimensional vectors of
   -- given type. Vectors will be hashed L times with k-dimensional p-stable (p=2) functions.

   function Cv_Create_Lsh (Ops     : Cv_Lsh_Operations_P;
                           D       : Integer;
                           L       : Integer := 10;
                           K       : Integer := 10;
                           Table_T : Unsigned_32 := Cv_Maketype (Cv_64f, 1);
                           R       : Long_Float := 4.0;
                           Seed    : Interfaces.Integer_64 := -1)
                           return Cv_Lsh_P;

   -- Construct in-memory LSH table, with n bins.

   function Cv_Create_Memory_Lsh (D       : Integer;
                                  N       : Integer;
                                  L       : Integer := 10;
                                  K       : Integer := 10;
                                  Table_T : Unsigned_32 := Cv_Maketype (Cv_64f, 1);
                                  R       : Long_Float := 4.0;
                                  Seed    : Integer_64 := -1)
                                  return Cv_Lsh_P;

   -- Free the given LSH structure.
   procedure Cv_Release_Lsh (Lsh : access Cv_Lsh_P);

   -- Return the number of vectors in the LSH.
   function Lsh_Size (Lsh : Cv_Lsh_P)
                      return Integer;

   -- Add vectors to the LSH structure, optionally returning indices.
   procedure Cv_Lsh_Add (Lsh     : Cv_Lsh_P;
                         Data    : Cv_Mat_P;
                         Indices : Cv_Mat_P := null);

   -- Remove vectors from LSH, as addressed by given indices.
   procedure Cv_Lsh_Remove (Lsh     : Cv_Lsh_P;
                            Indices : Cv_Mat_P);

   -- Query the LSH n times for at most k nearest points; data is n x d,
   -- indices and dist are n x k. At most emax stored points will be accessed.
   procedure Cv_Lsh_Query (Lsh          : Cv_Lsh_P;
                           Query_Points : Cv_Mat_P;
                           Indices      : Cv_Mat_P;
                           Dist         : Cv_Mat_P;
                           K            : Integer;
                           Emax         : Integer);
private

   pragma Import (C, Cv_Acc, "cvAcc");
   pragma Import (C, Cv_Square_Acc, "cvSquareAcc");
   pragma Import (C, Cv_Multiply_Acc, "cvMultiplyAcc");
   pragma Import (C, Cv_Running_Avg, "cvRunningAvg");

   pragma Import (C, Cv_Copy_Make_Border, "cvCopyMakeBorder");
   pragma Import (C, Cv_Smooth, "cvSmooth");
   pragma Import (C, Cv_Filter_2d, "cvFilter2D");
   pragma Import (C, Cv_Integral, "cvIntegral");
   pragma Import (C, Cv_Pyr_Down, "cvPyrDown");
   pragma Import (C, Cv_Pyr_Up, "cvPyrUp");
   pragma Import (C, Cv_Create_Pyramid, "cvCreatePyramid");
   pragma Import (C, Cv_Release_Pyramid, "cvReleasePyramid");
   pragma Import (C, Cv_Pyr_Mean_Shift_Filtering, "cvPyrMeanShiftFiltering");
   pragma Import (C, Cv_Pyr_Segmentation, "cvPyrSegmentation");
   pragma Import (C, Cv_Watershed, "cvWatershed");
   pragma Import (C, Cv_Inpaint, "cvInpaint");
   pragma Import (C, Cv_Sobel, "cvSobel");
   pragma Import (C, Cv_Laplace, "cvLaplace");
   pragma Import (C, Cv_Cvt_Color, "cvCvtColor");
   pragma Import (C, Cv_Resize, "cvResize");
   pragma Import (C, Cv_Warp_Affine, "cvWarpAffine");
   pragma Import (C, Cv_Get_Affine_Transform, "cvGetAffineTransform");
   pragma Import (C, Cv_2d_Rotation_Matrix, "cv2DRotationMatrix" );
   pragma Import (C, Cv_Warp_Perspective, "cvWarpPerspective");
   pragma Import (C, Cv_Get_Perspective_Transform, "cvGetPerspectiveTransform");
   pragma Import (C, Cv_Remap, "cvRemap");
   pragma Import (C, Cv_Convert_Maps, "cvConvertMaps");
   pragma Import (C, Cv_Log_Polar, "cvLogPolar");
   pragma Import (C, Cv_Linear_Polar, "cvLinearPolar");
   pragma Import (C, Cv_Undistort2, "cvUndistort2");
   pragma Import (C, Cv_Init_Undistort_Map, "cvInitUndistortMap");
   pragma Import (C, Cv_Init_Undistort_Rectify_Map, "cvInitUndistortRectifyMap");
   pragma Import (C, Cv_Undistort_Points, "cvUndistortPoints");
   pragma Import (C, Cv_Create_Structuring_Element_Ex, "cvCreateStructuringElementEx");
   pragma Import (C, Cv_Release_Structuring_Element, "cvReleaseStructuringElement");
   pragma Import (C, Cv_Erode, "cvErode");
   pragma Import (C, Cv_Dilate, "cvDilate");
   pragma Import (C, Cv_Morphology_Ex, "cvMorphologyEx");
   pragma Import (C, Cv_Moments, "cvMoments");
   pragma Import (C, Cv_Get_Spatial_Moment, "cvGetSpatialMoment");
   pragma Import (C, Cv_Get_Central_Moment, "cvGetCentralMoment");
   pragma Import (C, Cv_Get_Normalized_Central_Moment, "cvGetNormalizedCentralMoment");
   pragma Import (C, Cv_Get_Hu_Moments, "cvGetHuMoments");

   pragma Import (C, Cv_Get_Rect_Sub_Pix, "cvGetRectSubPix");
   pragma Import (C, Cv_Sample_Line, "cvSampleLine");
   pragma Import (C, Cv_Get_Quadrangle_Sub_Pix, "cvGetQuadrangleSubPix");
   pragma Import (C, Cv_Match_Template, "cvMatchTemplate");
   pragma Import (C, Cv_Calc_Emd2, "cvCalcEMD2");

   pragma Import (C, Cv_Find_Contours, "cvFindContours");
   pragma Import (C, Cv_Start_Find_Contours, "cvStartFindContours");
   pragma Import (C, Cv_Find_Next_Contour, "cvFindNextContour");
   pragma Import (C, Cv_Substitute_Contour, "cvSubstituteContour");
   pragma Import (C, Cv_End_Find_Contours, "cvEndFindContours");
   pragma Import (C, Cv_Approx_Chains, "cvApproxChains");
   pragma Import (C, Cv_Start_Read_Chain_Points, "cvStartReadChainPoints");
   pragma Import (C, Cv_Read_Chain_Point, "cvReadChainPoint");

   pragma Import (C, Cv_Init_Subdiv_Delaunay_2d, "cvInitSubdivDelaunay2D");
   pragma Import (C, Cv_Create_Subdiv_2d, "cvCreateSubdiv2D");

   pragma Import (C, Cv_Create_Subdiv_Delaunay_2d, "cvCreateSubdivDelaunay2D");
   pragma Import (C, Cv_Subdiv_Delaunay_2d_Insert, "cvSubdivDelaunay2DInsert");
   pragma Import (C, Cv_Subdiv_2d_Locate, "cvSubdiv2DLocate");
   pragma Import (C, Cv_Calc_Subdiv_Voronoi_2d, "cvCalcSubdivVoronoi2D");
   pragma Import (C, Cv_Clear_Subdiv_Voronoi_2d, "cvClearSubdivVoronoi2D");
   pragma Import (C, Cv_Find_Nearest_Point_2d, "cvFindNearestPoint2D");
   pragma Import (C, Cv_Subdiv_2d_Next_Edge, "cvSubdiv2DNextEdge");
   pragma Import (C, Cv_Subdiv_2d_Rotate_Edge, "cvSubdiv2DRotateEdge");
   pragma Import (C, Cv_Subdiv_2d_Sym_Edge, "cvSubdiv2DSymEdge");
   pragma Import (C, Cv_Subdiv_2d_Get_Edge, "cvSubdiv2DGetEdge");
   pragma Import (C, Cv_Subdiv_2d_Edge_Org, "cvSubdiv2DEdgeOrg");
   pragma Import (C, Cv_Subdiv_2d_Edge_Dst, "cvSubdiv2DEdgeDst");
   pragma Import (C, Cv_Triangle_Area, "cvTriangleArea");

   pragma Import (C, Cv_Approx_Poly, "cvApproxPoly");
   pragma Import (C, Cv_Arc_Length, "cvArcLength");
   pragma Import (C, Cv_Bounding_Rect, "cvBoundingRect");
   pragma Import (C, Cv_Contour_Area, "cvContourArea");
   pragma Import (C, Cv_Min_Area_Rect2, "cvMinAreaRect2");
   pragma Import (C, Cv_Min_Enclosing_Circle, "cvMinEnclosingCircle");
   pragma Import (C, Cv_Match_Shapes, "cvMatchShapes");
   pragma Import (C, Cv_Convex_Hull2, "cvConvexHull2");
   pragma Import (C, Cv_Check_Contour_Convexity, "cvCheckContourConvexity");
   pragma Import (C, Cv_Convexity_Defects, "cvConvexityDefects");
   pragma Import (C, Cv_Fit_Ellipse2, "cvFitEllipse2");
   pragma Import (C, Cv_Max_Rect, "cvMaxRect");
   pragma Import (C, Cv_Box_Points, "cvBoxPoints");
   pragma Import (C, Cv_Point_Seq_From_Mat, "cvPointSeqFromMat");
   pragma Import (C, Cv_Point_Polygon_Test, "cvPointPolygonTest");

   pragma Import (C, Cv_Create_Hist, "cvCreateHist");
   pragma Import (C, Cv_Set_Hist_Bin_Ranges, "cvSetHistBinRanges");
   pragma Import (C, Cv_Make_Hist_Header_For_Array, "cvMakeHistHeaderForArray");
   pragma Import (C, Cv_Release_Hist, "cvReleaseHist");
   pragma Import (C, Cv_Clear_Hist, "cvClearHist");
   pragma Import (C, Cv_Get_Min_Max_Hist_Value, "cvGetMinMaxHistValue");
   pragma Import (C, Cv_Normalize_Hist, "cvNormalizeHist");
   pragma Import (C, Cv_Thresh_Hist, "cvThreshHist");
   pragma Import (C, Cv_Compare_Hist, "cvCompareHist");
   pragma Import (C, Cv_Copy_Hist, "cvCopyHist");
   pragma Import (C, Cv_Calc_Bayesian_Prob, "cvCalcBayesianProb");
   pragma Import (C, Cv_Calc_Arr_Hist, "cvCalcArrHist");
   pragma Import (C, Cv_Calc_Hist, "cvCalcHist");
   pragma Import (C, Cv_Calc_Arr_Back_Project, "cvCalcArrBackProject");
   pragma Import (C, Cv_Calc_Arr_Back_Project_Patch, "cvCalcArrBackProjectPatch");
   pragma Import (C, Cv_Calc_Prob_Density, "cvCalcProbDensity");
   pragma Import (C, Cv_Equalize_Hist, "cvEqualizeHist");
   pragma Import (C, Cv_Dist_Transform, "cvDistTransform");
   pragma Import (C, Cv_Threshold, "cvThreshold");
   pragma Import (C, Cv_Adaptive_Threshold, "cvAdaptiveThreshold");
   pragma Import (C, Cv_Flood_Fill, "cvFloodFill");

   pragma Import (C, Cv_Canny, "cvCanny");
   pragma Import (C, Cv_Pre_Corner_Detect, "cvPreCornerDetect");
   pragma Import (C, Cv_Corner_Eigen_Vals_And_Vecs, "cvCornerEigenValsAndVecs");
   pragma Import (C, Cv_Corner_Min_Eigen_Val, "cvCornerMinEigenVal");
   pragma Import (C, Cv_Corner_Harris, "cvCornerHarris");
   pragma Import (C, Cv_Find_Corner_Sub_Pix, "cvFindCornerSubPix");
   pragma Import (C, Cv_Good_Features_To_Track, "cvGoodFeaturesToTrack");
   pragma Import (C, Cv_Hough_Lines2, "cvHoughLines2");
   pragma Import (C, Cv_Hough_Circles, "cvHoughCircles");
   pragma Import (C, Cv_Fit_Line, "cvFitLine");
   pragma Import (C, Cv_Create_Kd_Tree, "cvCreateKDTree");
   pragma Import (C, Cv_Create_Spill_Tree, "cvCreateSpillTree");
   pragma Import (C, Cv_Release_Feature_Tree, "cvReleaseFeatureTree");
   pragma Import (C, Cv_Find_Features, "cvFindFeatures");
   pragma Import (C, Cv_Find_Features_Boxed, "cvFindFeaturesBoxed");
   pragma Import (C, Cv_Create_Lsh, "cvCreateLSH");
   pragma Import (C, Cv_Create_Memory_Lsh, "cvCreateMemoryLSH");
   pragma Import (C, Cv_Release_Lsh, "cvReleaseLSH");
   pragma Import (C, Lsh_Size, "LSHSize");
   pragma Import (C, Cv_Lsh_Add, "cvLSHAdd");
   pragma Import (C, Cv_Lsh_Remove, "cvLSHRemove");
   pragma Import (C, Cv_Lsh_Query, "cvLSHQuery");
end Imgproc.Operations;
