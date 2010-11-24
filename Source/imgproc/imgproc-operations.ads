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
with Interfaces; use Interfaces;
with Core; use Core;
use Core;
use Core;

package Imgproc.Operations is
--

   -----------------------------------------------------------------------------
   -- Background statistics accumulation
   -----------------------------------------------------------------------------

   -- Adds image to accumulator
   procedure CvAcc (Image : Cv_Arr_P;
                    Sum   : Cv_Arr_P;
                    Mask  : Cv_Arr_P := null);

   -- Adds the square of the source image to the accumulator.
   procedure CvSquareAcc (Image : Cv_Arr_P;
                          Sqsum : Cv_Arr_P;
                          Mask  : Cv_Arr_P := null);

   -- Adds the product of two input images to the accumulator.
   procedure CvMultiplyAcc (Image1 : Cv_Arr_P;
                            Image2 : Cv_Arr_P;
                            Acc    : Cv_Arr_P;
                            Mask   : Cv_Arr_P := null);

   -- Updates the running average.
   procedure CvRunningAvg (Image : Cv_Arr_P;
                           Acc   : Cv_Arr_P;
                           Alpha : Long_Float;
                           Mask  : Cv_Arr_P := null);

   -----------------------------------------------------------------------------
   -- Image Processing
   -----------------------------------------------------------------------------

   -- Copies source 2D array inside of the larger destination array and
   -- makes a border of the specified type (IPL_BORDER_*) around the copied area.
   procedure CvCopyMakeBorder (Src         : Cv_Arr_P;
                               Dst         : Cv_Arr_P;
                               Offset      : Cv_Point;
                               Border      : Border_Type;
                               Value       : Cv_Scalar);

   -- Smoothes array (removes noise)
   procedure CvSmooth (Src        : Cv_Arr_P;
                       Dst        : Cv_Arr_P;
                       SmoothType : Smooth_Type;
                       Param1     : Integer := 3;
                       Param2     : Integer := 0;
                       Param3     : Long_Float := 0.0;
                       Param4     : Long_Float := 0.0);

   -- Convolves the image with the kernel
   procedure CvFilter2D (Src : Cv_Arr_P;
                         Dst : Cv_Arr_P;
                         Kernel : Cv_Mat_P;
                         Anchor : Cv_Point := (-1, -1));

   -- Finds integral image: SUM(X,Y) = sum(x<X,y<Y)I(x,y)
   procedure CvIntegral (Image : Cv_Arr_P;
                         Sum   : Cv_Arr_P;
                         SqSum : Cv_Arr_P := null;
                         TitledSum : Cv_Arr_P := null);

   -- Smoothes the input image with gaussian kernel and then down-samples it.
   -- dst_width = floor(src_width/2)[+1],
   -- dst_height = floor(src_height/2)[+1]
   procedure CvPyrDown (Src    : Cv_Arr_P;
                        Dst    : Cv_Arr_P;
                        Filter : Pyr_Filter := CV_GAUSSIAN_5x5);

   -- Up-samples image and smoothes the result with gaussian kernel.
   -- dst_width = src_width*2,
   -- dst_height = src_height*2
   procedure CvPyrUp (Src    : access Cv_Arr_P;
                      Dst    : access Cv_Arr;
                      Filter : Pyr_Filter := CV_GAUSSIAN_5X5);

   -- Builds pyramid for an image
   function CvCreatePyramid (Img          : Cv_Arr_P;
                             Extra_Layers : Integer;
                             Rate         : Long_Float;
                             Layer_Sizes  : C_Size_Ptr := null;
                             Bufarr       : Cv_Arr_P := null;
                             Calc         : Integer := 1;
                             Filter       : Pyr_Filter := Cv_Gaussian_5x5)
                             return C_Mat_P_Ptr;

   -- Releases pyramid
   procedure CvReleasePyramid (Pyramid      : access C_Mat_P_Ptr;
                               Extra_Layers : Integer);

   -- Does meanshift image segmentation
   procedure CvPyrMeanShiftFiltering (Src : Cv_Arr_P;
                                      Dst : Cv_Arr_P;
                                      Sp  : Long_Float;
                                      Sr  : Long_Float;
                                      Max_Level : Integer := 1;
                                      Termcrit  : Cv_Term_Criteria := CvTermCriteria (CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 5, 1.0));

   -- Segments image using seed markers
   procedure CvWatershed (Image : Cv_Arr_P;
                          Markers : Cv_Arr_P);

   -- Inpaints the selected region in the image.
   procedure CvInpaint (Src : Cv_Arr_P;
                        Mask : Cv_Arr_P;
                        Dst  : Cv_Arr_P;
                        InpaintRadius : Long_Float;
                        Flags         : Integer);

   -- Calculates the first, second, third or mxied image derivatives using an extended Sobel operator.
   procedure CvSobel (Src          : Cv_Arr_P;
                      Dst          : Cv_Arr_P;
                      Xorder       : Integer;
                      Yorder       : Integer;
                      ApertureSize : Integer := 3);

   -- Calculates the image Laplacian: (d2/dx + d2/dy)I
   procedure CvLaplace (Src          : Cv_Arr_P;
                        Dst          : Cv_Arr_P;
                        ApertureSize : Integer);

   -- Converts an image from one color space to another.
   procedure CvCvtColor (Src  : Cv_Arr_P;
                         Dst  : Cv_Arr_P;
                         Code : Color_Conversion);

   -- Resizes an image.
   procedure CvResize (Src : Cv_Arr_P;
                       Dst : Cv_Arr_P;
                       Interplation : Cv_Inter := CV_INTER_LINEAR);

   --Applies an affine transformation to an image.
   procedure CvWarpAffine (Src : Cv_Arr_P;
                           Dst : Cv_Arr_P;
                           MapMatrix : Cv_Mat_P;
                           Flags     : Integer := Cv_Inter'Pos(CV_INTER_LINEAR) + Cv_Warp'Pos(CV_WARP_FILL_OUTLIERS);
                           Fillval   : Cv_Scalar := CvScalarAll (0.0));

   -- Calculates the affine transform from 3 corresponding points.
   function CvGetAffineTransform (Src : Cv_Point_2D_32F_Array;
                                  Dst : Cv_Point_2D_32F_Array;
                                  MapMatrix : Cv_Mat_P) return Cv_Mat_P;

   -- Calculates the affine matrix of 2d rotation.
   function Cv2DRotationMatrix (Center : Cv_Point_2D_32F;
                                Angle  : Long_Float;
                                Scale  : Long_Float;
                                MapMatrix : Cv_Mat_P) return Cv_Mat_P;

   --Applies a perspective transformation to an image.
   procedure CvWarpPerspective (Src : Cv_Arr_P;
                                Dst : Cv_Arr_P;
                                MapMatrix : Cv_Mat_P;
                                Flags     : Integer := Cv_Inter'Pos(CV_INTER_LINEAR) + Cv_Warp'Pos(CV_WARP_FILL_OUTLIERS);
                                Fillval   : Cv_Scalar := CvScalarAll (0.0));

   -- Calculates the perspective transform from 4 corresponding points.
   function CvGetPerspectiveTransform (Src : Cv_Point_2D_32F_Array;
                                       Dst : Cv_Point_2D_32F_Array;
                                       MapMatrix : Cv_Mat_P) return Cv_Mat_P;

   -- Applies a generic geometrical transformation to the image.
   procedure CvRemap (Src : Cv_Arr_P;
                      Dst : Cv_Arr_P;
                      Mapx : Cv_Arr_P;
                      Mapy : Cv_Arr_P;
                      Flags   : Integer := Cv_Inter'Pos(CV_INTER_LINEAR) + Cv_Warp'Pos(CV_WARP_FILL_OUTLIERS);
                      Fillval : Cv_Scalar := CvScalarAll (0.0));

   -- Converts mapx & mapy from floating-point to integer formats for cvRemap
   procedure CvConvertMaps (Mapx     : Cv_Arr_P;
                            Mapy     : Cv_Arr_P;
                            Mapxy    : Cv_Arr_P;
                            Mapalpha : Cv_Arr_P);

   -- Remaps an image to log-polar space.
   procedure CvLogPolar (Src : Cv_Arr_P;
                         Dst : Cv_Arr_P;
                         Center : Cv_Point_2D_32F;
                         M      : Long_Float;
                         Flags  : Integer := Cv_Inter'Pos (CV_INTER_LINEAR) + Cv_Warp'Pos (CV_WARP_FILL_OUTLIERS));

   -- Performs forward or inverse linear-polar image transform
   procedure CvLinearPolar (Src        : Cv_Arr_P;
                            Dst        : Cv_Arr_P;
                            Center     : Cv_Point_2d_32f;
                            Max_Radius : Long_Float;
                            Flags      : Integer := Cv_Inter'Pos (CV_INTER_LINEAR) + Cv_Warp'Pos (CV_WARP_FILL_OUTLIERS));

   -- Transforms an image to compensate for lens distortion.
   procedure CvUndistort2 (Src : Cv_Arr_P;
                           Dst : Cv_Arr_P;
                           CameraMatrix : Cv_Mat_P;
                           DistCoefs    : Cv_Mat_P;
                           NewCameraMatrix : Cv_Mat_P);

      -- Computes an undistortion map.
   procedure CvInitUndistortMap (CameraMatrix : Cv_Mat_P;
                                 DistCoeffs   : Cv_Mat_P;
                                 Map1         : Cv_Arr_P;
                                 Map2         : Cv_Arr_P);

   -- Computes the undistortion and rectification transformation map.
   procedure CvInitUndistortRectifyMap (CameraMatrix : Cv_Mat_P;
                                        DistCoeffs   : Cv_Mat_P;
                                        R            : Cv_Mat_P;
                                        NewCameraMatrix : Cv_Mat_P;
                                        Map1            : Cv_Arr_P;
                                        Map2            : Cv_Arr_P);

   -- Computes the ideal point coordinates from the observed point coordinates.
   procedure CvUndistortPoints (Src          : Cv_Mat_P;
                                Dst          : Cv_Mat_P;
                                CameraMatrix : Cv_Mat_P;
                                DistCoeffs   : Cv_Mat_P;
                                R            : Cv_Mat_P := null;
                                P            : Cv_Mat_P := null);

   -- creates structuring element used for morphological operations
   function CvCreateStructuringElementEx (Cols     : Integer;
                                          Rows     : Integer;
                                          AnchorX  : Integer;
                                          AnchorY  : Integer;
                                          Shape    : Structuring_Shape;
                                          Values   : access Integer) return Ipl_Conv_Kernel_P;

   -- releases structuring element
   procedure CvReleaseStructuringElement (Element : access Ipl_Conv_Kernel_P);

   -- erodes input image (applies minimum filter) one or more times.
   -- if element pointer is NULL, 3x3 rectangular element is used
   procedure CvErode (Src        : Cv_Arr_P;
                      Dst        : Cv_Arr_P;
                      Element    : Ipl_Conv_Kernel_P := null;
                      Iterations : Integer := 1);

   -- dilates input image (applies maximum filter) one or more times.
   -- If element pointer is NULL, 3x3 rectangular element is used
   procedure CvDilate (Src        : Cv_Arr_P;
                       Dst        : Cv_Arr_P;
                       Element    : Ipl_Conv_Kernel_P := null;
                       Iterations : Integer := 1);

   -- Performs complex morphological transformation
   procedure CvMorphologyEx (Src        : Cv_Arr_P;
                             Dst        : Cv_Arr_P;
                             Temp       : Cv_Arr_P;
                             Element    : Ipl_Conv_Kernel_P;
                             Operation  : Morph_Operation;
                             Iterations : Integer);

   -- Calculates all of the moments up to the third order of a polygon or rasterized shape.
   procedure CvMoments (Arr     : Cv_Arr_P;
                        Moments : Cv_Moments_P;
                        Binary  : Integer := 0);

   -- Retrieves the spatial moment from the moment state structure.
   function CvGetSpatialMoment (Moments : Cv_Moments_P;
                                XOrder  : Integer;
                                YOrder  : Integer) return Long_Float;

   -- Retrieves the central moment from the moment state structure.
   function CvGetCentralMoment (Moments : Cv_Moments_P;
                                XOrder  : Integer;
                                YOrder  : Integer) return Long_Float;

   -- Retrieves the normalized central moment from the moment state structure.
   function CvGetNormalizedCentralMoment (Moments : Cv_Moments_P;
                                          XOrder  : Integer;
                                          YOrder  : Integer) return Long_Float;

   -- Calculates the seven Hu invariants.
   procedure CvGetHuMoments (Moments : Cv_Moments_P;
                             Hu      : Cv_Hu_Moments_P);

   -----------------------------------------------------------------------------
   -- Data sampling
   -----------------------------------------------------------------------------
   -- Fetches pixels that belong to the specified line segment and stores them to the buffer.
   -- Returns the number of retrieved points.
   function CvSampleLine (Image : Cv_Arr_P;
                          Pt1   : Cv_Point;
                          Pt2   : Cv_Point;
                          Buffer : Cv_Void_P;
                          Connectivity : Integer := 8) return Integer;

   -- Retrieves the rectangular image region with specified center from the input array.
   -- dst(x,y) <- src(x + center.x - dst_width/2, y + center.y - dst_height/2).
   -- Values of pixels with fractional coordinates are retrieved using bilinear interpolation
   procedure CvGetRectSubPix (Src : Cv_Arr_P;
                              Dst : Cv_Arr_P;
                              Center : CV_Point_2D_32F);

   --  Retrieves quadrangle from the input array.
   --   matrixarr = ( a11  a12 | b1 )   dst(x,y) <- src(A[x y]' + b)
   --               ( a21  a22 | b2 )   (bilinear interpolation is used to retrieve pixels
   --                                   with fractional coordinates)
   procedure CvGetQuadrangleSubPix (Src : Cv_Arr_P;
                                    Dst : Cv_Arr_P;
                                    MapMatrix : Cv_Mat_P);

   -- Measures similarity between template and overlapped windows in the source image
   -- and fills the resultant image with the measurements
   procedure CvMatchTemplate (Image : Cv_Arr_P;
                              Templ : Cv_Arr_P;
                              Result : Cv_Arr_P;
                              Method : Integer);

   --     Computes the minimal work distance between two weighted point configurations.
   function CvCalcEMD2 (Signature1    : access Cv_Arr;
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
   function CvFindContours (Image        : Cv_Arr_P;
                            Storage      : Cv_Mem_Storage_P;
                            FirstContour : access Cv_Seq_P;
                            HeaderSize   : Integer := Cv_Contour'Size;
                            Mode         : Cv_Retr := CV_RETR_LIST;
                            Method       : Cv_Chain_Enum := CV_CHAIN_APPROX_SIMPLE;
                            Offset       : Cv_Point := CvPoint (0, 0)) return Integer;

   -- Initializes the contour scanning process.
   function CvStartFindContours (Image      : Cv_Arr_P;
                                 Storage    : Cv_Mem_Storage_P;
                                 HeaderSize : Integer := Cv_Contour'Size;
                                 Mode       : Cv_Retr := CV_RETR_LIST;
                                 Method     : Cv_Chain_Enum := CV_CHAIN_APPROX_SIMPLE;
                                 Offset     : Cv_Point := CvPoint (0, 0)) return Cv_Contour_Scanner;

   -- Finds the next contour in the image.
   function CvFindNextContour (Scanner : Cv_Contour_Scanner) return Cv_Seq_P;

   -- Replaces a retrieved contour.
   procedure CvSubstituteContour (Scanner    : Cv_Contour_Scanner;
                                  NewContour : Cv_Seq_P);

   -- Finishes the scanning process.
   function CvEndFindContours (Scanner : Cv_Contour_Scanner_P) return Cv_Seq_P;

   --     Approximates Freeman chain with a polygonal curve.
   function CvApproxChains (Src_Seq           : access Cv_Seq;
                            Storage           : access Cv_Mem_Storage;
                            Method            : Cv_Chain_Enum := CV_CHAIN_APPROX_SIMPLE;
                            Parameter         : Long_Float := 0.0;
                            Minimal_Perimeter : Integer := 0;
                            Recursive         : Integer := 0)
                            return Cv_Seq_P;

   -- Initalizes Freeman chain reader.
   -- The reader is used to iteratively get coordinates of all the chain points.
   -- If the Freeman codes should be read as is, a simple sequence reader should be used
   procedure CvStartReadChainPoints (Chain  : Cv_Chain_P;
                                     Reader : Cv_Chain_Pt_Reader_P);

      -- Gets the next chain point.
   function CvReadChainPoint (Reader : Cv_Chain_Pt_Reader_P) return Cv_Point;

   -----------------------------------------------------------------------------
   -- Planar subdivisions
   -----------------------------------------------------------------------------
   -- Initializes Delaunay triangulation
   procedure CvInitSubdivDelaunay2D (Subdiv : Cv_Subdiv_2D_P;
                                     Rect   : Cv_Rect);

   -- Creates new subdivision
   function CvCreateSubdiv2D (Subdiv_Type : Integer;
                              Header_Size : Integer;
                              Vtx_Size    : Integer;
                              Quadedge_Size : Integer;
                              Storage       : Cv_Mem_Storage_P) return Cv_Subdiv_2D_P;

   -----------------------------------------------------------------------------
   -- high-level subdivision functions
   -----------------------------------------------------------------------------

   -- Creates an empty Delaunay triangulation.
   function CvCreateSubdivDelaunay2D (Rect    : Cv_Rect;
                                      Storage : Cv_Mem_Storage_P) return Cv_Subdiv_2D_P;

   --Inserts a single point into a Delaunay triangulation.
   function CvSubdivDelaunay2DInsert (Subdiv : Cv_Subdiv_2D_P;
                                      Pt     : Cv_Point_2D_32F) return Cv_Subdiv_2D_Point_P;

   -- Returns the location of a point within a Delaunay triangulation.
   function CvSubdiv2DLocate (Subdiv : Cv_Subdiv_2D_P;
                              Pt     : Cv_Point_2D_32F;
                              Edge   : Cv_Subdiv_2D_Edge;
                              Vertex : access Cv_Subdiv_2D_Point_P := null) return Cv_Subdiv_2D_Point_Location;

   -- Calculates the coordinates of Voronoi diagram cells.
   procedure CvCalcSubdivVoronoi2D (Subdiv : Cv_Subdiv_2D_P);

   -- Removes all virtual points.
   procedure CvClearSubdivVoronoi2D (Subdiv : Cv_Subdiv_2D_P);

   --Finds the closest subdivision vertex to the given point.
   function CvFindNearestPoint2D (Subdiv : Cv_Subdiv_2D_P;
                                  Pt     : Cv_Point_2D_32F) return Cv_Subdiv_2D_Point_P;

   -----------------------------------------------------------------------------
   -- Basic quad-edge navigation and operations
   -----------------------------------------------------------------------------
   -- Returns next edge around the edge origin
   function CvSubdiv2DNextEdge (Edge : Cv_Subdiv_2D_Edge) return Cv_Subdiv_2D_Edge;

   -- Returns another edge of the same quad-edge.
   function CvSubdiv2DRotateEdge (Edge : Cv_Subdiv_2D_Edge;
                                  Rotate : Integer) return Cv_Subdiv_2D_Edge;

   function CvSubdiv2DSymEdge (Edge : Cv_Subdiv_2D_Edge) return CV_Subdiv_2D_Edge;

   function CvSubdiv2DGetEdge (Edge      : Cv_Subdiv_2D_Edge;
                               Edge_Type : CV_Next_Edge_Type) return Cv_Subdiv_2D_Edge;

   function CvSubdiv2DEdgeOrg (Edege : Cv_Subdiv_2D_Edge) return Cv_Subdiv_2D_Edge_P;

   -- Returns the edge destination.
   function CvSubdiv2DEdgeDst (Edge : Cv_Subdiv_2D_Edge) return Cv_Subdiv_2D_Point_P;

   function CvTriangleArea (A : Cv_Point_2D_32F;
                            B : Cv_Point_2D_32F;
                            C : Cv_Point_2D_32F) return Long_Float;

   -----------------------------------------------------------------------------
   -- Contour Processing and Shape Analysis
   -----------------------------------------------------------------------------
   --     Approximates polygonal curve with the specified precision.
   function CvApproxPoly (Src_Seq     : Cv_Void_P;
                          Header_Size : Integer;
                          Storage     : access Cv_Mem_Storage;
                          Method      : Integer;
                          Parameter   : Long_Float;
                          Parameter2  : Integer := 0)
                          return Cv_Seq_P;

   --     Calculates the contour perimeter or the curve length.
   function CvArcLength (Curve     : Cv_Void_P;
                         Slice     : Cv_Slice := CvSlice (0);
                         Is_Closed : Integer := -1)
                         return Long_Float;

   function CvContourPerimeter (Curve : Cv_Void_P)
                                return Long_Float;

   -- Calculates contour boundning rectangle (update=1) or
   -- just retrieves pre-calculated rectangle (update=0)
   function CvBoundingRect (Points : access Cv_Arr;
                            Update : Integer := 0)
                            return Cv_Rect;

   --     Calculates the area of a whole contour or a contour section.
   function CvContourArea (Contour : access Cv_Arr;
                           Slice   : Cv_Slice := CvSlice (0))
                           return Long_Float;

   -- Finds the circumscribed rectangle of minimal area for a given 2D point set.
   function CvMinAreaRect2 (Points  : Cv_Arr_P; -- this might be a Cv_Point_Arr...
                            Storage : Cv_Mem_Storage_P := null) return Cv_Box_2D;

   -- Finds the circumscribed circle of minimal area for a given 2D point set.
   function CvMinEnclosingCircle (Points : Cv_Point_Array;
                                  Center : access Cv_Point_2D_32F;
                                  Radius : access Long_Float) return Integer;

   -- Compares two shapes.
   function CvMatchShapes (Object1   : Cv_Void_P;
                           Object2   : Cv_Void_P;
                           Method    : Integer;
                           Parameter : Long_Float := 0.0) return Long_Float;

   -- Finds the convex hull of a point set.
   function CvConvexHull2 (Input       : Cv_Arr_P;
                           Storage     : Cv_Void_P := null;
                           Orientation : Integer := CV_CLOCKWISE;
                           ReturPoints : Integer := 0) return Cv_Seq_P;

   --     Tests contour convexity.
   function CvCheckContourConvexity (Contour : access Cv_Arr)
                                     return Integer;

   -- Finds the convexity defects of a contour.
   function CvConvexityDefects (Contour    : Cv_Arr_P;
                                ConvexHull : Cv_Arr_P) return Cv_Seq_P;

   -- Fits an ellipse around a set of 2D points.
   function CvFitEllipse2 (Points : Cv_Arr_P) -- might be Cv_Point_Arr)
                           return Cv_Box_2D;

   -- Finds minimum rectangle containing two given rectangles
   function CvMaxRect (Rect1 : access Cv_Rect;
                       Rect2 : access Cv_Rect)
                       return Cv_Rect;

   -- Finds coordinates of the box vertices
   procedure CvBoxPoints (Box : Cv_Box_2D;
                          Pt  : out Legacy.Cv_Point_2d_32f_Array_4);

   -- Initializes sequence header for a matrix (column or row vector) of points -
   -- a wrapper for cvMakeSeqHeaderForArray (it does not initialize bounding rectangle!!!)
   function CvPointSeqFromMat (SeqKind       : Integer;
                               Mat           : Cv_Mat_P;
                               ContourHeader : Cv_Contour_P;
                               Block         : Cv_Seq_Block_P) return Cv_Seq_P;

   -- Checks whether the point is inside polygon, outside, on an edge (at a vertex).
   -- Returns positive, negative or zero value, correspondingly.
   -- Optionally, measures a signed distance between
   -- the point and the nearest polygon edge (measure_dist=1)
   function CvPointPolygonTest (Contour     : Cv_Arr_P;
                                Pt          : Cv_Point_2D_32F;
                                MeasureDist : Integer) return Long_Float;

   -----------------------------------------------------------------------------
   -- Histogram functions
   -----------------------------------------------------------------------------
   --Creates a histogram.
   function CvCreateHist (Dims     : Integer;
                          Sizes    : Cv_32S_Array;
                          HistType : Integer;
                          Ranges   : Cv_32F_Ptr_2d_Array := Cv_32F_Array_NULL;
                          Uniform  : Integer := 1) return Cv_Histogram_P;

    --Sets the bounds of the histogram bins.
   procedure CvSetHistBinRanges (Hist     : Cv_Histogram_P;
                                 Ranges   : Cv_32F_Ptr_2d_Array;
                                 Uniforrm : Integer);

   -- Makes a histogram out of an array.
   procedure CvMakeHistHeaderForArray (Dims    : Integer;
                                       Sizes   : Cv_32S_Array;
                                       Hist    : Cv_Histogram_P;
                                       Data    : Cv_32F_Array;
                                       Ranges  : Cv_32F_Ptr_2d_Array; -- fix me
                                       Uniform : Integer := 1);

   --Releases the histogram.
   procedure CvReleaseHist (Hist : access Cv_Histogram_P);

   -- Clears the histogram.
   procedure CvClearHist (Hist : Cv_Histogram_P);

   -- Finds the minimum and maximum histogram bins.
   procedure CvGetMinMaxHistValue (Hist     : Cv_Histogram_P;
                                   MinValue : access Float;
                                   MaxValue : access Float;
                                   MinIdx   : Cv_32S_Array := Cv_32s_Array_Null;
                                   MaxIdx   : Cv_32S_Array := Cv_32s_Array_Null);

   --Normalizes the histogram.
   procedure CvNormalizeHist (Hist   : Cv_Histogram_P;
                              Factor : Long_Float);

   -- Thresholds the histogram.
   procedure CvThreshHist (Hist      : Cv_Histogram_P;
                           Threshold : Long_Float);

   --Compares two dense histograms.
   function CvCompareHist (Hist1  : Cv_Histogram_P;
                           Hist2  : Cv_Histogram_P;
                           Method : Hist_Compare_Method) return Long_Float;

   -- Copies a histogram.
   procedure CvCopyHist (Src : Cv_Histogram_P;
                         Dst : access Cv_Histogram_P);

   -- Calculates bayesian probabilistic histograms
   -- (each or src and dst is an array of <number> histograms
   procedure CvCalcBayesianProb (Src   : C_Histogram_P_Ptr;
                                 Count : Integer;
                                 Dst   : C_Histogram_P_Ptr);

   -- Calculates array histogram
   procedure CvCalcArrHist (Arr        : C_Cv_Arr_P_Ptr;
                            Hist       : Cv_Histogram_P;
                            Accumulate : Integer := 0;
                            Mask       : Cv_Arr_P := null);

   -- Calculates the histogram of image(s).
   procedure CvCalcHist (Image      : Cv_Arr_P_Array;
                         Hist       : Cv_Histogram_P;
                         Accumulate : Integer := 0;
                         Mask       : Cv_Arr_P := null);

   --Calculates the back projection.
   procedure CvCalcArrBackProject (Image       : Cv_Arr_P_Array;
                                   BackProject : Cv_Arr_P;
                                   Hist        : Cv_Histogram_P);

   --Calculates the back projection.
   procedure CvCalcBackProject (Image       : Cv_Arr_P_Array;
                                BackProject : Cv_Arr_P;
                                Hist        : Cv_Histogram_P) renames CvCalcArrBackProject;

   --Locates a template within an image by using a histogram comparison.
   procedure CvCalcArrBackProjectPatch (Images    : Cv_Arr_P_Array;
                                     Dst       : Cv_Arr_P;
                                     PatchSize : Cv_Size;
                                     Hist      : Cv_Histogram_P;
                                     Method    : Hist_Compare_Method;
                                     Factor    : Float);

   --Locates a template within an image by using a histogram comparison.
   procedure CvCalcBackProjectPatch (Images    : Cv_Arr_P_Array;
                                     Dst       : Cv_Arr_P;
                                     PatchSize : Cv_Size;
                                     Hist      : Cv_Histogram_P;
                                     Method    : Hist_Compare_Method;
                                     Factor    : Float) renames CvCalcArrBackProjectPatch;

   --Divides one histogram by another.
   procedure CvCalcProbDensity (Hist1   : Cv_Histogram_P;
                                Hist2   : Cv_Histogram_P;
                                DstHist : Cv_Histogram_P;
                                Scale   : Long_Float := 255.0);

   -- equalizes histogram of 8-bit single-channel image
   procedure CvEqualizeHist (Src : Cv_Arr_P;
                             Dst : Cv_Arr_P);

   -- Calculates the distance to the closest zero pixel for all non-zero pixels of the source image.
   procedure CvDistTransform (Src          : Cv_Arr_P;
                              Dst          : Cv_Arr_P;
                              DistanceType : Integer := CV_DIST_L2;
                              MaskSize     : Integer := 3;
                              Mask         : access Float := null;
                              Labels       : Cv_Arr_P := null);

   -- Applies a fixed-level threshold to array elements.
   procedure CvThreshold (Src : Cv_Arr_P;
                          Dst : Cv_Arr_P;
                          Threshold : Long_Float;
                          MaxValue  : Long_Float;
                          ThresholdType : Threshold_Type);

   -- Applies adaptive threshold to grayscale image.
   -- The two parameters for methods CV_ADAPTIVE_THRESH_MEAN_C and
   -- CV_ADAPTIVE_THRESH_GAUSSIAN_C are:
   -- neighborhood size (3, 5, 7 etc.),
   -- and a constant subtracted from mean (...,-3,-2,-1,0,1,2,3,...)
   procedure CvAdaptiveThreshold (Src             : Cv_Arr_P;
                                  Dst             : Cv_Arr_P;
                                  MaxValue        : Long_Float;
                                  AdaptiveMethod  : Adaptive_Method := CV_ADAPTIVE_THRESH_MEAN_C;
                                  ThresholdType   : Threshold_Type := CV_THRESH_BINARY;
                                  BlockSize       : Integer := 3;
                                  Param1          : Long_Float := 5.0);

   -- Fills the connected component until the color difference gets large enough
   procedure CvFloodFill (Image : Cv_Arr_P;
                          SeedPoint : Cv_Point;
                          NewValue  : Cv_Scalar;
                          LoDiff    : Cv_Scalar := CvScalarAll (0.0);
                          UpDiff    : Cv_Scalar := CvScalarAll (0.0);
                          Comp      : Cv_Connected_Comp_P := null;
                          Flags     : Integer := 4;
                          Mask      : Cv_Arr_P := null);

   -----------------------------------------------------------------------------
   -- Feature detection
   -----------------------------------------------------------------------------
   -- Runs canny edge detector
   procedure CvCanny ( Image : Cv_Arr_P;
                      Edges : Cv_Arr_P;
                      Threshold1 : Long_Float;
                      Threshold2 : Long_Float;
                      Aperture_Size : Integer := 3);

   -- Calculates constraint image for corner detection
   -- Dx^2 * Dyy + Dxx * Dy^2 - 2 * Dx * Dy * Dxy.
   -- Applying threshold to the result gives coordinates of corners
   procedure CvPreCornerDetect ( Image : Cv_Arr_P;
                                Corners : Cv_Arr_P;
                                ApertureSize : Integer := 3);

   -- Calculates eigenvalues and eigenvectors of image blocks for corner detection.
   procedure CvCornerEigenValsAndVecs (Image : Cv_Arr_P;
                                       Eigenvv : Cv_Arr_P;
                                       BlockSize : Integer;
                                       Aperture_Size : Integer := 3);

   -- Calculates the minimal eigenvalue of gradient matrices for corner detection.
   procedure CvCornerMinEigenVal(Image : Cv_Arr_P;
                                 Eigenval : Cv_Arr_P;
                                 BlockSize : Integer;
                                 Aperture_Size : Integer := 3);

   --Harris edge detector.
   procedure CvCornerHarris (Image         : Cv_Arr_P;
                             Harris_Dst    : Cv_Arr_P;
                             BlockSize     : Integer;
                             Aperture_Size : Integer := 3;
                             K             : Long_Float := 0.04);

   --Refines the corner locations.
   procedure CvFindCornerSubPix (Image : Cv_Arr_P;
                                 Corners : Cv_Point_2D_32F; -- fixa
                                 Count   : Integer;
                                 Win     : Cv_Size;
                                 ZeroZone : Cv_Size;
                                 Criteria : Cv_Term_Criteria);

   -- Determines strong corners on an image.
   procedure CvGoodFeaturesToTrack (Image        : Cv_Arr_P;
                                    EigImage     : Cv_Arr_P;
                                    TempImage    : Cv_Arr_P;
                                    Corners      : Cv_Point_2D_32F_Array;
                                    CornerCount  : access Integer;
                                    QualityLevel : Long_Float;
                                    MinDistance  : Long_Float;
                                    Mask         : Cv_Arr_P := null;
                                    BlockSize    : Integer := 3;
                                    UseHarris    : Integer := 0;
                                    K            : Long_Float := 0.04);

   --Finds lines in a binary image using a Hough transform.
   function CvHoughLines2 (Image : Cv_Arr_P;
                           Storage : Cv_Void_P;
                           Method  : Integer;
                           Rho     : Long_Float;
                           Theta   : Long_Float;
                           Threshold : Integer;
                           Param1    : Long_Float := 0.0;
                           Param2    : Long_Float := 0.0) return Cv_Seq_P;

   -- Finds circles in the image
   function CvHoughCircles (Image : Cv_Arr_P;
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
   procedure CvFitLine (Points   : Cv_Arr_P;
                        DistType : Integer;
                        Param    : Long_Float;
                        Reps     : Long_Float;
                        Aepes    : Long_Float;
                        Line     : Cv_32F_Array);

   -- Constructs kd-tree from set of feature descriptors
   function CvCreateKDTree (Desc : Cv_Mat_P)
                            return Cv_Feature_Tree_P;

   -- Constructs spill-tree from set of feature descriptors
   function CvCreateSpillTree (Raw_Data : Cv_Mat_P;
                               Naive    : Integer := 50;
                               Rho      : Long_Float := 0.7;
                               Tau      : Long_Float := 0.1)
                               return Cv_Feature_Tree_P;

   -- Release feature tree
   procedure CvReleaseFeatureTree (Tr : Cv_Feature_Tree_P);

   -- Searches feature tree for k nearest neighbors of given reference points,
   -- searching (in case of kd-tree/bbf) at most emax leaves.
   procedure CvFindFeatures (Tr           : Cv_Feature_Tree_P;
                             Query_Points : Cv_Mat_P;
                             Indices      : Cv_Mat_P;
                             Dist         : Cv_Mat_P;
                             K            : Integer;
                             Emax         : Integer := 20);

   -- Search feature tree for all points that are inlier to given rect region.
   -- Only implemented for kd trees
   function CvFindFeaturesBoxed (Tr          : Cv_Feature_Tree_P;
                                 Bounds_Min  : Cv_Mat_P;
                                 Bounds_Max  : Cv_Mat_P;
                                 Out_Indices : Cv_Mat_P)
                                 return Integer;

   -- Construct a Locality Sensitive Hash (LSH) table, for indexing d-dimensional vectors of
   -- given type. Vectors will be hashed L times with k-dimensional p-stable (p=2) functions.
   function CvCreateLSH (Ops     : Cv_LSH_Operations_P;
                         D       : Integer;
                         L       : Integer := 10;
                         K       : Integer := 10;
                         Table_T : Integer := Cv_Maketype (Cv_64f, 1);
                         R       : Long_Float := 4.0;
                         Seed    : Interfaces.Integer_64 := -1)
                         return Cv_LSH_P;

   -- Construct in-memory LSH table, with n bins.
   function CvCreateMemoryLSH (D       : Integer;
                               N       : Integer;
                               L       : Integer := 10;
                               K       : Integer := 10;
                               Table_T : Integer := Cv_Maketype (Cv_64f, 1);
                               R       : Long_Float := 4.0;
                               Seed    : Integer_64 := -1)
                               return Cv_LSH_P;

   -- Free the given LSH structure.
   procedure CvReleaseLSH (Lsh : access Cv_LSH_P);

   -- Return the number of vectors in the LSH.
   function LSHSize (Lsh : Cv_LSH_P)
                     return Integer;

   -- Add vectors to the LSH structure, optionally returning indices.
   procedure CvLSHAdd (Lsh     : Cv_LSH_P;
                       Data    : Cv_Mat_P;
                       Indices : Cv_Mat_P := null);

   -- Remove vectors from LSH, as addressed by given indices.
   procedure CvLSHRemove (Lsh     : Cv_LSH_P;
                          Indices : Cv_Mat_P);

   -- Query the LSH n times for at most k nearest points; data is n x d,
   -- indices and dist are n x k. At most emax stored points will be accessed.
   procedure CvLSHQuery (Lsh          : Cv_LSH_P;
                         Query_Points : Cv_Mat_P;
                         Indices      : Cv_Mat_P;
                         Dist         : Cv_Mat_P;
                         K            : Integer;
                         Emax         : Integer);
private

   pragma Import (C, CvAcc, "cvAcc");
   pragma Import (C, CvSquareAcc, "cvSquareAcc");
   pragma Import (C, CvMultiplyAcc, "cvMultiplyAcc");
   pragma Import (C, CvRunningAvg, "cvRunningAvg");

   pragma Import (C, CvCopyMakeBorder, "cvCopyMakeBorder");
   pragma Import (C, CvSmooth, "cvSmooth");
   pragma Import (C, CvFilter2D, "cvFilter2D");
   pragma Import (C, CvIntegral, "cvIntegral");
   pragma Import (C, CvPyrDown, "cvPyrDown");
   pragma Import (C, CvPyrUp, "cvPyrUp");
   pragma Import (C, CvCreatePyramid, "cvCreatePyramid");
   pragma Import (C, CvReleasePyramid, "cvReleasePyramid");
   pragma Import (C, CvPyrMeanShiftFiltering, "cvPyrMeanShiftFiltering");
   pragma Import (C, CvWatershed, "cvWatershed");
   pragma Import (C, CvInpaint, "cvInpaint");
   pragma Import (C, CvSobel, "cvSobel");
   pragma Import (C, CvLaplace, "cvLaplace");
   pragma Import (C, CvCvtColor, "cvCvtColor");
   pragma Import (C, CvResize, "cvResize");
   pragma Import (C, CvWarpAffine, "cvWarpAffine");
   pragma Import (C, CvGetAffineTransform, "cvGetAffineTransform");
   pragma Import (C, Cv2DRotationMatrix, "cv2DRotationMatrix" );
   pragma Import (C, CvWarpPerspective, "cvWarpPerspective");
   pragma Import (C, CvGetPerspectiveTransform, "cvGetPerspectiveTransform");
   pragma Import (C, CvRemap, "cvRemap");
   pragma Import (C, CvConvertMaps, "cvConvertMaps");
   pragma Import (C, CvLogPolar, "cvLogPolar");
   pragma Import (C, CvLinearPolar, "cvLinearPolar");
   pragma Import (C, CvUndistort2, "cvUndistort2");
   pragma Import (C, CvInitUndistortMap, "cvInitUndistortMap");
   pragma Import (C, CvInitUndistortRectifyMap, "cvInitUndistortRectifyMap");
   pragma Import (C, CvUndistortPoints, "cvUndistortPoints");
   pragma Import (C, CvCreateStructuringElementEx, "cvCreateStructuringElementEx");
   pragma Import (C, CvReleaseStructuringElement, "cvReleaseStructuringElement");
   pragma Import (C, CvErode, "cvErode");
   pragma Import (C, CvDilate, "cvDilate");
   pragma Import (C, CvMorphologyEx, "cvMorphologyEx");
   pragma Import (C, CvMoments, "cvMoments");
   pragma Import (C, CvGetSpatialMoment, "cvGetSpatialMoment");
   pragma Import (C, CvGetCentralMoment, "cvGetCentralMoment");
   pragma Import (C, CvGetNormalizedCentralMoment, "cvGetNormalizedCentralMoment");
   pragma Import (C, CvGetHuMoments, "cvGetHuMoments");

   pragma Import (C, CvGetRectSubPix, "cvGetRectSubPix");
   pragma Import (C, CvSampleLine, "cvSampleLine");
   pragma Import (C, CvGetQuadrangleSubPix, "cvGetQuadrangleSubPix");
   pragma Import (C, CvMatchTemplate, "cvMatchTemplate");
   pragma Import (C, CvCalcEMD2, "cvCalcEMD2");

   pragma Import (C, CvFindContours, "cvFindContours");
   pragma Import (C, CvStartFindContours, "cvStartFindContours");
   pragma Import (C, CvFindNextContour, "cvFindNextContour");
   pragma Import (C, CvSubstituteContour, "cvSubstituteContour");
   pragma Import (C, CvEndFindContours, "cvEndFindContours");
   pragma Import (C, CvApproxChains, "cvApproxChains");
   pragma Import (C, CvStartReadChainPoints, "cvStartReadChainPoints");
   pragma Import (C, CvReadChainPoint, "cvReadChainPoint");

   pragma Import (C, CvInitSubdivDelaunay2D, "cvInitSubdivDelaunay2D");
   pragma Import (C, CvCreateSubdiv2D, "cvCreateSubdiv2D");

   pragma Import (C, CvCreateSubdivDelaunay2D, "cvCreateSubdivDelaunay2D");
   pragma Import (C, CvSubdivDelaunay2DInsert, "cvSubdivDelaunay2DInsert");
   pragma Import (C, CvSubdiv2DLocate, "cvSubdiv2DLocate");
   pragma Import (C, CvCalcSubdivVoronoi2D, "cvCalcSubdivVoronoi2D");
   pragma Import (C, CvClearSubdivVoronoi2D, "cvClearSubdivVoronoi2D");
   pragma Import (C, CvFindNearestPoint2D, "cvFindNearestPoint2D");
   pragma Import (C, CvSubdiv2DNextEdge, "cvSubdiv2DNextEdge");
   pragma Import (C, CvSubdiv2DRotateEdge, "cvSubdiv2DRotateEdge");
   pragma Import (C, CvSubdiv2DSymEdge, "cvSubdiv2DSymEdge");
   pragma Import (C, CvSubdiv2DGetEdge, "cvSubdiv2DGetEdge");
   pragma Import (C, CvSubdiv2DEdgeOrg, "cvSubdiv2DEdgeOrg");
   pragma Import (C, CvSubdiv2DEdgeDst, "cvSubdiv2DEdgeDst");
   pragma Import (C, CvTriangleArea, "cvTriangleArea");

   pragma Import (C, CvApproxPoly, "cvApproxPoly");
   pragma Import (C, CvArcLength, "cvArcLength");
   pragma Import (C, CvBoundingRect, "cvBoundingRect");
   pragma Import (C, CvContourArea, "cvContourArea");
   pragma Import (C, CvMinAreaRect2, "cvMinAreaRect2");
   pragma Import (C, CvMinEnclosingCircle, "cvMinEnclosingCircle");
   pragma Import (C, CvMatchShapes, "cvMatchShapes");
   pragma Import (C, CvConvexHull2, "cvConvexHull2");
   pragma Import (C, CvCheckContourConvexity, "cvCheckContourConvexity");
   pragma Import (C, CvConvexityDefects, "cvConvexityDefects");
   pragma Import (C, CvFitEllipse2, "cvFitEllipse2");
   pragma Import (C, CvMaxRect, "cvMaxRect");
   pragma Import (C, CvBoxPoints, "cvBoxPoints");
   pragma Import (C, CvPointSeqFromMat, "cvPointSeqFromMat");
   pragma Import (C, CvPointPolygonTest, "cvPointPolygonTest");

   pragma Import (C, CvCreateHist, "cvCreateHist");
   pragma Import (C, CvSetHistBinRanges, "cvSetHistBinRanges");
   pragma Import (C, CvMakeHistHeaderForArray, "cvMakeHistHeaderForArray");
   pragma Import (C, CvReleaseHist, "cvReleaseHist");
   pragma Import (C, CvClearHist, "cvClearHist");
   pragma Import (C, CvGetMinMaxHistValue, "cvGetMinMaxHistValue");
   pragma Import (C, CvNormalizeHist, "cvNormalizeHist");
   pragma Import (C, CvThreshHist, "cvThreshHist");
   pragma Import (C, CvCompareHist, "cvCompareHist");
   pragma Import (C, CvCopyHist, "cvCopyHist");
   pragma Import (C, CvCalcBayesianProb, "cvCalcBayesianProb");
   pragma Import (C, CvCalcArrHist, "cvCalcArrHist");
   pragma Import (C, CvCalcHist, "cvCalcArrHist");
   pragma Import (C, CvCalcArrBackProject, "cvCalcArrBackProject");
   pragma Import (C, CvCalcArrBackProjectPatch, "cvCalcArrBackProjectPatch");
   pragma Import (C, CvCalcProbDensity, "cvCalcProbDensity");
   pragma Import (C, CvEqualizeHist, "cvEqualizeHist");
   pragma Import (C, CvDistTransform, "cvDistTransform");
   pragma Import (C, CvThreshold, "cvThreshold");
   pragma Import (C, CvAdaptiveThreshold, "cvAdaptiveThreshold");
   pragma Import (C, CvFloodFill, "cvFloodFill");

   pragma Import (C, CvCanny, "cvCanny");
   pragma Import (C, CvPreCornerDetect, "cvPreCornerDetect");
   pragma Import (C, CvCornerEigenValsAndVecs, "cvCornerEigenValsAndVecs");
   pragma Import (C, CvCornerMinEigenVal, "cvCornerMinEigenVal");
   pragma Import (C, CvCornerHarris, "cvCornerHarris");
   pragma Import (C, CvFindCornerSubPix, "cvFindCornerSubPix");
   pragma Import (C, CvGoodFeaturesToTrack, "cvGoodFeaturesToTrack");
   pragma Import (C, CvHoughLines2, "cvHoughLines2");
   pragma Import (C, CvHoughCircles, "cvHoughCircles");
   pragma Import (C, CvFitLine, "cvFitLine");
   pragma Import (C, CvCreateKDTree, "cvCreateKDTree");
   pragma Import (C, CvCreateSpillTree, "cvCreateSpillTree");
   pragma Import (C, CvReleaseFeatureTree, "cvReleaseFeatureTree");
   pragma Import (C, CvFindFeatures, "cvFindFeatures");
   pragma Import (C, CvFindFeaturesBoxed, "cvFindFeaturesBoxed");
   pragma Import (C, CvCreateLSH, "cvCreateLSH");
   pragma Import (C, CvCreateMemoryLSH, "cvCreateMemoryLSH");
   pragma Import (C, CvReleaseLSH, "cvReleaseLSH");
   pragma Import (C, LSHSize, "LSHSize");
   pragma Import (C, CvLSHAdd, "cvLSHAdd");
   pragma Import (C, CvLSHRemove, "cvLSHRemove");
   pragma Import (C, CvLSHQuery, "cvLSHQuery");
end Imgproc.Operations;
