--------------------------------------------------------------------------------
-- Ada bindings for OpenCV 2.1.1 (from SVN 3 October 2010, rev. 3703)
-- Developed as a master thesis project at Mälardalens Högskola
-- OpenCV: http://opencv.willowgarage.com/
-- Ada bindings : http://not_available.nope/
-- License @ ./LICENSE (BSD license)
--------------------------------------------------------------------------------

--Contact-----------------------------------------------------------------------
-- Lars Cederholm, Niklas Pettersson
-- Mälardalens Högskola, http://www.mdh.se/
-- [lcm06001,npn06002]@student.mdh.se
--------------------------------------------------------------------------------

--File-Info---------------------------------------------------------------------
-- legacy.ads - legacy.hpp
-- legacy.hpp includes: imgproc.hpp, imgproc_c.h, features2d.hpp, calib3d.hpp
--------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with Imgproc; use Imgproc;
with Core; use Core;
with Interfaces.C;

package Legacy is

   -- Types that are not imported yet ---------------------------------------------
   type Cv_Matrix_3 is record
      M : Cv_32f_2d_Array (1 .. 3, 1 .. 3);
   end record;

   type Cv_Rand_State is record
      State    : Cv_RNG;
      Disttype : Integer;
      Param    : Cv_Scalar_Array (1 .. 2);
   end record;

   --------------------------------------------------------------------------------

   function CvSegmentImage (Src             : Cv_Arr_P;
                            Dst             : Cv_Arr_P;
                            Canny_Threshold : Long_Float;
                            Ffill_Threshold : Long_Float;
                            Storage         : Cv_Mem_Storage)
                            return Cv_Seq_P;

   -----------------------------------------------------------------------------
   -- Eigen objects ------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Callback is access function (Index     : Integer;
                                        Buffer    : Cv_Void_P;
                                        User_Data : Cv_Void_P)
                                        return Integer;
   pragma Convention (C, Cv_Callback);

   type Cv_Input_Type is (Callback, Data);
   type Cv_Input (Input_Type : Cv_Input_Type := Callback) is record
      case Input_Type is
         when Callback =>
            Callback : Cv_Callback;
         when Data =>
            Data     : Cv_Void_P;
      end case;
   end record;
   pragma Unchecked_Union (Cv_Input);
   pragma Convention (C_Pass_By_Copy, Cv_Input);

   CV_EIGOBJ_NO_CALLBACK     : constant := 0;
   CV_EIGOBJ_INPUT_CALLBACK  : constant := 1;
   CV_EIGOBJ_OUTPUT_CALLBACK : constant := 2;
   CV_EIGOBJ_BOTH_CALLBACK   : constant := 3;

   --     Calculates covariation matrix of a set of arrays
   procedure CvCalcCovarMatrixEx (N_Objects    : Integer;
                                  Input        : Cv_Void_P;
                                  Io_Flags     : Integer;
                                  Io_Buf_Size  : Integer;
                                  Buffer       : Cv_8u_Array;
                                  User_Data    : Cv_Void_P;
                                  Avg          : Ipl_Image_P;
                                  Covar_Matrix : Cv_32f_Array);

   --     Calculates eigen values and vectors of covariation matrix of a set of
   --     arrays
   procedure CvCalcEigenObjects (N_Objects   : Integer;
                                 Input       : Cv_Void_P;
                                 Output      : Cv_Void_P;
                                 Io_Flags    : Integer;
                                 Op_Buf_Size : Integer;
                                 User_Data   : Cv_Void_P;
                                 Calc_Limit  : Cv_Term_Criteria_P;
                                 Avg         : Ipl_Image_P;
                                 Eig_Vals    : Cv_32f_Array);
   --     Calculates dot product (obj - avg) * eigObj (i.e. projects image to
   --     eigen vector)
   function CvCalcDecompCoeff (Obj        : Ipl_Image_P;
                               N_Eig_Objs : Integer;
                               Avg        : Ipl_Image_P)
                               return Long_Float;

   --     Projects image to eigen space (finds all decomposion coefficients
   procedure CvEigenDecomposite (Obj        : Ipl_Image_P;
                                 N_Eig_Objs : Integer;
                                 Eig_Input  : Cv_Void_P;
                                 Io_Flags   : Integer;
                                 User_Data  : Cv_Void_P;
                                 Avg        : Ipl_Image_P;
                                 Eig_Vals   : Cv_32f_Array);

   --     Projects original objects used to calculate eigen space basis to that
   --     space
   procedure CvEigenProjection (Eig_Input  : Cv_Void_P;
                                N_Eig_Objs : Integer;
                                Io_Flags   : Integer;
                                User_Data  : Cv_Void_P;
                                Coeffs     : Cv_32f_Array;
                                Avg        : Ipl_Image_P;
                                Eig_Vals   : Cv_32f_Array);

   -- 1D/2D HMM ----------------------------------------------------------------
   -----------------------------------------------------------------------------

   -- Cv_Img_Obs_info ----------------------------------------------------------
   type Cv_Img_Obs_Info is record
      Obs_X    : Integer;
      Obs_Y    : Integer;
      Obs_Size : Integer;
      Obs      : Cv_32f_Array_P;
      State    : Cv_32s_Array_P;
      Mix      : Cv_32s_Array_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Img_Obs_Info);
   type Cv_1D_Obs_Info is new Cv_Img_Obs_Info;
   pragma Convention (C_Pass_By_Copy, Cv_1D_Obs_Info);

   type Cv_Img_Obs_Info_P is access all Cv_Img_Obs_Info;
   type Cv_1D_Obs_Info_P is access all Cv_1D_Obs_Info;

   type Cv_Img_Obs_Info_P_Array is array (Integer range <>) of aliased Cv_Img_Obs_Info_P;

   -- Cv_EHMM_State ------------------------------------------------------------
   type Cv_EHMM_State is record
      Num_Mix     : Integer;
      Mu          : Cv_32f_Array_P;
      Inv_Var     : Cv_32f_Array_P;
      Log_Var_Val : Cv_32f_Array_P;
      Weight      : Cv_32f_Array_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_EHMM_State);
   type Cv_EHMM_State_P is access all Cv_EHMM_State;

   -- Cv_EHMM ------------------------------------------------------------------
   type Cv_EHMM;
   type Cv_EHMM_P is access all Cv_EHMM;

   type Cv_EHMM_Union_Type is (State, Ehmm);
   type Cv_EHMM_Union (Union_Type : Cv_EHMM_Union_Type := State) is record
      case Union_Type is
         when State =>
            State : Cv_EHMM_State_P;
         when Ehmm =>
            Ehmm  : Cv_EHMM_P;
      end case;
   end record;
   pragma Unchecked_Union (Cv_EHMM_Union);
   pragma Convention (C_Pass_By_Copy, Cv_EHMM_Union);

   type Cv_EHMM is record
      Level      : Integer;
      Num_States : Integer;
      Trans_P    : Cv_32f_Array_P;    -- float *
      Obs_Prob   : Cv_32f_Pointer_Array_P; -- float **
      U          : Cv_EHMM_Union;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_EHMM);

   -- Embedded HMMs ------------------------------------------------------------
   --     Creates 2D HMM
   function CvCreate2DHMM (State_Number : Cv_32s_Array;
                           Num_Mix      : Cv_32s_Array;
                           Obs_Size     : Integer)
                           return Cv_EHMM_P;

   --     Releases HMM
   procedure CvRelease2DHMM (Hmm : access Cv_EHMM_P);

   procedure CV_COUNT_OBS (Roi       : Cv_Size;
                           Win       : Cv_Size;
                           Delta_Obs : Cv_Size;
                           Num_Obs   : out Cv_Size);

   --     Creates storage for observation vectors
   function CvCreateObsInfo (Num_Obs  : Cv_Size;
                             Obs_Size : Integer)
                             return Cv_Img_Obs_Info_P;

   --     Releases storage for observation vectors
   procedure CvReleaseObsInfo (Obs_Info : access Cv_Img_Obs_Info_P);

   --     The function takes an image on input and and returns the sequnce of
   --     Observations to be used with an embedded HMM; Each observation is
   --     top-left block of DCT coefficient matrix
   procedure CvImgToObs_DCT (Arr        : Cv_Arr_P;
                             Obs        : Cv_32f_Array;
                             Dct_Size   : Cv_Size;
                             Obs_Size   : Cv_Size;
                             Delta_Size : Cv_Size);

   --     Uniformly segments all observation vectors extracted from image
   procedure CvUniformImgSegm (Obs_Info : Cv_Img_Obs_Info_P;
                               Ehmm     : Cv_EHMM_P);

   --     Does mixture segmentation of the states of embedded HMM
   procedure CvInitMixSegm (Obs_Info_Array : Cv_Img_Obs_Info_P_Array;
                            Num_Img        : Integer;
                            Hmm            : Cv_EHMM_P);

   --     Function calculates means, variances, weights of every Gaussian
   --     Mixture of every low-level state of embedded HMM
   procedure CvEstimateHMMStateParams (Obs_Info_Array : Cv_Img_Obs_Info_P_Array;
                                       Num_Img        : Integer;
                                       Hmm            : Cv_EHMM_P);

   --     Function computes transition probability matrices of embedded HMM
   --     given observations segmentation
   procedure CvEstimateTransProb (Obs_Info_Array : Cv_Img_Obs_Info_P_Array;
                                  Num_Img        : Integer;
                                  Hmm            : Cv_EHMM_P);

   --     Function computes probabilities of appearing observations at any state
   --     (i.e. computes P(obs|state) for every pair(obs,state))
   procedure CvEstimateObsProb (Obs_Info : Cv_Img_Obs_Info_P;
                                Hmm      : Cv_EHMM_P);

   --     Runs Viterbi algorithm for embedded HMM
   function CvEViterbi (Obs_Info : Cv_Img_Obs_Info_P;
                        Hmm      : Cv_EHMM_P)
                        return Float;

   --     Function clusters observation vectors from several images
   --     given observations segmentation.
   --     Euclidean distance used for clustering vectors.
   --     Centers of clusters are given means of every mixture
   procedure CvMixSegmL2 (Obs_Info_Array : Cv_Img_Obs_Info_P_Array;
                          Num_Img        : Integer;
                          Hmm            : Cv_EHMM_P);

   -- A few function from old stereo gesture recognition demonstrations---------
   -----------------------------------------------------------------------------
   --     Creates hand mask image given several points on the hand
   procedure CvCreateHandMask (Hand_Points : Cv_Seq_P;
                               Img_Mask    : Ipl_Image_P;
                               Roi         : Cv_Rect_P);

   --     Finds hand region in range image data
   procedure CvFindHandRegion (Points  : Cv_Point_3d_32f_Array;
                               Count   : Integer;
                               Indexs  : Cv_Seq_P;
                               Line    : Cv_32f_Array;
                               Size    : Cv_Size_2d_32f;
                               Flag    : Integer;
                               Center  : Cv_Point_3d_32f_Array;
                               Storage : Cv_Mem_Storage_P;
                               Numbers : access Cv_Seq_P);

   --     Finds hand region in range image data (advanced version)
   procedure CvFindHandRegionA (Points  : Cv_Point_3d_32f_Array;
                                Count   : Integer;
                                Indexs  : Cv_Seq_P;
                                Line    : Cv_32f_Array;
                                Size    : Cv_Size_2d_32f;
                                Jc      : Integer;
                                Center  : Cv_Point_3d_32f_Array;
                                Storage : Cv_Mem_Storage_P;
                                Numbers : access Cv_Seq_P);

   --     Calculates the cooficients of the homography matrix
   procedure CvCalcImageHomography (Line       : Cv_32f_Array;
                                    Center     : Cv_Point_3d_32f_Array;
                                    Intrinsic  : Cv_32f_Array;
                                    Homography : Cv_32f_Array);

   -- Additional operations on Subdivisions ------------------------------------
   -----------------------------------------------------------------------------
   --     paints voronoi diagram: just demo function
   procedure IcvDrawMosaic (Subdiv : Cv_Subdiv_2d_P;
                            Src    : Ipl_Image_P;
                            Dst    : Ipl_Image_P);

   --     checks planar subdivision for correctness. It is not an absolute
   --     check, but it verifies some relations between quad-edges
   function IcvSubdiv2DCheck (Subdiv : Cv_Subdiv_2d_P)
                              return Integer;

   --     returns squared distance between two 2D points with floating-point
   --     coordinates.
   function IcvSqDist2D32f (Pt1 : Cv_Point_3d_32f;
                            Pt2 : Cv_Point_3d_32f)
                            return Long_Float;

   -- More operations on sequences ---------------------------------------------
   -----------------------------------------------------------------------------
   function CV_CURRENT_INT (Reader : Cv_Seq_Reader_P)
                            return Integer;

   function CV_PREV_INT (Reader : Cv_Seq_Reader_P)
                         return Integer;

   type Cv_Graph_Weighted_Vtx is record
      Flags  : Integer;
      First  : Cv_Graph_Edge_P;
      Weight : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Graph_Weighted_Vtx);

   type Cv_Graph_Weighted_Edge is record
      Flags  : Integer;
      Weight : Float;
      Next   : Cv_Graph_Edge_P_Array;
      Vtx    : Cv_Graph_Vtx_P_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Graph_Weighted_Edge);

   type Cv_Graph_Weight_Type is (CV_NOT_WEIGHTED,
                                 CV_WEIGHTED_VTX,
                                 CV_WEIGTHED_EDGE,
                                 CV_WEIGHTED_ALL);

   for Cv_Graph_Weight_Type use (CV_NOT_WEIGHTED  => 0,
                                 CV_WEIGHTED_VTX  => 1,
                                 CV_WEIGTHED_EDGE => 2,
                                 CV_WEIGHTED_ALL  => 3);

   --     Calculates histogram of a contour
   procedure CvCalcPGH (Contour : Cv_Seq_P;
                        Hist    : Cv_Histogram_P);

   CV_DOMINANT_IPAN : constant := 1;

   --     Finds high-curvature points of the contour
   function CvFindDominantPoints (Contour    : Cv_Seq_P;
                                  Storage    : CV_Mem_Storage_P;
                                  Method     : Integer := CV_DOMINANT_IPAN;
                                  Parameter1 : Long_Float := 0.0;
                                  Parameter2 : Long_Float := 0.0;
                                  Parameter3 : Long_Float := 0.0;
                                  Parameter4 : Long_Float := 0.0)
                                  return Cv_Seq_P;

   -- Stereo Correspondence ----------------------------------------------------
   type Cv_Clique_Finder is record
      Graph          : Cv_Graph_P;
      Adj_Mat        : Cv_32s_Pointer_Array_P;
      N              : Integer; -- Graph size

      -- stacks, counters etc
      K              : Integer; -- Stack size
      Current_Comp   : Cv_32s_Pointer;
      All_Cliques    : Cv_32s_Pointer_Array_P;

      Ne             : Cv_32s_Pointer;
      Ce             : Cv_32s_Pointer;
      Fixp           : Cv_32s_Pointer;
      Nod            : Cv_32s_Pointer;
      S              : Cv_32s_Pointer;

      Status         : Integer;
      Best_Score     : Integer;
      Weighted       : Integer;
      Weighted_Edges : Integer;
      Best_Weight    : Float;
      Edge_Weights   : Cv_32f_Pointer;
      Vertex_Weights : Cv_32f_Pointer;

      Cur_Weight     : Cv_32f_Pointer;
      Cand_Weight    : Cv_32f_Pointer;
   end record;

   CLIQUE_TIME_OFF : constant := 2;
   CLIQUE_FOUND    : constant := 1;
   CLIQUE_END      : constant := 0;

   CV_UNDEF_SC_PARAM    : constant := 12345;
   CV_UNDEF_SC_PARAM_F  : constant Float := 12345.0;
   CV_UNDEF_SC_PARAM_LF : constant Long_Float := 12345.0;

   CV_IDP_BIRCHFIELD_PARAM1 : constant := 25;
   CV_IDP_BIRCHFIELD_PARAM2 : constant := 5;
   CV_IDP_BIRCHFIELD_PARAM3 : constant := 12;
   CV_IDP_BIRCHFIELD_PARAM4 : constant := 15;
   CV_IDP_BIRCHFIELD_PARAM5 : constant := 25;

   CV_DISPARITY_BIRCHFIELD : constant := 0;

   --     find stereo correspondence on stereo-pair
   procedure CvFindStereoCorrespondence (Left_Image    : Cv_Arr_P;
                                         Right_Image   : Cv_Arr_P;
                                         Mode          : Integer;
                                         Disp_Image    : Cv_Arr_P;
                                         Max_Disparity : Integer;
                                         Param1        : Long_Float := CV_UNDEF_SC_PARAM_LF;
                                         Param2        : Long_Float := CV_UNDEF_SC_PARAM_LF;
                                         Param3        : Long_Float := CV_UNDEF_SC_PARAM_LF;
                                         Param4        : Long_Float := CV_UNDEF_SC_PARAM_LF;
                                         Param5        : Long_Float := CV_UNDEF_SC_PARAM_LF);


   pragma Import (C, CvFindStereoCorrespondence, "cvFindStereoCorrespondence");

   -- Epiline Functions --------------------------------------------------------
   type Cv_Stereo_Line_Coeff is record
      Xcoef    : Long_Float;
      Xcoef_A  : Long_Float;
      Xcoef_B  : Long_Float;
      Xcoef_AB : Long_Float;

      Ycoef    : Long_Float;
      Ycoef_A  : Long_Float;
      Ycoef_B  : Long_Float;
      Ycoef_AB : Long_Float;

      Zcoef    : Long_Float;
      Zcoef_A  : Long_Float;
      Zcoef_B  : Long_Float;
      Zcoef_AB : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Stereo_Line_Coeff);
   type Cv_Stereo_Line_Coeff_P is access all Cv_Stereo_Line_Coeff;



   type Cv_Camera is record
      Img_Size   : Cv_32f_Array (1 .. 2); -- size of the camera view, used during calibration
      Matrix     : Cv_32f_Array (1 .. 9); -- intinsic camera parameters:  [ fx 0 cx; 0 fy cy; 0 0 1 ]
      Distortion : Cv_32f_Array (1 .. 4); -- distortion coefficients - two coefficients for radial distortion
      -- and another two for tangential: [ k1 k2 p1 p2 ]

      Rot_Matr   : Cv_32f_Array (1 .. 9); -- rotation matrix and transition vector relatively
      Trans_Vect : Cv_32f_Array (1 .. 3); -- to some reference point in the space
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Camera);
   type Cv_Camera_P is access all Cv_Camera;

   type Cv_Camera_Array is array (Integer range <>) of aliased Cv_Camera;
   type Cv_Camera_P_Array is array (Integer range <>) of aliased Cv_Camera_P;
   subtype Cv_Camera_P_Array_2 is Cv_Camera_P_Array (1 .. 2);

   subtype Cv_Point_3d_32f_Array_2 is Cv_Point_3d_32f_Array (1 .. 2);

   type Cv_Point_2d_32f_Array_AxB is array (Integer range <>, Integer range <>) of aliased Cv_Point_2d_32f;
   subtype Cv_Point_2d_32f_Array_2x4 is Cv_Point_2d_32f_Array_AxB (1 .. 2, 1 .. 4);

   type Cv_64f_Array_AxBxC is array (Integer range <>, Integer range <>, Integer range <>) of aliased Long_Float;
   subtype Cv_64f_Array_2x3x3 is Cv_64f_Array_AxBxC (1 .. 2, 1 .. 3, 1 .. 3);


   subtype Cv_64f_Array_3x3 is Cv_64f_2d_Array (1 .. 3, 1 .. 3);
   subtype Cv_64f_Array_4x2 is Cv_64f_2d_Array (1 .. 4, 1 .. 2);

   type Cv_Stereo_Camera is record
      Camera            : Cv_Camera_P_Array_2; -- two individual camera parameters
      Fund_Matr         : Cv_32f_Array (1 .. 9); -- fundamental matrix

      -- New part for stereo
      Epipole           : Cv_Point_3d_32f_Array_2;
      Quad              : Cv_Point_2d_32f_Array_2x4;

      Coeffs            : Cv_64f_Array_2x3x3;
      Border            : Cv_Point_2d_32f_Array_2x4;
      Warpsize          : Cv_Size;
      Line_Coeffs       : Cv_Stereo_Line_Coeff_P;
      Need_Swap_Cameras : Integer;
      Rot_Matrix        : Cv_32f_Array (1 .. 9);
      Trans_Vector      : Cv_32f_Array (1 .. 3);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Stereo_Camera);
   type Cv_Stereo_Camera_P is access all Cv_Stereo_Camera;

   type Cv_Contour_Orientation is record
      Egvals  : Cv_32f_Array (1 .. 2);
      Egvects : Cv_32f_Array (1 .. 4);
      Max     : Float;
      Min     : Float;
      Imax    : Integer;
      Imin    : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Contour_Orientation);
   type Cv_Contour_Orientation_P is access all Cv_Contour_Orientation;

   CV_CAMERA_TO_WARP : constant := 1;
   CV_WARP_TO_CAMERA : constant := 2;

   function IcvConvertWarpCoordinates (Coeffs       : Cv_64f_Array_3x3;
                                       Camera_Point : Cv_Point_2d_32f_P;
                                       Warp_Point   : Cv_Point_2d_32f_P;
                                       Direction    : Integer)
                                       return Integer;

   function IcvGetSymPoint3D (Point_Corner : Cv_Point_3d_64f;
                              Point1       : Cv_Point_3d_64f;
                              Point2       : Cv_Point_3d_64f;
                              Point_Sym2   : Cv_Point_3d_64f_P)
                              return Integer;

   procedure IcvGetPieceLength3D (Point1 : Cv_Point_3d_64f;
                                  Point2 : Cv_Point_3d_64f;
                                  Dist   : access Long_Float);

   function IcvCompute3DPoint (Alpha  : Long_Float;
                               Beta   : Long_Float;
                               Coeffs : Cv_Stereo_Line_Coeff_P;
                               Point  : Cv_Point_3d_64f_P)
                               return Integer;

   function IcvCreateConvertMatrVect (Rot_Matr1        : Cv_64f_Array;
                                      Trans_Vect1      : Cv_64f_Array;
                                      Rot_Matr2        : Cv_64f_Array;
                                      Trans_Vect2      : Cv_64f_Array;
                                      Conv_Rot_Matr    : Cv_64f_Array;
                                      Conv_Transv_Vect : Cv_64f_Array)
                                      return Integer;

   function IcvConvertPointSystem (M2         : Cv_Point_3d_64f;
                                   M1         : Cv_Point_3d_64f_P;
                                   Rot_Matr   : Cv_64f_Array;
                                   Trans_Vect : Cv_64f_Array)
                                   return Integer;

   function IcvComputeCoeffForStereo (Stereo_Camera : Cv_Stereo_Camera_P)
                                      return Integer;

   function IcvGetCrossPieceVector (P1_Start : Cv_Point_2d_32f;
                                    P1_End   : Cv_Point_2d_32f;
                                    V2_Start : Cv_Point_2d_32f;
                                    V2_End   : Cv_Point_2d_32f;
                                    Cross    : Cv_Point_2d_32f_P)
                                    return Integer;

   function IcvGetCrossLineDirect (P1    : Cv_Point_2d_32f;
                                   P2    : Cv_Point_2d_32f;
                                   A     : Float;
                                   B     : Float;
                                   C     : Float;
                                   Cross : Cv_Point_2d_32f_P)
                                   return Integer;

   function IcvDefinePointPosition (Point1 : Cv_Point_2d_32f;
                                    Point2 : Cv_Point_2d_32f;
                                    Point  : Cv_Point_2d_32f)
                                    return Float;

   function IcvStereoCalibration (Num_Images    : Integer;
                                  Nums          : Cv_32s_Array;
                                  Image_Size    : Cv_Size;
                                  Image_Points1 : Cv_Point_2d_32f;
                                  Image_Points2 : Cv_Point_2d_32f;
                                  Object_Points : Cv_Point_3d_32f_P;
                                  Stereoparams  : Cv_Stereo_Camera_P)
                                  return Integer;

   function IcvComputeRestStereoParams (Stereoparams : Cv_Stereo_Camera_P)
                                        return Integer;

   procedure CvComputePerspectiveMap (Coeffs     : Cv_64f_Array_3x3;
                                      Rect_Map_X : Cv_Arr_P;
                                      Rect_Map_Y : Cv_Arr_P);

   function IcvComCoeffForLine (Point1           : Cv_Point_2d_64f;
                                Point2           : Cv_Point_2d_64f;
                                Point3           : Cv_Point_2d_64f;
                                Point4           : Cv_Point_2d_64f;
                                Can_Matr1        : Cv_64f_Array;
                                Rot_Matr1        : Cv_64f_Array;
                                Trans_Vect1      : Cv_64f_Array;
                                Cam_Matr2        : Cv_64f_Array;
                                Rot_Matr2        : Cv_64f_Array;
                                Trans_Vect2      : Cv_64f_Array;
                                Coeffs           : Cv_Stereo_Line_Coeff_P;
                                Need_Swap_Camera : access Integer)
                                return Integer;

   function IcvGetDirectionForPoint (Point    : Cv_Point_2d_64f;
                                     Cam_Matr : Cv_64f_Array;
                                     Direct   : Cv_Point_3d_64f_P)
                                     return Integer;

   function IcvGetCrossLines (Point11   : Cv_Point_3d_64f;
                              Point12   : Cv_Point_3d_64f;
                              Point21   : Cv_Point_3d_64f;
                              Point22   : Cv_Point_3d_64f;
                              Mid_Point : Cv_Point_3d_64f_P)
                              return Integer;

   function IcvComputeStereoLineCoeffs (Point_A    : Cv_Point_3d_64f;
                                        Point_B    : Cv_Point_3d_64f;
                                        Point_Cam1 : Cv_Point_3d_64f;
                                        Gamma      : Long_Float;
                                        Coeffs     : Cv_Stereo_Line_Coeff_P)
                                        return Integer;

   function IcvGetAngleLine (Start_Point : Cv_Point_2d_64f;
                             Image_Size  : Cv_Size;
                             Point1      : Cv_Point_2d_64f_P;
                             Point2      : Cv_Point_2d_64f_P)
                             return Integer;

   procedure IcvGetCoefForPiece (P_Start : Cv_Point_2d_64f;
                                 P_End   : Cv_Point_2d_64f;
                                 A       : access Long_Float;
                                 B       : access Long_Float;
                                 C       : access Long_Float;
                                 Result  : access Integer);

   procedure IcvComputeeInfiniteProject1 (Rot_Matr  : Cv_64f_Array;
                                          Cam_Matr1 : Cv_64f_Array;
                                          Cam_Matr2 : Cv_64f_Array;
                                          Point1    : Cv_Point_2d_32f;
                                          Point2    : Cv_Point_2d_32f_P);

   procedure IcvComputeeInfiniteProject2 (Rot_Matr  : Cv_64f_Array;
                                          Cam_Matr1 : Cv_64f_Array;
                                          Cam_Matr2 : Cv_64f_Array;
                                          Point1    : Cv_Point_2d_32f_P;
                                          Point2    : Cv_Point_2d_32f);

   procedure IcvGetCrossDirectDirect (Direct1 : Cv_64f_Array;
                                      Direct2 : Cv_64f_Array;
                                      Cross   : Cv_Point_2d_64f_P;
                                      Result  : access Integer);

   procedure IcvGetCrossPieceDirect (P_Start : Cv_Point_2d_64f;
                                     P_End   : Cv_Point_2d_64f;
                                     A       : Long_Float;
                                     B       : Long_Float;
                                     C       : Long_Float;
                                     Cross   : Cv_Point_2d_64f_P;
                                     Result  : access Integer);

   procedure IcvGetCrossPiecePiece (P1_Start : Cv_Point_2d_64f;
                                    P1_End   : Cv_Point_2d_64f;
                                    P2_Start : Cv_Point_2d_64f;
                                    P2_End   : Cv_Point_2d_64f;
                                    Cross    : Cv_Point_2d_64f_P;
                                    Result   : access Integer);

   procedure IcvGetPieceLength (Point1 : Cv_Point_2d_64f;
                                Point2 : Cv_Point_2d_64f;
                                Dist   : access Long_Float);

   procedure IcvGetCrossRectDirect (Image_Size  : Cv_Size;
                                    A           : Long_Float;
                                    B           : Long_Float;
                                    C           : Long_Float;
                                    Point_Start : Cv_Point_2d_64f_P;
                                    Point_End   : Cv_Point_2d_64f_P;
                                    Result      : access Integer);

   procedure IcvProjectPointToImage (Point      : Cv_Point_3d_64f;
                                     Cam_Matr   : Cv_64f_Array;
                                     Rot_Matr   : Cv_64f_Array;
                                     Trans_Vect : Cv_64f_Array;
                                     Proj_Point : Cv_Point_2d_64f_P);

   procedure IcvGetQuadsTransform (Image_Size   : Cv_Size;
                                   Cam_Matr1    : Cv_64f_Array;
                                   Rot_Matr1    : Cv_64f_Array;
                                   Trans_Vect1  : Cv_64f_Array;
                                   Cam_Matr2    : Cv_64f_Array;
                                   Rot_Matr2    : Cv_64f_Array;
                                   Trans_Vect2  : Cv_64f_Array;
                                   Warp_Size    : Cv_Size_P;
                                   Quad1        : Cv_64f_Array_4x2;
                                   Quad2        : Cv_64f_Array_4x2;
                                   Fund_Matr    : Cv_64f_Array;
                                   Epipole1     : Cv_Point_3d_64f_P;
                                   Epipole2     : Cv_Point_3d_64f_P);

   procedure IcvGetQuadsTransformStruct (Stereo_Camera : Cv_Stereo_Camera_P);

   procedure IcvComputeStereoParamsForCameras (Stereo_Camera : Cv_Stereo_Camera_P);

   procedure IcvGetCutPiece (Area_Line_Coef1 : Cv_64f_Array;
                             Area_Line_Coef2 : Cv_64f_Array;
                             Epipole         : Cv_Point_2d_64f;
                             Image_Size      : Cv_Size;
                             Point11         : Cv_Point_2d_64f_P;
                             Point12         : Cv_Point_2d_64f_P;
                             Point21         : Cv_Point_2d_64f_P;
                             Point22         : Cv_Point_2d_64f_P;
                             Result          : access Integer);

   procedure IcvGetMiddleAnglePoint (Base_Point : Cv_Point_2d_64f;
                                     Point1     : Cv_Point_2d_64f;
                                     Point2     : Cv_Point_2d_64f;
                                     Mid_Point  : Cv_Point_2d_64f_P);

   procedure IcvGetNormalDirect (Direct      : Cv_64f_Array;
                                 Point       : Cv_Point_2d_64f;
                                 Norm_Direct : CV_64f_Array);

   function IcvGetVect (Base_Point : Cv_Point_2d_64f;
                        Point1     : Cv_Point_2d_64f;
                        Point      : Cv_Point_2d_64f)
                        return Long_Float;

   procedure IcvProjectPointToDirect (Point         : Cv_Point_2d_64f;
                                      Line_Coeff    : Cv_64f_Array;
                                      Project_Point : Cv_Point_2d_64f_P);

   procedure IcvGetDistanceFromPointToDirect (Point     : Cv_Point_2d_64f;
                                              Line_Coef : Cv_64f_Array;
                                              Dist      : access Long_Float);

   function IcvCreateIsometricImage (Src                  : Ipl_Image_P;
                                     Dst                  : Ipl_Image_P;
                                     Desired_Depth        : Integer;
                                     Desired_Num_Channels : Integer)
                                     return Ipl_Image_P;

   procedure CvDeInterlace (Frame      : Cv_Arr_P;
                            Field_Even : Cv_Arr_P;
                            Field_Odd  : Cv_Arr_P);

   -- Contour Tree -------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Contour_Tree is record
   -- CV_TREE_NODE_FIELDS
      Flags        : Integer;
      Header_Size  : Integer;
      H_Prev       : access Cv_Seq;
      H_Next       : access Cv_Seq;
      V_Prev       : access Cv_Seq;
      V_Next       : access Cv_Seq;

      -- CV_SEQUENCE_FIELDS
      Total        : Integer;
      Elem_Size    : Integer;
      Block_Max    : Cv_Void_P;
      Ptr          : Cv_Void_P;
      Delta_Elems  : Integer;
      Storage      : access Cv_Mem_Storage;
      Free_Blocks  : access Cv_Seq_Block;
      First        : access Cv_Seq_Block;

      P1           : Cv_Point;
      P2           : Cv_Point;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Contour_Tree);

   type Cv_Contour_Tree_P is access Cv_Contour_Tree;

   --     Creates a hierarchical representation of a contour.
   function CvCreateContourTree (Contour   : Cv_Seq_P;
                                 Storage   : Cv_Mem_Storage_P;
                                 Threshold : Long_Float) return Cv_Contour_Tree_P;

   --     Reconstruct (completelly or partially) contour a from contour tree
   function CvContourFromContourTree (Tree     : access Cv_Contour_Tree;
                                      Storage  : access Cv_Mem_Storage;
                                      Criteria : Cv_Term_Criteria)
                                      return access Cv_Seq;

   CV_CONTOUR_TREES_MATCH_I1 : constant := 1;

   --     Compares two contours using their tree representations.
   function CvMatchContourTrees (Tree1     : Cv_Contour_Tree_P;
                                 Tree2     : Cv_Contour_Tree_P;
                                 Method    : Integer;
                                 Threshold : Long_Float) return Long_Float;

   -- Contour Morphing ---------------------------------------------------------
   -----------------------------------------------------------------------------

   --     finds correspondence between two contours
   function CvCalcContoursCorrespondence (Contour1 : Cv_Seq_P;
                                          Contour2 : Cv_Seq_P;
                                          Storage  : Cv_Mem_Storage_P)
                                          return Cv_Seq_P;

   --     morphs contours using the pre-calculated correspondence:
   --     alpha=0 ~ contour1, alpha=1 ~ contour2
   function CvMorphContours (Contour1 : Cv_Seq_P;
                             Contour2 : Cv_Seq_P;
                             Corr     : Cv_Seq_P;
                             Alpha    : Long_Float;
                             Storage  : Cv_Mem_Storage_P)
                             return Cv_Seq_P;

   CV_VALUE : constant := 1;
   CV_ARRAY : constant := 2;

   --     Changes the contour position to minimize its energy.
   procedure CvSnakeImage (Image        : Ipl_Image_P;
                           Points       : Cv_Point_Array;
                           Length       : Integer;
                           Alpha        : Cv_32F_Array;
                           Beta         : Cv_32F_Array;
                           Gamma        : Cv_32F_Array;
                           CoeffUsage   : Integer;
                           Win          : Cv_Size;
                           Criteria     : Cv_Term_Criteria;
                           CalcGradient : Integer := 1);

   -- Texture Descriptors ------------------------------------------------------
   -----------------------------------------------------------------------------

   CV_GLCM_OPTIMIZATION_NONE      : constant  := -2;
   CV_GLCM_OPTIMIZATION_LUT       : constant := -1;
   CV_GLCM_OPTIMIZATION_HISTOGRAM : constant := 0;

   CV_GLCMDESC_OPTIMIZATION_ALLOWDOUBLENEST : constant := 10;
   CV_GLCMDESC_OPTIMIZATION_ALLOWTRIPLENEST : constant := 11;
   CV_GLCMDESC_OPTIMIZATION_HISTOGRAM       : constant := 4;

   CV_GLCMDESC_ENTROPY            : constant := 0;
   CV_GLCMDESC_ENERGY             : constant := 1;
   CV_GLCMDESC_HOMOGENITY         : constant := 2;
   CV_GLCMDESC_CONTRAST           : constant := 3;
   CV_GLCMDESC_CLUSTERTENDENCY    : constant := 4;
   CV_GLCMDESC_CLUSTERSHADE       : constant := 5;
   CV_GLCMDESC_CORRELATION        : constant := 6;
   CV_GLCMDESC_CORRELATIONINFO1   : constant := 7;
   CV_GLCMDESC_CORRELATIONINFO2   : constant := 8;
   CV_GLCMDESC_MAXIMUMPROBABILITY : constant := 9;

   CV_GLCM_ALL  : constant := 0;
   CV_GLCM_GLCM : constant := 1;
   CV_GLCM_DESC : constant := 2;

   CV_MAX_NUM_GREY_LEVELS_8U : constant := 256;

   type Cv_GLCM is record
      Matrix_Side_Length           : Integer;
      Num_Matrices                 : Integer;
      Matrices                     : access Cv_64F_Pointer_Array_P; -- double***

      Num_Lookup_Table_Elements    : Integer;
      Forward_Lookup_Table         : Cv_32s_Array (1 .. CV_MAX_NUM_GREY_LEVELS_8U);
      Reverse_Lookup_Table         : Cv_32s_Array (1 .. CV_MAX_NUM_GREY_LEVELS_8U);
      Descriptors                  : Cv_64F_Pointer_Array_P; -- double**
      Num_Descriptors              : Integer;
      Descriptor_Optimization_Type : Integer;
      Optimization_Type            : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_GLCM);
   type Cv_GLCM_P is access all Cv_GLCM;

   function CvCreateGLCM (Src_Image           : Ipl_Image_P;
                          Step_Magnitude      : Integer;
                          Step_Direction      : Cv_32s_Pointer := null;
                          Num_Step_Directions : Integer := 0;
                          Optimization_Type   : Integer := CV_GLCM_OPTIMIZATION_NONE)
                          return Cv_GLCM_P;

   procedure CvReleaseGLCM (GLCM : access Cv_GLCM_P;
                            Flag : Integer := CV_GLCM_ALL);

   procedure CvCreateGLCMDescriptors (Dest_GLCM                    : Cv_GLCM_P;
                                      Descriptor_Optimization_Type : Integer := CV_GLCMDESC_OPTIMIZATION_ALLOWDOUBLENEST);

   function CvGetGLCMDescriptor (GLCM       : Cv_GLCM_P;
                                 Step       : Integer;
                                 Descriptor : Integer)
                                 return Long_Float;

   procedure CvGetGLCMDescriptorStatistics (GLCM               : Cv_GLCM_P;
                                            Descriptor         : Integer;
                                            Average            : access Long_Float;
                                            Standard_Deviation : access Long_Float);

   function CvCreateGLCMImage (GLCM : Cv_GLCM_P;
                               Step : Integer)
                               return Ipl_Image_P;

   -- Face, eyes and mouth tracking --------------------------------------------
   -----------------------------------------------------------------------------

   NUM_FACE_ELEMENTS : constant := 3;

   type Cv_Face_Tracker is null record; -- Locally declared class in .cpp file
   pragma Convention (C_Pass_By_Copy, Cv_Face_Tracker);
   type Cv_Face_Tracker_P is access Cv_Face_Tracker;

   type Cv_Face_Elements is (Cv_Face_Mouth,
                             Cv_Face_Left_Eye,
                             Cv_Face_Right_Eye);
   for Cv_Face_Elements use (Cv_Face_Mouth     => 0,
                             Cv_Face_Left_Eye  => 1,
                             Cv_Face_Right_Eye => 2);

   function CvInitFaceTracker (P_Face_Tracking : Cv_Face_Tracker_P;
                               Img_Gray        : Ipl_Image_P;
                               P_Rects         : Cv_Rect_Array;
                               N_Rects         : Integer)
                               return Cv_Face_Tracker_P;

   function CvTrackFace (P_Face_Tracker  : Cv_Face_Tracker_P;
                         Img_Gray        : Ipl_Image_P;
                         P_Rects         : Cv_Rect_Array;
                         N_Rects         : Integer;
                         Pt_Rotate       : Cv_Point_P;
                         Db_Angle_Rotate : access Long_Float)
                         return Integer;

   procedure CvReleaseFaceTracker (PP_Face_Tracker : access Cv_Face_Tracker_P);

   type Cv_Face_Data is record
      Mouth_Rect     : Cv_Rect;
      Left_Eye_Rect  : Cv_Rect;
      Right_Eye_Rect : Cv_Rect;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Face_Data);

   function CvFindFace (Image   : Ipl_Image_P;
                        Storage : Cv_Mem_Storage_P)
                        return Cv_Seq_P;

   function CvPostBoostingFindFace (Image   : Ipl_Image_P;
                                    Storage : Cv_Mem_Storage_P)
                                    return Cv_Seq_P;

   -- 3D Tracker ---------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Bool is new Interfaces.C.Unsigned_Char;

   type Cv_3d_Tracker_2d_Tracked_Object is record
      Id : Integer;
      P  : Cv_Point_2d_32f;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_3d_Tracker_2d_Tracked_Object);
   type Cv_3d_Tracker_2d_Tracked_Object_P is access Cv_3d_Tracker_2d_Tracked_Object;
   type Cv_3d_Tracker_2d_Tracked_Object_Array is array (Integer range <>) of aliased Cv_3d_Tracker_2d_Tracked_Object;

   function Cv3dTracker2dTrackedObject (Id : Integer;
                                        P  : Cv_Point_2d_32f)
                                        return Cv_3d_Tracker_2d_Tracked_Object;

   type Cv_3d_Tracker_Tracked_Object is record
      Id : Integer;
      P  : Cv_Point_3d_32f;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_3d_Tracker_Tracked_Object);
   type Cv_3d_Tracker_Tracked_Object_P is access Cv_3d_Tracker_Tracked_Object;
   type Cv_3d_Tracker_Tracked_Object_Array is array (Integer range <>) of aliased Cv_3d_Tracker_Tracked_Object;

   function Cv3dTrackerTrackedObject (Id : Integer;
                                      P  : Cv_Point_3d_32f)
                                      return Cv_3d_Tracker_Tracked_Object;

   type Cv_3d_Tracker_Camera_Info is record
      Valid           : Cv_Bool;
      Mat             : Cv_32f_2d_Array (1 .. 4, 1 .. 4);
      Principal_Point : Cv_Point_2d_32f;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_3d_Tracker_Camera_Info);
   type Cv_3d_Tracker_Camera_Info_P is access Cv_3d_Tracker_Camera_Info;
   type Cv_3d_Tracker_Camera_Info_Array is array (Integer range <>) of aliased Cv_3d_Tracker_Camera_Info;

   type Cv_3d_Tracker_Camera_Intrinsics is record
      Principal_Point : Cv_Point_2d_32f;
      Focal_Length    : Cv_32f_Array (1 .. 2);
      Distortion      : Cv_32f_Array (1 .. 4);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_3d_Tracker_Camera_Intrinsics);
   type Cv_3d_Tracker_Camera_Intrinsics_P is access Cv_3d_Tracker_Camera_Intrinsics;
   type Cv_3d_Tracker_Camera_Intrinsics_Array is array (Integer range <>) of aliased Cv_3d_Tracker_Camera_Intrinsics;

   function Cv3dTrackerCalibrateCameras (Num_Cameras       : Integer;
                                         Camera_Intrinsics : Cv_3d_Tracker_Camera_Intrinsics_Array; -- size = num_cameras
                                         Etalon_Size       : Cv_Size;
                                         Square_Size       : Float;
                                         Samples           : Ipl_Image_P_Array; -- size = num_cameras
                                         Camera_Info       : Cv_3d_Tracker_Camera_Info_Array) -- size = num_cameras
                                         return Cv_Bool;

   function Cv3dTrackerLocateObjects (Num_Cameras     : Integer;
                                      Num_Objects     : Integer;
                                      Camera_Info     : Cv_3d_Tracker_Camera_Info_Array; -- size = num_cameras
                                      Tracking_Info   : Cv_3d_Tracker_2d_Tracked_Object_Array; -- size = num_objects*num_cameras
                                      Tracked_Objects : Cv_3d_Tracker_Tracked_Object_Array) -- size = num_objects
                                      return Integer;
   --   tracking_info is a rectangular array; one row per camera, num_objects
   --   elements per row. The id field of any unused slots must be -1. Ids need
   --   not be ordered or consecutive. On completion, the return value is the
   --   number of objects located; i.e., the number of Objects visible by more
   --   than one camera. The id field of any unused slots in tracked objects is
   --   set to -1.

   -- Skeletons and Linear_Contour Models --------------------------------------
   -----------------------------------------------------------------------------
   --     Lee Parameters
   CV_LEE_INT    : constant := 0;
   CV_LEE_FLOAT  : constant := 1;
   CV_LEE_DOUBLE : constant := 2;
   CV_LEE_AUTO   : constant := -1;
   CV_LEE_ERODE  : constant := 0;
   CV_LEE_ZOOM   : constant := 1;
   CV_LEE_NON    : constant := 2;

   CV_LEE_INT_F    : constant := 0.0;
   CV_LEE_FLOAT_F  : constant := 1.0;
   CV_LEE_DOUBLE_F : constant := 2.0;
   CV_LEE_AUTO_F   : constant := -1.0;
   CV_LEE_ERODE_F  : constant := 0.0;
   CV_LEE_ZOOM_F   : constant := 1.0;
   CV_LEE_NON_F    : constant := 2.0;

   --     #define CV_VORONOISITE2D_FIELDS() \
   --      struct CvVoronoiNode2D *node[2]; \
   --      struct CvVoronoiEdge2D *edge[2];

   --     #define CV_VORONOIEDGE2D_FIELDS() \
   --      struct CvVoronoiNode2D *node[2]; \
   --      struct CvVoronoiSite2D *site[2]; \
   --      struct CvVoronoiEdge2D *next[4];

   --     #define CV_VORONOINODE2D_FIELDS()    \
   --      CV_SET_ELEM_FIELDS(CvVoronoiNode2D) \
   --      CvPoint2D32f pt;                    \
   --      float radius;

   --     #define CV_VORONOIDIAGRAM2D_FIELDS() \
   --      CV_GRAPH_FIELDS()                   \
   --      CvSet *sites;

   type Cv_Voronoi_Site_2d;
   type Cv_Voronoi_Site_2d_Array;
   type Cv_Voronoi_Site_2d_Array_2;

   type Cv_Voronoi_Edge_2d;
   type Cv_Voronoi_Edge_2d_Array;
   type Cv_Voronoi_Edge_2d_Array_2;
   type Cv_Voronoi_Edge_2d_Array_4;

   type Cv_Voronoi_Node_2d;
   type Cv_Voronoi_Node_2d_Array;
   type Cv_Voronoi_Node_2d_Array_2;


   type Cv_Voronoi_Site_2d is record
   -- CV_VORONOISITE2D_FIELDS()
      Edge : access Cv_Voronoi_Edge_2d_Array_2;
      Node : access Cv_Voronoi_Node_2d_Array_2;
      Next : access Cv_Voronoi_Site_2d_Array_2;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Voronoi_Site_2d);
   type Cv_Voronoi_Site_2d_P is access Cv_Voronoi_Site_2d;
   type Cv_Voronoi_Site_2d_Array is array (Integer range <>) of aliased Cv_Voronoi_Site_2d;
   type Cv_Voronoi_Site_2d_Array_2 is array (Integer range 1 .. 2) of aliased Cv_Voronoi_Site_2d;

   type Cv_Voronoi_Edge_2d is record
   -- CV_VORONOIEDGE2D_FIELDS()
      Node : access Cv_Voronoi_Node_2d_Array_2;
      Site : access Cv_Voronoi_Site_2d_Array_2;
      Next : access Cv_Voronoi_Edge_2d_Array_4;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Voronoi_Edge_2d);
   type Cv_Voronoi_Edge_2d_P is access Cv_Voronoi_Edge_2d;
   type Cv_Voronoi_Edge_2d_Array is array (Integer range <>) of aliased Cv_Voronoi_Edge_2d;
   type Cv_Voronoi_Edge_2d_Array_2 is array (Integer range 1 .. 2) of aliased Cv_Voronoi_Edge_2d;
   type Cv_Voronoi_Edge_2d_Array_4 is array (Integer range 1 .. 4) of aliased Cv_Voronoi_Edge_2d;

   type Cv_Voronoi_Node_2d is record
   -- CV_VORONOINODE2D_FIELDS()
      Flags     : Integer;
      Next_Free : access Cv_Voronoi_Node_2d;
      Pt        : Cv_Point_2d_32f;
      Radius    : Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Voronoi_Node_2d);
   type Cv_Voronoi_Node_2d_P is access Cv_Voronoi_Node_2d;
   type Cv_Voronoi_Node_2d_Array is array (Integer range <>) of aliased Cv_Voronoi_Node_2d;
   type Cv_Voronoi_Node_2d_Array_2 is array (Integer range 1 .. 2) of aliased Cv_Voronoi_Node_2d;


   type Cv_Voronoi_Diagram_2d is record
   -- CV_VORONOIDIAGRAM2D_FIELDS()
      Flags           : Integer;       --CV_TREE_NODE_FIELDS(CvSeq);
      HeaderSize      : Integer;
      HPrev           : Cv_Seq_P;
      HNext           : Cv_Seq_P;
      VPrev           : Cv_Seq_P;
      VNext           : Cv_Seq_P;

      Total           : Integer; --CV_SEQUENCE_FIELDS
      ElemSize        : Integer;
      BlockMax        : Cv_Void_P;
      Ptr             : Cv_Void_P;
      DeltaElems      : Integer;
      Storage         : Cv_Mem_Storage_P;
      FreeBlocks      : Cv_Seq_Block_P;
      First           : Cv_Seq_Block_P;

      Free_Elems      : Cv_Set_Elem_Pointer;
      Active_Count    : Integer;
      Edges           : Cv_Set_Pointer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Voronoi_Diagram_2d);
   type Cv_Voronoi_Diagram_2d_P is access Cv_Voronoi_Diagram_2d;
   type Cv_Voronoi_Diagram_2d_Array is array (Integer range <>) of aliased Cv_Voronoi_Diagram_2d;

   --     Computes Voronoi Diagram for given polygons with holes
   function CvVoronoiDiagramFromContour (Contour_Seq         : Cv_Seq_P;
                                         Voronoi_Diagram     : access Cv_Voronoi_Diagram_2d_P;
                                         Voronoi_Storage     : Cv_Mem_Storage_P;
                                         Contour_Type        : Integer := Cv_Lee_Int;
                                         Contour_Orientation : Integer := -1;
                                         Attempt_Number      : Integer := 10)
                                         return Integer;

   --     Computes Voronoi Diagram for domains in given image
   function CvVoronoiDiagramFromImage (P_Image               : Ipl_Image_P;
                                       Contour_Seq           : access Cv_Seq_P;
                                       Voronoi_Diagram       : access Cv_Voronoi_Diagram_2d_P;
                                       Voronoi_Storage       : Cv_Mem_Storage_P;
                                       Regularization_Method : Integer := Cv_Lee_Non;
                                       Approx_Precision      : Float := Cv_Lee_Auto_F)
                                       return Integer;

   --     Deallocates the storage
   procedure CvReleaseVoronoiStorage (Voronoi_Diagram   : Cv_Voronoi_Diagram_2d_P;
                                      P_Voronoi_Storage : access Cv_Mem_Storage_P);


   -- Linear-Contour Model -----------------------------------------------------
   type Cv_LCM_Edge is record
      Flags  : Integer;
      Weight : Float;
      Next   : Cv_Graph_Edge_P_Array;
      Vtx    : Cv_Graph_Vtx_P_Array;
      Chain  : Cv_Seq_P;
      Width  : Float;
      Index1 : Integer;
      Index2 : Integer;
   end record;

   type Cv_LCM_Node is record
      Flags   : Integer;
      First   : Cv_Graph_Edge_P;
      Contour : Cv_Contour_P;
   end record;

   --     Computes hybrid model from Voronoi Diagram
   function CvLinearContorModelFromVoronoiDiagram (Voronoi_Diagram : Cv_Voronoi_Diagram_2d_P;
                                                   Max_Width       : Float)
                                                   return Cv_Graph_P;

   --     Releases hybrid model storage
   procedure CvReleaseLinearContorModelStorage (Graph : access Cv_Graph_P);

   subtype Cv_Point_2d_32f_Array_4 is Cv_Point_2d_32f_Array (1 .. 4);

   procedure CvInitPerspectiveTransform (Size     : Cv_Size;
                                         Vertex   : Cv_Point_2d_32f_Array_4;
                                         Matrix   : Cv_64f_Array_3x3;
                                         Rect_Map : Cv_Arr_P);

   -- View Morphing Functions --------------------------------------------------
   --     The order of the function corresponds to the order they should appear
   --     in the view morphing pipeline

   --     Finds ending points of scanlines on left and right images of
   --     stereo-pair
   procedure CvMakeScanlines (Matrix     : Cv_Matrix_3;
                              Img_Size   : Cv_Size;
                              Scanlines1 : Cv_32s_Pointer;
                              Scanlines2 : Cv_32s_Pointer;
                              Lengths1   : Cv_32s_Pointer;
                              Lengths2   : Cv_32s_Pointer;
                              Line_Count : access Integer);

   --     Grab pixel values from scanlines and stores them sequentially
   --     (some sort of perspective image transform)
   procedure CvPreWarpImage (Line_Count : Integer;
                             Img        : Ipl_Image_P;
                             Dst        : Cv_8u_Pointer;
                             Dst_Nums   : Cv_32s_Pointer;
                             Scan_Lines : Cv_32s_Pointer);

   --     Approximate each grabbed scanline by a sequence of runs
   --     (lossy run-length compression)
   procedure CvFindRuns (Line_Count    : Integer;
                         Prewarp1      : Cv_8u_Pointer;
                         Prewarp2      : Cv_8u_Pointer;
                         Line_Lengths1 : Cv_32s_Pointer;
                         Line_Lengths2 : Cv_32s_Pointer;
                         Runs1         : Cv_32s_Pointer;
                         Runs2         : Cv_32s_Pointer;
                         Num_Runs1     : Cv_32s_Pointer;
                         Num_Runs2     : Cv_32s_Pointer);

   --     Compares two sets of compressed scanlines
   procedure CvDynamicCorrespondMulti (Line_Count  : Integer;
                                       First       : Cv_32s_Pointer;
                                       First_Runs  : Cv_32s_Pointer;
                                       Second      : Cv_32s_Pointer;
                                       Second_Runs : Cv_32s_Pointer;
                                       First_Corr  : Cv_32s_Pointer;
                                       Second_Corr : Cv_32s_Pointer);

   --     Finds scanline ending coordinates for some intermediate "virtual"
   --     camera position
   procedure CvMakeAlphaScanlines (Scanlines1  : Cv_32s_Pointer;
                                   Scanlines2  : Cv_32s_Pointer;
                                   Scanlines_A : Cv_32s_Pointer;
                                   Lengths     : Cv_32s_Pointer;
                                   Line_Count  : Integer;
                                   Alpha       : Float);

   --     Blends data of the left and right image scanlines to get
   --     pixel values of "virtual" image scanlines
   procedure CvMorphEpilinesMulti (Line_Count  : Integer;
                                   First_Pix   : Cv_8u_Pointer;
                                   First_Num   : Cv_32s_Pointer;
                                   Second_Pix  : Cv_8u_Pointer;
                                   Second_Num  : Cv_32s_Pointer;
                                   Dst_Pix     : Cv_8u_Pointer;
                                   Dst_Num     : Cv_32s_Pointer;
                                   Alpha       : Float;
                                   First       : Cv_32s_Pointer;
                                   First_Runs  : Cv_32s_Pointer;
                                   Second      : Cv_32s_Pointer;
                                   Second_Runs : Cv_32s_Pointer;
                                   First_Corr  : Cv_32s_Pointer;
                                   Second_Corr : Cv_32s_Pointer);

   --     Does reverse warping of the morphing result to make
   --     it fill the destination image rectangle
   procedure CvPostWarpImage (Line_Count : Integer;
                              Src        : Cv_8u_Pointer;
                              Src_Nums   : Cv_32s_Pointer;
                              Img        : Ipl_Image_P;
                              Scanlines  : Cv_32s_Pointer);

   --     Deletes Moire (missed pixels that appear due to discretization)
   procedure CvDeleteMoire (Img : Ipl_Image_P);

   -- ConDenstation state.
   type Cv_Con_Densation is
      record
         MP           : Integer;
         DP           : Integer;
         DynamMatr    : Cv_32F_Array_P;
         State        : Cv_32F_Array_P;
         SamplesNum   : Integer;
         FlSamples    : access Cv_32F_Array_P;
         FlNewSamples : access Cv_32F_Array_P;
         FlConfidance : Cv_32F_Array_P;
         FlCumulative : Cv_32F_Array_P;
         Temp         : Cv_32F_Array_P;
         RandomSample : Cv_32F_Array_P;
         RandS        : Cv_Rand_State;
      end record;

   type Cv_Con_Densation_P is access Cv_Con_Densation;

   --     Allocates the ConDensation filter structure.
   function CvCreateConDensation (DynamParams   : Integer;
                                  MeasureParams : Integer;
                                  SampleCount   : Integer) return Cv_Con_Densation_P;

   --     Initializes the sample set for the ConDensation algorithm.
   procedure CvConDensInitSampleSet (Condens    : Cv_Con_Densation_P;
                                     LowerBound : Cv_Mat_P;
                                     UpperBound : Cv_Mat_P);

   --     Updates ConDensation filter by time (predict future state of the system)
   procedure CvConDensUpdateByTime (Condens : Cv_Con_Densation_P);

   function IplWidth (Img : Ipl_Image_P)
                      return Integer;

   function IplHeight (Img : Ipl_Image_P)
                       return Integer;

private

   pragma Import (C, CvSegmentImage, "cvSegmentImage");
   pragma Import (C, CvCalcCovarMatrixEx, "cvCalcCovarMatrixEx");
   pragma Import (C, CvCalcEigenObjects, "cvCalcEigenObjects");
   pragma Import (C, CvCalcDecompCoeff, "cvCalcDecompCoeff");
   pragma Import (C, CvEigenDecomposite, "CvEigenDecomposite");
   pragma Import (C, CvEigenProjection, "cvEigenProjection");
   pragma Import (C, CvCreate2DHMM, "cvCreate2DHMM");
   pragma Import (C, CvRelease2DHMM, "cvRelease2DHMM");
   pragma Import (C, CvCreateObsInfo, "cvCreateObsInfo");
   pragma Import (C, CvReleaseObsInfo, "cvReleaseObsInfo");
   pragma Import (C, CvImgToObs_DCT, "cvImgToObs_DCT");
   pragma Import (C, CvUniformImgSegm, "cvUniformImgSegm");
   pragma Import (C, CvInitMixSegm, "cvInitMixSegm");
   pragma Import (C, CvEstimateHMMStateParams, "cvEstimateHMMStateParams");
   pragma Import (C, CvEstimateTransProb, "cvEstimateTransProb");
   pragma Import (C, CvEstimateObsProb, "cvEstimateObsProb");
   pragma Import (C, CvEViterbi, "cvEViterbi");
   pragma Import (C, CvMixSegmL2, "cvMixSegmL2");
   pragma Import (C, CvCreateHandMask, "cvCreateHandMask");
   pragma Import (C, CvFindHandRegion, "cvFindHandRegion");
   pragma Import (C, CvFindHandRegionA, "cvFindHandRegionA");
   pragma Import (C, CvCalcImageHomography, "cvCalcImageHomography");
   pragma Import (C, IcvDrawMosaic, "icvDrawMosaic");
   pragma Import (C, IcvSubdiv2DCheck, "icvSubdiv2DCheck");
   pragma Import (C, IcvSqDist2D32f, "icvSqDist2D32f");
   pragma Import (C, CvCalcPGH, "cvCalcPGH");
   pragma Import (C, CvFindDominantPoints, "cvFindDominantPoints");
   pragma Import (C, IcvConvertWarpCoordinates, "icvConvertWarpCoordinates");
   pragma Import (C, IcvGetSymPoint3D, "icvGetSymPoint3D");
   pragma Import (C, IcvGetPieceLength3D, "icvGetPieceLength3D");
   pragma Import (C, IcvCompute3DPoint, "icvCompute3DPoint");
   pragma Import (C, IcvCreateConvertMatrVect, "icvCreateConvertMatrVect");
   pragma Import (C, IcvConvertPointSystem, "icvConvertPointSystem");
   pragma Import (C, IcvComputeCoeffForStereo, "icvComputeCoeffForStereo");
   pragma Import (C, IcvGetCrossPieceVector, "icvGetCrossPieceVector");
   pragma Import (C, IcvGetCrossLineDirect, "icvGetCrossLineDirect");
   pragma Import (C, IcvDefinePointPosition, "icvDefinePointPosition");
   pragma Import (C, IcvStereoCalibration, "icvStereoCalibration");
   pragma Import (C, IcvComputeRestStereoParams, "icvComputeRestStereoParams");
   pragma Import (C, CvComputePerspectiveMap, "cvComputePerspectiveMap");
   pragma Import (C, IcvComCoeffForLine, "icvComCoeffForLine");
   pragma Import (C, IcvGetDirectionForPoint, "icvGetDirectionForPoint");
   pragma Import (C, IcvGetCrossLines, "icvGetCrossLines");
   pragma Import (C, IcvComputeStereoLineCoeffs, "icvComputeStereoLineCoeffs");
   pragma Import (C, IcvGetAngleLine, "icvGetAngleLine");
   pragma Import (C, IcvGetCoefForPiece, "icvGetCoefForPiece");
   pragma Import (C, IcvComputeeInfiniteProject1, "icvComputeeInfiniteProject1");
   pragma Import (C, IcvComputeeInfiniteProject2, "icvComputeeInfiniteProject2");
   pragma Import (C, IcvGetCrossDirectDirect, "icvGetCrossDirectDirect");
   pragma Import (C, IcvGetCrossPieceDirect, "icvGetCrossPieceDirect");
   pragma Import (C, IcvGetCrossPiecePiece, "icvGetCrossPiecePiece");
   pragma Import (C, IcvGetPieceLength, "icvGetPieceLength");
   pragma Import (C, IcvGetCrossRectDirect, "icvGetCrossRectDirect");
   pragma Import (C, IcvProjectPointToImage, "icvProjectPointToImage");
   pragma Import (C, IcvGetQuadsTransform, "icvGetQuadsTransform");
   pragma Import (C, IcvGetQuadsTransformStruct, "icvGetQuadsTransformStruct");
   pragma Import (C, IcvComputeStereoParamsForCameras, "icvComputeStereoParamsForCameras");
   pragma Import (C, IcvGetCutPiece, "icvGetCutPiece");
   pragma Import (C, IcvGetMiddleAnglePoint, "icvGetMiddleAnglePoint");
   pragma Import (C, IcvGetNormalDirect, "icvGetNormalDirect");
   pragma Import (C, IcvGetVect, "icvGetVect");
   pragma Import (C, IcvProjectPointToDirect, "icvProjectPointToDirect");
   pragma Import (C, IcvGetDistanceFromPointToDirect, "icvGetDistanceFromPointToDirect");
   pragma Import (C, IcvCreateIsometricImage, "icvCreateIsometricImage");
   pragma Import (C, CvDeInterlace, "cvDeInterlace");
   pragma Import (C, CvCreateContourTree, "cvCreateContourTree");
   pragma Import (C, CvContourFromContourTree, "cvContourFromContourTree");
   pragma Import (C, CvMatchContourTrees, "cvMatchContourTrees");
   pragma Import (C, CvCalcContoursCorrespondence, "cvCalcContoursCorrespondence");
   pragma Import (C, CvMorphContours, "cvMorphContours");
   pragma Import (C, CvSnakeImage, "cvSnakeImage");
   pragma Import (C, CvCreateGLCM, "cvCreateGLCM");
   pragma Import (C, CvReleaseGLCM, "cvReleaseGLCM");
   pragma Import (C, CvCreateGLCMDescriptors, "cvCreateGLCMDescriptors");
   pragma Import (C, CvGetGLCMDescriptor, "cvGetGLCMDescriptor");
   pragma Import (C, CvGetGLCMDescriptorStatistics, "cvGetGLCMDescriptorStatistics");
   pragma Import (C, CvCreateGLCMImage, "cvCreateGLCMImage");
   pragma Import (C, CvInitFaceTracker, "cvInitFaceTracker");
   pragma Import (C, CvTrackFace, "cvTrackFace");
   pragma Import (C, CvReleaseFaceTracker, "cvReleaseFaceTracker");
   pragma Import (C, CvFindFace, "cvFindFace");
   pragma Import (C, CvPostBoostingFindFace, "cvPostBoostingFindFace");
   pragma Import (C, Cv3dTracker2dTrackedObject, "cv3dTracker2dTrackedObject");
   pragma Import (C, Cv3dTrackerTrackedObject, "cv3dTrackerTrackedObject");
   pragma Import (C, Cv3dTrackerCalibrateCameras, "cv3dTrackerCalibrateCameras");
   pragma Import (C, Cv3dTrackerLocateObjects, "cv3dTrackerLocateObjects");
   pragma Import (C, CvLinearContorModelFromVoronoiDiagram, "cvLinearContorModelFromVoronoiDiagram");
   pragma Import (C, CvReleaseLinearContorModelStorage, "cvReleaseLinearContorModelStorage");
   pragma Import (C, CvInitPerspectiveTransform, "cvInitPerspectiveTransform");
   pragma Import (C, CvVoronoiDiagramFromContour, "cvVoronoiDiagramFromContour");
   pragma Import (C, CvVoronoiDiagramFromImage, "cvVoronoiDiagramFromImage");
   pragma Import (C, CvReleaseVoronoiStorage, "cvReleaseVoronoiStorage");
   pragma Import (C, CvMakeScanlines, "cvMakeScanlines");
   pragma Import (C, CvPreWarpImage, "cvPreWarpImage");
   pragma Import (C, CvFindRuns, "CvFindRuns");
   pragma Import (C, CvDynamicCorrespondMulti, "cvDynamicCorrespondMulti");
   pragma Import (C, CvMakeAlphaScanlines, "cvMakeAlphaScanlines");
   pragma Import (C, CvMorphEpilinesMulti, "cvMorphEpilinesMulti");
   pragma Import (C, CvPostWarpImage, "cvPostWarpImage");
   pragma Import (C, CvDeleteMoire, "cvDeleteMoire");
   pragma Import (C, CvCreateConDensation, "cvCreateConDensation");
   pragma Import (C, CvConDensInitSampleSet, "cvConDensInitSampleSet");
   pragma Import (C, CvConDensUpdateByTime, "cvConDensUpdateByTime");
end Legacy;
