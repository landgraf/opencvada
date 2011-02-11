--------------------------------------------------------------------------------
-- ada Bindings for OpenCV 2.1.1 (from SVN 3 October 2010, rev. 3703)
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
   pragma Convention (C_Pass_By_Copy, Cv_Matrix_3);

   type Cv_Rand_State is record
      State    : Cv_Rng;
      Disttype : Integer;
      Param    : Cv_Scalar_Array (1 .. 2);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Rand_State);

   --------------------------------------------------------------------------------

   function Cv_Segment_Image (Src             : Cv_Arr_Ptr;
                              Dst             : Cv_Arr_Ptr;
                              Canny_Threshold : Long_Float;
                              Ffill_Threshold : Long_Float;
                              Storage         : Cv_Mem_Storage)
                              return Cv_Seq_Ptr;
   function Cv_Segment_Image (Src             : Cv_Mat_Ptr;
                              Dst             : Cv_Mat_Ptr;
                              Canny_Threshold : Long_Float;
                              Ffill_Threshold : Long_Float;
                              Storage         : Cv_Mem_Storage)
                              return Cv_Seq_Ptr;
   function Cv_Segment_Image (Src             : Ipl_Image_Ptr;
                              Dst             : Ipl_Image_Ptr;
                              Canny_Threshold : Long_Float;
                              Ffill_Threshold : Long_Float;
                              Storage         : Cv_Mem_Storage)
                              return Cv_Seq_Ptr;
   -----------------------------------------------------------------------------
   -- Eigen objects ------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Callback is access function (Index     : Integer;
                                        Buffer    : Cv_Void_Ptr;
                                        User_Data : Cv_Void_Ptr)
                                        return Integer;
   pragma Convention (C, Cv_Callback);

   type Cv_Input_Type is (Callback, Data);
   type Cv_Input (Input_Type : Cv_Input_Type := Callback) is record
      case Input_Type is
         when Callback =>
            Callback : Cv_Callback;
         when Data =>
            Data     : Cv_Void_Ptr;
      end case;
   end record;
   pragma Unchecked_Union (Cv_Input);
   pragma Convention (C_Pass_By_Copy, Cv_Input);

   Cv_Eigobj_No_Callback     : constant := 0;
   Cv_Eigobj_Input_Callback  : constant := 1;
   Cv_Eigobj_Output_Callback : constant := 2;
   Cv_Eigobj_Both_Callback   : constant := 3;

   --     Calculates covariation matrix of a set of arrays
   procedure Cv_Calc_Covar_Matrix_Ex (N_Objects    : Integer;
                                      Input        : Cv_Void_Ptr;
                                      Io_Flags     : Integer;
                                      Io_Buf_Size  : Integer;
                                      Buffer       : Cv_8u_Array;
                                      User_Data    : Cv_Void_Ptr;
                                      Avg          : Ipl_Image_Ptr;
                                      Covar_Matrix : Cv_32f_Array);

   --     Calculates eigen values and vectors of covariation matrix of a set of
   --     arrays
   procedure Cv_Calc_Eigen_Objects (N_Objects   : Integer;
                                    Input       : Cv_Void_Ptr;
                                    Output      : Cv_Void_Ptr;
                                    Io_Flags    : Integer;
                                    Op_Buf_Size : Integer;
                                    User_Data   : Cv_Void_Ptr;
                                    Calc_Limit  : Cv_Term_Criteria_P;
                                    Avg         : Ipl_Image_Ptr;
                                    Eig_Vals    : Cv_32f_Array);
   --     Calculates dot product (obj - avg) * eigObj (i.e. projects image to
   --     eigen vector)
   function Cv_Calc_Decomp_Coeff (Obj        : Ipl_Image_Ptr;
                                  N_Eig_Objs : Integer;
                                  Avg        : Ipl_Image_Ptr)
                                  return Long_Float;

   --     Projects image to eigen space (finds all decomposion coefficients
   procedure Cv_Eigen_Decomposite (Obj        : Ipl_Image_Ptr;
                                   N_Eig_Objs : Integer;
                                   Eig_Input  : Cv_Void_Ptr;
                                   Io_Flags   : Integer;
                                   User_Data  : Cv_Void_Ptr;
                                   Avg        : Ipl_Image_Ptr;
                                   Eig_Vals   : Cv_32f_Array);

   --     Projects original objects used to calculate eigen space basis to that
   --     space
   procedure Cv_Eigen_Projection (Eig_Input  : Cv_Void_Ptr;
                                  N_Eig_Objs : Integer;
                                  Io_Flags   : Integer;
                                  User_Data  : Cv_Void_Ptr;
                                  Coeffs     : Cv_32f_Array;
                                  Avg        : Ipl_Image_Ptr;
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
   type Cv_1d_Obs_Info is new Cv_Img_Obs_Info;
   pragma Convention (C_Pass_By_Copy, Cv_1d_Obs_Info);

   type Cv_Img_Obs_Info_Ptr is access all Cv_Img_Obs_Info;
   type Cv_1d_Obs_Info_Ptr is access all Cv_1d_Obs_Info;

   type Cv_Img_Obs_Info_P_Array is array (Integer range <>) of aliased Cv_Img_Obs_Info_Ptr;

   -- Cv_EHMM_State ------------------------------------------------------------
   type Cv_Ehmm_State is record
      Num_Mix     : Integer;
      Mu          : Cv_32f_Array_P;
      Inv_Var     : Cv_32f_Array_P;
      Log_Var_Val : Cv_32f_Array_P;
      Weight      : Cv_32f_Array_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Ehmm_State);
   type Cv_Ehmm_State_Ptr is access all Cv_Ehmm_State;

   -- Cv_EHMM ------------------------------------------------------------------
   type Cv_Ehmm;
   type Cv_Ehmm_Ptr is access all Cv_Ehmm;

   type Cv_Ehmm_Union_Type is (State, Ehmm);
   type Cv_Ehmm_Union (Union_Type : Cv_Ehmm_Union_Type := State) is record
      case Union_Type is
         when State =>
            State : Cv_Ehmm_State_Ptr;
         when Ehmm =>
            Ehmm  : Cv_Ehmm_Ptr;
      end case;
   end record;
   pragma Unchecked_Union (Cv_Ehmm_Union);
   pragma Convention (C_Pass_By_Copy, Cv_Ehmm_Union);

   type Cv_Ehmm is record
      Level      : Integer;
      Num_States : Integer;
      Trans_P    : Cv_32f_Array_P;    -- float *
      Obs_Prob   : Cv_32f_Pointer_Array_P; -- float **
      U          : Cv_Ehmm_Union;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Ehmm);

   -- Embedded HMMs ------------------------------------------------------------
   --     Creates 2D HMM
   function Cv_Create_2d_Hmm (State_Number : Cv_32s_Array;
                              Num_Mix      : Cv_32s_Array;
                              Obs_Size     : Integer)
                              return Cv_Ehmm_Ptr;

   --     Releases HMM
   procedure Cv_Release_2d_Hmm (Hmm : access Cv_Ehmm_Ptr);

   procedure Cv_Count_Obs (Roi       : Cv_Size;
                           Win       : Cv_Size;
                           Delta_Obs : Cv_Size;
                           Num_Obs   : out Cv_Size);

   --     Creates storage for observation vectors
   function Cv_Create_Obs_Info (Num_Obs  : Cv_Size;
                                Obs_Size : Integer)
                                return Cv_Img_Obs_Info_Ptr;

   --     Releases storage for observation vectors
   procedure Cv_Release_Obs_Info (Obs_Info : access Cv_Img_Obs_Info_Ptr);

   --     The function takes an image on input and and returns the sequnce of
   --     Observations to be used with an embedded HMM; Each observation is
   --     top-left block of DCT coefficient matrix
   procedure Cv_Img_To_Obs_Dct (Arr        : Cv_Arr_Ptr;
                                Obs        : Cv_32f_Array;
                                Dct_Size   : Cv_Size;
                                Obs_Size   : Cv_Size;
                                Delta_Size : Cv_Size);
   procedure Cv_Img_To_Obs_Dct (Arr        : Cv_Mat_Ptr;
                                Obs        : Cv_32f_Array;
                                Dct_Size   : Cv_Size;
                                Obs_Size   : Cv_Size;
                                Delta_Size : Cv_Size);
   procedure Cv_Img_To_Obs_Dct (Arr        : Ipl_Image_Ptr;
                                Obs        : Cv_32f_Array;
                                Dct_Size   : Cv_Size;
                                Obs_Size   : Cv_Size;
                                Delta_Size : Cv_Size);

   --     Uniformly segments all observation vectors extracted from image
   procedure Cv_Uniform_Img_Segm (Obs_Info : Cv_Img_Obs_Info_Ptr;
                                  Ehmm     : Cv_Ehmm_Ptr);

   --     Does mixture segmentation of the states of embedded HMM
   procedure Cv_Init_Mix_Segm (Obs_Info_Array : Cv_Img_Obs_Info_P_Array;
                               Num_Img        : Integer;
                               Hmm            : Cv_Ehmm_Ptr);

   --     Function calculates means, variances, weights of every Gaussian
   --     Mixture of every low-level state of embedded HMM
   procedure Cv_Estimate_Hmm_State_Params (Obs_Info_Array : Cv_Img_Obs_Info_P_Array;
                                           Num_Img        : Integer;
                                           Hmm            : Cv_Ehmm_Ptr);

   --     Function computes transition probability matrices of embedded HMM
   --     given observations segmentation
   procedure Cv_Estimate_Trans_Prob (Obs_Info_Array : Cv_Img_Obs_Info_P_Array;
                                     Num_Img        : Integer;
                                     Hmm            : Cv_Ehmm_Ptr);

   --     Function computes probabilities of appearing observations at any state
   --     (i.e. computes P(obs|state) for every pair(obs,state))
   procedure Cv_Estimate_Obs_Prob (Obs_Info : Cv_Img_Obs_Info_Ptr;
                                   Hmm      : Cv_Ehmm_Ptr);

   --     Runs Viterbi algorithm for embedded HMM
   function Cv_E_Viterbi (Obs_Info : Cv_Img_Obs_Info_Ptr;
                          Hmm      : Cv_Ehmm_Ptr)
                          return Float;

   --     Function clusters observation vectors from several images
   --     given observations segmentation.
   --     Euclidean distance used for clustering vectors.
   --     Centers of clusters are given means of every mixture
   procedure Cv_Mix_Segm_L2 (Obs_Info_Array : Cv_Img_Obs_Info_P_Array;
                             Num_Img        : Integer;
                             Hmm            : Cv_Ehmm_Ptr);

   -- A few function from old stereo gesture recognition demonstrations---------
   -----------------------------------------------------------------------------
   --     Creates hand mask image given several points on the hand
   procedure Cv_Create_Hand_Mask (Hand_Points : Cv_Seq_Ptr;
                                  Img_Mask    : Ipl_Image_Ptr;
                                  Roi         : Cv_Rect_Ptr);

   --     Finds hand region in range image data
   procedure Cv_Find_Hand_Region (Points  : Cv_Point_3d_32f_Array;
                                  Count   : Integer;
                                  Indexs  : Cv_Seq_Ptr;
                                  Line    : Cv_32f_Array;
                                  Size    : Cv_Size_2d_32f;
                                  Flag    : Integer;
                                  Center  : Cv_Point_3d_32f_Array;
                                  Storage : Cv_Mem_Storage_Ptr;
                                  Numbers : access Cv_Seq_Ptr);

   --     Finds hand region in range image data (advanced version)
   procedure Cv_Find_Hand_Region_A (Points  : Cv_Point_3d_32f_Array;
                                    Count   : Integer;
                                    Indexs  : Cv_Seq_Ptr;
                                    Line    : Cv_32f_Array;
                                    Size    : Cv_Size_2d_32f;
                                    Jc      : Integer;
                                    Center  : Cv_Point_3d_32f_Array;
                                    Storage : Cv_Mem_Storage_Ptr;
                                    Numbers : access Cv_Seq_Ptr);

   --     Calculates the cooficients of the homography matrix
   procedure Cv_Calc_Image_Homography (Line       : Cv_32f_Array;
                                       Center     : Cv_Point_3d_32f_Array;
                                       Intrinsic  : Cv_32f_Array;
                                       Homography : Cv_32f_Array);

   -- Additional operations on Subdivisions ------------------------------------
   -----------------------------------------------------------------------------
   --     paints voronoi diagram: just demo function
   procedure Icv_Draw_Mosaic (Subdiv : Cv_Subdiv_2d_Ptr;
                              Src    : Ipl_Image_Ptr;
                              Dst    : Ipl_Image_Ptr);

   --     checks planar subdivision for correctness. It is not an absolute
   --     check, but it verifies some relations between quad-edges
   function Icv_Subdiv_2d_Check (Subdiv : Cv_Subdiv_2d_Ptr)
                                 return Integer;

   --     returns squared distance between two 2D points with floating-point
   --     coordinates.
   function Icv_Sq_Dist_2d_32f (Pt1 : Cv_Point_3d_32f;
                                Pt2 : Cv_Point_3d_32f)
                                return Long_Float;

   -- More operations on sequences ---------------------------------------------
   -----------------------------------------------------------------------------
   function Cv_Current_Int (Reader : Cv_Seq_Reader_Ptr)
                            return Integer;

   function Cv_Prev_Int (Reader : Cv_Seq_Reader_Ptr)
                         return Integer;

   type Cv_Graph_Weighted_Vtx is record
      Flags  : Integer;
      First  : Cv_Graph_Edge_Ptr;
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

   type Cv_Graph_Weight_Type is new Integer;

   Cv_Not_Weighted : constant Cv_Graph_Weight_Type := 0;
   Cv_Weighted_Vtx : constant Cv_Graph_Weight_Type :=  1;
   Cv_Weigthed_Edge : constant Cv_Graph_Weight_Type := 2;
   Cv_Weighted_All : constant Cv_Graph_Weight_Type := 3;

   --     Calculates histogram of a contour
   procedure Cv_Calc_Pgh (Contour : Cv_Seq_Ptr;
                          Hist    : Cv_Histogram_Ptr);

   Cv_Dominant_Ipan : constant := 1;

   --     Finds high-curvature points of the contour
   function Cv_Find_Dominant_Points (Contour    : Cv_Seq_Ptr;
                                     Storage    : Cv_Mem_Storage_Ptr;
                                     Method     : Integer := Cv_Dominant_Ipan;
                                     Parameter1 : Long_Float := 0.0;
                                     Parameter2 : Long_Float := 0.0;
                                     Parameter3 : Long_Float := 0.0;
                                     Parameter4 : Long_Float := 0.0)
                                     return Cv_Seq_Ptr;

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
   pragma Convention (C_Pass_By_Copy, Cv_Clique_Finder);

   Clique_Time_Off : constant := 2;
   Clique_Found    : constant := 1;
   Clique_End      : constant := 0;

   Cv_Undef_Sc_Param    : constant := 12345;
   Cv_Undef_Sc_Param_F  : constant Float := 12345.0;
   Cv_Undef_Sc_Param_Lf : constant Long_Float := 12345.0;

   Cv_Idp_Birchfield_Param1 : constant := 25;
   Cv_Idp_Birchfield_Param2 : constant := 5;
   Cv_Idp_Birchfield_Param3 : constant := 12;
   Cv_Idp_Birchfield_Param4 : constant := 15;
   Cv_Idp_Birchfield_Param5 : constant := 25;

   Cv_Disparity_Birchfield : constant := 0;

   --     find stereo correspondence on stereo-pair
   procedure Cv_Find_Stereo_Correspondence (Left_Image    : Cv_Arr_Ptr;
                                            Right_Image   : Cv_Arr_Ptr;
                                            Mode          : Integer;
                                            Disp_Image    : Cv_Arr_Ptr;
                                            Max_Disparity : Integer;
                                            Param1        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param2        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param3        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param4        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param5        : Long_Float := Cv_Undef_Sc_Param_Lf);
   procedure Cv_Find_Stereo_Correspondence (Left_Image    : Cv_Mat_Ptr;
                                            Right_Image   : Cv_Mat_Ptr;
                                            Mode          : Integer;
                                            Disp_Image    : Cv_Mat_Ptr;
                                            Max_Disparity : Integer;
                                            Param1        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param2        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param3        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param4        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param5        : Long_Float := Cv_Undef_Sc_Param_Lf);
   procedure Cv_Find_Stereo_Correspondence (Left_Image    : Ipl_Image_Ptr;
                                            Right_Image   : Ipl_Image_Ptr;
                                            Mode          : Integer;
                                            Disp_Image    : Ipl_Image_Ptr;
                                            Max_Disparity : Integer;
                                            Param1        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param2        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param3        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param4        : Long_Float := Cv_Undef_Sc_Param_Lf;
                                            Param5        : Long_Float := Cv_Undef_Sc_Param_Lf);


   -- Epiline Functions --------------------------------------------------------
   type Cv_Stereo_Line_Coeff is record
      Xcoef    : Long_Float;
      Xcoef_A  : Long_Float;
      Xcoef_B  : Long_Float;
      Xcoef_Ab : Long_Float;

      Ycoef    : Long_Float;
      Ycoef_A  : Long_Float;
      Ycoef_B  : Long_Float;
      Ycoef_Ab : Long_Float;

      Zcoef    : Long_Float;
      Zcoef_A  : Long_Float;
      Zcoef_B  : Long_Float;
      Zcoef_Ab : Long_Float;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Stereo_Line_Coeff);
   type Cv_Stereo_Line_Coeff_Ptr is access all Cv_Stereo_Line_Coeff;



   type Cv_Camera is record
      Img_Size   : Cv_32f_Array (1 .. 2); -- size of the camera view, used during calibration
      Matrix     : Cv_32f_Array (1 .. 9); -- intinsic camera parameters:  [ fx 0 cx; 0 fy cy; 0 0 1 ]
      Distortion : Cv_32f_Array (1 .. 4); -- distortion coefficients - two coefficients for radial distortion
      -- and another two for tangential: [ k1 k2 p1 p2 ]

      Rot_Matr   : Cv_32f_Array (1 .. 9); -- rotation matrix and transition vector relatively
      Trans_Vect : Cv_32f_Array (1 .. 3); -- to some reference point in the space
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Camera);
   type Cv_Camera_Ptr is access all Cv_Camera;

   type Cv_Camera_Array is array (Integer range <>) of aliased Cv_Camera;
   type Cv_Camera_P_Array is array (Integer range <>) of aliased Cv_Camera_Ptr;
   subtype Cv_Camera_P_Array_2 is Cv_Camera_P_Array (1 .. 2);

   subtype Cv_Point_3d_32f_Array_2 is Cv_Point_3d_32f_Array (1 .. 2);

   type Cv_Point_2d_32f_Array_Axb is array (Integer range <>, Integer range <>) of aliased Cv_Point_2d_32f;
   subtype Cv_Point_2d_32f_Array_2x4 is Cv_Point_2d_32f_Array_Axb (1 .. 2, 1 .. 4);

   type Cv_64f_Array_Axbxc is array (Integer range <>, Integer range <>, Integer range <>) of aliased Long_Float;
   subtype Cv_64f_Array_2x3x3 is Cv_64f_Array_Axbxc (1 .. 2, 1 .. 3, 1 .. 3);


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
      Line_Coeffs       : Cv_Stereo_Line_Coeff_Ptr;
      Need_Swap_Cameras : Integer;
      Rot_Matrix        : Cv_32f_Array (1 .. 9);
      Trans_Vector      : Cv_32f_Array (1 .. 3);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Stereo_Camera);
   type Cv_Stereo_Camera_Ptr is access all Cv_Stereo_Camera;

   type Cv_Contour_Orientation is record
      Egvals  : Cv_32f_Array (1 .. 2);
      Egvects : Cv_32f_Array (1 .. 4);
      Max     : Float;
      Min     : Float;
      Imax    : Integer;
      Imin    : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Contour_Orientation);
   type Cv_Contour_Orientation_Ptr is access all Cv_Contour_Orientation;

   Cv_Camera_To_Warp : constant := 1;
   Cv_Warp_To_Camera : constant := 2;

   function Icv_Convert_Warp_Coordinates (Coeffs       : Cv_64f_Array_3x3;
                                          Camera_Point : Cv_Point_2d_32f_Ptr;
                                          Warp_Point   : Cv_Point_2d_32f_Ptr;
                                          Direction    : Integer)
                                          return Integer;

   function Icv_Get_Sympoint_3d (Point_Corner : Cv_Point_3d_64f;
                                 Point1       : Cv_Point_3d_64f;
                                 Point2       : Cv_Point_3d_64f;
                                 Point_Sym2   : Cv_Point_3d_64f_Ptr)
                                 return Integer;

   procedure Icv_Get_Piece_Length_3d (Point1 : Cv_Point_3d_64f;
                                      Point2 : Cv_Point_3d_64f;
                                      Dist   : access Long_Float);

   function Icv_Compute_3d_Point (Alpha  : Long_Float;
                                  Beta   : Long_Float;
                                  Coeffs : Cv_Stereo_Line_Coeff_Ptr;
                                  Point  : Cv_Point_3d_64f_Ptr)
                                  return Integer;

   function Icv_Create_Convert_Matr_Vect (Rot_Matr1        : Cv_64f_Array;
                                          Trans_Vect1      : Cv_64f_Array;
                                          Rot_Matr2        : Cv_64f_Array;
                                          Trans_Vect2      : Cv_64f_Array;
                                          Conv_Rot_Matr    : Cv_64f_Array;
                                          Conv_Transv_Vect : Cv_64f_Array)
                                          return Integer;

   function Icv_Convert_Point_System (M2         : Cv_Point_3d_64f;
                                      M1         : Cv_Point_3d_64f_Ptr;
                                      Rot_Matr   : Cv_64f_Array;
                                      Trans_Vect : Cv_64f_Array)
                                      return Integer;

   function Icv_Compute_Coeff_For_Stereo (Stereo_Camera : Cv_Stereo_Camera_Ptr)
                                          return Integer;

   function Icv_Get_Cross_Piece_Vector (P1_Start : Cv_Point_2d_32f;
                                        P1_End   : Cv_Point_2d_32f;
                                        V2_Start : Cv_Point_2d_32f;
                                        V2_End   : Cv_Point_2d_32f;
                                        Cross    : Cv_Point_2d_32f_Ptr)
                                        return Integer;

   function Icv_Get_Cross_Line_Direct (P1    : Cv_Point_2d_32f;
                                       P2    : Cv_Point_2d_32f;
                                       A     : Float;
                                       B     : Float;
                                       C     : Float;
                                       Cross : Cv_Point_2d_32f_Ptr)
                                       return Integer;

   function Icv_Define_Point_Position (Point1 : Cv_Point_2d_32f;
                                       Point2 : Cv_Point_2d_32f;
                                       Point  : Cv_Point_2d_32f)
                                       return Float;

   function Icv_Stereo_Calibration (Num_Images    : Integer;
                                    Nums          : Cv_32s_Array;
                                    Image_Size    : Cv_Size;
                                    Image_Points1 : Cv_Point_2d_32f;
                                    Image_Points2 : Cv_Point_2d_32f;
                                    Object_Points : Cv_Point_3d_32f_Ptr;
                                    Stereoparams  : Cv_Stereo_Camera_Ptr)
                                    return Integer;

   function Icv_Compute_Rest_Stereo_Params (Stereoparams : Cv_Stereo_Camera_Ptr)
                                            return Integer;

   procedure Cv_Compute_Perspective_Map (Coeffs     : Cv_64f_Array_3x3;
                                         Rect_Map_X : Cv_Arr_Ptr;
                                         Rect_Map_Y : Cv_Arr_Ptr);
   procedure Cv_Compute_Perspective_Map (Coeffs     : Cv_64f_Array_3x3;
                                         Rect_Map_X : Cv_Mat_Ptr;
                                         Rect_Map_Y : Cv_Mat_Ptr);
   procedure Cv_Compute_Perspective_Map (Coeffs     : Cv_64f_Array_3x3;
                                         Rect_Map_X : Ipl_Image_Ptr;
                                         Rect_Map_Y : Ipl_Image_Ptr);

   function Icv_Com_Coeff_For_Line (Point1           : Cv_Point_2d_64f;
                                    Point2           : Cv_Point_2d_64f;
                                    Point3           : Cv_Point_2d_64f;
                                    Point4           : Cv_Point_2d_64f;
                                    Can_Matr1        : Cv_64f_Array;
                                    Rot_Matr1        : Cv_64f_Array;
                                    Trans_Vect1      : Cv_64f_Array;
                                    Cam_Matr2        : Cv_64f_Array;
                                    Rot_Matr2        : Cv_64f_Array;
                                    Trans_Vect2      : Cv_64f_Array;
                                    Coeffs           : Cv_Stereo_Line_Coeff_Ptr;
                                    Need_Swap_Camera : access Integer)
                                    return Integer;

   function Icv_Get_Direction_For_Point (Point    : Cv_Point_2d_64f;
                                         Cam_Matr : Cv_64f_Array;
                                         Direct   : Cv_Point_3d_64f_Ptr)
                                         return Integer;

   function Icv_Get_Cross_Lines (Point11   : Cv_Point_3d_64f;
                                 Point12   : Cv_Point_3d_64f;
                                 Point21   : Cv_Point_3d_64f;
                                 Point22   : Cv_Point_3d_64f;
                                 Mid_Point : Cv_Point_3d_64f_Ptr)
                                 return Integer;

   function Icv_Compute_Stereo_Line_Coeffs (Point_A    : Cv_Point_3d_64f;
                                            Point_B    : Cv_Point_3d_64f;
                                            Point_Cam1 : Cv_Point_3d_64f;
                                            Gamma      : Long_Float;
                                            Coeffs     : Cv_Stereo_Line_Coeff_Ptr)
                                            return Integer;

   function Icv_Get_Angle_Line (Start_Point : Cv_Point_2d_64f;
                                Image_Size  : Cv_Size;
                                Point1      : Cv_Point_2d_64f_Ptr;
                                Point2      : Cv_Point_2d_64f_Ptr)
                                return Integer;

   procedure Icv_Get_Coef_For_Piece (P_Start : Cv_Point_2d_64f;
                                     P_End   : Cv_Point_2d_64f;
                                     A       : access Long_Float;
                                     B       : access Long_Float;
                                     C       : access Long_Float;
                                     Result  : access Integer);

   procedure Icv_Computee_Infinite_Project1 (Rot_Matr  : Cv_64f_Array;
                                             Cam_Matr1 : Cv_64f_Array;
                                             Cam_Matr2 : Cv_64f_Array;
                                             Point1    : Cv_Point_2d_32f;
                                             Point2    : Cv_Point_2d_32f_Ptr);

   procedure Icv_Computee_Infinite_Project2 (Rot_Matr  : Cv_64f_Array;
                                             Cam_Matr1 : Cv_64f_Array;
                                             Cam_Matr2 : Cv_64f_Array;
                                             Point1    : Cv_Point_2d_32f_Ptr;
                                             Point2    : Cv_Point_2d_32f);

   procedure Icv_Get_Cross_Direct_Direct (Direct1 : Cv_64f_Array;
                                          Direct2 : Cv_64f_Array;
                                          Cross   : Cv_Point_2d_64f_Ptr;
                                          Result  : access Integer);

   procedure Icv_Get_Cross_Piece_Direct (P_Start : Cv_Point_2d_64f;
                                         P_End   : Cv_Point_2d_64f;
                                         A       : Long_Float;
                                         B       : Long_Float;
                                         C       : Long_Float;
                                         Cross   : Cv_Point_2d_64f_Ptr;
                                         Result  : access Integer);

   procedure Icv_Get_Cross_Piece_Piece (P1_Start : Cv_Point_2d_64f;
                                        P1_End   : Cv_Point_2d_64f;
                                        P2_Start : Cv_Point_2d_64f;
                                        P2_End   : Cv_Point_2d_64f;
                                        Cross    : Cv_Point_2d_64f_Ptr;
                                        Result   : access Integer);

   procedure Icv_Get_Piece_Length (Point1 : Cv_Point_2d_64f;
                                   Point2 : Cv_Point_2d_64f;
                                   Dist   : access Long_Float);

   procedure Icv_Get_Cross_Rect_Direct (Image_Size  : Cv_Size;
                                        A           : Long_Float;
                                        B           : Long_Float;
                                        C           : Long_Float;
                                        Point_Start : Cv_Point_2d_64f_Ptr;
                                        Point_End   : Cv_Point_2d_64f_Ptr;
                                        Result      : access Integer);

   procedure Icv_Project_Point_To_Image (Point      : Cv_Point_3d_64f;
                                         Cam_Matr   : Cv_64f_Array;
                                         Rot_Matr   : Cv_64f_Array;
                                         Trans_Vect : Cv_64f_Array;
                                         Proj_Point : Cv_Point_2d_64f_Ptr);

   procedure Icv_Get_Quads_Transform (Image_Size   : Cv_Size;
                                      Cam_Matr1    : Cv_64f_Array;
                                      Rot_Matr1    : Cv_64f_Array;
                                      Trans_Vect1  : Cv_64f_Array;
                                      Cam_Matr2    : Cv_64f_Array;
                                      Rot_Matr2    : Cv_64f_Array;
                                      Trans_Vect2  : Cv_64f_Array;
                                      Warp_Size    : Cv_Size_Ptr;
                                      Quad1        : Cv_64f_Array_4x2;
                                      Quad2        : Cv_64f_Array_4x2;
                                      Fund_Matr    : Cv_64f_Array;
                                      Epipole1     : Cv_Point_3d_64f_Ptr;
                                      Epipole2     : Cv_Point_3d_64f_Ptr);

   procedure Icv_Get_Quads_Transform_Struct (Stereo_Camera : Cv_Stereo_Camera_Ptr);

   procedure Icv_Compute_Stereo_Params_For_Cameras (Stereo_Camera : Cv_Stereo_Camera_Ptr);

   procedure Icv_Get_Cut_Piece (Area_Line_Coef1 : Cv_64f_Array;
                                Area_Line_Coef2 : Cv_64f_Array;
                                Epipole         : Cv_Point_2d_64f;
                                Image_Size      : Cv_Size;
                                Point11         : Cv_Point_2d_64f_Ptr;
                                Point12         : Cv_Point_2d_64f_Ptr;
                                Point21         : Cv_Point_2d_64f_Ptr;
                                Point22         : Cv_Point_2d_64f_Ptr;
                                Result          : access Integer);

   procedure Icv_Get_Middle_Angle_Point (Base_Point : Cv_Point_2d_64f;
                                         Point1     : Cv_Point_2d_64f;
                                         Point2     : Cv_Point_2d_64f;
                                         Mid_Point  : Cv_Point_2d_64f_Ptr);

   procedure Icv_Get_Normal_Direct (Direct      : Cv_64f_Array;
                                    Point       : Cv_Point_2d_64f;
                                    Norm_Direct : Cv_64f_Array);

   function Icv_Get_Vect (Base_Point : Cv_Point_2d_64f;
                          Point1     : Cv_Point_2d_64f;
                          Point      : Cv_Point_2d_64f)
                          return Long_Float;

   procedure Icv_Project_Point_To_Direct (Point         : Cv_Point_2d_64f;
                                          Line_Coeff    : Cv_64f_Array;
                                          Project_Point : Cv_Point_2d_64f_Ptr);

   procedure Icv_Get_Distance_From_Point_To_Direct (Point     : Cv_Point_2d_64f;
                                                    Line_Coef : Cv_64f_Array;
                                                    Dist      : access Long_Float);

   function Icv_Create_Isometric_Image (Src                  : Ipl_Image_Ptr;
                                        Dst                  : Ipl_Image_Ptr;
                                        Desired_Depth        : Unsigned_32;
                                        Desired_Num_Channels : Integer)
                                        return Ipl_Image_Ptr;

   procedure Cv_De_Interlace (Frame      : Cv_Arr_Ptr;
                              Field_Even : Cv_Arr_Ptr;
                              Field_Odd  : Cv_Arr_Ptr);
   procedure Cv_De_Interlace (Frame      : Cv_Mat_Ptr;
                              Field_Even : Cv_Mat_Ptr;
                              Field_Odd  : Cv_Mat_Ptr);
   procedure Cv_De_Interlace (Frame      : Ipl_Image_Ptr;
                              Field_Even : Ipl_Image_Ptr;
                              Field_Odd  : Ipl_Image_Ptr);

   -- Contour Tree -------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Contour_Tree is record
   -- CV_TREE_NODE_FIELDS
      Flags        : Integer;
      Header_Size  : Integer;
      H_Prev       : Cv_Seq_Ptr;
      H_Next       : Cv_Seq_Ptr;
      V_Prev       : Cv_Seq_Ptr;
      V_Next       : Cv_Seq_Ptr;

      -- CV_SEQUENCE_FIELDS
      Total        : Integer;
      Elem_Size    : Integer;
      Block_Max    : Cv_Void_Ptr;
      Ptr          : Cv_Void_Ptr;
      Delta_Elems  : Integer;
      Storage      : Cv_Mem_Storage_Ptr;
      Free_Blocks  : Cv_Seq_Block_Ptr;
      First        : Cv_Seq_Block_Ptr;

      P1           : Cv_Point;
      P2           : Cv_Point;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Contour_Tree);

   type Cv_Contour_Tree_Ptr is access all cv_Contour_Tree;

   --     Creates a hierarchical representation of a contour.
   function Cv_Create_Contour_Tree (Contour   : Cv_Seq_Ptr;
                                    Storage   : Cv_Mem_Storage_Ptr;
                                    Threshold : Long_Float) return Cv_Contour_Tree_Ptr;

   --     Reconstruct (completelly or partially) contour a from contour tree
   function Cv_Contour_From_Contour_Tree (Tree     : Cv_Contour_Tree_Ptr;
                                          Storage  : Cv_Mem_Storage_Ptr;
                                          Criteria : Cv_Term_Criteria)
                                          return Cv_Seq_Ptr;

   Cv_Contour_Trees_Match_I1 : constant := 1;

   --     Compares two contours using their tree representations.
   function Cv_Match_Contour_Trees (Tree1     : Cv_Contour_Tree_Ptr;
                                    Tree2     : Cv_Contour_Tree_Ptr;
                                    Method    : Integer;
                                    Threshold : Long_Float) return Long_Float;

   -- Contour Morphing ---------------------------------------------------------
   -----------------------------------------------------------------------------

   --     finds correspondence between two contours
   function Cv_Calc_Contours_Correspondence (Contour1 : Cv_Seq_Ptr;
                                             Contour2 : Cv_Seq_Ptr;
                                             Storage  : Cv_Mem_Storage_Ptr)
                                             return Cv_Seq_Ptr;

   --     morphs contours using the pre-calculated correspondence:
   --     alpha=0 ~ contour1, alpha=1 ~ contour2
   function Cv_Morph_Contours (Contour1 : Cv_Seq_Ptr;
                               Contour2 : Cv_Seq_Ptr;
                               Corr     : Cv_Seq_Ptr;
                               Alpha    : Long_Float;
                               Storage  : Cv_Mem_Storage_Ptr)
                               return Cv_Seq_Ptr;

   Cv_Value : constant := 1;
   Cv_Array : constant := 2;

   --     Changes the contour position to minimize its energy.
   procedure Cv_Snake_Image (Image        : Ipl_Image_Ptr;
                             Points       : Cv_Point_Array;
                             Length       : Integer;
                             Alpha        : Cv_32f_Array;
                             Beta         : Cv_32f_Array;
                             Gamma        : Cv_32f_Array;
                             Coeffusage   : Integer;
                             Win          : Cv_Size;
                             Criteria     : Cv_Term_Criteria;
                             Calcgradient : Integer := 1);

   -- Texture Descriptors ------------------------------------------------------
   -----------------------------------------------------------------------------

   Cv_Glcm_Optimization_None      : constant  := -2;
   Cv_Glcm_Optimization_Lut       : constant := -1;
   Cv_Glcm_Optimization_Histogram : constant := 0;

   Cv_Glcmdesc_Optimization_Allowdoublenest : constant := 10;
   Cv_Glcmdesc_Optimization_Allowtriplenest : constant := 11;
   Cv_Glcmdesc_Optimization_Histogram       : constant := 4;

   Cv_Glcmdesc_Entropy            : constant := 0;
   Cv_Glcmdesc_Energy             : constant := 1;
   Cv_Glcmdesc_Homogenity         : constant := 2;
   Cv_Glcmdesc_Contrast           : constant := 3;
   Cv_Glcmdesc_Clustertendency    : constant := 4;
   Cv_Glcmdesc_Clustershade       : constant := 5;
   Cv_Glcmdesc_Correlation        : constant := 6;
   Cv_Glcmdesc_Correlationinfo1   : constant := 7;
   Cv_Glcmdesc_Correlationinfo2   : constant := 8;
   Cv_Glcmdesc_Maximumprobability : constant := 9;

   Cv_Glcm_All  : constant := 0;
   Cv_Glcm_Glcm : constant := 1;
   Cv_Glcm_Desc : constant := 2;

   Cv_Max_Num_Grey_Levels_8u : constant := 256;

   type Cv_Glcm is record
      Matrix_Side_Length           : Integer;
      Num_Matrices                 : Integer;
      Matrices                     : access Cv_64f_Pointer_Array_P; -- double***

      Num_Lookup_Table_Elements    : Integer;
      Forward_Lookup_Table         : Cv_32s_Array (1 .. Cv_Max_Num_Grey_Levels_8u);
      Reverse_Lookup_Table         : Cv_32s_Array (1 .. Cv_Max_Num_Grey_Levels_8u);
      Descriptors                  : Cv_64f_Pointer_Array_P; -- double**
      Num_Descriptors              : Integer;
      Descriptor_Optimization_Type : Integer;
      Optimization_Type            : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Glcm);
   type Cv_Glcm_Ptr is access all Cv_Glcm;

   function Cv_Create_Glcm (Src_Image           : Ipl_Image_Ptr;
                            Step_Magnitude      : Integer;
                            Step_Direction      : Cv_32s_Pointer := null;
                            Num_Step_Directions : Integer := 0;
                            Optimization_Type   : Integer := Cv_Glcm_Optimization_None)
                            return Cv_Glcm_Ptr;

   procedure Cv_Release_Glcm (Glcm : access Cv_Glcm_Ptr;
                              Flag : Integer := Cv_Glcm_All);

   procedure Cv_Create_Glcm_Descriptors (Dest_Glcm                    : Cv_Glcm_Ptr;
                                         Descriptor_Optimization_Type : Integer := Cv_Glcmdesc_Optimization_Allowdoublenest);

   function Cv_Get_Glcm_Descriptor (Glcm       : Cv_Glcm_Ptr;
                                    Step       : Integer;
                                    Descriptor : Integer)
                                    return Long_Float;

   procedure Cv_Get_Glcm_Descriptor_Statistics (Glcm               : Cv_Glcm_Ptr;
                                                Descriptor         : Integer;
                                                Average            : access Long_Float;
                                                Standard_Deviation : access Long_Float);

   function Cv_Create_Glcm_Image (Glcm : Cv_Glcm_Ptr;
                                  Step : Integer)
                                  return Ipl_Image_Ptr;

   -- Face, eyes and mouth tracking --------------------------------------------
   -----------------------------------------------------------------------------

   Num_Face_Elements : constant := 3;

   type Cv_Face_Tracker is null record; -- Locally declared class in .cpp file
   pragma Convention (C_Pass_By_Copy, Cv_Face_Tracker);
   type Cv_Face_Tracker_Ptr is access all Cv_Face_Tracker;

   type Cv_Face_Elements is new Integer;
   Cv_Face_Mouth : constant Cv_Face_Elements := 0;
   Cv_Face_Left_Eye : constant Cv_Face_Elements := 1;
   Cv_Face_Right_Eye : constant Cv_Face_Elements := 2;

   function Cv_Init_Face_Tracker (P_Face_Tracking : Cv_Face_Tracker_Ptr;
                                  Img_Gray        : Ipl_Image_Ptr;
                                  P_Rects         : Cv_Rect_Array;
                                  N_Rects         : Integer)
                                  return Cv_Face_Tracker_Ptr;

   function Cv_Track_Face (P_Face_Tracker  : Cv_Face_Tracker_Ptr;
                           Img_Gray        : Ipl_Image_Ptr;
                           P_Rects         : Cv_Rect_Array;
                           N_Rects         : Integer;
                           Pt_Rotate       : Cv_Point_Ptr;
                           Db_Angle_Rotate : access Long_Float)
                           return Integer;

   procedure Cv_Release_Face_Tracker (Pp_Face_Tracker : access Cv_Face_Tracker_Ptr);

   type Cv_Face_Data is record
      Mouth_Rect     : Cv_Rect;
      Left_Eye_Rect  : Cv_Rect;
      Right_Eye_Rect : Cv_Rect;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Face_Data);

   function Cv_Find_Face (Image   : Ipl_Image_Ptr;
                          Storage : Cv_Mem_Storage_Ptr)
                          return Cv_Seq_Ptr;

   function Cv_Post_Boosting_Find_Face (Image   : Ipl_Image_Ptr;
                                        Storage : Cv_Mem_Storage_Ptr)
                                        return Cv_Seq_Ptr;

   -- 3D Tracker ---------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Bool is new Interfaces.C.Unsigned_Char;

   type Cv_3d_Tracker_2d_Tracked_Object is record
      Id : Integer;
      P  : Cv_Point_2d_32f;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_3d_Tracker_2d_Tracked_Object);
   type Cv_3d_Tracker_2d_Tracked_Object_Ptr is access all Cv_3d_Tracker_2d_Tracked_Object;
   type Cv_3d_Tracker_2d_Tracked_Object_Array is array (Integer range <>) of aliased Cv_3d_Tracker_2d_Tracked_Object;

   function Cv_Create_3d_Tracker_2d_Tracked_Object (Id : Integer;
                                                    P  : Cv_Point_2d_32f)
                                                    return Cv_3d_Tracker_2d_Tracked_Object;

   type Cv_3d_Tracker_Tracked_Object is record
      Id : Integer;
      P  : Cv_Point_3d_32f;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_3d_Tracker_Tracked_Object);
   type Cv_3d_Tracker_Tracked_Object_Ptr is access all cv_3d_Tracker_Tracked_Object;
   type Cv_3d_Tracker_Tracked_Object_Array is array (Integer range <>) of aliased Cv_3d_Tracker_Tracked_Object;

   function Cv_Create_3d_Tracker_Tracked_Object (Id : Integer;
                                                 P  : Cv_Point_3d_32f)
                                                 return Cv_3d_Tracker_Tracked_Object;

   type Cv_3d_Tracker_Camera_Info is record
      Valid           : Cv_Bool;
      Mat             : Cv_32f_2d_Array (1 .. 4, 1 .. 4);
      Principal_Point : Cv_Point_2d_32f;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_3d_Tracker_Camera_Info);
   type Cv_3d_Tracker_Camera_Info_Ptr is access all cv_3d_Tracker_Camera_Info;
   type Cv_3d_Tracker_Camera_Info_Array is array (Integer range <>) of aliased Cv_3d_Tracker_Camera_Info;

   type Cv_3d_Tracker_Camera_Intrinsics is record
      Principal_Point : Cv_Point_2d_32f;
      Focal_Length    : Cv_32f_Array (1 .. 2);
      Distortion      : Cv_32f_Array (1 .. 4);
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_3d_Tracker_Camera_Intrinsics);
   type Cv_3d_Tracker_Camera_Intrinsics_Ptr is access all Cv_3d_Tracker_Camera_Intrinsics;
   type Cv_3d_Tracker_Camera_Intrinsics_Array is array (Integer range <>) of aliased Cv_3d_Tracker_Camera_Intrinsics;

   function Cv_3d_Tracker_Calibrate_Cameras (Num_Cameras       : Integer;
                                             Camera_Intrinsics : Cv_3d_Tracker_Camera_Intrinsics_Array; -- size = num_cameras
                                             Etalon_Size       : Cv_Size;
                                             Square_Size       : Float;
                                             Samples           : Ipl_Image_P_Array; -- size = num_cameras
                                             Camera_Info       : Cv_3d_Tracker_Camera_Info_Array) -- size = num_cameras
                                             return Cv_Bool;

   function Cv_3d_Tracker_Locate_Objects (Num_Cameras     : Integer;
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
   Cv_Lee_Int    : constant := 0;
   Cv_Lee_Float  : constant := 1;
   Cv_Lee_Double : constant := 2;
   Cv_Lee_Auto   : constant := -1;
   Cv_Lee_Erode  : constant := 0;
   Cv_Lee_Zoom   : constant := 1;
   Cv_Lee_Non    : constant := 2;

   Cv_Lee_Int_F    : constant := 0.0;
   Cv_Lee_Float_F  : constant := 1.0;
   Cv_Lee_Double_F : constant := 2.0;
   Cv_Lee_Auto_F   : constant := -1.0;
   Cv_Lee_Erode_F  : constant := 0.0;
   Cv_Lee_Zoom_F   : constant := 1.0;
   Cv_Lee_Non_F    : constant := 2.0;

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
   type Cv_Voronoi_Site_2d_Ptr is access all cv_Voronoi_Site_2d;
   type Cv_Voronoi_Site_2d_Array is array (Integer range <>) of aliased Cv_Voronoi_Site_2d;
   type Cv_Voronoi_Site_2d_Array_2 is array (Integer range 1 .. 2) of aliased Cv_Voronoi_Site_2d;

   type Cv_Voronoi_Edge_2d is record
   -- CV_VORONOIEDGE2D_FIELDS()
      Node : access Cv_Voronoi_Node_2d_Array_2;
      Site : access Cv_Voronoi_Site_2d_Array_2;
      Next : access Cv_Voronoi_Edge_2d_Array_4;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Voronoi_Edge_2d);
   type Cv_Voronoi_Edge_2d_Ptr is access all Cv_Voronoi_Edge_2d;
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
   type Cv_Voronoi_Node_2d_Ptr is access all Cv_Voronoi_Node_2d;
   type Cv_Voronoi_Node_2d_Array is array (Integer range <>) of aliased Cv_Voronoi_Node_2d;
   type Cv_Voronoi_Node_2d_Array_2 is array (Integer range 1 .. 2) of aliased Cv_Voronoi_Node_2d;


   type Cv_Voronoi_Diagram_2d is record
   -- CV_VORONOIDIAGRAM2D_FIELDS()
      Flags           : Integer;       --CV_TREE_NODE_FIELDS(CvSeq);
      Headersize      : Integer;
      Hprev           : Cv_Seq_Ptr;
      Hnext           : Cv_Seq_Ptr;
      Vprev           : Cv_Seq_Ptr;
      Vnext           : Cv_Seq_Ptr;

      Total           : Integer; --CV_SEQUENCE_FIELDS
      Elemsize        : Integer;
      Blockmax        : Cv_Void_Ptr;
      Ptr             : Cv_Void_Ptr;
      Deltaelems      : Integer;
      Storage         : Cv_Mem_Storage_Ptr;
      Freeblocks      : Cv_Seq_Block_Ptr;
      First           : Cv_Seq_Block_Ptr;

      Free_Elems      : Cv_Set_Elem_Pointer;
      Active_Count    : Integer;
      Edges           : Cv_Set_Pointer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Voronoi_Diagram_2d);
   type Cv_Voronoi_Diagram_2d_Ptr is access all Cv_Voronoi_Diagram_2d;
   type Cv_Voronoi_Diagram_2d_Array is array (Integer range <>) of aliased Cv_Voronoi_Diagram_2d;

   --     Computes Voronoi Diagram for given polygons with holes
   function Cv_Voronoi_Diagram_From_Contour (Contour_Seq         : Cv_Seq_Ptr;
                                             Voronoi_Diagram     : access Cv_Voronoi_Diagram_2d_Ptr;
                                             Voronoi_Storage     : Cv_Mem_Storage_Ptr;
                                             Contour_Type        : Integer := Cv_Lee_Int;
                                             Contour_Orientation : Integer := -1;
                                             Attempt_Number      : Integer := 10)
                                             return Integer;

   --     Computes Voronoi Diagram for domains in given image
   function Cv_Voronoi_Diagram_From_Image (P_Image               : Ipl_Image_Ptr;
                                           Contour_Seq           : access Cv_Seq_Ptr;
                                           Voronoi_Diagram       : access Cv_Voronoi_Diagram_2d_Ptr;
                                           Voronoi_Storage       : Cv_Mem_Storage_Ptr;
                                           Regularization_Method : Integer := Cv_Lee_Non;
                                           Approx_Precision      : Float := Cv_Lee_Auto_F)
                                           return Integer;

   --     Deallocates the storage
   procedure Cv_Release_Voronoi_Storage (Voronoi_Diagram   : Cv_Voronoi_Diagram_2d_Ptr;
                                         P_Voronoi_Storage : access Cv_Mem_Storage_Ptr);


   -- Linear-Contour Model -----------------------------------------------------
   type Cv_Lcm_Edge is record
      Flags  : Integer;
      Weight : Float;
      Next   : Cv_Graph_Edge_P_Array;
      Vtx    : Cv_Graph_Vtx_P_Array;
      Chain  : Cv_Seq_Ptr;
      Width  : Float;
      Index1 : Integer;
      Index2 : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Lcm_Edge);

   type Cv_Lcm_Node is record
      Flags   : Integer;
      First   : Cv_Graph_Edge_Ptr;
      Contour : Cv_Contour_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Lcm_Node);

   --     Computes hybrid model from Voronoi Diagram
   function Cv_Linear_Contor_Model_From_Voronoi_Diagram (Voronoi_Diagram : Cv_Voronoi_Diagram_2d_Ptr;
                                                         Max_Width       : Float)
                                                         return Cv_Graph_P;

   --     Releases hybrid model storage
   procedure Cv_Release_Linear_Contor_Model_Storage (Graph : access Cv_Graph_P);

   subtype Cv_Point_2d_32f_Array_4 is Cv_Point_2d_32f_Array (1 .. 4);

   procedure Cv_Init_Perspective_Transform (Size     : Cv_Size;
                                            Vertex   : Cv_Point_2d_32f_Array_4;
                                            Matrix   : Cv_64f_Array_3x3;
                                            Rect_Map : Cv_Arr_Ptr);
   procedure Cv_Init_Perspective_Transform (Size     : Cv_Size;
                                            Vertex   : Cv_Point_2d_32f_Array_4;
                                            Matrix   : Cv_64f_Array_3x3;
                                            Rect_Map : Cv_Mat_Ptr);
   procedure Cv_Init_Perspective_Transform (Size     : Cv_Size;
                                            Vertex   : Cv_Point_2d_32f_Array_4;
                                            Matrix   : Cv_64f_Array_3x3;
                                            Rect_Map : Ipl_Image_Ptr);

   -- View Morphing Functions --------------------------------------------------
   --     The order of the function corresponds to the order they should appear
   --     in the view morphing pipeline

   --     Finds ending points of scanlines on left and right images of
   --     stereo-pair
   procedure Cv_Make_Scanlines (Matrix     : Cv_Matrix_3;
                                Img_Size   : Cv_Size;
                                Scanlines1 : Cv_32s_Pointer;
                                Scanlines2 : Cv_32s_Pointer;
                                Lengths1   : Cv_32s_Pointer;
                                Lengths2   : Cv_32s_Pointer;
                                Line_Count : access Integer);

   --     Grab pixel values from scanlines and stores them sequentially
   --     (some sort of perspective image transform)
   procedure Cv_Pre_Warp_Image (Line_Count : Integer;
                                Img        : Ipl_Image_Ptr;
                                Dst        : Cv_8u_Pointer;
                                Dst_Nums   : Cv_32s_Pointer;
                                Scan_Lines : Cv_32s_Pointer);

   --     Approximate each grabbed scanline by a sequence of runs
   --     (lossy run-length compression)
   procedure Cv_Find_Runs (Line_Count    : Integer;
                           Prewarp1      : Cv_8u_Pointer;
                           Prewarp2      : Cv_8u_Pointer;
                           Line_Lengths1 : Cv_32s_Pointer;
                           Line_Lengths2 : Cv_32s_Pointer;
                           Runs1         : Cv_32s_Pointer;
                           Runs2         : Cv_32s_Pointer;
                           Num_Runs1     : Cv_32s_Pointer;
                           Num_Runs2     : Cv_32s_Pointer);

   --     Compares two sets of compressed scanlines
   procedure Cv_Dynamic_Correspond_Multi (Line_Count  : Integer;
                                          First       : Cv_32s_Pointer;
                                          First_Runs  : Cv_32s_Pointer;
                                          Second      : Cv_32s_Pointer;
                                          Second_Runs : Cv_32s_Pointer;
                                          First_Corr  : Cv_32s_Pointer;
                                          Second_Corr : Cv_32s_Pointer);

   --     Finds scanline ending coordinates for some intermediate "virtual"
   --     camera position
   procedure Cv_Make_Alpha_Scanlines (Scanlines1  : Cv_32s_Pointer;
                                      Scanlines2  : Cv_32s_Pointer;
                                      Scanlines_A : Cv_32s_Pointer;
                                      Lengths     : Cv_32s_Pointer;
                                      Line_Count  : Integer;
                                      Alpha       : Float);

   --     Blends data of the left and right image scanlines to get
   --     pixel values of "virtual" image scanlines
   procedure Cv_Morph_Epilines_Multi (Line_Count  : Integer;
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
   procedure Cv_Post_Warp_Image (Line_Count : Integer;
                                 Src        : Cv_8u_Pointer;
                                 Src_Nums   : Cv_32s_Pointer;
                                 Img        : Ipl_Image_Ptr;
                                 Scanlines  : Cv_32s_Pointer);

   --     Deletes Moire (missed pixels that appear due to discretization)
   procedure Cv_Delete_Moire (Img : Ipl_Image_Ptr);

   -- ConDenstation state.
   type Cv_Con_Densation is
      record
         Mp           : Integer;
         Dp           : Integer;
         Dynammatr    : Cv_32f_Array_P;
         State        : Cv_32f_Array_P;
         Samplesnum   : Integer;
         Flsamples    : access Cv_32f_Array_P;
         Flnewsamples : access Cv_32f_Array_P;
         Flconfidance : Cv_32f_Array_P;
         Flcumulative : Cv_32f_Array_P;
         Temp         : Cv_32f_Array_P;
         Randomsample : Cv_32f_Array_P;
         Rands        : Cv_Rand_State;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_Con_Densation);

   type Cv_Con_Densation_Ptr is access all Cv_Con_Densation;

   --     Allocates the ConDensation filter structure.
   function Cv_Create_Condensation (Dynamparams   : Integer;
                                    Measureparams : Integer;
                                    Samplecount   : Integer) return Cv_Con_Densation_Ptr;

   --     Initializes the sample set for the ConDensation algorithm.
   procedure Cv_Condens_Init_Sample_Set (Condens    : Cv_Con_Densation_Ptr;
                                         Lowerbound : Cv_Mat_Ptr;
                                         Upperbound : Cv_Mat_Ptr);

   --     Updates ConDensation filter by time (predict future state of the system)
   procedure Cv_Condens_Update_By_Time (Condens : Cv_Con_Densation_Ptr);

   function Ipl_Width (Img : Ipl_Image_Ptr)
                       return Integer;

   function Ipl_Height (Img : Ipl_Image_Ptr)
                        return Integer;

private

   pragma Import (C, Cv_Segment_Image, "cvSegmentImage");
   pragma Import (C, Cv_Calc_Covar_Matrix_Ex, "cvCalcCovarMatrixEx");
   pragma Import (C, Cv_Calc_Eigen_Objects, "cvCalcEigenObjects");
   pragma Import (C, Cv_Calc_Decomp_Coeff, "cvCalcDecompCoeff");
   pragma Import (C, Cv_Eigen_Decomposite, "CvEigenDecomposite");
   pragma Import (C, Cv_Eigen_Projection, "cvEigenProjection");
   pragma Import (C, Cv_Create_2d_Hmm, "cvCreate2DHMM");
   pragma Import (C, Cv_Release_2d_Hmm, "cvRelease2DHMM");
   pragma Import (C, Cv_Create_Obs_Info, "cvCreateObsInfo");
   pragma Import (C, Cv_Release_Obs_Info, "cvReleaseObsInfo");
   pragma Import (C, Cv_Img_To_Obs_Dct, "cvImgToObs_DCT");
   pragma Import (C, Cv_Uniform_Img_Segm, "cvUniformImgSegm");
   pragma Import (C, Cv_Init_Mix_Segm, "cvInitMixSegm");
   pragma Import (C, Cv_Estimate_Hmm_State_Params, "cvEstimateHMMStateParams");
   pragma Import (C, Cv_Estimate_Trans_Prob, "cvEstimateTransProb");
   pragma Import (C, Cv_Estimate_Obs_Prob, "cvEstimateObsProb");
   pragma Import (C, Cv_E_Viterbi, "cvEViterbi");
   pragma Import (C, Cv_Mix_Segm_L2, "cvMixSegmL2");
   pragma Import (C, Cv_Create_Hand_Mask, "cvCreateHandMask");
   pragma Import (C, Cv_Find_Hand_Region, "cvFindHandRegion");
   pragma Import (C, Cv_Find_Hand_Region_A, "cvFindHandRegionA");
   pragma Import (C, Cv_Calc_Image_Homography, "cvCalcImageHomography");
   pragma Import (C, Icv_Draw_Mosaic, "icvDrawMosaic");
   pragma Import (C, Icv_Subdiv_2d_Check, "icvSubdiv2DCheck");
   pragma Import (C, Icv_Sq_Dist_2d_32f, "icvSqDist2D32f");
   pragma Import (C, Cv_Calc_Pgh, "cvCalcPGH");
   pragma Import (C, Cv_Find_Dominant_Points, "cvFindDominantPoints");
   pragma Import (C, Icv_Convert_Warp_Coordinates, "icvConvertWarpCoordinates");
   pragma Import (C, Icv_Get_Sympoint_3d, "icvGetSymPoint3D");
   pragma Import (C, Icv_Get_Piece_Length_3d, "icvGetPieceLength3D");
   pragma Import (C, Icv_Compute_3d_Point, "icvCompute3DPoint");
   pragma Import (C, Icv_Create_Convert_Matr_Vect, "icvCreateConvertMatrVect");
   pragma Import (C, Icv_Convert_Point_System, "icvConvertPointSystem");
   pragma Import (C, Icv_Compute_Coeff_For_Stereo, "icvComputeCoeffForStereo");
   pragma Import (C, Icv_Get_Cross_Piece_Vector, "icvGetCrossPieceVector");
   pragma Import (C, Icv_Get_Cross_Line_Direct, "icvGetCrossLineDirect");
   pragma Import (C, Icv_Define_Point_Position, "icvDefinePointPosition");
   pragma Import (C, Icv_Stereo_Calibration, "icvStereoCalibration");
   pragma Import (C, Icv_Compute_Rest_Stereo_Params, "icvComputeRestStereoParams");
   pragma Import (C, Cv_Compute_Perspective_Map, "cvComputePerspectiveMap");
   pragma Import (C, Icv_Com_Coeff_For_Line, "icvComCoeffForLine");
   pragma Import (C, Icv_Get_Direction_For_Point, "icvGetDirectionForPoint");
   pragma Import (C, Icv_Get_Cross_Lines, "icvGetCrossLines");
   pragma Import (C, Icv_Compute_Stereo_Line_Coeffs, "icvComputeStereoLineCoeffs");
   pragma Import (C, Icv_Get_Angle_Line, "icvGetAngleLine");
   pragma Import (C, Icv_Get_Coef_For_Piece, "icvGetCoefForPiece");
   pragma Import (C, Icv_Computee_Infinite_Project1, "icvComputeeInfiniteProject1");
   pragma Import (C, Icv_Computee_Infinite_Project2, "icvComputeeInfiniteProject2");
   pragma Import (C, Icv_Get_Cross_Direct_Direct, "icvGetCrossDirectDirect");
   pragma Import (C, Icv_Get_Cross_Piece_Direct, "icvGetCrossPieceDirect");
   pragma Import (C, Icv_Get_Cross_Piece_Piece, "icvGetCrossPiecePiece");
   pragma Import (C, Icv_Get_Piece_Length, "icvGetPieceLength");
   pragma Import (C, Icv_Get_Cross_Rect_Direct, "icvGetCrossRectDirect");
   pragma Import (C, Icv_Project_Point_To_Image, "icvProjectPointToImage");
   pragma Import (C, Icv_Get_Quads_Transform, "icvGetQuadsTransform");
   pragma Import (C, Icv_Get_Quads_Transform_Struct, "icvGetQuadsTransformStruct");
   pragma Import (C, Icv_Compute_Stereo_Params_For_Cameras, "icvComputeStereoParamsForCameras");
   pragma Import (C, Icv_Get_Cut_Piece, "icvGetCutPiece");
   pragma Import (C, Icv_Get_Middle_Angle_Point, "icvGetMiddleAnglePoint");
   pragma Import (C, Icv_Get_Normal_Direct, "icvGetNormalDirect");
   pragma Import (C, Icv_Get_Vect, "icvGetVect");
   pragma Import (C, Icv_Project_Point_To_Direct, "icvProjectPointToDirect");
   pragma Import (C, Icv_Get_Distance_From_Point_To_Direct, "icvGetDistanceFromPointToDirect");
   pragma Import (C, Icv_Create_Isometric_Image, "icvCreateIsometricImage");
   pragma Import (C, Cv_De_Interlace, "cvDeInterlace");
   pragma Import (C, Cv_Create_Contour_Tree, "cvCreateContourTree");
   pragma Import (C, Cv_Contour_From_Contour_Tree, "cvContourFromContourTree");
   pragma Import (C, Cv_Match_Contour_Trees, "cvMatchContourTrees");
   pragma Import (C, Cv_Calc_Contours_Correspondence, "cvCalcContoursCorrespondence");
   pragma Import (C, Cv_Morph_Contours, "cvMorphContours");
   pragma Import (C, Cv_Snake_Image, "cvSnakeImage");
   pragma Import (C, Cv_Create_Glcm, "cvCreateGLCM");
   pragma Import (C, Cv_Release_Glcm, "cvReleaseGLCM");
   pragma Import (C, Cv_Create_Glcm_Descriptors, "cvCreateGLCMDescriptors");
   pragma Import (C, Cv_Get_Glcm_Descriptor, "cvGetGLCMDescriptor");
   pragma Import (C, Cv_Get_Glcm_Descriptor_Statistics, "cvGetGLCMDescriptorStatistics");
   pragma Import (C, Cv_Create_Glcm_Image, "cvCreateGLCMImage");
   pragma Import (C, Cv_Init_Face_Tracker, "cvInitFaceTracker");
   pragma Import (C, Cv_Track_Face, "cvTrackFace");
   pragma Import (C, Cv_Release_Face_Tracker, "cvReleaseFaceTracker");
   pragma Import (C, Cv_Find_Face, "cvFindFace");
   pragma Import (C, Cv_Post_Boosting_Find_Face, "cvPostBoostingFindFace");
   pragma Import (C, Cv_Create_3d_Tracker_2d_Tracked_Object, "cv3dTracker2dTrackedObject");
   pragma Import (C, Cv_Create_3d_Tracker_Tracked_Object, "cv3dTrackerTrackedObject");
   pragma Import (C, Cv_3d_Tracker_Calibrate_Cameras, "cv3dTrackerCalibrateCameras");
   pragma Import (C, Cv_3d_Tracker_Locate_Objects, "cv3dTrackerLocateObjects");
   pragma Import (C, Cv_Linear_Contor_Model_From_Voronoi_Diagram, "cvLinearContorModelFromVoronoiDiagram");
   pragma Import (C, Cv_Release_Linear_Contor_Model_Storage, "cvReleaseLinearContorModelStorage");
   pragma Import (C, Cv_Init_Perspective_Transform, "cvInitPerspectiveTransform");
   pragma Import (C, Cv_Voronoi_Diagram_From_Contour, "cvVoronoiDiagramFromContour");
   pragma Import (C, Cv_Voronoi_Diagram_From_Image, "cvVoronoiDiagramFromImage");
   pragma Import (C, Cv_Release_Voronoi_Storage, "cvReleaseVoronoiStorage");
   pragma Import (C, Cv_Make_Scanlines, "cvMakeScanlines");
   pragma Import (C, Cv_Pre_Warp_Image, "cvPreWarpImage");
   pragma Import (C, Cv_Find_Runs, "CvFindRuns");
   pragma Import (C, Cv_Dynamic_Correspond_Multi, "cvDynamicCorrespondMulti");
   pragma Import (C, Cv_Make_Alpha_Scanlines, "cvMakeAlphaScanlines");
   pragma Import (C, Cv_Morph_Epilines_Multi, "cvMorphEpilinesMulti");
   pragma Import (C, Cv_Post_Warp_Image, "cvPostWarpImage");
   pragma Import (C, Cv_Delete_Moire, "cvDeleteMoire");
   pragma Import (C, Cv_Create_Condensation, "cvCreateConDensation");
   pragma Import (C, Cv_Condens_Init_Sample_Set, "cvConDensInitSampleSet");
   pragma Import (C, Cv_Condens_Update_By_Time, "cvConDensUpdateByTime");
   pragma Import (C, Cv_Find_Stereo_Correspondence, "cvFindStereoCorrespondence");
   pragma Import (C, Cv_Current_Int, "cvCurrentInt");
   pragma Import (C, Cv_Prev_Int, "cvPrevInt");
end Legacy;
