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
-- video-blob_track_classes.ads - video-blob_track.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

with Interfaces; use Interfaces;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Core; use Core;

package Video.Blob_Track is

   -- Cv_Blob ------------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Blob is record
      X  : Float;
      Y  : Float;
      W  : Float;
      H  : Float;
      ID : Integer;
   end record;
   type Cv_Blob_P is access Cv_Blob;
   pragma Convention (C_Pass_By_Copy, Cv_Blob);
   pragma Convention (C, Cv_Blob_P);

   type Cv_Blob_Array is array (Integer range <>) of aliased Cv_Blob;

   -- Cv_Draw_Shape ------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Shape_Type is (Rect, Ellipse);

   type Cv_Draw_Shape is record
      Shape : Shape_Type;
      Color : Cv_Scalar;
   end record;
   type Cv_Draw_Shape_P is access Cv_Draw_Shape;
   pragma Convention (C_Pass_By_Copy, Cv_Draw_Shape);
   pragma Convention (C, Cv_Draw_Shape_P);

   type Cv_Draw_Shape_Array is array (Integer range <>) of aliased Cv_Draw_Shape;

   -- Cv_Kalman ----------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Kalman is record
      MP : Integer; -- Number of measurement vector dimensions
      DP : Integer; -- Number of state vector dimensions
      CP : Integer; -- Number of control vector dimensions

      Poster_State            : Cv_32f_Pointer;
      Prior_State             : Cv_32f_Pointer;
      Dyam_Matr               : Cv_32f_Pointer;
      Measurement_Matr        : Cv_32f_Pointer;
      MN_Covariance           : Cv_32f_Pointer;
      PN_Covariance           : Cv_32f_Pointer;
      Kalm_Gain_Matr          : Cv_32f_Pointer;
      Prior_Error_Covariance  : Cv_32f_Pointer;
      Poster_Error_Covariance : Cv_32f_Pointer;
      Temp1                   : Cv_32f_Pointer;
      Temp2                   : Cv_32f_Pointer;

      State_Pre               : Cv_Mat_P;
      State_Post              : Cv_Mat_P;
      Transition_Matrix       : Cv_Mat_P;
      Control_Matrix          : Cv_Mat_P;
      Measurement_Matrix      : Cv_Mat_P;
      Process_Noise_Cov       : Cv_Mat_P;
      Measurement_Noise_Cov   : Cv_Mat_P;
      Error_Cov_Pre           : Cv_Mat_P;
      Gain                    : Cv_Mat_P;
      Error_Cov_Post          : Cv_Mat_P;

      Mat_Temp1               : Cv_Mat_P;
      Mat_Temp2               : Cv_Mat_P;
      Mat_Temp3               : Cv_Mat_P;
      Mat_Temp4               : Cv_Mat_P;
      Mat_Temp5               : Cv_Mat_P;
   end record;
   type Cv_Kalman_P is access Cv_Kalman;
   pragma Convention (C_Pass_By_Copy, Cv_Kalman);
   pragma Convention (C, Cv_Kalman_P);

   -- Random stuff -------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Elem_Format_Array is private;

   type Cv_Blob_Seq is record
      Mem_P       : Cv_Mem_Storage_P;
      Seq_P       : Cv_Seq_P;
      Elem_Format : Elem_Format_Array;
   end record;
   type Cv_Blob_Seq_P is access Cv_Blob_Seq;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Seq);
   pragma Convention (C, Cv_Blob_Seq_P);

   type Cv_Blob_Detector is private;
   type Cv_Blob_Detector_P is access all Cv_Blob_Detector;
   pragma Convention (C, Cv_Blob_Detector_P);

   type Cv_Blob_Track_Analysis is private;
   type Cv_Blob_Track_Analysis_P is access all Cv_Blob_Track_Analysis;
   pragma Convention (C, Cv_Blob_Track_Analysis_P);

   type Cv_Blob_Track_Analysis_Height is private;
   type Cv_Blob_Track_Analysis_Height_P is access all Cv_Blob_Track_Analysis_Height;
   pragma Convention (C, Cv_Blob_Track_Analysis_Height_P);

   type Cv_Blob_Track_Analysis_One is private;
   type Cv_Blob_Track_Analysis_One_P is access all Cv_Blob_Track_Analysis_One;
   pragma Convention (C, Cv_Blob_Track_Analysis_One_P);

   type Cv_Blob_Track_FV_Gen is private;
   type Cv_Blob_Track_FV_Gen_P is access all Cv_Blob_Track_FV_Gen;
   pragma Convention (C, Cv_Blob_Track_FV_Gen_P);

   type Cv_Blob_Track_Gen is private;
   type Cv_Blob_Track_Gen_P is access all Cv_Blob_Track_Gen;
   pragma Convention (C, Cv_Blob_Track_Gen_P);

   type Cv_Blob_Track_Post_Proc is private;
   type Cv_Blob_Track_Post_Proc_P is access all Cv_Blob_Track_Post_Proc;
   pragma Convention (C, Cv_Blob_Track_Post_Proc_P);

   type Cv_Blob_Track_Post_Proc_One is private;
   type Cv_Blob_Track_Post_Proc_One_P is access all Cv_Blob_Track_Post_Proc_One;
   pragma Convention (C, Cv_Blob_Track_Post_Proc_One_P);

   type Cv_Blob_Track_Predictor is private;
   type Cv_Blob_Track_Predictor_P is access all Cv_Blob_Track_Predictor;
   pragma Convention (C, Cv_Blob_Track_Predictor_P);

   type Cv_Blob_Track_Seq is record
      Mem_P : Cv_Mem_Storage_P;
      Seq_P : Cv_Seq_P;
   end record;
   type Cv_Blob_Track_Seq_P is access Cv_Blob_Track_Seq;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Seq);
   pragma Convention (C, Cv_Blob_Track_Seq_P);

   type Cv_Blob_Tracker is private;
   type Cv_Blob_Tracker_P is access all Cv_Blob_Tracker;
   pragma Convention (C, Cv_Blob_Tracker_P);

   type Cv_Blob_Tracker_Auto is private;
   type Cv_Blob_Tracker_Auto_P is access all Cv_Blob_Tracker_Auto;
   pragma Convention (C, Cv_Blob_Tracker_Auto_P);

   type Cv_Blob_Track_Predict_Kalman is record
      Blob_Predict    : Cv_Blob;
      Kalman_P        : Cv_Kalman_P;
      Frame           : Integer;
      Model_Noise     : Float;
      Data_Noise_Pos  : Float;
      Data_Noise_Size : Float;
   end record;
   type Cv_Blob_Track_Predict_Kalman_P is access Cv_Blob_Track_Predict_Kalman;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Predict_Kalman);
   pragma Convention (C, Cv_Blob_Track_Predict_Kalman_P);

   type Cv_Def_Blob_Tracker is record
      Blob         : Cv_Blob;
      Predictor    : Cv_Blob_Track_Predict_Kalman_P;
      Blob_Predict : Cv_Blob;
      Collision    : Integer;
      Blob_Hyp_P   : Cv_Blob_Seq_P;
      Aver_FG      : Float;
   end record;
   type Cv_Def_Blob_Tracker_P is access Cv_Def_Blob_Tracker;
   pragma Convention (C_Pass_By_Copy, Cv_Def_Blob_Tracker);
   pragma Convention (C, Cv_Def_Blob_Tracker_P);

   type Cv_Blob_Tracker_CC is record
   -- Cv_VS_Module (Private)
   --           Param_List_P        : Cv_Def_Param_P;
   --           Module_Type_Name_P  : Interfaces.C.Strings.Chars_Ptr;
   --           Module_Name_P       : Interfaces.C.Strings.Chars_Ptr;
   --           Nick_Name_P         : Interfaces.C.Strings.Chars_Ptr;
   -- Cv_VS_Module (Protected)
      Wnd                 : Integer;
      -- Cv_Blob_tracker_CC (Private)
      Alpha_Size          : Float;
      Alpha_Pos           : Float;
      Alpha               : Float;
      Collision           : Integer;
      Confidence_Type     : Integer;
      Confidence_Type_Str : Interfaces.C.Strings.Chars_Ptr;
      Blob_List           : Cv_Blob_Seq;
      Blob_List_New       : Cv_Blob_Seq;
      Mem_P               : Cv_Mem_Storage_P;
      Clear_Hyp           : Integer;
      Img_P               : Ipl_Image_P;
      Img_FG              : Ipl_Image_P;
   end record;
   type Cv_Blob_Tracker_CC_P is access Cv_Blob_Tracker_CC;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_CC);
   pragma Convention (C, Cv_Blob_Tracker_CC_P);

   type Cv_Blob_Tracker_One is private;
   type Cv_Blob_Tracker_One_P is access Cv_Blob_Tracker_One;
   pragma Convention (C, Cv_Blob_Tracker_One_P);

   type Cv_FG_Detector is private;
   type Cv_FG_Detector_P is access Cv_FG_Detector;
   pragma Convention (C, Cv_FG_Detector_P);

   type Cv_Image_Drawer is record
      Image : Ipl_Image_P;
      Shape : Cv_Draw_Shape_Array (1 .. 16);
   end record;
   type Cv_Image_Drawer_P is access Cv_Image_Drawer;
   pragma Convention (C_Pass_By_Copy, Cv_Image_Drawer);
   pragma Convention (C, Cv_Image_Drawer_P);







   -- Cv_Def_Param -------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Def_Param;
   type Cv_Def_Param_P is access Cv_Def_Param;
   type Cv_Def_Param is record
      Next      : Cv_Def_Param_P;
      Name_P    : Chars_Ptr;
      Comment_P : Chars_Ptr;
      Double_P  : access Long_Float;
      Double_V  : Long_Float;
      Float_P   : access Float;
      Float_V   : Float;
      Int_P     : access Integer;
      Int_V     : Integer;
      Str_P     : access Chars_Ptr;
      Str       : Chars_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Cv_Def_Param);
   pragma Convention (C, Cv_Def_Param_P);

   type Cv_Def_Param_Array is array (Integer range <>) of aliased Cv_Def_Param;

   -- Cv_Blob_Track ------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Blob_Track is record
      Track_ID    : Integer;
      Start_Frame : Integer;
      Blob_Seq_P  : Cv_Blob_Seq_P; -- TODO: This is a "class" in subpackage.
   end record;
   type Cv_Blob_Track_P is access Cv_Blob_Track;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track);
   pragma Convention (C, Cv_Blob_Track_P);

   type Cv_Blob_Track_Array is array (Integer range <>) of aliased Cv_Blob_Track;

   -- Cv_Detected_Blob ---------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Detected_Blob is record
      X        : Float;
      Y        : Float;
      W        : Float;
      H        : Float;
      ID       : Integer;
      Response : Float;
   end record;
   type Cv_Detected_Blob_P is access Cv_Detected_Blob;
   pragma Convention (C_Pass_By_Copy, Cv_Detected_Blob);
   pragma Convention (C, Cv_Detected_Blob_P);

   type Cv_Detected_Blob_Array is array (Integer range <>) of aliased Cv_Detected_Blob;

   -- Cv_Blob_Tracker_Param_MS -------------------------------------------------
   -----------------------------------------------------------------------------
   -- Some declarations for specific MeanShift trackers
   Profile_Epanechnikov : constant := 0;
   Profile_Dog          : constant := 1;

   type Cv_Blob_Tracker_Param_MS is record
      No_Of_Sig_Bits     : Integer;
      Appearance_Profile : Integer;
      Meanshift_Profile  : Integer;
      Sigma              : Float;
   end record;
   type Cv_Blob_Tracker_Param_MS_P is access Cv_Blob_Tracker_Param_MS;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_Param_MS);
   pragma Convention (C, Cv_Blob_Tracker_Param_MS_P);

   type Cv_Blob_Tracker_Param_MS_Array is array (Integer range <>) of aliased Cv_Blob_Tracker_Param_MS;

   -- Cv_Blob_Tracker_Param_LH -------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Blob_Tracker_Param_LH is record
      Hist_Type   : Integer;
      Scale_After : Integer;
   end record;
   type Cv_Blob_Tracker_Param_LH_P is access Cv_Blob_Tracker_Param_LH;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_Param_LH);
   pragma Convention (C, Cv_Blob_Tracker_Param_LH_P);

   type Cv_Blob_Tracker_Param_LH_Array is array (Integer range <>) of aliased Cv_Blob_Tracker_Param_LH;

   -- Cv_Blob_Tracker_Auto_Param_1 ---------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Blob_Tracker_Auto_Param_1 is record
      FG_Train_Frames : Integer;
      FG_P            : Cv_FG_Detector_P;          -- TODO: This is a "class" in subpackage.
      BD_P            : Cv_Blob_Detector_P;        -- TODO: This is a "class" in subpackage.
      BT_P            : Cv_Blob_Tracker_P;         -- TODO: This is a "class" in subpackage.
      BT_Gen_P        : Cv_Blob_Track_Gen_P;       -- TODO: This is a "class" in subpackage.
      BTPP_P          : Cv_Blob_Track_Post_Proc_P; -- TODO: This is a "class" in subpackage.
      Use_PP_Data     : Integer;
      BTA_P           : Cv_Blob_Track_Analysis_P;  -- TODO: This is a "class" in subpackage.
   end record;
   type Cv_Blob_Tracker_Auto_Param_1_P is access Cv_Blob_Tracker_Auto_Param_1;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_Auto_Param_1);
   pragma Convention (C, Cv_Blob_Tracker_Auto_Param_1_P);

   type Cv_Blob_Tracker_Auto_Param_1_Array is array (Integer range <>) of aliased Cv_Blob_Tracker_Auto_Param_1;

   -- Cv_Track_Time_Pos --------------------------------------------------------
   -----------------------------------------------------------------------------
   type Cv_Track_Time_Pos is record
      Len1    : Integer;
      Len2    : Integer;
      Beg1    : Integer;
      Beg2    : Integer;
      End1    : Integer;
      End2    : Integer;
      Com_Len : Integer;
      Shift1  : Integer;
      Shift2  : Integer;
   end record;
   type Cv_Track_Time_Pos_P is access Cv_Track_Time_Pos;
   pragma Convention (C_Pass_By_Copy, Cv_Track_Time_Pos);
   pragma Convention (C, Cv_Track_Time_Pos_P);

   type Cv_Track_Time_Pos_Array is array (Integer range <>) of aliased Cv_Track_Time_Pos;

   -- CV_BT_HIST_TYPE ----------------------------------------------------------
   -----------------------------------------------------------------------------
   CV_BT_HIST_TYPE_S   : constant := 0;
   CV_BT_HIST_TYPE_MG  : constant := 1;
   CV_BT_HIST_TYPE_MG2 : constant := 2;
   CV_BT_HIST_TYPE_H   : constant := 3;

   -- CV_NOISE -----------------------------------------------------------------
   -----------------------------------------------------------------------------
   CV_NOISE_NONE            : constant := 0;
   CV_NOISE_GAUSSIAN        : constant := 1;
   CV_NOISE_UNIFORM         : constant := 2;
   CV_NOISE_SPECKLE         : constant := 3;
   CV_NOISE_SALT_AND_PEPPER : constant := 4;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Detector ------------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Detector is
--        type Cv_Blob_Detector is private;
--        type Cv_Blob_Detector_P is access all Cv_Blob_Detector;
--        pragma Convention (C, Cv_Blob_Detector_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Detector is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Detector);
   end Blob_Detector;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Analysis ------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Analysis is
--        type Cv_Blob_Track_Analysis is private;
--        type Cv_Blob_Track_Analysis_P is access all Cv_Blob_Track_Analysis;
--        pragma Convention (C, Cv_Blob_Track_Analysis_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Track_Analysis is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Analysis);
   end Blob_Track_Analysis;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Analysis_Height -----------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Analysis_Height is
--        type Cv_Blob_Track_Analysis_Height is private;
--        type Cv_Blob_Track_Analysis_Height_P is access all Cv_Blob_Track_Analysis_Height;
--        pragma Convention (C, Cv_Blob_Track_Analysis_Height_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Track_Analysis_Height is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Analysis_Height);
   end Blob_Track_Analysis_Height;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Analysis_One --------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Analysis_One is
--        type Cv_Blob_Track_Analysis_One is private;
--        type Cv_Blob_Track_Analysis_One_P is access all Cv_Blob_Track_Analysis_One;
--        pragma Convention (C, Cv_Blob_Track_Analysis_One_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Track_Analysis_One is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Analysis_One);
   end Blob_Track_Analysis_One;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_FV_Gen --------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_FV_Gen is
--        type Cv_Blob_Track_FV_Gen is private;
--        type Cv_Blob_Track_FV_Gen_P is access all Cv_Blob_Track_FV_Gen;
--        pragma Convention (C, Cv_Blob_Track_FV_Gen_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Track_FV_Gen is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_FV_Gen);
   end Blob_Track_FV_Gen;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Gen -----------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Gen is
--        type Cv_Blob_Track_Gen is private;
--        type Cv_Blob_Track_Gen_P is access all Cv_Blob_Track_Gen;
--        pragma Convention (C, Cv_Blob_Track_Gen_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Track_Gen is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Gen);
   end Blob_Track_Gen;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Post_Proc -----------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Post_Proc is
--        type Cv_Blob_Track_Post_Proc is private;
--        type Cv_Blob_Track_Post_Proc_P is access all Cv_Blob_Track_Post_Proc;
--        pragma Convention (C, Cv_Blob_Track_Post_Proc_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Track_Post_Proc is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Post_Proc);
   end Blob_Track_Post_Proc;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Post_Proc_1 ---------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Post_Proc_One is
--        type Cv_Blob_Track_Post_Proc_One is private;
--        type Cv_Blob_Track_Post_Proc_One_P is access all Cv_Blob_Track_Post_Proc_One;
--        pragma Convention (C, Cv_Blob_Track_Post_Proc_One_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Track_Post_Proc_One is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Post_Proc_One);
   end Blob_Track_Post_Proc_One;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Predictor -----------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Predictor is
--        type Cv_Blob_Track_Predictor is private;
--        type Cv_Blob_Track_Predictor_P is access all Cv_Blob_Track_Predictor;
--        pragma Convention (C, Cv_Blob_Track_Predictor_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Track_Predictor is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Predictor);
   end Blob_Track_Predictor;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Seq -----------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Seq is
--        type Cv_Blob_Track_Seq is record
--           Mem_P : Cv_Mem_Storage_P;
--           Seq_P : Cv_Seq_P;
--        end record;
--        type Cv_Blob_Track_Seq_P is access Cv_Blob_Track_Seq;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Seq);
--        pragma Convention (C, Cv_Blob_Track_Seq_P);

      function New_CvBlobTrackSeq (Track_Size : Integer)
                                   return Cv_Blob_Track_Seq_P;

      procedure Delete_CvBlobTrackSeq (This : Cv_Blob_Track_Seq_P);

      function GetBlobTrack (This        : Cv_Blob_Track_Seq_P;
                             Track_Index : Integer)
                          return Cv_Blob_Track_P;

      function GetBlobTrackByID (This     : Cv_Blob_Track_Seq_P;
                                 Track_ID : Integer)
                              return Cv_Blob_Track_P;

      procedure DelBlobTrack (This        : Cv_Blob_Track_Seq_P;
                              Track_Index : Integer);

      procedure DelBlobTrackByID (This     : Cv_Blob_Track_Seq_P;
                                  Track_ID : Integer);

      procedure Clear (This : Cv_Blob_Track_Seq_P);

      procedure AddBlobTrack (This        : Cv_Blob_Track_Seq_P;
                              Track_ID    : Integer;
                              Start_Frame : Integer);

      function GetBlobTrackNum (This : Cv_Blob_Track_Seq_P)
                             return Integer;
   private
      pragma Import (C, New_CvBlobTrackSeq, "new_CvBlobTrackSeq");
      pragma Import (C, Delete_CvBlobTrackSeq, "delete_CvBlobTrackSeq");
      pragma Import (C, GetBlobTrack, "GetBlobTrack");
      pragma Import (C, GetBlobTrackByID, "GetBlobTrackByID");
      pragma Import (C, DelBlobTrack, "DelBlobTrack");
      pragma Import (C, DelBlobTrackByID, "DelBlobTrackByID");
      pragma Import (C, Clear, "Clear");
      pragma Import (C, AddBlobTrack, "AddBlobTrack");
      pragma Import (C, GetBlobTrackNum, "GetBlobTrackNum");
   end Blob_Track_Seq;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Tracker -------------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Tracker is
--        type Cv_Blob_Tracker is private;
--        type Cv_Blob_Tracker_P is access all Cv_Blob_Tracker;
--        pragma Convention (C, Cv_Blob_Tracker_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Tracker is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker);
   end Blob_Tracker;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Tracker_Auto --------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Tracker_Auto is
--        type Cv_Blob_Tracker_Auto is private;
--        type Cv_Blob_Tracker_Auto_P is access all Cv_Blob_Tracker_Auto;
--        pragma Convention (C, Cv_Blob_Tracker_Auto_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Tracker_Auto is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_Auto);
   end Blob_Tracker_Auto;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Track_Predict_Kalman ------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Track_Predict_Kalman is
--        type Cv_Blob_Track_Predict_Kalman is record
--           Blob_Predict    : Cv_Blob;
--           Kalman_P        : Cv_Kalman_P;
--           Frame           : Integer;
--           Model_Noise     : Float;
--           Data_Noise_Pos  : Float;
--           Data_Noise_Size : Float;
--        end record;
--        type Cv_Blob_Track_Predict_Kalman_P is access Cv_Blob_Track_Predict_Kalman;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Predict_Kalman);
--        pragma Convention (C, Cv_Blob_Track_Predict_Kalman_P);

      function New_CvBlobTrackPredictKalman return Cv_Blob_Track_Predict_Kalman_P;

      procedure Delete_CvBlobTrackPredictKalman (This : Cv_Blob_Track_Predict_Kalman_P);

      function Predict (This : Cv_Blob_Track_Predict_Kalman_P)
                     return Cv_Blob_P;

      procedure Update (This   : Cv_Blob_Track_Predict_Kalman_P;
                        Blob_P : Cv_Blob_P);

      procedure Release (This : Cv_Blob_Track_Predict_Kalman_P);
   private
      pragma Import (C, New_CvBlobTrackPredictKalman, "new_CvBlobTrackPredictKalman");
      pragma Import (C, Delete_CvBlobTrackPredictKalman, "delete_CvBlobTrackPredictKalman");
      pragma Import (C, Predict, "CvBlobTrackPredictKalman_Predict");
      pragma Import (C, Update, "CvBlobTrackPredictKalman_Update");
      pragma Import (C, Release, "CvBlobTrackPredictKalman_Release");
   end Blob_Track_Predict_Kalman;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Tracker_CC ----------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Tracker_CC is
--        type Cv_Def_Blob_Tracker is record
--           Blob         : Cv_Blob;
--           Predictor    : Cv_Blob_Track_Predict_Kalman_P;
--           Blob_Predict : Cv_Blob;
--           Collision    : Integer;
--           Blob_Hyp_P   : Cv_Blob_Seq_P;
--           Aver_FG      : Float;
--        end record;
--        type Cv_Def_Blob_Tracker_P is access Cv_Def_Blob_Tracker;
--        pragma Convention (C_Pass_By_Copy, Cv_Def_Blob_Tracker);
--        pragma Convention (C, Cv_Def_Blob_Tracker_P);
--
--        type Cv_Blob_Tracker_CC is record
--        -- Cv_VS_Module (Private)
--  --           Param_List_P        : Cv_Def_Param_P;
--  --           Module_Type_Name_P  : Interfaces.C.Strings.Chars_Ptr;
--  --           Module_Name_P       : Interfaces.C.Strings.Chars_Ptr;
--  --           Nick_Name_P         : Interfaces.C.Strings.Chars_Ptr;
--        -- Cv_VS_Module (Protected)
--           Wnd                 : Integer;
--        -- Cv_Blob_tracker_CC (Private)
--           Alpha_Size          : Float;
--           Alpha_Pos           : Float;
--           Alpha               : Float;
--           Collision           : Integer;
--           Confidence_Type     : Integer;
--           Confidence_Type_Str : Interfaces.C.Strings.Chars_Ptr;
--           Blob_List           : Cv_Blob_Seq;
--           Blob_List_New       : Cv_Blob_Seq;
--           Mem_P               : Cv_Mem_Storage_P;
--           Clear_Hyp           : Integer;
--           Img_P               : Ipl_Image_P;
--           Img_FG              : Ipl_Image_P;
--        end record;
--        type Cv_Blob_Tracker_CC_P is access Cv_Blob_Tracker_CC;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_CC);
--        pragma Convention (C, Cv_Blob_Tracker_CC_P);

      function New_CvBlobTrackerCC return Cv_Blob_Tracker_CC_P;

      procedure Delete_CvBlobTrackerCC (This : Cv_Blob_Tracker_CC_P);

      function GetBlobNum (This : Cv_Blob_Tracker_CC_P)
                        return Integer;

      function GetBlob (This       : Cv_Blob_Tracker_CC_P;
                        Blob_Index : Integer)
                     return Cv_Blob_P;

      procedure SetBlob (This       : Cv_Blob_Tracker_CC_P;
                         Blob_Index : Integer;
                         Blob_P     : Cv_Blob_P);

      function GetBlobByID (This    : Cv_Blob_Tracker_CC_P;
                            Blob_ID : Integer)
                         return Cv_Blob_P;

      procedure DelBlob (This       : Cv_Blob_Tracker_CC_P;
                         Blob_Index : Integer);

      procedure Release (This : Cv_Blob_Tracker_CC_P);

      function AddBlob (This     : Cv_Blob_Tracker_CC_P;
                        Blob_P   : Cv_Blob_P;
                        Img_FG_P : Ipl_Image_P := null)
                     return Cv_Blob_P;

      procedure Process (This     : Cv_Blob_Tracker_CC_P;
                         Img_P    : Ipl_Image_P;
                         Img_FG_P : Ipl_Image_P := null);

      procedure ProcessBlob (This     : Cv_Blob_Tracker_CC_P;
                             Blob_P   : Cv_Blob_P);

      function GetConfidence (This       : Cv_Blob_Tracker_CC_P;
                              Blob_Index : Integer;
                              Blob_P     : Cv_Blob_P;
                              Img_FG_P   : Ipl_Image_P := null)
                           return Long_Float;

      function GetConfidenceList (This        : Cv_Blob_Tracker_CC_P;
                                  Blob_List_P : Cv_Blob_Seq_P;
                                  Img_P       : Ipl_Image_P;
                                  Img_FG_P    : Ipl_Image_P)
                               return Long_Float;

      procedure UpdateBlob (This       : Cv_Blob_Tracker_CC_P;
                            Blob_Index : Integer;
                            Img_FG_P   : Ipl_Image_P);

      procedure Update (This     : Cv_Blob_Tracker_CC_P;
                        Img_P    : Ipl_Image_P;
                        Img_FG_P : Ipl_Image_P := null);

      function GetBlobIndexByID (This    : Cv_Blob_Tracker_CC_P;
                                 Blob_ID : Integer)
                              return Integer;

--        function GetBlobByID (This    : Cv_Blob_Tracker_CC_P;
--                              Blob_ID : Integer)
--                           return Cv_Blob_P;

      procedure DelBlobByID (This    : Cv_Blob_Tracker_CC_P;
                             Blob_ID : Integer);

      procedure SetBlobByID (This    : Cv_Blob_Tracker_CC_P;
                             Blob_ID : Integer;
                             Blob_P  : Cv_Blob_P);

      procedure ParamUpdate (This : Cv_Blob_Tracker_CC_P);

      function GetBlobHypNum (This       : Cv_Blob_Tracker_CC_P;
                              Blob_Index : Integer)
                           return Integer;

      function GetBlobHyp (This       : Cv_Blob_Tracker_CC_P;
                           Blob_Index : Integer;
                           Hypothesis : Integer)
                        return Cv_Blob_P;

      procedure SetBlobHyp (This       : Cv_Blob_Tracker_CC_P;
                            Blob_Index : Integer;
                            Blob_P     : Cv_Blob_P);
   private
      pragma Import (C, New_CvBlobTrackerCC, "new_CvBlobTrackerCC");
      pragma Import (C, Delete_CvBlobTrackerCC, "delete_CvBlobTrackerCC");
      pragma Import (C, GetBlobNum, "CvBlobTrackerCC_GetBlobNum");
      pragma Import (C, GetBlob, "CvBlobTrackerCC_GetBlob");
      pragma Import (C, SetBlob, "CvBlobTrackerCC_SetBlob");
      pragma Import (C, GetBlobByID, "CvBlobTrackerCC_GetBlobByID");
      pragma Import (C, DelBlob, "CvBlobTrackerCC_DelBlob");
      pragma Import (C, Release, "CvBlobTrackerCC_Release");
      pragma Import (C, AddBlob, "CvBlobTrackerCC_AddBlob");
      pragma Import (C, Process, "CvBlobTrackerCC_Process");
      pragma Import (C, ProcessBlob, "CvBlobTrackerCC_ProcessBlob");
      pragma Import (C, GetConfidence, "CvBlobTrackerCC_GetConfidence");
      pragma Import (C, GetConfidenceList, "CvBlobTrackerCC_GetConfidenceList");
      pragma Import (C, UpdateBlob, "CvBlobTrackerCC_UpdateBlob");
      pragma Import (C, Update, "CvBlobTrackerCC_Update");
      pragma Import (C, GetBlobIndexByID, "CvBlobTrackerCC_GetBlobIndexByID");
--        pragma Import (C, GetBlobByID, "CvBlobTrackerCC_GetBlobByID");
      pragma Import (C, SetBlobByID, "CvBlobTrackerCC_SetBlobByID");
      pragma Import (C, ParamUpdate, "CvBlobTrackerCC_ParamUpdate");
      pragma Import (C, GetBlobHypNum, "CvBlobTrackerCC_GetBlobHypNum");
      pragma Import (C, GetBlobHyp, "CvBlobTrackerCC_GetBlobHyp");
      pragma Import (C, SetBlobHyp, "CvBlobTrackerCC_SetBlobHyp");
      pragma Import (C, DelBlobByID, "CvBlobTrackerCC_DelBlobByID");
   end Blob_Tracker_CC;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Tracker_One ---------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Tracker_One is
--        type Cv_Blob_Tracker_One is private;
--        type Cv_Blob_Tracker_One_P is access Cv_Blob_Tracker_One;
--        pragma Convention (C, Cv_Blob_Tracker_One_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_Blob_Tracker_One is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_One);
   end Blob_Tracker_One;

   -- Class Interface ----------------------------------------------------------
   -- FG_Detector --------------------------------------------------------------
   -----------------------------------------------------------------------------
   package FG_Detector is
--        type Cv_FG_Detector is private;
--        type Cv_FG_Detector_P is access Cv_FG_Detector;
--        pragma Convention (C, Cv_FG_Detector_P);
   private
      Is_Abstract : constant Boolean := True;
--        type Cv_FG_Detector is null record;
--        pragma Convention (C_Pass_By_Copy, Cv_FG_Detector);
   end FG_Detector;

   -- Class Interface ----------------------------------------------------------
   -- Image_Drawer -------------------------------------------------------------
   -----------------------------------------------------------------------------
   package Image_Drawer is
--        type Cv_Image_Drawer is record
--           Image : Ipl_Image_P;
--           Shape : Cv_Draw_Shape_Array (1 .. 16);
--        end record;
--        type Cv_Image_Drawer_P is access Cv_Image_Drawer;
--        pragma Convention (C_Pass_By_Copy, Cv_Image_Drawer);
--        pragma Convention (C, Cv_Image_Drawer_P);

      function New_CvImageDrawer return Cv_Image_Drawer_P;

      procedure Delete_CvImageDrawer (This : Cv_Image_Drawer_P);

      procedure SetShapes (This   : Cv_Image_Drawer_P;
                           Shapes : Cv_Draw_Shape_Array;
                           Num    : Integer);

      function Draw (This     : Cv_Image_Drawer_P;
                     Src      : Cv_Arr_P;
                     Blob_Seq : Cv_Blob_Seq_P;
                     Roi_Seq  : Cv_Seq_P)
                  return Ipl_Image_P;

      function GetImage (This : Cv_Image_Drawer_P)
                      return Ipl_Image_P;

   private
      pragma Import (C, New_CvImageDrawer, "new_CvImageDrawer");
      pragma Import (C, Delete_CvImageDrawer, "delete_CvImageDrawer");
      pragma Import (C, SetShapes, "SetShapes");
      pragma Import (C, Draw, "Draw");
      pragma Import (C, GetImage, "GetImage");
   end Image_Drawer;

   -- Class Interface ----------------------------------------------------------
   -- Blob_Seq -----------------------------------------------------------------
   -----------------------------------------------------------------------------
   package Blob_Seq is
--        type Elem_Format_Array is private;
--
--        type Cv_Blob_Seq is record
--           Mem_P       : Cv_Mem_Storage_P;
--           Seq_P       : Cv_Seq_P;
--           Elem_Format : Elem_Format_Array;
--        end record;
--        type Cv_Blob_Seq_P is access Cv_Blob_Seq;
--        pragma Convention (C_Pass_By_Copy, Cv_Blob_Seq);
--        pragma Convention (C, Cv_Blob_Seq_P);

      function New_CvBlobSeq (Blob_Size : Integer)
                              return Cv_Blob_Seq_P;

      procedure Delete_CvBlobSeq (This : Cv_Blob_Seq_P);

      function GetBlob (This       : Cv_Blob_Seq_P;
                        Blob_Index : Integer)
                        return Cv_Blob_P;

      function GetBlobByID (This    : Cv_Blob_Seq_P;
                            Blob_ID : Integer)
                            return Cv_Blob_P;

      procedure DelBlob (This       : Cv_Blob_Seq_P;
                         Blob_Index : Integer);

      procedure DelBlobByID (This    : Cv_Blob_Seq_P;
                             Blob_ID : Integer);

      procedure Clear (This : Cv_Blob_Seq_P);

      procedure AddBlob (This : Cv_Blob_Seq_P;
                         Blob : Cv_Blob_P);

      function GetBlobNum (This : Cv_Blob_Seq_P)
                           return Integer;

      procedure Write (This : Cv_Blob_Seq_P;
                       Fs   : Cv_File_Storage_P;
                       Name : Chars_Ptr);

      procedure Load (This : Cv_Blob_Seq_P;
                      Fs   : Cv_File_Storage_P;
                      Node : Cv_File_Node_P);

      procedure AddFormat (This : Cv_Blob_Seq_P;
                           Str  : Chars_Ptr); -- Wrap string
   private
--        type Elem_Format_Array is array (Integer range 1 .. 1024) of Interfaces.Unsigned_8;

      pragma Import (C, New_CvBlobSeq, "new_CvBlobSeq");
      pragma Import (C, Delete_CvBlobSeq, "delete_CvBlobSeq");
      pragma Import (C, GetBlob, "GetBlob");
      pragma Import (C, GetBlobByID, "GetBlobByID");
      pragma Import (C, DelBlob, "DelBlob");
      pragma Import (C, DelBlobByID, "DelBlobByID");
      pragma Import (C, Clear, "Clear");
      pragma Import (C, AddBlob, "AddBlob");
      pragma Import (C, GetBlobNum, "GetBlobNum");
      pragma Import (C, Write, "Write");
      pragma Import (C, Load, "Load");
      pragma Import (C, AddFormat, "AddFormat");
   end Blob_Seq;
private
   type Cv_Blob_Detector is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Detector);

   type Cv_Blob_Track_Analysis is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Analysis);

   type Cv_Blob_Track_Analysis_Height is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Analysis_Height);

   type Cv_Blob_Track_Analysis_One is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Analysis_One);

   type Cv_Blob_Track_FV_Gen is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_FV_Gen);

   type Cv_Blob_Track_Gen is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Gen);

   type Cv_Blob_Track_Post_Proc is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Post_Proc);

   type Cv_Blob_Track_Post_Proc_One is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Post_Proc_One);

   type Cv_Blob_Track_Predictor is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Track_Predictor);

   type Cv_Blob_Tracker is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker);

   type Cv_Blob_Tracker_Auto is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_Auto);

   type Cv_Blob_Tracker_One is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Blob_Tracker_One);

   type Cv_FG_Detector is null record;
   pragma Convention (C_Pass_By_Copy, Cv_FG_Detector);

   type Elem_Format_Array is array (Integer range 1 .. 1024) of Interfaces.Unsigned_8;
end Video.Blob_Track;
