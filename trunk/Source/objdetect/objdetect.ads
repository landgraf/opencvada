-----------------------------------------------------------------------
-- Ada bindings for OpenCV 2.1.1 (from SVN 3 October 2010, rev. 3703)
-- Developed as a master thesis project at M�lardalens H�gskola
-- OpenCV: http://opencv.willowgarage.com/
-- Ada bindings : http://not_available.nope/
-- License @ ./LICENSE (BSD license)
-----------------------------------------------------------------------

--Contact--------------------------------------------------------------
-- Lars Cederholm, Niklas Pettersson
-- M�lardalens H�gskola, http://www.mdh.se/
-- [lcm06001,npn06002]@student.mdh.se
-----------------------------------------------------------------------

--File-Info-------------------------------------------------------------
-- objdetect.ads - objdetect.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------
with Core; use Core;
with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Pointers;

package Objdetect is

   CV_HAAR_MAGIC_VAL : constant := 16#42500000#;
   CV_TYPE_NAME_HAAR : constant String := "opencv-haar-classifier";

      CV_HAAR_FEATURE_MAX : constant := 3;

   -----------------------------------------------------------------------------
   -- Types
   -----------------------------------------------------------------------------
   type Cv_Haar_Classifier_Cascade;
   type Cv_Haar_Classifier_Cascade_P is access Cv_Haar_Classifier_Cascade;

   type Haar_Rect is
      record
         R : Cv_Rect;
         Weight : Float;
      end record;

   type Haar_Rect_Arr is array (Integer range 1 .. CV_HAAR_FEATURE_MAX) of Haar_Rect;
   type Cv_Haar_Feature is
      record
         Tilted : Integer;
         Rect   : Haar_Rect_Arr;
      end record;
   type Cv_Haar_Feature_P is access Cv_Haar_Feature;

   type Cv_Haar_Classifier is
      record
         Count       : Integer;
         HaarFeature : Cv_Haar_Feature;
         Threshold   : Cv_32F_Array_P;
         Left        : Cv_32S_Array_P;
         Right       : Cv_32S_Array_P;
         Alpha       : Cv_32F_Array_P;
      end record;
   type Cv_Haar_Classifier_P is access Cv_Haar_Classifier;

   type Cv_Haar_Stage_Classifier is
      record
         Count : Integer;
         Threshold : Float;
         Classifier : Cv_Haar_Classifier_P;
         Next       : Integer;
         Child      : Integer;
         Parent     : Integer;
      end record;

   type Cv_Haar_Stage_Classifier_Arr is array (Integer range <>) of Cv_Haar_Stage_Classifier;
   type Cv_Haar_Stage_Classifier_Arr_P is access Cv_Haar_Stage_Classifier_Arr;

   type Cv_Hid_Haar_Classifier_Cascade is null record;

   type Cv_Haar_Classifier_Cascade is
      record
         Flags           : Integer;
         Count           : Integer;
         OrigWindowSize  : Cv_Size;
         RealWindowSize  : Cv_Size;
         Scale           : Long_Float;
         StageClassifier : Cv_Haar_Stage_Classifier_Arr_P;
         HidCascade      : Cv_Hid_Haar_Classifier_Cascade;
      end record;

   type Cv_Avg_Comp is
      record
         Rect      : Cv_Rect;
         Neighbors : Integer;
      end record;
   -----------------------------------------------------------------------------

   --     #define CV_IS_HAAR_CLASSIFIER( haar )                                                    \
   --      ((haar) != NULL &&                                                                   \
   --      (((const CvHaarClassifierCascade*)(haar))->flags & CV_MAGIC_MASK)==CV_HAAR_MAGIC_VAL)
   function CV_IS_HAAR_CLASSIFIER (Haar : Cv_Haar_Classifier_Cascade_P) return Integer;

   -- Loads haar classifier cascade from a directory.
   -- It is obsolete: convert your cascade to xml and use cvLoad instead
   function CvLoadHaarClassifierCascade (Directory : String;
                                         OrigWindowSize : Cv_Size) return Cv_Haar_Classifier_Cascade_P;

   -- Releases the haar classifier cascade.
   procedure CvReleaseHaarClassifierCascade (Cascade : access Cv_Haar_Classifier_Cascade_P);

   CV_HAAR_DO_CANNY_PRUNING    : constant := 1;
   CV_HAAR_SCALE_IMAGE         : constant := 2;
   CV_HAAR_FIND_BIGGEST_OBJECT : constant := 4;
   CV_HAAR_DO_ROUGH_SEARCH     : constant := 8;

   -- Detects objects in the image.
   function CvHaarDetectObjects (Image        : Cv_Arr_P;
                                 Cascade      : Cv_Haar_Classifier_Cascade_P;
                                 Storage      : Cv_Mem_Storage_P;
                                 ScaleFactor  : Long_Float := 1.1;
                                 MinNeighbors : Integer := 3;
                                 Flags        : Integer := 0;
                                 MinSize      : Cv_Size := CvSize (0, 0)) return Cv_Seq_P;

   -- Assigns images to the hidden cascade.
   procedure CvSetImagesForHaarClassifierCascade (Cascade   : Cv_Haar_Classifier_Cascade_P;
                                                  Sum       : Cv_Arr_P;
                                                  Sqsum     : Cv_Arr_P;
                                                  TiltedSum : Cv_Arr_P;
                                                  Scale     : Long_Float);

   -- Runs a cascade of boosted classifiers at the given image location.
   function CvRunHaarClassifierCascade (Cascade : Cv_Haar_Classifier_Cascade_P;
                                        Pt      : Cv_Point;
                                        StartStage : Integer := 0) return Integer;

   -----------------------------------------------------------------------------
   -- Latent SVM Object Detection functions.
   -----------------------------------------------------------------------------

   -- DataType: STRUCT position
   -- Structure describes the position of the filter in the feature pyramid
   -- l - level in the feature pyramid
   -- (x, y) - coordinate in level l
   type Cv_LSVM_Filter_Position is
      record
         X : Unsigned_32;
         Y : Unsigned_32;
         L : Unsigned_32;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_LSVM_Filter_Position);

   -- DataType: STRUCT filterObject
   -- Description of the filter, which corresponds to the part of the object
   -- V               - ideal (penalty = 0) position of the partial filter
   --                   from the root filter position (V_i in the paper)
   -- penaltyFunction - vector describes penalty function (d_i in the paper)
   --                   pf[0] * x + pf[1] * y + pf[2] * x^2 + pf[3] * y^2
   -- FILTER DESCRIPTION
   --   Rectangular map (sizeX x sizeY),
   --   every cell stores feature vector (dimension = p)
   -- H               - matrix of feature vectors
   --                   to set and get feature vectors (i,j)
   --                   used formula H[(j * sizeX + i) * p + k], where
   --                   k - component of feature vector in cell (i, j)
   -- END OF FILTER DESCRIPTION
   -- xp              - auxillary parameter for internal use
   --                   size of row in feature vectors
   --                   (yp = (int) (p / xp); p = xp * yp)
   type Cv_LSVM_Filter_Object is
      record
         V : Cv_LSVM_Filter_Position;
         Fine_Function : Core.Cv_32f_Array (1 .. 4);
         Size_X        : Unsigned_32;
         Size_Y        : Unsigned_32;
         P             : Unsigned_32;
         Xp            : Unsigned_32;
         H             : Core.Cv_32f_Array_P;
      end record;
   pragma Convention (C_Pass_By_Copy, Cv_LSVM_Filter_Object);
   type Cv_LSVM_Filter_Object_Array is array (Integer range <>) of aliased Cv_LSVM_Filter_Object;
   Null_Cv_LSVM_Filter_Object : Cv_LSVM_Filter_Object;
   -- C style pointer
   package Cv_LSVM_Filter_Object_Pointer_Pkg is new Interfaces.C.Pointers (Integer, Cv_LSVM_Filter_Object, Cv_LSVM_Filter_Object_Array, Null_Cv_LSVM_Filter_Object);
   type Cv_LSVM_Filter_Object_Pointer is new Cv_LSVM_Filter_Object_Pointer_Pkg.Pointer;
   -- C type equal **
   type Cv_LSVM_Filter_Object_2D_Array is array (Integer range <>) of Cv_LSVM_Filter_Object_Pointer;
   type Cv_LSVM_Filter_Object_2D_Array_P is access Cv_LSVM_Filter_Object_2D_Array;

   -- data type: STRUCT CvLatentSvmDetector
   -- structure contains internal representation of trained Latent SVM detector
   -- num_filters			- total number of filters (root plus part) in model
   -- num_components		- number of components in model
   -- num_part_filters		- array containing number of part filters for each component
   -- filters				- root and part filters for all model components
   -- b					- biases for all model components
   -- score_threshold		- confidence level threshold
   type Cv_Latent_Svm_Detector is
      record
         Num_Filters : Integer;
         Num_Components : Integer;
         Num_Part_Filters : Core.Cv_32s_Array_P;
         Filters          : Cv_LSVM_Filter_Object_2D_Array_P;
         B                : Core.Cv_32f_Array_P;
         Score_Threshold  : Float;
      end record;
   type Cv_Latent_Svm_Detector_P is access Cv_Latent_Svm_Detector;

   -- data type: STRUCT CvObjectDetection
   -- structure contains the bounding box and confidence level for detected object
   -- rect					- bounding box for a detected object
   -- score				- confidence level
   type Cv_Object_Detection is
      record
         Rect : Core.Cv_Rect;
         Score : Float;
      end record;
   type Cv_Object_Detection_P is access Cv_Object_Detection;

   -----------------------------------------------------------------------------
   -- Object Detection using Latent SVM
   -----------------------------------------------------------------------------
   -- load trained detector from a file
   -- API
   -- CvLatentSvmDetector* cvLoadLatentSvmDetector(const char* filename);
   -- INPUT
   -- filename				- path to the file containing the parameters of
   --						- trained Latent SVM detector
   -- OUTPUT
   -- trained Latent SVM detector in internal representation
   function CvLoadLatentSvmDetector (Filename : String) return Cv_Latent_Svm_Detector_P;

   -- release memory allocated for CvLatentSvmDetector structure
   -- API
   -- void cvReleaseLatentSvmDetector(CvLatentSvmDetector** detector);
   -- INPUT
   -- detector				- CvLatentSvmDetector structure to be released
   -- OUTPUT
   procedure CvReleaseLatentSvmDetector (Detector : access Cv_Latent_Svm_Detector);

   -- find rectangular regions in the given image that are likely
   -- to contain objects and corresponding confidence levels
   --
   -- API
   -- CvSeq* cvLatentSvmDetectObjects(const IplImage* image,
   --									CvLatentSvmDetector* detector,
   --									CvMemStorage* storage,
   --									float overlap_threshold = 0.5f);
   -- INPUT
   -- image				- image to detect objects in
   -- detector				- Latent SVM detector in internal representation
   -- storage				- memory storage to store the resultant sequence
   --							of the object candidate rectangles
   -- overlap_threshold	- threshold for the non-maximum suppression algorithm
   -- = 0.5f [Here Will Be The Reference To Original Paper]
   -- OUTPUT
   -- sequence of detected objects (bounding boxes and confidence levels stored in CvObjectDetection structures
   function CvLatentSvmDetectObjects (Image : Ipl_Image_P;
                                      Detector : Cv_Latent_Svm_Detector;
                                      Storage  : Cv_Mem_Storage_P;
                                      Overlap_Threshold : Float := 0.5) return Cv_Seq_P;
private
   -- Wrapper due to String.
   function WCvLoadHaarClassifierCascade (Directory : String_C;
                                         OrigWindowSize : Cv_Size) return Cv_Haar_Classifier_Cascade_P;

   function WCvLoadLatentSvmDetector(Filename : String_C) return Cv_Latent_Svm_Detector_P;

   pragma Import (C, WCvLoadHaarClassifierCascade, "cvLoadHaarClassifierCascade");
   pragma Import (C, CvHaarDetectObjects, "cvHaarDetectObjects");
   pragma Import (C, CvSetImagesForHaarClassifierCascade, "cvSetImagesForHaarClassifierCascade");
   pragma Import (C, CvReleaseHaarClassifierCascade, "cvReleaseHaarClassifierCascade");
   pragma Import (C, CvRunHaarClassifierCascade, "cvRunHaarClassifierCascade");

   pragma Import (C, WCvLoadLatentSvmDetector, "cvLoadLatentSvmDetector");
   pragma Import (C, CvReleaseLatentSvmDetector, "cvReleaseLatentSvmDetector");
   pragma Import (C, CvLatentSvmDetectObjects, "cvLatentSvmDetectObjects");
end Objdetect;