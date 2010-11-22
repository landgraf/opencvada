
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
-- objdetect.adb - objdetect.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Objdetect is
--


   --     #define CV_IS_HAAR_CLASSIFIER( haar )                                                    \
   --      ((haar) != NULL &&                                                                   \
   --      (((const CvHaarClassifierCascade*)(haar))->flags & CV_MAGIC_MASK)==CV_HAAR_MAGIC_VAL)
   function CV_IS_HAAR_CLASSIFIER (Haar : Cv_Haar_Classifier_Cascade_P) return Integer is
   begin
      if not (Haar = null) and (Unsigned_32 (Haar.Flags) and Unsigned_32 (CV_MAGIC_MASK)) = CV_HAAR_MAGIC_VAL then
         return 1;
      else return 0;
      end if;
   end CV_IS_HAAR_CLASSIFIER;

   function CvLoadHaarClassifierCascade (Directory      : String;
                                         OrigWindowSize : Cv_Size) return Cv_Haar_Classifier_Cascade_P is
   begin
      return WCvLoadHaarClassifierCascade (+Directory, OrigWindowSize);
   end CvLoadHaarClassifierCascade;

   function CvLoadLatentSvmDetector (Filename : String) return Cv_Latent_Svm_Detector_P is
   begin
      return WCvLoadLatentSvmDetector (+Filename);
   end CvLoadLatentSvmDetector;
end Objdetect;
