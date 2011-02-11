
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
   function Cv_Is_Haar_Classifier (Haar : Cv_Haar_Classifier_Cascade_Ptr) return Integer is
   begin
      if not (Haar = null) and (Unsigned_32 (Haar.all.Flags) and Unsigned_32 (CV_MAGIC_MASK)) = CV_HAAR_MAGIC_VAL then
         return 1;
      else return 0;
      end if;
   end Cv_Is_Haar_Classifier;

   function Cv_Load_Haar_Classifier_Cascade (Directory      : String;
                                         OrigWindowSize : Cv_Size) return Cv_Haar_Classifier_Cascade_Ptr is
   begin
      return W_Cv_Load_Haar_Classifier_Cascade (+Directory, OrigWindowSize);
   end Cv_Load_Haar_Classifier_Cascade;

   function Cv_Load_Latent_Svm_Detector (Filename : String) return Cv_Latent_Svm_Detector_Ptr is
   begin
      return W_Cv_Load_Latent_Svm_Detector (+Filename);
   end Cv_Load_Latent_Svm_Detector;
end Objdetect;
