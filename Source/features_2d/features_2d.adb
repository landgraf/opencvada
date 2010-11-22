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
-- features_2d.adb - features_2d.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Features_2D is
--

   --              // returns default parameters
   function CvSURFParams (Threshold        : Long_Float;
                          Extended         : Integer := 0) return Cv_SURF_Params is
      Params : Cv_SURF_Params;
   begin
      Params.Extended := Extended;
      Params.HessianThreshold := Threshold;
      Params.NOctaves := 4;
      Params.NOctaveLayers := 2;
      return Params;
   end CvSURFParams;

   -- Constructor for Cv_Star_Detector_Params with default values.
   function CvStarDetectorParams (MaxSize                : Integer := 45;
                                  ResponeThreshold       : Integer := 30;
                                  LineThresholdProjected : Integer := 10;
                                  LineThresholdBinarized : Integer := 8;
                                  SuppressNonmaxSize     : Integer := 5) return Cv_Star_Detector_Params is
      Params : Cv_Star_Detector_Params;
   begin
      Params.MaxSize := MaxSize;
      Params.ResponeThreshold := ResponeThreshold;
      Params.LineThresholdProjected := LineThresholdProjected;
      Params.LineThresholdBinarized := LineThresholdBinarized;
      Params.SuppressNonmaxSize := SuppressNonmaxSize;
      return Params;
   end CvStarDetectorParams;
end Features_2D;
