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

package body Features_2d is
--

   --              // returns default parameters
   function Cv_Create_Surf_Params (Threshold        : Long_Float;
                                   Extended         : Integer := 0) return Cv_Surf_Params is
      Params : Cv_Surf_Params;
   begin
      Params.Extended := Extended;
      Params.Hessianthreshold := Threshold;
      Params.Noctaves := 4;
      Params.Noctavelayers := 2;
      return Params;
   end Cv_Create_Surf_Params;

   -- Constructor for Cv_Star_Detector_Params with default values.
   function Cv_Create_Star_Detector_Params (Maxsize                : Integer := 45;
                                            Responethreshold       : Integer := 30;
                                            Linethresholdprojected : Integer := 10;
                                            Linethresholdbinarized : Integer := 8;
                                            Suppressnonmaxsize     : Integer := 5) return Cv_Star_Detector_Params is
      Params : Cv_Star_Detector_Params;
   begin
      Params.Maxsize := Maxsize;
      Params.Responethreshold := Responethreshold;
      Params.Linethresholdprojected := Linethresholdprojected;
      Params.Linethresholdbinarized := Linethresholdbinarized;
      Params.Suppressnonmaxsize := Suppressnonmaxsize;
      return Params;
   end Cv_Create_Star_Detector_Params;
end Features_2d;
