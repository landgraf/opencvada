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
-- ff_opencv.adb - ff_opencv.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Ff_Opencv is
   function CvCreateFileCapture_FFMPEG (Filename : String) return Cv_Capture_P is
   begin
      return WCvCreateFileCapture_FFMPEG (New_String (Filename));
   end CvCreateFileCapture_FFMPEG;
end Ff_Opencv;
