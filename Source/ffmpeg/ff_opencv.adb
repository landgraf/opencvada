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
-- ff_opencv.adb - ff_opencv.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Ff_Opencv is
   function Cv_Create_File_Capture_Ffmpeg (Filename : String) return Cv_Capture_P is
   begin
      return W_Cv_Create_File_Capture_Ffmpeg (New_String (Filename));
   end Cv_Create_File_Capture_Ffmpeg;
end Ff_Opencv;
