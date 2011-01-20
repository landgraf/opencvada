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
-- imgproc_c.adb - imgproc_c.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Imgproc.Operations is
   function Cv_Contour_Perimeter (Curve : Cv_Void_P)
                                return Long_Float is
   begin
      return Cv_Arc_Length (Curve, CvSlice (0), 1);
   end Cv_Contour_Perimeter;
end Imgproc.Operations;
