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
-- imgproc-compat_c.ads - imgproc-compat_c.hpp
-- Should be considered depricated.
-----------------------------------------------------------------------

package Imgproc.Compat_C is
   function CvMatArray (Rows : Integer;
                        Cols : Integer;
                        Mat_Array_Type : Integer;
                        Count          : Integer;
                        Data           : Cv_Void_P) return Cv_Mat;

private
   pragma Import (C, CvMatArray, "cvMatArray");
end Imgproc.Compat_C;
