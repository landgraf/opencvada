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
-- ff_opencv.ads - ff_opencv.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------
with Highgui; use Highgui;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Core; use Core;
package Ff_Opencv is
   function CvCreateFileCapture_FFMPEG (Filename : String)
                                        return Cv_Capture_P;

   procedure CvReleaseCapture_FFMPEG (Capture : access Cv_Capture_P);

   function CvSetCaptureProperty_FFMPEG (Capture : Cv_Capture_P;
                                         Prop_Id : Highgui.Capture_Property;
                                         Value   : Long_Float)
                                         return Integer;

   function CvGetCaptureProperty_FFMPEG (Capture : Cv_Capture_P;
                                         Prop_Id : Highgui.Capture_Property)
                                         return Long_Float;

   function CvGrabFrame_FFMPEG (Capture : Cv_Capture_P)
                                return Integer;

   function CvRetrieveFrame_FFMPEG (Capture : Cv_Capture_P;
                                    Stream_Idx : Integer)
                                    return Ipl_Image_P;

   function CvCreateVideoWriter_FFMPEG (Filename : String;
                                        Fourcc   : Integer;
                                        Fps      : Long_Float;
                                        Frame_Size : Cv_Size;
                                        Is_Color   : Integer)
                                        return Cv_Video_Writer_P;

   procedure CvReleaseVideoWriter_FFMPEG (Writer : access Cv_Video_Writer_P);

   function CvWriteFrame_FFMPEG (Writer : Cv_Video_Writer_P;
                                 Image  : Ipl_Image_P)
                                 return Integer;
private
   function WCvCreateFileCapture_FFMPEG (Filename : Chars_Ptr)
                                         return Cv_Capture_P;

   pragma Import (C, WCvCreateFileCapture_FFMPEG, "cvCreateFileCapture");
   pragma Import (C, CvReleaseCapture_FFMPEG, "cvReleaseCapture_FFMPEG");
   pragma Import (C, CvSetCaptureProperty_FFMPEG, "cvSetCaptureProperty_FFMPEG");
   pragma Import (C, CvGetCaptureProperty_FFMPEG, "cvGetCaptureProperty_FFMPEG");
   pragma Import (C, CvGrabFrame_FFMPEG, "cvGrabFrame_FFMPEG");
   pragma Import (C, CvRetrieveFrame_FFMPEG, "cvRetrieveFrame_FFMPEG");
   pragma Import (C, CvCreateVideoWriter_FFMPEG, "cvCreateVideoWriter_FFMPEG");
   pragma Import (C, CvReleaseVideoWriter_FFMPEG, "cvReleaseVideoWriter_FFMPEG");
   pragma Import (C, CvWriteFrame_FFMPEG, "cvWriteFrame_FFMPEG");
end Ff_Opencv;
