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
   function Cv_Create_File_Capture_Ffmpeg (Filename : String)
                                        return Cv_Capture_P;

   procedure Cv_Release_Capture_Ffmpeg (Capture : access Cv_Capture_P);

   function Cv_Set_Capture_Property_Ffmpeg (Capture : Cv_Capture_P;
                                         Prop_Id : Highgui.Capture_Property;
                                         Value   : Long_Float)
                                         return Integer;

   function Cv_Get_Capture_Property_Ffmpeg (Capture : Cv_Capture_P;
                                         Prop_Id : Highgui.Capture_Property)
                                         return Long_Float;

   function Cv_Grab_Frame_Ffmpeg (Capture : Cv_Capture_P)
                                return Integer;

   function Cv_Retrieve_Frame_Ffmpeg (Capture : Cv_Capture_P;
                                    Stream_Idx : Integer)
                                    return Ipl_Image_P;

   function Cv_Create_Video_Writer_Ffmpeg (Filename : String;
                                        Fourcc   : Integer;
                                        Fps      : Long_Float;
                                        Frame_Size : Cv_Size;
                                        Is_Color   : Integer)
                                        return Cv_Video_Writer_P;

   procedure Cv_Release_Video_Writer_Ffmpeg (Writer : access Cv_Video_Writer_P);

   function Cv_Write_Frame_Ffmpeg (Writer : Cv_Video_Writer_P;
                                 Image  : Ipl_Image_P)
                                 return Integer;
private
   function W_Cv_Create_File_Capture_Ffmpeg (Filename : Chars_Ptr)
                                         return Cv_Capture_P;

   pragma Import (C, W_Cv_Create_File_Capture_Ffmpeg, "cvCreateFileCapture");
   pragma Import (C, Cv_Release_Capture_Ffmpeg, "cvReleaseCapture_FFMPEG");
   pragma Import (C, Cv_Set_Capture_Property_Ffmpeg, "cvSetCaptureProperty_FFMPEG");
   pragma Import (C, Cv_Get_Capture_Property_Ffmpeg, "cvGetCaptureProperty_FFMPEG");
   pragma Import (C, Cv_Grab_Frame_Ffmpeg, "cvGrabFrame_FFMPEG");
   pragma Import (C, Cv_Retrieve_Frame_Ffmpeg, "cvRetrieveFrame_FFMPEG");
   pragma Import (C, Cv_Create_Video_Writer_Ffmpeg, "cvCreateVideoWriter_FFMPEG");
   pragma Import (C, Cv_Release_Video_Writer_Ffmpeg, "cvReleaseVideoWriter_FFMPEG");
   pragma Import (C, Cv_Write_Frame_Ffmpeg, "cvWriteFrame_FFMPEG");
end Ff_Opencv;
