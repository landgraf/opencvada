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
-- highgui.ads - highgui.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

with Core; use Core;
with Core.Class_Mat;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

--use Core.Class_Mat;
package Highgui is
--with package Class_Mat;

   -------
   -- Ada stuff
   ---------
   type Cv_Capture is null record;
   type Cv_Capture_P is access Cv_Capture;

   type Compression_Type is ( CV_IMWRITE_JPEG_QUALITY,
                             CV_IMWRITE_PNG_COMPRESSION,
                             CV_IMWRITE_PXM_BINARY );
   for Compression_Type use
     (CV_IMWRITE_JPEG_QUALITY    => 1,
      CV_IMWRITE_PNG_COMPRESSION => 16,
      CV_IMWRITE_PXM_BINARY      => 32);

   type File_Settings is record
      Compression                         : Compression_Type;
      Compression_Rate                    : Integer;
      Not_Used                            : Integer;
   end record;

   function CreateFileSettings ( Compression     : Compression_Type;
                                Compression_Rate : Integer;
                                Not_Used         : Integer := 0) return File_Settings;

   --

   procedure NamedWindow (Window_Name : access String_CPP;
                          Flags       : Integer := 1);

   procedure DestroyWindow (Window_Name : in out Chars_Ptr);

   procedure Im_Show (Window_Name    : in out Chars_Ptr;
                      Matrix         : Core.Interface_Mat_P);
   pragma Import (CPP, Im_Show, "_ZN2cv6imshowERKSsRKNS_3MatE");
   -- CV_EXPORTS bool imwrite( const string& filename, const Mat& img,
   --                const vector<int>& params=vector<int>());

   type F_Settings is array (Integer range 1 .. 3) of Integer;

   function Im_Write (File_Name : access String_CPP;
                      Matrix    : Core.Interface_Mat_P;
                      Settings  : F_Settings) return Interfaces.Integer_8;
   pragma Import (CPP, Im_Write, "_ZN2cv7imwriteERKSsRKNS_3MatERKSt6vectorIiSaIiEE");

   function GetWindowProperty (Window_Name : access String_CPP;
                               Prop_Id     : Integer) return Long_Float;
   pragma Import (CPP, GetWindowProperty, "_ZN2cv17getWindowPropertyERKSsi");

   procedure SetWindowProperty (Window_Name : access String_CPP;
                               Prop_Id     : Integer;
                               Prop_Value : Long_Float);
   pragma Import (CPP, SetWindowProperty, "_ZN2cv17setWindowPropertyERKSsid");

   function WaitKey (Delay_Ms : Integer := 0) return Integer;
   pragma Import (CPP, WaitKey, "_ZN2cv7waitKeyEi");

   package Class_Video_Capture is
      type Video_Capture is tagged limited record
         Cap : Cv_Capture_P;
      end record;
      pragma Import (CPP, Video_Capture);

      function New_Video_Capture return Video_Capture;
      pragma CPP_Constructor (New_Video_Capture, "_ZN2cv12VideoCaptureC1Ev");

      function New_Video_Capture (Device : Integer) return Video_Capture;
      pragma CPP_Constructor (New_Video_Capture, "_ZN2cv12VideoCaptureC1Ei");

      function New_Video_Capture (File_Name : System.Address) return Video_Capture;
      pragma CPP_Constructor (New_Video_Capture, "_ZN2cv12VideoCaptureC1ERKSs");

      function Retrieve (This    : access Video_Capture;
                         Image   : Core.Interface_Mat_P;
                         Channel : Integer := 0) return Interfaces.Integer_8;
      pragma Import (CPP, Retrieve, "_ZN2cv12VideoCapture8retrieveERNS_3MatEi");

      function Open_File (This      : access Video_Capture;
                          File_Name : access String_CPP) return Interfaces.Integer_8;
      pragma Import (CPP, Open_File, "_ZN2cv12VideoCapture4openERKSs");

      function Open_Device (This   : access Video_Capture;
                            Device : Integer) return Interfaces.Integer_8;
      pragma Import (CPP, Open_Device, "_ZN2cv12VideoCapture4openEi");
   end Class_Video_Capture;

private
   pragma Import (CPP, NamedWindow, "_ZN2cv11namedWindowERKSsi");
   pragma Import (CPP, DestroyWindow, "_ZN2cv13destroyWindowERKSs");
   -- _ZN2cv11namedWindowERKSsi
   -- __ZN2cv11namedWindowERKSsi
   procedure Nulled;
end Highgui;
