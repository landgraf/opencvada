-----------------------------------------------------------------------
-- Ada bindings for OpenCV 2.1.1 (from SVN 3 October 2010, rev. 3703)
-- Developed as a master thesis project at Mälardalens Högskola
-- OpenCV: http://opencv.willowgarage.com/
-- Ada bindings : http://code.google.com/p/opencvada/
-- License @ ./LICENSE (BSD license)
-----------------------------------------------------------------------

--Contact--------------------------------------------------------------
-- Lars Cederholm, Niklas Pettersson
-- Mälardalens Högskola, http://www.mdh.se/
-- [lcm06001,npn06002]@student.mdh.se
-----------------------------------------------------------------------

--File-Info-------------------------------------------------------------
-- highgui_c.ads - highgui_c.h
-- Comments, Information, Other
-----------------------------------------------------------------------

with Core; use Core;
with Core.Operations; use Core.Operations;
with Interfaces.C.Strings;
with Interfaces; use Interfaces;
with Core; use Core;
use Core;

package Highgui is
--

   -----------------------------------------------------------------------------
   -- Basic GUI functions
   -----------------------------------------------------------------------------

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



   -----------------------------------------------------------------------------
   -- QT
   -----------------------------------------------------------------------------

   --// new font for QT
   type Cv_Font_Weight is (CV_FONT_LIGHT,
                           CV_FONT_NORMAL,
                           CV_FONT_DEMIBOLD,
                           CV_FONT_BOLD,
                           CV_FONT_BLACK);
   for Cv_Font_Weight use (CV_FONT_LIGHT    => 25,
                           CV_FONT_NORMAL   => 50,
                           CV_FONT_DEMIBOLD => 63,
                           CV_FONT_BOLD     => 75,
                           CV_FONT_BLACK    => 87);

   type Cv_Font_Style is (CV_STYLE_NORMAL,
                          CV_STYLE_ITALIC,
                          CV_STYLE_OBLIQUE);
   for Cv_Font_Style  use (CV_STYLE_NORMAL  => 0,
                           CV_STYLE_ITALIC  => 1,
                           CV_STYLE_OBLIQUE => 2);

   --  //for color cvScalar(blue_component, green_component, red\_component[, alpha_component])
   --  //and alpha= 0 <-> 0xFF (not transparent <-> transparent)
   function CvFontQt (Name_Font  : String;
                      Point_Size : Integer := -1;
                      Color      : Cv_Scalar := Core.CvScalarAll (0.0);
                      Weight     : Cv_Font_Weight := CV_FONT_NORMAL;
                      Style      : Cv_Font_Style :=  CV_STYLE_NORMAL;
                      Spacing    : Integer := 0) return Cv_Font;

   procedure CvAddText (Img  : Cv_Arr_P;
                        Text : String;
                        Org  : Cv_Point;
                        Arg2 : Cv_Font_P);

   procedure CvDisplayOverlay (Name     : String;
                               Text     : String;
                               Delay_Ms : Integer);

   procedure CvDisplayStatusBar (Name     : String;
                                 Text     : String;
                                 Delay_Ms : Integer);

   type Cv_OpenGL_Callback is access procedure (User_Data : Cv_Void_P);
   pragma Convention (C, Cv_OpenGL_Callback);

   procedure CvCreateOpenGLCallback (Window_Name     : String;
                                     Callback_OpenGL : Cv_OpenGL_Callback;
                                     User_DAta       : Cv_Void_P := null;
                                     Angle           : Long_Float := -1.0;
                                     Zmin            : Long_Float := -1.0;
                                     Zmax            : Long_Float := -1.0);

   procedure CvSaveWindowParameters (Name : String);

   procedure CvLoadWindowParameters (Name : String);

   type CvStartLoop_Function is access function (Argc : Integer;
                                                 Argv : C_String_Ptr) return Integer;
   pragma Convention (C, CvStartLoop_Function);

   function CvStartLoop (Pt2func : CvStartLoop_Function;
                         Argc    : Integer;
                         Argv    : C_String_Ptr) return Integer;

   procedure CvStopLoop;

   type Cv_Button_Callback is access procedure (State     : Integer;
                                                User_Data : Cv_Void_P);
   pragma Convention (C, Cv_Button_Callback);

   type Cv_Button_Type is (CV_PUSH_BUTTON,
                           CV_CHECKBOX,
                           CV_RADIOBOX);
   for Cv_Button_Type use (CV_PUSH_BUTTON => 0,
                           CV_CHECKBOX    => 1,
                           CV_RADIOBOX    => 2);

   function CvCreateButton (Button_Name         : String;
                            On_Change           : Cv_Button_Callback := null;
                            User_Data           : Cv_Void_P := null;
                            Button_Type         : Cv_Button_Type := CV_PUSH_BUTTON;
                            Intial_Button_State : Integer := 0) return Integer;

   -----------------------------------------------------------------------------
   --
   -----------------------------------------------------------------------------

   -- this function is used to set some external parameters in case of X Window
   function CvInitSystem (Argc : Integer;
                          Argv : C_String_Ptr) return Integer;

   function CvStartWindowThread return Integer;

   type Highgui_Window_Params is new Short_Integer;
   -- These 3 flags are used by cvSet/GetWindowProperty
   CV_WND_PROP_FULLSCREEN : constant Highgui_Window_Params := 0; -- To change/get window's fullscreen property
   CV_WND_PROP_AUTOSIZE : constant Highgui_Window_Params := 1; -- To change/get window's autosize property
   CV_WND_PROP_ASPECTRATIO : constant Highgui_Window_Params := 2; --To change/get window's aspectratio property

   -- These 2 flags are used by cvNamedWindow and cvSet/GetWindowProperty
   CV_WINDOW_NORMAL : constant Highgui_Window_Params := 16#0000_0000#; -- The user can resize the window (no constraint)  / also use to switch a fullscreen window to a normal size
   CV_WINDOW_AUTOSIZE : constant Highgui_Window_Params := 16#0000_0001#; -- The user cannot resize the window, the size is constrainted by the image displayed

   -- Those flags are only for Qt
   CV_GUI_EXPANDED : constant Highgui_Window_Params := 16#0000_0000#; -- Status bar and tool bar
   CV_GUI_NORMAL : constant Highgui_Window_Params := 16#0000_0010#; -- Old fashious way

   -- These 3 flags are used by cvNamedWindow and cvSet/GetWindowProperty
   CV_WINDOW_FULLSCREEN : constant Highgui_Window_Params := 1; -- Change the window to fullscreen
   CV_WINDOW_FREERATIO : constant Highgui_Window_Params := 16#0000_0100#; -- The image expends as much as it can (no ratio constraint)
   CV_WINDOW_KEEPRATIO  : constant Highgui_Window_Params := 16#0000_0000#; -- The ration image is respected.

   -- create window
   function CvNamedWindow (WindowName  : String;
                           Flags       : Highgui_Window_Params := CV_WINDOW_AUTOSIZE) return Integer;

   -- Set and Get Property of the window
   procedure CvSetWindowProperty (Name       : String;
                                  Prop_Id    : Highgui_Window_Params;
                                  Prop_Value : Long_Float);
   function CvGetWindowProperty (Name    : String;
                                 Prop_Id : Highgui_Window_Params) return Long_Float;

   -- display image within window (highgui windows remember their content)
   procedure CvShowImage (WindowName  : String;
                          Image       : Cv_Arr_P);

   -- resize/move window
   procedure CvResizeWindow (WindowName   : String;
                             Width        : Integer;
                             Height       : Integer );
   procedure CvMoveWindow (WindowName : String;
                           X          : Integer;
                           Y          : Integer);

   -- destroy window and all the trackers associated with it
   procedure CvDestroyWindow (WindowName : String );
   procedure CvDestroyAllWindows;

   -- get native window handle (HWND in case of Win32 and Widget in case of X Window)
   function CvGetWindowHandle (Window_Name : String)
                               return Cv_Void_P;

   function CvGetWindowName (Window_Handle : Cv_Void_P)
                             return Interfaces.C.Strings.Chars_Ptr;

   type Cv_Trackbar_Callback is access procedure ( Position : Integer ) ;
   pragma Convention (C, Cv_Trackbar_Callback);

   --create trackbar and display it on top of given window, set callback
   function CvCreateTrackbar (Trackbar_Name : String;
                              Window_Name   : String;
                              Value         : access Integer;
                              Count         : Integer;
                              On_Change     : Cv_Trackbar_Callback) return Integer;

   type Cv_Trackbar_Callback2 is access procedure (Position  : Integer;
                                                   User_Data : Cv_Void_P) ;
   pragma Convention (C, Cv_Trackbar_Callback2);

   function CvCreateTrackbar2 (Trackbar_Name : String;
                               Window_Name   : String;
                               Value         : Integer;
                               Count         : Integer;
                               On_Change     : Cv_Trackbar_Callback2 := null;
                               User_Data     : Cv_Void_P) return Integer;

   -- retrieve or set trackbar position
   function CvGetTrackbarPos (Trackbar_Name : String;
                              Window_Name   : String) return Integer;

   procedure CvSetTrackbarPos (Trackbar_Name : String;
                               Window_Name   : String;
                               Pos           : Integer);

   CV_EVENT_MOUSEMOVE     : constant := 0;
   CV_EVENT_LBUTTONDOWN   : constant := 1;
   CV_EVENT_RBUTTONDOWN   : constant := 2;
   CV_EVENT_MBUTTONDOWN   : constant := 3;
   CV_EVENT_LBUTTONUP     : constant := 4;
   CV_EVENT_RBUTTONUP     : constant := 5;
   CV_EVENT_MBUTTONUP     : constant := 6;
   CV_EVENT_LBUTTONDBLCLK : constant := 7;
   CV_EVENT_RBUTTONDBLCLK : constant := 8;
   CV_EVENT_MBUTTONDBLCLK : constant := 9;

   CV_EVENT_FLAG_LBUTTON  : constant := 1;
   CV_EVENT_FLAG_RBUTTON  : constant := 2;
   CV_EVENT_FLAG_MBUTTON  : constant := 4;
   CV_EVENT_FLAG_CTRLKEY  : constant := 8;
   CV_EVENT_FLAG_SHIFTKEY : constant := 16;
   CV_EVENT_FLAG_ALTKEY   : constant := 32;

   type Cv_Mouse_Callback is access procedure (Event : Integer;
                                               X     : Integer;
                                               Y     : Integer;
                                               Flags : Integer;
                                               Param : Cv_Void_P);
   pragma Convention (C, Cv_Mouse_Callback);

   -- assign callback for mouse events
   procedure CvSetMouseCallback (Window_Name : String_C;
                                 On_Mouse    : Cv_Mouse_Callback;
                                 Param       : Cv_Void_P := null);

   CV_LOAD_IMAGE_UNCHANGED : constant := -1; --8bit, color or not
   CV_LOAD_IMAGE_GRAYSCALE : constant := 0; -- 8bit, gray
   CV_LOAD_IMAGE_COLOR : constant := 1; -- ?, color
   CV_LOAD_IMAGE_ANYDEPTH : constant := 2; -- any depth, ?
   CV_LOAD_IMAGE_ANYCOLOR : constant := 4; -- ?, any color

   -- Load image from file
   -- iscolor can be a combination of above flags where CV_LOAD_IMAGE_UNCHANGED
   -- overrides the other flags
   -- using CV_LOAD_IMAGE_ANYCOLOR alone is equivalent to CV_LOAD_IMAGE_UNCHANGED
   -- unless CV_LOAD_IMAGE_ANYDEPTH is specified images are converted to 8bit
   function CvLoadImage (Filename : String;
                         Iscolor  : Integer := CV_LOAD_IMAGE_COLOR) return Ipl_Image_P;
   function CvLoadImageM ( Filename : String;
                          Iscolor  : Integer
                          := CV_LOAD_IMAGE_COLOR) return Cv_Mat_P;



--     type File_Settings is record
--        Compression                         : Compression_Type;
--        Compression_Rate                    : Integer;
--        Not_Used                            : Integer;
--     end record;



   -- save image to file
   function CvSaveImage (Filename      : String;
                         Image         : Cv_Arr_P;
                         Settings      : File_Settings := CreateFileSettings (CV_IMWRITE_JPEG_QUALITY, 95)) return Integer;

   -- decode image stored in the buffer
   function CvDecodeImage (Buf      : Cv_Mat_P;
                           Is_Color : Integer := Cv_Load_Image_Color) return Ipl_Image_P;
   function CvDecodeImageM (Buf      : Cv_Mat_P;
                            Is_Color : Integer := Cv_Load_Image_Color) return Cv_Mat_P;

   -- encode image and store the result as a byte vector (single-row 8uC1 matrix)
   function CvEncodeImage (Ext    : String;
                           Image  : Cv_Arr_P;
                           Params : File_Settings) return Cv_Mat_P;

   type Convert_Image_Flags is (CV_CVTIMG_FLIP, CV_CVTIMG_SWAP_RB);
   for Convert_Image_Flags use (CV_CVTIMG_FLIP    => 1,
                                CV_CVTIMG_SWAP_RB => 2);
   -- utility function: convert one image to another with optional vertical flip
   procedure CvConvertImage (Src   : Cv_Arr_P;
                             Dst   : Cv_Arr_P;
                             Flags : Convert_Image_Flags);

   -- wait for key event infinitely (delay<=0) or for "delay" milliseconds
   function CvWaitKey (Ms_Delay : Integer := 0 ) return Character;

   -----------------------------------------------------------------------------
   -- Working with Video Files and Cameras
   -----------------------------------------------------------------------------


   -- start capturing frames from video file
   function CvCreateFileCapture ( Name : String ) return Cv_Capture_P;

   type Cv_Cap is new Integer;
   CV_CAP_ANY : Cv_Cap := 0; --     /  / autodetect
   CV_CAP_MIL : Cv_Cap := 100; --   // MIL proprietary drivers
   CV_CAP_VFW : Cv_Cap := 200;   --// platform native
   CV_CAP_FIREWARE : Cv_Cap := 300;   --// IEEE 1394 drivers
   CV_CAP_FIREWIRE : Cv_Cap := CV_CAP_FIREWARE;
   CV_CAP_IEEE1394 : Cv_Cap := CV_CAP_FIREWARE;
   CV_CAP_DC1394   : Cv_Cap := CV_CAP_FIREWARE;
   CV_CAP_CMU1394  : Cv_Cap := CV_CAP_FIREWARE;
   CV_CAP_STEREO : Cv_Cap := 400;   --// TYZX proprietary Drivers
   CV_CAP_TYZX     : Cv_Cap := CV_CAP_STEREO;
   CV_TYZX_LEFT    : Cv_Cap := CV_CAP_STEREO;
   CV_TYZX_RIGHT : Cv_Cap := 401;
   CV_TYZX_COLOR : Cv_Cap := 402;
   CV_TYZX_Z : Cv_Cap := 403;
   CV_CAP_QT : Cv_Cap := 500; --   // QuickTime
   CV_CAP_UNICAP : Cv_Cap := 600; --   // Unicap drivers
   CV_CAP_DSHOW : Cv_Cap := 700;   --// DirectShow (via videoInput)
   CV_CAP_PVAPI : Cv_Cap := 800;
   CV_CAP_V4L : Cv_Cap := CV_CAP_VFW;
   CV_CAP_V4L2  : Cv_Cap := CV_CAP_VFW;

   -- start capturing frames from camera: index = camera_index + domain_offset (CV_CAP_*)
   function CvCreateCameraCapture ( Index : Cv_Cap ) return Cv_Capture_P ;

   -- grab a frame, return 1 on success, 0 on fail.
   -- this function is thought to be fast
   function CvGrabFrame (Capture : Cv_Capture_P) return Integer;

   -- get the frame grabbed with cvGrabFrame(..)
   -- This function may apply some frame processing like
   -- frame decompression, flipping etc.
   function CvRetrieveFrame (Capture : Cv_Capture_P ) return Ipl_Image_P;

   --  Just a combination of cvGrabFrame and cvRetrieveFrame
   function CvQueryFrame ( Capture : Cv_Capture_P ) return Ipl_Image_P;

   -- stop capturing/reading and free resources
   procedure CvReleaseCapture ( Capture : access Cv_Capture_P );

   type Capture_Property is (CV_CAP_PROP_POS_MSEC,
                             CV_CAP_PROP_POS_FRAMES,
                             CV_CAP_PROP_POS_AVI_RATIO,
                             CV_CAP_PROP_FRAME_WIDTH,
                             CV_CAP_PROP_FRAME_HEIGHT,
                             CV_CAP_PROP_FPS,
                             CV_CAP_PROP_FOURCC,
                             CV_CAP_PROP_FRAME_COUNT,
                             CV_CAP_PROP_FORMAT,
                             CV_CAP_PROP_MODE,
                             CV_CAP_PROP_BRIGHTNESS,
                             CV_CAP_PROP_CONTRAST,
                             CV_CAP_PROP_SATURATION,
                             CV_CAP_PROP_HUE,
                             CV_CAP_PROP_GAIN,
                             CV_CAP_PROP_EXPOSURE,
                             CV_CAP_PROP_CONVERT_RGB,
                             CV_CAP_PROP_WHITE_BALANCE,
                             CV_CAP_PROP_RECTIFICATION);
   for Capture_Property use
     (CV_CAP_PROP_POS_MSEC      => 0,
      CV_CAP_PROP_POS_FRAMES    => 1,
      CV_CAP_PROP_POS_AVI_RATIO => 2,
      CV_CAP_PROP_FRAME_WIDTH   => 3,
      CV_CAP_PROP_FRAME_HEIGHT  => 4,
      CV_CAP_PROP_FPS           => 5,
      CV_CAP_PROP_FOURCC        => 6,
      CV_CAP_PROP_FRAME_COUNT   => 7,
      CV_CAP_PROP_FORMAT        => 8,
      CV_CAP_PROP_MODE          => 9,
      CV_CAP_PROP_BRIGHTNESS    => 10,
      CV_CAP_PROP_CONTRAST      => 11,
      CV_CAP_PROP_SATURATION    => 12,
      CV_CAP_PROP_HUE           => 13,
      CV_CAP_PROP_GAIN          => 14,
      CV_CAP_PROP_EXPOSURE      => 15,
      CV_CAP_PROP_CONVERT_RGB   => 16,
      CV_CAP_PROP_WHITE_BALANCE => 17,
      CV_CAP_PROP_RECTIFICATION => 18);

   -- retrieve or set capture properties
   function CvGetCaptureProperty ( Capture    : Cv_Capture_P;
                                  Property_Id :  Capture_Property ) return Float;
   function CvSetCaptureProperty ( Capture    : Cv_Capture_P;
                                  Property_Id :  Capture_Property;
                                  Value       : Float) return Integer;

   -- Return the type of the capturer (eg, CV_CAP_V4W, CV_CAP_UNICAP), which is unknown if created with CV_CAP_ANY
   function CvGetCaptureDomain (Capture : Cv_Capture) return Cv_Cap;

   -- "black box" video file writer structure
   type Cv_Video_Writer is null record;
   type Cv_Video_Writer_P is access Cv_Video_Writer;

   -- Video codec
   -- C Macro #define CV_FOURCC(c1,c2,c3,c4) (((c1)&255) + (((c2)&255)<<8) + (((c3)&255)<<16) + (((c4)&255)<<24))
   -- ('F','F','D','S') - works under windows
   function CV_FOURCC (C1 : Character;
                       C2 : Character ;
                       C3 : Character;
                       C4 : Character) return Integer;

   -- Open Codec Selection Dialog (Windows only)
   CV_FOURCC_PROMPT : Integer := -1;

   -- Use default codec for specified filename (Linux only)
   function CV_FOURCC_DEFAULT (C1 : Character := 'I';
                               C2 : Character := 'Y';
                               C3 : Character := 'U';
                               C4 : Character := 'V') return Integer renames CV_FOURCC;

   -- initialize video file writer
   function CvCreateVideoWriter (Filename       : String;
                                 Fourcc         : Integer;
                                 Fps            : Long_Float;
                                 Width          : Integer;
                                 Height         : Integer;
                                 Is_Color       : Integer) return Cv_Video_Writer_P;

   -- write frame to video file
   function CvWriteFrame ( Writer : Cv_Video_Writer_P;
                          Image  : Ipl_Image_P) return Integer;

   --- close video file writer
   procedure CvReleaseVideoWriter (Writer : access Cv_Video_Writer_P);

private
   function WCvFontQt (Name_Font  : String_C;
                       Point_Size : Integer;
                       Color      : Cv_Scalar;
                       Weight     : Cv_Font_Weight;
                       Style      : Cv_Font_Style;
                       Spacing    : Integer) return Cv_Font;

   procedure WCvAddText (Img  : Cv_Arr_P;
                         Text : String_C;
                         Org  : Cv_Point;
                         Arg2 : Cv_Font_P);

   procedure WCvDisplayOverlay (Name     : String_C;
                                Text     : String_C;
                                Delay_Ms : Integer);

   procedure WCvDisplayStatusBar (Name     : String_C;
                                  Text     : String_C;
                                  Delay_Ms : Integer);

   procedure WCvCreateOpenGLCallback (Window_Name     : String_C;
                                      Callback_OpenGL : Cv_OpenGL_Callback;
                                      User_DAta       : Cv_Void_P;
                                      Angle           : Long_Float;
                                      Zmin            : Long_Float;
                                      Zmax            : Long_Float);

   procedure WCvSaveWindowParameters (Name : String_C);

   procedure WCVLoadWindowParameters (Name : String_C);

   function WCvCreateButton (Button_Name         : String_C;
                             On_Change           : Cv_Button_Callback;
                             User_Data           : Cv_Void_P;
                             Button_Type         : Cv_Button_Type;
                             Intial_Button_State : Integer) return Integer;
   --
   function WCvInitSystem (Argc : Integer;
                           Argv : C_String_Ptr) return Integer;

   function WCvNamedWindow (WindowName  : String_C;
                            Flags       : Highgui_Window_Params := CV_WINDOW_AUTOSIZE) return Integer;

   procedure WCvSetWindowProperty (Name       : String_C;
                                   Prop_Id    : Highgui_Window_Params;
                                   Prop_Value : Long_Float);
   function WCvGetWindowProperty (Name    : String_C;
                                  Prop_Id : Highgui_Window_Params) return Long_Float;

   procedure WCvShowImage (WindowName  : String_C;
                           Image       : Cv_Arr_P);

   procedure WCvResizeWindow (WindowName   : String_C;
                              Width        : Integer;
                              Height       : Integer );

   procedure WCvMoveWindow (WindowName : String_C;
                            X          : Integer;
                            Y          : Integer);

   procedure WCvDestroyWindow (WindowName : String );

   function WCvGetWindowHandle (Window_Name : String_C)
                                return Cv_Void_P;

   function WCvCreateTrackbar (Trackbar_Name : String_C;
                               Window_Name   : String_C;
                               Value         : access Integer;
                               Count         : Integer;
                               On_Change     : Cv_Trackbar_Callback) return Integer;

   function WCvCreateTrackbar2 (Trackbar_Name : String_C;
                                Window_Name   : String_C;
                                Value         : Integer;
                                Count         : Integer;
                                On_Change     : Cv_Trackbar_Callback2 := null;
                                User_Data     : Cv_Void_P) return Integer;

   -- retrieve or set trackbar position
   function WCvGetTrackbarPos (Trackbar_Name : String_C;
                               Window_Name   : String_C) return Integer;

   procedure WCvSetTrackbarPos (Trackbar_Name : String_C;
                                Window_Name   : String_C;
                                Pos           : Integer);
   function WCvLoadImage ( Filename : String_C; Iscolor : Integer) return Ipl_Image_P;
   function WCvLoadImageM ( Filename : String_C; Iscolor : Integer) return Cv_Mat_P;

   function WCvSaveImage (Filename      : String_C;
                          Image         : Cv_Arr_P;
                          Settings      : File_Settings) return Integer;

   function WCvEncodeImage (Ext    : String_C;
                            Image  : Cv_Arr_P;
                            Params : File_Settings) return Cv_Mat_P;

   function WCvCreateFileCapture ( Name : String_C ) return Cv_Capture_P;

   function WCvCreateVideoWriter (Filename       : String_C;
                                  Fourcc         : Integer;
                                  Fps            : Long_Float;
                                  Width          : Integer;
                                  Height         : Integer;
                                  Is_Color       : Integer
                                 ) return Cv_Video_Writer_P;

   pragma Import (C, WCvFontQt, "cvFontQt");
   pragma Import (C, WCvAddText, "cvAddText");
   pragma Import (C, WCvDisplayOverlay, "cvDisplayOverlay");
   pragma Import (C, WCVDisplayStatusBar, "cvDisplayStatusBar");
   pragma Import (C, WCvCreateOpenGLCallback, "cvCreateOpenGLCallback");
   pragma Import (C, WcvSaveWindowParameters, "cvSaveWindowParameters");
   pragma Import (C, WCVLoadWindowParameters, "cvLoadWindowParameters");
   pragma Import (C, CvStartLoop, "cvStartLoop");
   pragma Import (C, CvStopLoop, "cvStopLoop");
   pragma Import (C, WCvCreateButton, "cvCreateButton");
   --
   pragma Import (C, WCvInitSystem, "cvInitSystem");
   pragma Import (C, CvStartWindowThread, "cvStartWindowThread");
   pragma Import (C, WCvNamedWindow, "cvNamedWindow");
   pragma Import (C, WCvSetWindowProperty, "cvSetWindowProperty");
   pragma Import (C, WCvGetWindowProperty, "cvGetWindowProperty");
   pragma Import (C, WCvShowImage, "cvShowImage");
   pragma Import (C, WCvResizeWindow, "cvResizeWindow");
   pragma Import (C, WCvMoveWindow, "cvMoveWindow");
   pragma Import (C, WCvDestroyWindow, "cvDestroyWindow");
   pragma Import (C, CvDestroyAllWindows, "cvDestroyAllWindows");
   pragma Import (C, WCvGetWindowHandle, "cvGetWindowHandle");
   pragma Import (C, CvGetWindowName, "cvGetWindowName");
   pragma Import (C, WCvCreateTrackbar, "cvCreateTrackbar");
   pragma Import (C, WCvCreateTrackbar2, "cvCreateTrackbar2");
   pragma Import (C, WCvGetTrackbarPos, "cvGetTrackbarPos");
   pragma Import (C, WCvSetTrackbarPos, "cvSetTrackbarPos");
   pragma Import (C, CvSetMouseCallback, "cvSetMouseCallback");
   pragma Import (C, WCvLoadImage, "cvLoadImage");
   pragma Import (C, WCvLoadImageM, "cvLoadImageM");
   pragma Import (C, WCvSaveImage, "cvSaveImage");
   pragma Import (C, CvDecodeImage, "cvDecodeImage");
   pragma Import (C, CvDecodeImageM, "cvDecodeImageM");
   pragma Import (C, WCvEncodeImage, "cvEncodeImage");
   pragma Import (C, CvConvertImage, "cvConvertImage");
   pragma Import (C, CvWaitKey, "cvWaitKey");
   pragma Import (C, WCvCreateFileCapture, "cvCreateFileCapture");
   pragma Import (C, CvCreateCameraCapture, "cvCreateCameraCapture");
   pragma Import (C, CvGrabFrame, "cvGrabFrame");
   pragma Import (C, CvRetrieveFrame, "cvRetrieveFrame");
   pragma Import (C, CvQueryFrame, "cvQueryFrame");
   pragma Import (C, CvReleaseCapture, "cvReleaseCapture");
   pragma Import (C, CvGetCaptureProperty, "cvGetCaptureProperty");
   pragma Import (C, CvSetCaptureProperty, "cvSetCaptureProperty");
   pragma Import (C, CvGetCaptureDomain, "cvGetCaptureDomain");
   pragma Import (C, WCvCreateVideoWriter, "cvCreateVideoWriter");
   pragma Import (C, CvWriteFrame, "cvWriteFrame");
   pragma Import (C, CvReleaseVideoWriter, "cvReleaseVideoWriter");
end Highgui;
