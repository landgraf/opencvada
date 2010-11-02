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
-- highgui_c.adb - highgui_c.h
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Highgui.Highgui_C is
--

   --  //for color cvScalar(blue_component, green_component, red\_component[, alpha_component])
   --  //and alpha= 0 <-> 0xFF (not transparent <-> transparent)
   function CvFontQt (Name_Font  : String;
                      Point_Size : Integer := -1;
                      Color      : Cv_Scalar := Core_Types_C.CvScalarAll (0.0);
                      Weight     : Cv_Font_Weight := CV_FONT_NORMAL;
                      Style      : Cv_Font_Style :=  CV_STYLE_NORMAL;
                      Spacing    : Integer := 0) return Cv_Font is
   begin
      return WCvFontQt (Name_Font  => +Name_Font,
                        Point_Size => Point_Size,
                        Color      => Color,
                        Weight     => Weight,
                        Style      => Style,
                        Spacing    => Spacing);
   end CvFontQt;

   procedure CvAddText (Img  : Cv_Arr_P;
                        Text : String;
                        Org  : Cv_Point;
                        Arg2 : Cv_Font_P) is
   begin
      WcvAddText (Img, +Text, Org, Arg2);
   end CvAddText;

   procedure CvDisplayOverlay (Name     : String;
                               Text     : String;
                               Delay_Ms : Integer) is
   begin
      WCvDisplayOverlay (+Name, +Text, Delay_Ms);
   end CvDisplayOverlay;

   procedure CvDisplayStatusBar (Name     : String;
                                 Text     : String;
                                 Delay_Ms : Integer) is
   begin
      WCvDisplayStatusBar (+Name, +Text, Delay_Ms);
   end CvDisplayStatusBar;

   procedure CvCreateOpenGLCallback (Window_Name     : String;
                                     Callback_OpenGL : Cv_OpenGL_Callback;
                                     User_DAta       : Cv_Void_P := null;
                                     Angle           : Long_Float := -1.0;
                                     Zmin            : Long_Float := -1.0;
                                     Zmax            : Long_Float := -1.0) is
   begin
      WCvCreateOpenGLCallback (+Window_Name, Callback_OpenGL, User_Data, Angle, Zmin, Zmax);
   end CvCreateOpenGLCallback;

   procedure CvSaveWindowParameters (Name : String) is
   begin
      WCvSaveWindowParameters (+Name);
   end CvSaveWindowParameters;

   procedure CvLoadWindowParameters (Name : String) is
   begin
      WCVLoadWindowParameters (+Name);
   end CVLoadWindowParameters;

   function CvCreateButton (Button_Name         : String;
                            On_Change           : Cv_Button_Callback := null;
                            User_Data           : Cv_Void_P := null;
                            Button_Type         : Cv_Button_Type := CV_PUSH_BUTTON;
                            Intial_Button_State : Integer := 0) return Integer is
   begin
      return WcvCreateButton (+Button_Name, On_Change, User_Data, Button_Type, Intial_Button_State);
   end CvCreateButton;

   -----------------------------------------------------------------------------
   --
   -----------------------------------------------------------------------------

   -- Initialisez HighGUI
   -- Not used in Windows.
   function CvInitSystem (Argc : Integer;
                          Argv : C_String_Ptr) return Integer is
   begin
      return WCvInitSystem (Argc, Argv);
   end CvInitSystem;

   -- Creates a Window.
   -- Name : Identifier of the window, and titlebar name.
   -- Flags : CV_WINDOW_AUTOSIZE or 0
   -- Return : ?
   function CvNamedWindow (WindowName  : String;
                           Flags       : Highgui_Window_Params := CV_WINDOW_AUTOSIZE) return Integer is
   begin
      return WCvNamedWindow (+WindowName, Flags);
   end CvNamedWindow;

   procedure CvSetWindowProperty (Name       : String;
                                  Prop_Id    : Highgui_Window_Params;
                                  Prop_Value : Long_Float) is
   begin
      WCvSetWindowProperty (+Name, Prop_Id, Prop_Value);
   end CvSetWindowProperty;

   function CvGetWindowProperty (Name    : String;
                                 Prop_Id : Highgui_Window_Params) return Long_Float is
   begin
      return WCvGetWindowProperty (+Name, Prop_Id);
   end CvGetWindowProperty;

   procedure CvShowImage (WindowName  : String;
                          Image       : Cv_Arr_P) is
   begin
      WCvShowImage (+WindowName, Image);
   end CvShowImage;

   procedure CvResizeWindow (WindowName   : String;
                             Width        : Integer;
                             Height       : Integer ) is
   begin
      WCvResizeWindow (+WindowName, Width, Height);
   end CvREsizeWindow;

   procedure CvMoveWindow (WindowName : String;
                           X          : Integer;
                           Y          : Integer) is
   begin
      WCvMoveWindow (+WindowName, X, Y);
   end CvMoveWindow;

   procedure CvDestroyWindow (WindowName : String ) is
   begin
      WCvDestroyWindow (WindowName & ASCII.NUL);
   end CvDestroyWindow;

   function CvGetWindowHandle (Window_Name : String)
                               return Cv_Void_P is
   begin
      return WCvGetWindowHandle (+Window_Name);
   end CvGetWindowHandle;

   function CvCreateTrackbar (Trackbar_Name : String;
                              Window_Name   : String;
                              Value         : access Integer;
                              Count         : Integer;
                              On_Change     : Cv_Trackbar_Callback) return Integer is
   begin
      return WCvCreateTrackbar ( +Trackbar_Name, +Window_Name, Value, Count, On_Change);
   end CvCreateTrackbar;

   function CvCreateTrackbar2 (Trackbar_Name : String;
                               Window_Name   : String;
                               Value         : Integer;
                               Count         : Integer;
                               On_Change     : Cv_Trackbar_Callback2 := null;
                               User_Data     : Cv_Void_P) return Integer is
   begin
      return WCvCreateTrackbar2 (+Trackbar_Name, +Window_Name, Value, Count, On_Change, User_Data);
   end CvCreateTrackbar2;

   function CvGetTrackbarPos (Trackbar_Name : String;
                              Window_Name   : String) return Integer is
   begin
      return WCvGetTrackbarPos (+Trackbar_Name, +Window_Name);
   end CvGetTrackbarPos;

   procedure CvSetTrackbarPos (Trackbar_Name : String;
                               Window_Name   : String;
                               Pos           : Integer) is
   begin
      Wcvsettrackbarpos (+Trackbar_Name,
                         +Window_Name,
                         Pos);
   end CvSetTrackbarPos;

   function CvLoadImage (Filename : String;
                         Iscolor  : Integer := CV_LOAD_IMAGE_COLOR) return Ipl_Image_P is
   begin
      return WCvLoadImage (+Filename, Iscolor);
   end CvLoadImage;

   function CvLoadImageM ( Filename : String;
                          Iscolor  : Integer := CV_LOAD_IMAGE_COLOR) return Cv_Mat_P is
   begin
      return WCvLoadImageM (+Filename, Iscolor);
   end CvLoadImageM;

   function CvSaveImage (Filename      : String;
                         Image         : Cv_Arr_P;
                         Settings      : File_Settings := CreateFileSettings (CV_IMWRITE_JPEG_QUALITY, 95)) return Integer is
   begin
      return WCvSaveImage (+Filename, Image, Settings);
   end CvSaveImage;



   function CvEncodeImage (Ext    : String;
                           Image  : Cv_Arr_P;
                           Params : File_Settings) return Cv_Mat_P is
   begin
      return WCvEncodeImage (+Ext, Image, Params);
   end CvEncodeImage;

   function CvCreateFileCapture ( Name : String ) return Cv_Capture_P is
   begin
      return WCvCreateFileCapture (+Name);
   end CvCreateFileCapture;

   -- C Macro #define CV_FOURCC(c1,c2,c3,c4) (((c1)&255) + (((c2)&255)<<8) + (((c3)&255)<<16) + (((c4)&255)<<24))
   function CV_FOURCC ( C1 : Character; C2 : Character ; C3 : Character; C4 : Character) return Integer is
      Result : Unsigned_32 := 0;
   begin
      Result := (Unsigned_32 (Character'Pos (C1)) and 255) + (Shift_Left (Unsigned_32 (Character'Pos (C2)) and 255, 8)) + Shift_Left (Unsigned_32 (Character'Pos (C3)) and 255, 16) + Shift_Left (Unsigned_32 (Character'Pos (C4)) and 255, 24);
      --Result := Unsigned_32 (Character'Pos (C1)) + (Shift_Left (Unsigned_32 (Character'Pos (C2)), 8)) + Shift_Left (Unsigned_32 (Character'Pos (C3)), 16) + Shift_Left (Unsigned_32 (Character'Pos (C4)), 24);
      --Ada.Text_IO.Put_Line ("Result: " & Result'Img & Shift_Left (Unsigned_32 (Character'Pos (C2))and 255, 8)'Img);
      return Integer (Result);
   end CV_FOURCC;

   function CvCreateVideoWriter (Filename       : String;
                                 Fourcc         : Integer;
                                 Fps            : Long_Float;
                                 Width          : Integer;
                                 Height         : Integer;
                                 Is_Color       : Integer) return Cv_Video_Writer_P is
   begin
      return WCvCreateVideoWriter (+Filename, Fourcc, Fps, Width, Height, Is_Color);
   end CvCreateVideoWriter;

end Highgui.Highgui_C;
