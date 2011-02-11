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

package Highgui is
--

   -----------------------------------------------------------------------------
   -- Basic GUI functions
   -----------------------------------------------------------------------------

   -------
   -- Ada stuff
   ---------
   type Cv_Capture is null record;
   type Cv_Capture_P is access all Cv_Capture;

   type Compression_Type is new Integer;
   Cv_Imwrite_Jpeg_Quality : constant Compression_Type := 1;
   Cv_Imwrite_Png_Compression : constant Compression_Type := 16;
   Cv_Imwrite_Pxm_Binary : constant Compression_Type := 32;

   type File_Settings is record
      Compression                         : Compression_Type;
      Compression_Rate                    : Integer;
      Not_Used                            : Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, File_Settings);

   -- Creates a new File_Settings struct
   function Create_File_Settings ( Compression     : Compression_Type;
                                  Compression_Rate : Integer;
                                  Not_Used         : Integer := 0) return File_Settings;



   -----------------------------------------------------------------------------
   -- QT
   -----------------------------------------------------------------------------

   --// new font for QT
   type Cv_Font_Weight is new Integer;
   Cv_Font_Light : constant Cv_Font_Weight := 25;
   Cv_Font_Normal : constant Cv_Font_Weight := 50;
   Cv_Font_Demibold : constant Cv_Font_Weight := 63;
   Cv_Font_Bold : constant Cv_Font_Weight := 75;
   Cv_Font_Black : constant Cv_Font_Weight := 87;

   type Cv_Font_Style is new Integer;
   Cv_Style_Normal : constant Cv_Font_Style := 0;
   Cv_Style_Italic : constant Cv_Font_Style := 1;
   Cv_Style_Oblique : constant Cv_Font_Style := 2;

   --  //for color cvScalar(blue_component, green_component, red\_component[, alpha_component])
   --  //and alpha= 0 <-> 0xFF (not transparent <-> transparent)
   function Cv_Font_Qt (Name_Font  : String;
                        Point_Size : Integer := -1;
                        Color      : Cv_Scalar := Core.Cv_Scalar_All (0.0);
                        Weight     : Cv_Font_Weight := Cv_Font_Normal;
                        Style      : Cv_Font_Style :=  Cv_Style_Normal;
                        Spacing    : Integer := 0) return Cv_Font;

   procedure Cv_Add_Text (Img  : Cv_Arr_P;
                          Text : String;
                          Org  : Cv_Point;
                          Arg2 : Cv_Font_P);
   procedure Cv_Add_Text (Img  : Cv_Mat_P;
                          Text : String;
                          Org  : Cv_Point;
                          Arg2 : Cv_Font_P);
   procedure Cv_Add_Text (Img  : Ipl_Image_P;
                          Text : String;
                          Org  : Cv_Point;
                          Arg2 : Cv_Font_P);

   procedure Cv_Display_Overlay (Name     : String;
                                 Text     : String;
                                 Delay_Ms : Integer);

   procedure Cv_Display_Status_Bar (Name     : String;
                                    Text     : String;
                                    Delay_Ms : Integer);

   type Cv_Opengl_Callback is access procedure (User_Data : Cv_Void_P);
   pragma Convention (C, Cv_Opengl_Callback);

   procedure Cv_Create_Opengl_Callback (Window_Name     : String;
                                        Callback_Opengl : Cv_Opengl_Callback;
                                        User_Data       : Cv_Void_P := null;
                                        Angle           : Long_Float := -1.0;
                                        Zmin            : Long_Float := -1.0;
                                        Zmax            : Long_Float := -1.0);

   procedure Cv_Save_Window_Parameters (Name : String);

   procedure Cv_Load_Window_Parameters (Name : String);

   type Cvstartloop_Function is access function (Argc : Integer;
                                                 Argv : Cv_String_Pointer) return Integer;
   pragma Convention (C, Cvstartloop_Function);

   function Cv_Start_Loop (Pt2func : Cvstartloop_Function;
                           Argc    : Integer;
                           Argv    : Cv_String_Pointer) return Integer;

   procedure Cv_Stop_Loop;

   type Cv_Button_Callback is access procedure (State     : Integer;
                                                User_Data : Cv_Void_P);
   pragma Convention (C, Cv_Button_Callback);

   type Cv_Button_Type is new Integer;
   Cv_Push_Button : constant Cv_Button_Type := 0;
   Cv_Checkbox : constant Cv_Button_Type := 1;
   Cv_Radiobox : constant Cv_Button_Type := 2;

   function Cv_Create_Button (Button_Name         : String;
                              On_Change           : Cv_Button_Callback := null;
                              User_Data           : Cv_Void_P := null;
                              Button_Type         : Cv_Button_Type := Cv_Push_Button;
                              Intial_Button_State : Integer := 0) return Integer;

   procedure Cv_Create_Button (Button_Name         : String;
                               On_Change           : Cv_Button_Callback := null;
                               User_Data           : Cv_Void_P := null;
                               Button_Type         : Cv_Button_Type := Cv_Push_Button;
                               Intial_Button_State : Integer := 0);

   -----------------------------------------------------------------------------
   --
   -----------------------------------------------------------------------------

   -- this function is used to set some external parameters in case of X Window
   function Cv_Init_System (Argc : Integer;
                            Argv : Cv_String_Pointer) return Integer;

   function Cv_Start_Window_Thread return Integer;

   type Highgui_Window_Params is new Short_Integer;
   -- These 3 flags are used by cvSet/GetWindowProperty
   Cv_Wnd_Prop_Fullscreen : constant Highgui_Window_Params := 0; -- To change/get window's fullscreen property
   Cv_Wnd_Prop_Autosize : constant Highgui_Window_Params := 1; -- To change/get window's autosize property
   Cv_Wnd_Prop_Aspectratio : constant Highgui_Window_Params := 2; --To change/get window's aspectratio property

   -- These 2 flags are used by cvNamedWindow and cvSet/GetWindowProperty
   Cv_Window_Normal : constant Highgui_Window_Params := 16#0000_0000#; -- The user can resize the window (no constraint)  / also use to switch a fullscreen window to a normal size
   Cv_Window_Autosize : constant Highgui_Window_Params := 16#0000_0001#; -- The user cannot resize the window, the size is constrainted by the image displayed

   -- Those flags are only for Qt
   Cv_Gui_Expanded : constant Highgui_Window_Params := 16#0000_0000#; -- Status bar and tool bar
   Cv_Gui_Normal : constant Highgui_Window_Params := 16#0000_0010#; -- Old fashious way

   -- These 3 flags are used by cvNamedWindow and cvSet/GetWindowProperty
   Cv_Window_Fullscreen : constant Highgui_Window_Params := 1; -- Change the window to fullscreen
   Cv_Window_Freeratio : constant Highgui_Window_Params := 16#0000_0100#; -- The image expends as much as it can (no ratio constraint)
   Cv_Window_Keepratio  : constant Highgui_Window_Params := 16#0000_0000#; -- The ration image is respected.

   -- create window
   function Cv_Named_Window (Windowname  : String;
                             Flags       : Highgui_Window_Params := Cv_Window_Autosize) return Integer;
   procedure Cv_Named_Window (Windowname  : String;
                              Flags       : Highgui_Window_Params := Cv_Window_Autosize);

   -- Set and Get Property of the window
   procedure Cv_Set_Window_Property (Name       : String;
                                     Prop_Id    : Highgui_Window_Params;
                                     Prop_Value : Long_Float);
   function Cv_Get_Window_Property (Name    : String;
                                    Prop_Id : Highgui_Window_Params) return Long_Float;

   -- display image within window (highgui windows remember their content)
   procedure Cv_Show_Image (Windowname  : String;
                            Image       : Cv_Arr_P);
   procedure Cv_Show_Image (Windowname  : String;
                            Image       : Cv_Mat_P);
   procedure Cv_Show_Image (Windowname  : String;
                            Image       : Ipl_Image_P);

   -- resize/move window
   procedure Cv_Resize_Window (Windowname   : String;
                               Width        : Integer;
                               Height       : Integer );
   procedure Cv_Move_Window (Windowname : String;
                             X          : Integer;
                             Y          : Integer);

   -- destroy window and all the trackers associated with it
   procedure Cv_Destroy_Window (Windowname : String );
   procedure Cv_Destroy_All_Windows;

   -- get native window handle (HWND in case of Win32 and Widget in case of X Window)
   function Cv_Get_Window_Handle (Window_Name : String)
                                  return Cv_Void_P;

   function Cv_Get_Window_Name (Window_Handle : Cv_Void_P)
                                return Interfaces.C.Strings.Chars_Ptr;

   type Cv_Trackbar_Callback is access procedure ( Position : Integer ) ;
   pragma Convention (C, Cv_Trackbar_Callback);

   --create trackbar and display it on top of given window, set callback
   function Cv_Create_Trackbar (Trackbar_Name : String;
                                Window_Name   : String;
                                Value         : access Integer;
                                Count         : Integer;
                                On_Change     : Cv_Trackbar_Callback) return Integer;
   procedure Cv_Create_Trackbar (Trackbar_Name : String;
                                 Window_Name   : String;
                                 Value         : access Integer;
                                 Count         : Integer;
                                 On_Change     : Cv_Trackbar_Callback);

   type Cv_Trackbar_Callback2 is access procedure (Position  : Integer;
                                                   User_Data : Cv_Void_P) ;
   pragma Convention (C, Cv_Trackbar_Callback2);

   function Cv_Create_Trackbar2 (Trackbar_Name : String;
                                 Window_Name   : String;
                                 Value         : Integer;
                                 Count         : Integer;
                                 On_Change     : Cv_Trackbar_Callback2 := null;
                                 User_Data     : Cv_Void_P) return Integer;

   procedure Cv_Create_Trackbar2 (Trackbar_Name : String;
                                  Window_Name   : String;
                                  Value         : Integer;
                                  Count         : Integer;
                                  On_Change     : Cv_Trackbar_Callback2 := null;
                                  User_Data     : Cv_Void_P);

   -- retrieve or set trackbar position
   function Cv_Get_Trackbar_Pos (Trackbar_Name : String;
                                 Window_Name   : String) return Integer;

   procedure Cv_Set_Trackbar_Pos (Trackbar_Name : String;
                                  Window_Name   : String;
                                  Pos           : Integer);

   Cv_Event_Mousemove     : constant := 0;
   Cv_Event_Lbuttondown   : constant := 1;
   Cv_Event_Rbuttondown   : constant := 2;
   Cv_Event_Mbuttondown   : constant := 3;
   Cv_Event_Lbuttonup     : constant := 4;
   Cv_Event_Rbuttonup     : constant := 5;
   Cv_Event_Mbuttonup     : constant := 6;
   Cv_Event_Lbuttondblclk : constant := 7;
   Cv_Event_Rbuttondblclk : constant := 8;
   Cv_Event_Mbuttondblclk : constant := 9;

   Cv_Event_Flag_Lbutton  : constant := 1;
   Cv_Event_Flag_Rbutton  : constant := 2;
   Cv_Event_Flag_Mbutton  : constant := 4;
   Cv_Event_Flag_Ctrlkey  : constant := 8;
   Cv_Event_Flag_Shiftkey : constant := 16;
   Cv_Event_Flag_Altkey   : constant := 32;

   type Cv_Mouse_Callback is access procedure (Event : Integer;
                                               X     : Integer;
                                               Y     : Integer;
                                               Flags : Integer;
                                               Param : Cv_Void_P);
   pragma Convention (C, Cv_Mouse_Callback);

   -- assign callback for mouse events
   procedure Cv_Set_Mouse_Callback (Window_Name : String_C;
                                    On_Mouse    : Cv_Mouse_Callback;
                                    Param       : Cv_Void_P := null);

   Cv_Load_Image_Unchanged : constant := -1; --8bit, color or not
   Cv_Load_Image_Grayscale : constant := 0; -- 8bit, gray
   Cv_Load_Image_Color     : constant := 1; -- ?, color
   Cv_Load_Image_Anydepth  : constant := 2; -- any depth, ?
   Cv_Load_Image_Anycolor  : constant := 4; -- ?, any color

   -- Load image from file
   -- iscolor can be a combination of above flags where CV_LOAD_IMAGE_UNCHANGED
   -- overrides the other flags
   -- using CV_LOAD_IMAGE_ANYCOLOR alone is equivalent to CV_LOAD_IMAGE_UNCHANGED
   -- unless CV_LOAD_IMAGE_ANYDEPTH is specified images are converted to 8bit
   function Cv_Load_Image (Filename : String;
                           Iscolor  : Integer := Cv_Load_Image_Color) return Ipl_Image_P;
   function Cv_Load_Image_M ( Filename : String;
                             Iscolor  : Integer
                             := Cv_Load_Image_Color) return Cv_Mat_P;

   -- save image to file
   function Cv_Save_Image (Filename      : String;
                           Image         : Cv_Arr_P;
                           Settings      : File_Settings := Create_File_Settings (Cv_Imwrite_Jpeg_Quality, 95)) return Integer;

   procedure Cv_Save_Image (Filename      : String;
                            Image         : Cv_Arr_P;
                            Settings      : File_Settings := Create_File_Settings (Cv_Imwrite_Jpeg_Quality, 95));
   function Cv_Save_Image (Filename      : String;
                           Image         : Cv_Mat_P;
                           Settings      : File_Settings := Create_File_Settings (Cv_Imwrite_Jpeg_Quality, 95)) return Integer;

   procedure Cv_Save_Image (Filename      : String;
                            Image         : Cv_Mat_P;
                            Settings      : File_Settings := Create_File_Settings (Cv_Imwrite_Jpeg_Quality, 95));
   function Cv_Save_Image (Filename      : String;
                           Image         : Ipl_Image_P;
                           Settings      : File_Settings := Create_File_Settings (Cv_Imwrite_Jpeg_Quality, 95)) return Integer;

   procedure Cv_Save_Image (Filename      : String;
                            Image         : Ipl_Image_P;
                            Settings      : File_Settings := Create_File_Settings (Cv_Imwrite_Jpeg_Quality, 95));

   -- decode image stored in the buffer
   function Cv_Decode_Image (Buf      : Cv_Mat_P;
                             Is_Color : Integer := Cv_Load_Image_Color) return Ipl_Image_P;
   function Cv_Decode_Image_M (Buf      : Cv_Mat_P;
                               Is_Color : Integer := Cv_Load_Image_Color) return Cv_Mat_P;

   -- encode image and store the result as a byte vector (single-row 8uC1 matrix)
   function Cv_Encode_Image (Ext    : String;
                             Image  : Cv_Arr_P;
                             Params : File_Settings) return Cv_Mat_P;
   function Cv_Encode_Image (Ext    : String;
                             Image  : Cv_Mat_P;
                             Params : File_Settings) return Cv_Mat_P;
   function Cv_Encode_Image (Ext    : String;
                             Image  : Ipl_Image_P;
                             Params : File_Settings) return Cv_Mat_P;

   type Convert_Image_Flags is new Integer;
   Cv_Cvtimg_Flip : constant Convert_Image_Flags := 1;
   Cv_Cvtimg_Swap_Rb : constant Convert_Image_Flags := 2;
   -- utility function: convert one image to another with optional vertical flip
   procedure Cv_Convert_Image (Src   : Cv_Arr_P;
                               Dst   : Cv_Arr_P;
                               Flags : Convert_Image_Flags);
   procedure Cv_Convert_Image (Src   : Cv_Mat_P;
                               Dst   : Cv_Mat_P;
                               Flags : Convert_Image_Flags);
   procedure Cv_Convert_Image (Src   : Ipl_Image_P;
                               Dst   : Ipl_Image_P;
                               Flags : Convert_Image_Flags);

   -- wait for key event infinitely (delay<=0) or for "delay" milliseconds
   function Cv_Wait_Key (Ms_Delay : Integer := 0 ) return Character;
   procedure Cv_Wait_Key (Ms_Delay : Integer := 0 );

   -----------------------------------------------------------------------------
   -- Working with Video Files and Cameras
   -----------------------------------------------------------------------------


   -- start capturing frames from video file
   function Cv_Create_File_Capture ( Name : String ) return Cv_Capture_P;

   type Cv_Cap is new Integer;
   Cv_Cap_Any : Cv_Cap := 0; --     /  / autodetect
   Cv_Cap_Mil : Cv_Cap := 100; --   // MIL proprietary drivers
   Cv_Cap_Vfw : Cv_Cap := 200;   --// platform native
   Cv_Cap_Fireware : Cv_Cap := 300;   --// IEEE 1394 drivers
   Cv_Cap_Firewire : Cv_Cap := Cv_Cap_Fireware;
   Cv_Cap_Ieee1394 : Cv_Cap := Cv_Cap_Fireware;
   Cv_Cap_Dc1394   : Cv_Cap := Cv_Cap_Fireware;
   Cv_Cap_Cmu1394  : Cv_Cap := Cv_Cap_Fireware;
   Cv_Cap_Stereo : Cv_Cap := 400;   --// TYZX proprietary Drivers
   Cv_Cap_Tyzx     : Cv_Cap := Cv_Cap_Stereo;
   Cv_Tyzx_Left    : Cv_Cap := Cv_Cap_Stereo;
   Cv_Tyzx_Right : Cv_Cap := 401;
   Cv_Tyzx_Color : Cv_Cap := 402;
   Cv_Tyzx_Z : Cv_Cap := 403;
   Cv_Cap_Qt : Cv_Cap := 500; --   // QuickTime
   Cv_Cap_Unicap : Cv_Cap := 600; --   // Unicap drivers
   Cv_Cap_Dshow : Cv_Cap := 700;   --// DirectShow (via videoInput)
   Cv_Cap_Pvapi : Cv_Cap := 800;
   Cv_Cap_V4l : Cv_Cap := Cv_Cap_Vfw;
   Cv_Cap_V4l2  : Cv_Cap := Cv_Cap_Vfw;

   -- start capturing frames from camera: index = camera_index + domain_offset (CV_CAP_*)
   function Cv_Create_Camera_Capture ( Index : Cv_Cap ) return Cv_Capture_P ;

   -- grab a frame, return 1 on success, 0 on fail.
   -- this function is thought to be fast
   function Cv_Grab_Frame (Capture : Cv_Capture_P) return Integer;
   procedure Cv_Grab_Frame (Capture : Cv_Capture_P);

   -- get the frame grabbed with cvGrabFrame(..)
   -- This function may apply some frame processing like
   -- frame decompression, flipping etc.
   function Cv_Retrieve_Frame (Capture : Cv_Capture_P ) return Ipl_Image_P;

   --  Just a combination of cvGrabFrame and cvRetrieveFrame
   function Cv_Query_Frame ( Capture : Cv_Capture_P ) return Ipl_Image_P;

   -- stop capturing/reading and free resources
   procedure Cv_Release_Capture ( Capture : access Cv_Capture_P );

   type Capture_Property is new Integer;
   Cv_Cap_Prop_Pos_Msec : constant Capture_Property := 0;
   Cv_Cap_Prop_Pos_Frames : constant Capture_Property := 1;
   Cv_Cap_Prop_Pos_Avi_Ratio : constant Capture_Property := 2;
   Cv_Cap_Prop_Frame_Width : constant Capture_Property := 3;
   Cv_Cap_Prop_Frame_Height : constant Capture_Property := 4;
   Cv_Cap_Prop_Fps : constant Capture_Property := 5;
   Cv_Cap_Prop_Fourcc : constant Capture_Property := 6;
   Cv_Cap_Prop_Frame_Count : constant Capture_Property := 7;
   Cv_Cap_Prop_Format : constant Capture_Property := 8;
   Cv_Cap_Prop_Mode : constant Capture_Property := 9;
   Cv_Cap_Prop_Brightness : constant Capture_Property := 10;
   Cv_Cap_Prop_Contrast : constant Capture_Property := 11;
   Cv_Cap_Prop_Saturation : constant Capture_Property := 12;
   Cv_Cap_Prop_Hue : constant Capture_Property := 13;
   Cv_Cap_Prop_Gain : constant Capture_Property := 14;
   Cv_Cap_Prop_Exposure : constant Capture_Property := 15;
   Cv_Cap_Prop_Convert_Rgb : constant Capture_Property := 16;
   Cv_Cap_Prop_White_Balance : constant Capture_Property := 17;
   Cv_Cap_Prop_Rectification : constant Capture_Property := 18;

   -- retrieve or set capture properties
   function Cv_Get_Capture_Property ( Capture    : Cv_Capture_P;
                                     Property_Id :  Capture_Property ) return Float;
   function Cv_Set_Capture_Property ( Capture    : Cv_Capture_P;
                                     Property_Id :  Capture_Property;
                                     Value       : Float) return Integer;

   -- Return the type of the capturer (eg, CV_CAP_V4W, CV_CAP_UNICAP), which is unknown if created with CV_CAP_ANY
   function Cv_Get_Capture_Domain (Capture : Cv_Capture_P) return Cv_Cap;

   -- "black box" video file writer structure
   type Cv_Video_Writer is null record;
   pragma Convention (C_Pass_By_Copy, Cv_Video_Writer);
   type Cv_Video_Writer_P is access all Cv_Video_Writer;

   -- Video codec
   -- C Macro #define CV_FOURCC(c1,c2,c3,c4) (((c1)&255) + (((c2)&255)<<8) + (((c3)&255)<<16) + (((c4)&255)<<24))
   -- ('F','F','D','S') - works under windows
   function Cv_Fourcc (C1 : Character;
                       C2 : Character ;
                       C3 : Character;
                       C4 : Character) return Integer;

   -- Open Codec Selection Dialog (Windows only)
   Cv_Fourcc_Prompt : Integer := -1;

   -- Use default codec for specified filename (Linux only)
   function Cv_Fourcc_Default (C1 : Character := 'I';
                               C2 : Character := 'Y';
                               C3 : Character := 'U';
                               C4 : Character := 'V') return Integer renames Cv_Fourcc;

   -- initialize video file writer
   function Cv_Create_Video_Writer (Filename       : String;
                                    Fourcc         : Integer;
                                    Fps            : Long_Float;
                                    Width          : Integer;
                                    Height         : Integer;
                                    Is_Color       : Integer) return Cv_Video_Writer_P;

   -- write frame to video file
   function Cv_Write_Frame ( Writer : Cv_Video_Writer_P;
                            Image  : Ipl_Image_P) return Integer;
   procedure Cv_Write_Frame ( Writer : Cv_Video_Writer_P;
                             Image  : Ipl_Image_P);

   --- close video file writer
   procedure Cv_Release_Video_Writer (Writer : access Cv_Video_Writer_P);

private
   function W_Cv_Font_Qt (Name_Font  : String_C;
                          Point_Size : Integer;
                          Color      : Cv_Scalar;
                          Weight     : Cv_Font_Weight;
                          Style      : Cv_Font_Style;
                          Spacing    : Integer) return Cv_Font;

   procedure W_Cv_Add_Text (Img  : Cv_Arr_P;
                            Text : String_C;
                            Org  : Cv_Point;
                            Arg2 : Cv_Font_P);

   procedure W_Cv_Display_Overlay (Name     : String_C;
                                   Text     : String_C;
                                   Delay_Ms : Integer);

   procedure W_Cv_Display_Status_Bar (Name     : String_C;
                                      Text     : String_C;
                                      Delay_Ms : Integer);

   procedure W_Cv_Create_Opengl_Callback (Window_Name     : String_C;
                                          Callback_Opengl : Cv_Opengl_Callback;
                                          User_Data       : Cv_Void_P;
                                          Angle           : Long_Float;
                                          Zmin            : Long_Float;
                                          Zmax            : Long_Float);

   procedure W_Cv_Save_Window_Parameters (Name : String_C);

   procedure W_Cv_Load_Window_Parameters (Name : String_C);

   function W_Cv_Create_Button (Button_Name         : String_C;
                                On_Change           : Cv_Button_Callback;
                                User_Data           : Cv_Void_P;
                                Button_Type         : Cv_Button_Type;
                                Intial_Button_State : Integer) return Integer;
   --
   function W_Cv_Init_System (Argc : Integer;
                              Argv : Cv_String_Pointer) return Integer;

   function W_Cv_Named_Window (Windowname  : String_C;
                               Flags       : Highgui_Window_Params := Cv_Window_Autosize) return Integer;

   procedure W_Cv_Set_Window_Property (Name       : String_C;
                                       Prop_Id    : Highgui_Window_Params;
                                       Prop_Value : Long_Float);
   function W_Cv_Get_Window_Property (Name    : String_C;
                                      Prop_Id : Highgui_Window_Params) return Long_Float;

   procedure W_Cv_Show_Image (Windowname  : String_C;
                              Image       : Cv_Arr_P);

   procedure W_Cv_Resize_Window (Windowname   : String_C;
                                 Width        : Integer;
                                 Height       : Integer );

   procedure W_Cv_Move_Window (Windowname : String_C;
                               X          : Integer;
                               Y          : Integer);

   procedure W_Cv_Destroy_Window (Windowname : String );

   function W_Cv_Get_Window_Handle (Window_Name : String_C)
                                    return Cv_Void_P;

   function W_Cv_Create_Trackbar (Trackbar_Name : String_C;
                                  Window_Name   : String_C;
                                  Value         : access Integer;
                                  Count         : Integer;
                                  On_Change     : Cv_Trackbar_Callback) return Integer;

   function W_Cv_Create_Trackbar2 (Trackbar_Name : String_C;
                                   Window_Name   : String_C;
                                   Value         : Integer;
                                   Count         : Integer;
                                   On_Change     : Cv_Trackbar_Callback2 := null;
                                   User_Data     : Cv_Void_P) return Integer;

   -- retrieve or set trackbar position
   function W_Cv_Get_Trackbar_Pos (Trackbar_Name : String_C;
                                   Window_Name   : String_C) return Integer;

   procedure W_Cv_Set_Trackbar_Pos (Trackbar_Name : String_C;
                                    Window_Name   : String_C;
                                    Pos           : Integer);
   function W_Cv_Load_Image ( Filename : String_C; Iscolor : Integer) return Ipl_Image_P;
   function W_Cv_Load_Image_M ( Filename : String_C; Iscolor : Integer) return Cv_Mat_P;

   function W_Cv_Save_Image (Filename      : String_C;
                             Image         : Cv_Arr_P;
                             Settings      : File_Settings) return Integer;
   function W_Cv_Save_Image (Filename      : String_C;
                             Image         : Cv_Mat_P;
                             Settings      : File_Settings) return Integer;
   function W_Cv_Save_Image (Filename      : String_C;
                             Image         : Ipl_Image_P;
                             Settings      : File_Settings) return Integer;

   function W_Cv_Encode_Image (Ext    : String_C;
                               Image  : Cv_Arr_P;
                               Params : File_Settings) return Cv_Mat_P;
   function W_Cv_Encode_Image (Ext    : String_C;
                               Image  : Cv_Mat_P;
                               Params : File_Settings) return Cv_Mat_P;
   function W_Cv_Encode_Image (Ext    : String_C;
                               Image  : Ipl_Image_P;
                               Params : File_Settings) return Cv_Mat_P;

   function W_Cv_Create_File_Capture ( Name : String_C ) return Cv_Capture_P;

   function W_Cv_Create_Video_Writer (Filename       : String_C;
                                      Fourcc         : Integer;
                                      Fps            : Long_Float;
                                      Width          : Integer;
                                      Height         : Integer;
                                      Is_Color       : Integer
                                     ) return Cv_Video_Writer_P;

   pragma Import (C, W_Cv_Font_Qt, "cvFontQt");
   pragma Import (C, W_Cv_Add_Text, "cvAddText");
   pragma Import (C, W_Cv_Display_Overlay, "cvDisplayOverlay");
   pragma Import (C, W_Cv_Display_Status_Bar, "cvDisplayStatusBar");
   pragma Import (C, W_Cv_Create_Opengl_Callback, "cvCreateOpenGLCallback");
   pragma Import (C, W_Cv_Save_Window_Parameters, "cvSaveWindowParameters");
   pragma Import (C, W_Cv_Load_Window_Parameters, "cvLoadWindowParameters");
   pragma Import (C, Cv_Start_Loop, "cvStartLoop");
   pragma Import (C, Cv_Stop_Loop, "cvStopLoop");
   pragma Import (C, W_Cv_Create_Button, "cvCreateButton");
   --
   pragma Import (C, W_Cv_Init_System, "cvInitSystem");
   pragma Import (C, Cv_Start_Window_Thread, "cvStartWindowThread");
   pragma Import (C, W_Cv_Named_Window, "cvNamedWindow");
   pragma Import (C, W_Cv_Set_Window_Property, "cvSetWindowProperty");
   pragma Import (C, W_Cv_Get_Window_Property, "cvGetWindowProperty");
   pragma Import (C, W_Cv_Show_Image, "cvShowImage");
   pragma Import (C, W_Cv_Resize_Window, "cvResizeWindow");
   pragma Import (C, W_Cv_Move_Window, "cvMoveWindow");
   pragma Import (C, W_Cv_Destroy_Window, "cvDestroyWindow");
   pragma Import (C, Cv_Destroy_All_Windows, "cvDestroyAllWindows");
   pragma Import (C, W_Cv_Get_Window_Handle, "cvGetWindowHandle");
   pragma Import (C, Cv_Get_Window_Name, "cvGetWindowName");
   pragma Import (C, W_Cv_Create_Trackbar, "cvCreateTrackbar");
   pragma Import (C, W_Cv_Create_Trackbar2, "cvCreateTrackbar2");
   pragma Import (C, W_Cv_Get_Trackbar_Pos, "cvGetTrackbarPos");
   pragma Import (C, W_Cv_Set_Trackbar_Pos, "cvSetTrackbarPos");
   pragma Import (C, Cv_Set_Mouse_Callback, "cvSetMouseCallback");
   pragma Import (C, W_Cv_Load_Image, "cvLoadImage");
   pragma Import (C, W_Cv_Load_Image_M, "cvLoadImageM");
   pragma Import (C, W_Cv_Save_Image, "cvSaveImage");
   pragma Import (C, Cv_Decode_Image, "cvDecodeImage");
   pragma Import (C, Cv_Decode_Image_M, "cvDecodeImageM");
   pragma Import (C, W_Cv_Encode_Image, "cvEncodeImage");
   pragma Import (C, Cv_Convert_Image, "cvConvertImage");
   pragma Import (C, Cv_Wait_Key, "cvWaitKey");
   pragma Import (C, W_Cv_Create_File_Capture, "cvCreateFileCapture");
   pragma Import (C, Cv_Create_Camera_Capture, "cvCreateCameraCapture");
   pragma Import (C, Cv_Grab_Frame, "cvGrabFrame");
   pragma Import (C, Cv_Retrieve_Frame, "cvRetrieveFrame");
   pragma Import (C, Cv_Query_Frame, "cvQueryFrame");
   pragma Import (C, Cv_Release_Capture, "cvReleaseCapture");
   pragma Import (C, Cv_Get_Capture_Property, "cvGetCaptureProperty");
   pragma Import (C, Cv_Set_Capture_Property, "cvSetCaptureProperty");
   pragma Import (C, Cv_Get_Capture_Domain, "cvGetCaptureDomain");
   pragma Import (C, W_Cv_Create_Video_Writer, "cvCreateVideoWriter");
   pragma Import (C, Cv_Write_Frame, "cvWriteFrame");
   pragma Import (C, Cv_Release_Video_Writer, "cvReleaseVideoWriter");
end Highgui;
