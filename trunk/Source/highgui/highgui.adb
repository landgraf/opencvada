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

package body Highgui is
--

   --  //for color cvScalar(blue_component, green_component, red\_component[, alpha_component])
   --  //and alpha= 0 <-> 0xFF (not transparent <-> transparent)
   function Cv_Font_Qt (Name_Font  : String;
                        Point_Size : Integer := -1;
                        Color      : Cv_Scalar := Core.Cvscalarall (0.0);
                        Weight     : Cv_Font_Weight := Cv_Font_Normal;
                        Style      : Cv_Font_Style :=  Cv_Style_Normal;
                        Spacing    : Integer := 0) return Cv_Font is
   begin
      return W_Cv_Font_Qt (Name_Font  => +Name_Font,
                           Point_Size => Point_Size,
                           Color      => Color,
                           Weight     => Weight,
                           Style      => Style,
                           Spacing    => Spacing);
   end Cv_Font_Qt;

   procedure Cv_Add_Text (Img  : Cv_Arr_P;
                          Text : String;
                          Org  : Cv_Point;
                          Arg2 : Cv_Font_P) is
   begin
      W_Cv_Add_Text (Img, +Text, Org, Arg2);
   end Cv_Add_Text;

   procedure Cv_Display_Overlay (Name     : String;
                                 Text     : String;
                                 Delay_Ms : Integer) is
   begin
      W_Cv_Display_Overlay (+Name, +Text, Delay_Ms);
   end Cv_Display_Overlay;

   procedure Cv_Display_Status_Bar (Name     : String;
                                    Text     : String;
                                    Delay_Ms : Integer) is
   begin
      W_Cv_Display_Status_Bar (+Name, +Text, Delay_Ms);
   end Cv_Display_Status_Bar;

   procedure Cv_Create_Opengl_Callback (Window_Name     : String;
                                        Callback_Opengl : Cv_Opengl_Callback;
                                        User_Data       : Cv_Void_P := null;
                                        Angle           : Long_Float := -1.0;
                                        Zmin            : Long_Float := -1.0;
                                        Zmax            : Long_Float := -1.0) is
   begin
      W_Cv_Create_Opengl_Callback (+Window_Name, Callback_Opengl, User_Data, Angle, Zmin, Zmax);
   end Cv_Create_Opengl_Callback;

   procedure Cv_Save_Window_Parameters (Name : String) is
   begin
      W_Cv_Save_Window_Parameters (+Name);
   end Cv_Save_Window_Parameters;

   procedure Cv_Load_Window_Parameters (Name : String) is
   begin
      W_Cv_Load_Window_Parameters (+Name);
   end Cv_Load_Window_Parameters;

   function Cv_Create_Button (Button_Name         : String;
                              On_Change           : Cv_Button_Callback := null;
                              User_Data           : Cv_Void_P := null;
                              Button_Type         : Cv_Button_Type := Cv_Push_Button;
                              Intial_Button_State : Integer := 0) return Integer is
   begin
      return W_Cv_Create_Button (+Button_Name, On_Change, User_Data, Button_Type, Intial_Button_State);
   end Cv_Create_Button;

   -----------------------------------------------------------------------------
   --
   -----------------------------------------------------------------------------

   -- Initialisez HighGUI
   -- Not used in Windows.
   function Cv_Init_System (Argc : Integer;
                            Argv : Cv_String_Pointer) return Integer is
   begin
      return W_Cv_Init_System (Argc, Argv);
   end Cv_Init_System;

   -- Creates a Window.
   -- Name : Identifier of the window, and titlebar name.
   -- Flags : CV_WINDOW_AUTOSIZE or 0
   -- Return : ?
   function Cv_Named_Window (Windowname  : String;
                             Flags       : Highgui_Window_Params := Cv_Window_Autosize) return Integer is
   begin
      return W_Cv_Named_Window (+Windowname, Flags);
   end Cv_Named_Window;

   procedure Cv_Set_Window_Property (Name       : String;
                                     Prop_Id    : Highgui_Window_Params;
                                     Prop_Value : Long_Float) is
   begin
      W_Cv_Set_Window_Property (+Name, Prop_Id, Prop_Value);
   end Cv_Set_Window_Property;

   function Cv_Get_Window_Property (Name    : String;
                                    Prop_Id : Highgui_Window_Params) return Long_Float is
   begin
      return W_Cv_Get_Window_Property (+Name, Prop_Id);
   end Cv_Get_Window_Property;

   procedure Cv_Show_Image (Windowname  : String;
                            Image       : Cv_Arr_P) is
   begin
      W_Cv_Show_Image (+Windowname, Image);
   end Cv_Show_Image;

   procedure Cv_Resize_Window (Windowname   : String;
                               Width        : Integer;
                               Height       : Integer ) is
   begin
      W_Cv_Resize_Window (+Windowname, Width, Height);
   end Cv_Resize_Window;

   procedure Cv_Move_Window (Windowname : String;
                             X          : Integer;
                             Y          : Integer) is
   begin
      W_Cv_Move_Window (+Windowname, X, Y);
   end Cv_Move_Window;

   procedure Cv_Destroy_Window (Windowname : String ) is
   begin
      W_Cv_Destroy_Window (Windowname & Ascii.Nul);
   end Cv_Destroy_Window;

   function Cv_Get_Window_Handle (Window_Name : String)
                                  return Cv_Void_P is
   begin
      return W_Cv_Get_Window_Handle (+Window_Name);
   end Cv_Get_Window_Handle;

   function Cv_Create_Trackbar (Trackbar_Name : String;
                                Window_Name   : String;
                                Value         : access Integer;
                                Count         : Integer;
                                On_Change     : Cv_Trackbar_Callback) return Integer is
   begin
      return W_Cv_Create_Trackbar ( +Trackbar_Name, +Window_Name, Value, Count, On_Change);
   end Cv_Create_Trackbar;

   function Cv_Create_Trackbar2 (Trackbar_Name : String;
                                 Window_Name   : String;
                                 Value         : Integer;
                                 Count         : Integer;
                                 On_Change     : Cv_Trackbar_Callback2 := null;
                                 User_Data     : Cv_Void_P) return Integer is
   begin
      return W_Cv_Create_Trackbar2 (+Trackbar_Name, +Window_Name, Value, Count, On_Change, User_Data);
   end Cv_Create_Trackbar2;

   function Cv_Get_Trackbar_Pos (Trackbar_Name : String;
                                 Window_Name   : String) return Integer is
   begin
      return W_Cv_Get_Trackbar_Pos (+Trackbar_Name, +Window_Name);
   end Cv_Get_Trackbar_Pos;

   procedure Cv_Set_Trackbar_Pos (Trackbar_Name : String;
                                  Window_Name   : String;
                                  Pos           : Integer) is
   begin
      W_Cv_Set_Trackbar_Pos (+Trackbar_Name,
                             +Window_Name,
                             Pos);
   end Cv_Set_Trackbar_Pos;

   function Cv_Load_Image (Filename : String;
                           Iscolor  : Integer := Cv_Load_Image_Color) return Ipl_Image_P is
   begin
      return W_Cv_Load_Image (+Filename, Iscolor);
   end Cv_Load_Image;

   function Cv_Load_Image_M ( Filename : String;
                             Iscolor  : Integer := Cv_Load_Image_Color) return Cv_Mat_P is
   begin
      return W_Cv_Load_Image_M (+Filename, Iscolor);
   end Cv_Load_Image_M;

   function Cv_Save_Image (Filename      : String;
                           Image         : Cv_Arr_P;
                           Settings      : File_Settings := Create_File_Settings (Cv_Imwrite_Jpeg_Quality, 95)) return Integer is
   begin
      return W_Cv_Save_Image (+Filename, Image, Settings);
   end Cv_Save_Image;

   function Create_File_Settings ( Compression     : Compression_Type;
                                  Compression_Rate : Integer;
                                  Not_Used         : Integer := 0) return File_Settings is
   begin
      return File_Settings'( Compression, Compression_Rate, Not_Used );
   end Create_File_Settings;

   function Cv_Encode_Image (Ext    : String;
                             Image  : Cv_Arr_P;
                             Params : File_Settings) return Cv_Mat_P is
   begin
      return W_Cv_Encode_Image (+Ext, Image, Params);
   end Cv_Encode_Image;

   function Cv_Create_File_Capture ( Name : String ) return Cv_Capture_P is
   begin
      return W_Cv_Create_File_Capture (+Name);
   end Cv_Create_File_Capture;

   -- C Macro #define CV_FOURCC(c1,c2,c3,c4) (((c1)&255) + (((c2)&255)<<8) + (((c3)&255)<<16) + (((c4)&255)<<24))
   function Cv_Fourcc ( C1 : Character; C2 : Character ; C3 : Character; C4 : Character) return Integer is
      Result : Unsigned_32 := 0;
   begin
      Result := (Unsigned_32 (Character'Pos (C1)) and 255) + (Shift_Left (Unsigned_32 (Character'Pos (C2)) and 255, 8)) + Shift_Left (Unsigned_32 (Character'Pos (C3)) and 255, 16) + Shift_Left (Unsigned_32 (Character'Pos (C4)) and 255, 24);
      --Result := Unsigned_32 (Character'Pos (C1)) + (Shift_Left (Unsigned_32 (Character'Pos (C2)), 8)) + Shift_Left (Unsigned_32 (Character'Pos (C3)), 16) + Shift_Left (Unsigned_32 (Character'Pos (C4)), 24);
      --Ada.Text_IO.Put_Line ("Result: " & Result'Img & Shift_Left (Unsigned_32 (Character'Pos (C2))and 255, 8)'Img);
      return Integer (Result);
   end Cv_Fourcc;

   function Cv_Create_Video_Writer (Filename       : String;
                                    Fourcc         : Integer;
                                    Fps            : Long_Float;
                                    Width          : Integer;
                                    Height         : Integer;
                                    Is_Color       : Integer) return Cv_Video_Writer_P is
   begin
      return W_Cv_Create_Video_Writer (+Filename, Fourcc, Fps, Width, Height, Is_Color);
   end Cv_Create_Video_Writer;

end Highgui;
