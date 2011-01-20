--
with Ada.Text_IO; use Ada.Text_IO;
with Core; use Core;
with Imgproc;
use Imgproc;
with Imgproc.Operations;
use Imgproc.Operations;
with Core.Operations;
use Core.Operations;
with Highgui;
use Highgui;
with Ada.Command_Line;
with Interfaces; use Interfaces;
--
procedure Polar_Transforms is
   procedure Help is
   begin
      New_Line;
      Put_Line ("This program illustrates Linear-Polar and Log-Polar image transforms");
      Put_Line ("Call:");
      Put_Line ("./polar_transforms [[camera number -- Default 0],[AVI path_filename]]");
      New_Line;
   end Help;

   Capture : aliased Cv_Capture_P := null;
   Log_Polar_Img, Lin_Polar_Img, Recovered_Img : Ipl_Image_P := null;
   Frame : Ipl_Image_P;
   Temp : Integer;
   Center : aliased Cv_Point_2D_32F;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Capture := Highgui.Cv_Create_Camera_Capture (0);
   elsif Integer'Value (Ada.Command_Line.Argument (1)) >= 0 then
      Temp := Integer'Value (Ada.Command_Line.Argument (1));
      if Character'Pos (Ada.Command_Line.Argument (1) (1)) <  58 and Character'Pos (Ada.Command_Line.Argument (1) (1)) > 47  then
         Capture := Cv_Create_Camera_Capture (Cv_Cap (Temp));
      else
         Capture := Cv_Create_File_Capture (Ada.Command_Line.Argument (1));
      end if;
   end if;

   if Capture = null then
      Put_Line ("Could not initialize capturing...");
      Put_Line ("Usage: %s <CAMERA_NUMBER>    , or \n       %s <VIDEO_FILE");
      return;
   end if;

   Temp := Cv_Named_Window ("Linear-Polar", 0);
   Temp := Cv_Named_Window ( "Log-Polar", 0 );
   Temp := Cv_Named_Window ( "Recovered image", 0 );

   Cv_Move_Window ( "Linear-Polar", 20, 20 );
   Cv_Move_Window ( "Log-Polar", 700, 20 );
   Cv_Move_Window ( "Recovered image", 20, 400 );

   loop
      Frame := Cv_Query_Frame (Capture);
      exit when Frame = null;

      if Log_Polar_Img = null then
         Log_Polar_Img := Cv_Create_Image ( Cv_Create_Size(Frame.all.Width, Frame.all.Height), IPL_DEPTH_8U, 3 );
         Lin_Polar_Img := Cv_Create_Image ( Cv_Create_Size(Frame.all.Width, Frame.all.Height), IPL_DEPTH_8U, 3 );
         Recovered_Img := Cv_Create_Image ( Cv_Create_Size (Frame.all.Width, Frame.all.Height), IPL_DEPTH_8U,3 );
         Put_Line("Images done");
      end if;
      --        Interfaces.Shift_Right;

      Center.X := Float (Interfaces.Shift_Right (Unsigned_32 (Frame.all.Width), 1));
      Center.Y := Float (Interfaces.Shift_Right (Unsigned_32 (Frame.all.Height), 1));
      Cv_Log_Polar (+Frame, +Log_Polar_Img,
                  Center,
                  70.0,
                 1+ 8);

      Cv_Linear_Polar (+Frame, +Lin_Polar_Img,
                     Center,
                     70.0,
                     1 + 8);

      Cv_Linear_Polar (+Lin_Polar_Img, +Recovered_Img,
                     Center,
                     70.0,
                     16 + 1 + 8);

      Cv_Show_Image ("Log-Polar", +Log_Polar_Img );
      Cv_Show_Image ("Linear-Polar", +Lin_Polar_Img );
      Cv_Show_Image ("Recovered image", +Recovered_Img );
      exit when Cv_Wait_Key (10) = Ascii.Esc;
   end loop;
   Cv_Release_Capture ( Capture'Access );
   Cv_Destroy_Window ("Linear-Polar");
   Cv_Destroy_Window ("Log-Polar");
   Cv_Destroy_Window ("Recovered image");
end Polar_Transforms;
