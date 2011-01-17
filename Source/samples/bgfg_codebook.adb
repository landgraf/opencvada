--

with Video.Background_Segm; use Video.Background_Segm;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Core; use Core;
with Highgui; use Highgui;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Core.Operations; use Core.Operations;
with Imgproc.Operations;
use Imgproc.Operations;
with Imgproc; use Imgproc;
with Ada.Characters.Handling;

procedure Bgfg_Codebook is
   Model     : Cv_BG_Code_Book_Model_P := null;
   NChannels : constant Integer := 3;

   procedure Help is
   begin
      Put_Line ("Du gör fel, gör om gör rätt");
   end Help;

   Filename  : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   Raw_Image, Yuv_Image, Imask_Code_Book, Imask_Code_Book_CC : Ipl_Image_P;
   Capture   : aliased Cv_Capture_P;
   Char      : Character;
   N, Nframes : Integer := 0;
   Nframes_To_Learn_Bg : Integer := 300;

   Temp_String : Unbounded_String := To_Unbounded_String("");

   Pause, Singlestep : Boolean := False;
   Ret       : Integer;
   Ret_Seq   : Cv_Seq_P; -- ignore
begin
   Model := CvCreateBGCodeBookModel;
   Model.all.Mod_Min (1) := 3; Model.all.Mod_Min (2) := 3; Model.all.Mod_Min (3) := 3;
   Model.all.Mod_Max (1) := 10; Model.all.Mod_Max (2) := 10; Model.all.Mod_Max (3) := 10;
   Model.all.Cb_Bounds (1) := 10; Model.all.Cb_Bounds (2) := 10; Model.all.Cb_Bounds (3) := 10;
   for I in Integer range 1 .. Ada.Command_Line.Argument_Count loop
      if Ada.Strings.Fixed.Count (Ada.Command_Line.Argument (I), "--nframes=") > 0 then
         Temp_String := To_Unbounded_String (Ada.Strings.Fixed.Tail(Ada.Command_Line.Argument(I),Ada.Command_Line.Argument(I)'length - Ada.Strings.Fixed.Index (Ada.Command_Line.Argument (I), "=")));
         Nframes_To_Learn_BG := Integer'Value (To_String (Temp_String));
         if Nframes_To_Learn_Bg <= 0 then
            Help;
            return;
         end if;
      else
         Filename := Filename & Ada.Command_Line.argument(I);
      end if;
   end loop;

   if Filename = Null_Unbounded_String then
      Put_Line ("Camera, go");
      Capture := Highgui.CvCreateCameraCapture (0);
   else
      Put_Line ("File, go");
      Capture := CvCreateFileCapture (To_String (Filename));
   end if;

   if Capture = null then
      Put_Line ("capture, no go");
      Help;
      return;
   end if;

   loop
      if not Pause then
         Raw_Image := CvQueryFrame (Capture);
         Nframes := Nframes + 1;
         exit when Raw_Image = null;
      end if;

      if Singlestep then
         Pause := True;
      end if;

      -- first frame
      if Nframes = 1 and Raw_Image /= null then
         Yuv_Image := CvCloneImage (Raw_Image);
         Imask_Code_Book := CvCreateImage (CvGetSize (+Raw_Image), Ipl_Depth_8u, 1);
         Imask_Code_Book_CC := CvCreateImage (CvGetSize (+Raw_Image), Ipl_Depth_8u, 1);
         CvSet (+Imask_Code_Book, CvScalar (255.0));

         Ret := CvNamedWindow ("Raw", 1);
         Ret := CvNamedWindow ("ForegroundCodeBook", 1);
         Ret := CvNamedWindow ("CodeBook_ConnectComp", 1);
      end if;

      --  If we've got an rawImage and are good to go:
      if Raw_Image /= null then
         CvCvtColor (+Raw_Image, +Yuv_Image, Cv_Bgr2yCrCb);
         if not Pause and Nframes - 1 < Nframes_To_Learn_Bg then
            CvBgCodeBookUpdate (Model, +Yuv_Image);
         end if;

         if Nframes - 1 = Nframes_To_Learn_Bg then
            CvBgCodeBookClearStale (Model, Model.all.T / 2);

            Put_Line (Nframes'Img);
            CvReleaseCapture (Capture'Access);
            CvDestroyWindow ( "Raw" );
            CvDestroyWindow ( "ForegroundCodeBook");
            CvDestroyWindow ( "CodeBook_ConnectComp");
            return;
         end if;

         if Nframes - 1 >= Nframes_To_Learn_Bg then
            Ret := CvBgCodeBookDiff (Model, +Yuv_Image, +Imask_Code_Book);
            CvCopy (+Imask_Code_Book, +Imask_Code_Book_Cc);
            Ret_Seq := CvSegmentfgmask (+Imask_Code_Book_Cc);
         end if;

         -- Display
         CvShowImage ( "Raw", +Raw_Image );
         CvShowImage ( "ForegroundCodeBook", +Imask_Code_Book);
         CvShowImage ( "CodeBook_ConnectComp", +Imask_Code_Book_CC);
      end if;

      Char := CvWaitKey (10);
      --Put (Char);
      Char:=Ada.Characters.Handling.To_Lower (Char);
      exit when Char = Ascii.Esc or Char = 'q';

      case Char is
         when 'h' =>
            Help;
         when 'p' =>
            Pause := not Pause;
         when 's' =>
            Singlestep := not Singlestep;
            Pause := False;
         when 'r' =>
            Pause := False;
            Singlestep := False;
         when ' ' =>
            CvBgCodeBookClearStale (Model, 0);
            Nframes := 0;

         when others =>
            null;
      end case;
   end loop;


   Put_Line(Nframes'Img);
   CvReleaseCapture (Capture'Access);
   CvDestroyWindow ( "Raw" );
   CvDestroyWindow ( "ForegroundCodeBook");
   CvDestroyWindow ( "CodeBook_ConnectComp");
end Bgfg_Codebook;
