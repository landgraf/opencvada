with Core; use Core;
with Core.Operations; use Core.Operations;
with Imgproc; use Imgproc;
with Imgproc.Operations; use Imgproc.Operations;
with Highgui; use Highgui;

with Features_2d; use Features_2d;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Unchecked_Conversion;

procedure Mser is

   type Cv_Seq_P_P is access Cv_Seq_P;
   type Cv_Contour_P_P is access Cv_Contour_P;

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Contour_P_P);

   function From_Void is
     new Ada.Unchecked_Conversion (Source => Cv_Void_P,
                                   Target => Cv_Seq_P_P);

   procedure Help is
   begin
      Put_Line ("This program demonstrated the Maximal Extermal Region interest point detector.");
      Put_Line ("It finds the most stable (in size) dark and white regions as a threshold is increased.");
      Put_Line ("Call: ./mser < path_and_image_filename, Default is 'puzzle.png'>");
   end Help;

   Colors : constant array (0 .. 10) of Cv_Scalar := (Cv_Create_Scalar (0.0, 0.0, 255.0),
                                                      Cv_Create_Scalar (0.0, 128.0, 255.0),
                                                      Cv_Create_Scalar (0.0, 255.0, 255.0),
                                                      Cv_Create_Scalar (0.0, 255.0, 0.0),
                                                      Cv_Create_Scalar (255.0, 128.0, 0.0),
                                                      Cv_Create_Scalar (255.0, 255.0, 0.0),
                                                      Cv_Create_Scalar (255.0, 0.0, 0.0),
                                                      Cv_Create_Scalar (255.0, 0.0, 255.0),
                                                      Cv_Create_Scalar (255.0, 255.0, 255.0),
                                                      Cv_Create_Scalar (196.0, 255.0, 255.0),
                                                      Cv_Create_Scalar (255.0, 255.0, 196.0));

   B_Colors : constant array (0 .. 8, 0 .. 2) of Unsigned_8 := ((0, 0, 255),
                                                                (0, 128, 255),
                                                                (0, 255, 255),
                                                                (0, 255, 0),
                                                                (255, 128, 0),
                                                                (255, 255, 0),
                                                                (255, 0, 0),
                                                                (255, 0, 255),
                                                                (255, 255, 255));

   Path     : Unbounded_String;

   Img      : aliased Ipl_Image_P;
   Rsp      : aliased Ipl_Image_P;
   Rsp_Array : Cv_8u_Array_P;
   Ellipses : aliased Ipl_Image_P;

   Contours : aliased Cv_Seq_P := new Cv_Seq;
   Storage  : Cv_Mem_Storage_P;
   Hsv      : Ipl_Image_P;
   Params   : Cv_MSER_Params;

   R        : Cv_Seq_P;
   Pt       : Cv_Point_P;

   Contour  : Cv_Contour_P;
   Box      : Cv_Box_2d;
begin
   Help;
   if Ada.Command_Line.Argument_Count = 0 then
      Path := To_Unbounded_String ("puzzle.png");
   else
      Path := To_Unbounded_String(Ada.Command_Line.Argument (1));
   end if;

   Img := Cv_Load_Image (To_String (Path), Cv_Load_Image_Grayscale);

   if Img = null then
      Put_Line ("Could not load " & To_String(Path));
      return;
   end if;

   Storage := Cv_Create_Mem_Storage;

   Rsp := Cv_Load_Image (To_String (Path), CV_LOAD_IMAGE_COLOR);
   Ellipses := Cv_Clone_Image (Rsp);
   Cv_Cvt_Color (To_Arr (Img), To_Arr(Ellipses), CV_GRAY2BGR);
   Hsv := Cv_Create_Image (Cv_Get_Size (To_Arr (Rsp)), IPL_DEPTH_8U, 3);
   Cv_Cvt_Color (To_Arr (Rsp), To_Arr (Hsv), CV_BGR2YCrCb);
   Params := Cv_Create_Mser_Params;

   Cv_Extract_Mser (To_Arr (Hsv), null, Contours'Access, Storage, Params);
   Rsp_Array := new Cv_8u_Array (1 .. (Rsp.all.Width * Rsp.all.Height * 3));
   Rsp_Array := new Cv_8u_Array (0 .. (Rsp.all.Width * Rsp.all.Height * 3) - 1);
   Rsp_Array.all := Cv_8u_Pointer_Pkg.Value(Rsp.all.Image_Data, Ptrdiff_T(Rsp.all.Width * Rsp.all.Height * 3));

   for I in reverse 0 .. Contours.all.Total - 1 loop
      R := From_Void (Cv_Get_Seq_Elem (Contours, I)).all;
      for J in Integer range 0 .. R.all.Total - 1 loop
         Pt := From_Void (Cv_Get_Seq_Elem (R, J));

         Rsp_Array.all (Pt.all.X * 3 + Pt.all.Y * Rsp.all.Width_Step) := B_Colors ((I mod 9), 2);
         Rsp_Array.all (Pt.all.X * 3 + 1 + Pt.all.Y * Rsp.all.Width_Step) := B_Colors ((I mod 9), 1);
         Rsp_Array.all (Pt.all.X * 3 + 2 + Pt.all.Y * Rsp.all.Width_Step) := B_Colors ((I mod 9), 0);
      end loop;
   end loop;

   Rsp.all.Image_Data := Rsp_Array.all (0)'Access;

   for I in Integer range 0 .. Contours.all.Total - 1 loop
      Contour := From_Void (Cv_Get_Seq_Elem (Contours, I)).all;
      Box := Cv_Fit_Ellipse2 (To_Arr (Contour));
      Box.Angle := CV_PI / 2.0 - Box.Angle;

      if Contour.all.Color > 0 then
         Cv_Ellipse_Box (To_Arr (Ellipses), Box, Colors (9), 2);
      else
         Cv_Ellipse_Box (To_Arr (Ellipses), Box, Colors (2), 2);
      end if;
   end loop;

   Main_Loop :
   declare
      I_Ret : Integer;
      C_Ret : Character;
   begin
--        Put_Line ("n_channels:" & Ellipses.all.N_Channels'Img);

      I_Ret := Cv_Save_Image ("rsp.png", To_Arr (Rsp));

      I_Ret := Cv_Named_Window ("original", 0);
      Cv_Show_Image ("original", To_Arr (Img));

      I_Ret := Cv_Named_Window ("response", 0);
      Cv_Show_Image ("response", To_Arr (Rsp));

      I_Ret := Cv_Named_Window ("ellipses", 0);
      Cv_Show_Image ("ellipses", To_Arr (Ellipses));

      C_Ret := Cv_Wait_Key (0);

      Cv_Destroy_Window ("original");
      Cv_Destroy_Window ("response");
      Cv_Destroy_Window ("ellipses");

      Cv_Release_Image (Rsp'Access);
      Cv_Release_Image (Img'Access);
      Cv_Release_Image (Ellipses'Access);
   end Main_Loop;
end Mser;
