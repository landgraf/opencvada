--

with Objdetect;
use Objdetect;
with Highgui;
use Highgui;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Core; use Core;
with Core.Operations; use Core.Operations;
with Ada.Real_Time;
use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Command_Line;
use Ada.Command_Line;
use Ada.Command_Line;

procedure Latentsvmdetect is
   Model_Filename : Unbounded_String := To_Unbounded_String("cat.xml");
   Image_Filename : Unbounded_String := To_Unbounded_String ("cat.jpg");


   procedure Help is
   begin
      null;
   end Help;

   procedure Detect_And_Draw_Objects (Image : Ipl_Image_Ptr;
                                      Detector : Cv_Latent_Svm_Detector_Ptr) is
      Storage : aliased Cv_Mem_Storage_Ptr := Cv_Create_Mem_Storage (0);
      Detections : Cv_Seq_Ptr := null;
      I          : Integer := 0;
      Start, Finish : Time;
      Detection     : Cv_Object_Detection;
      Bounding_Box  : Cv_Rect;


      function To_Object_Detection_Ptr is
        new Ada.Unchecked_Conversion (Source => Cv_Void_Ptr,
                                      Target => Cv_Object_Detection_Ptr);

   begin
      Start := Ada.Real_Time.Clock;
      Detections := Cv_Latent_Svm_Detect_Objects (Image, Detector, Storage);
      Finish := Ada.Real_Time.Clock;
      Put_Line (Ada.Real_Time.To_Duration (Finish - Start)'img);

      for I in Integer range 0 .. Detections.all.Total-1
      loop
         Detection := To_Object_Detection_Ptr (Cv_Get_Seq_Elem (Detections, I)).all;
         Bounding_Box := Detection.Rect;
         Cv_Rectangle (+Image,
                      Cv_Create_Point (Bounding_Box.X, Bounding_Box.Y),
                      Cv_Create_Point (Bounding_Box.X + Bounding_Box.Width, Bounding_Box.Y + Bounding_Box.Height),
                      Cv_Rgb (255, 0, 0), 3);
      end loop;
      Cv_Release_Mem_Storage (Storage'Access);
   end Detect_And_Draw_Objects;
   Image          : aliased Ipl_Image_Ptr;
   Detector       : aliased Cv_Latent_Svm_Detector_Ptr;
   Ret            : Integer;
begin
   if Ada.Command_Line.Argument_Count = 2 then
      Image_Filename := To_Unbounded_String(Argument (1));
      Model_Filename := To_Unbounded_String(Argument (2));
   end if;

   Image := Cv_Load_Image (To_String(Image_Filename));

   if Image = null then
      return;
   end if;

   Detector := Cv_Load_Latent_Svm_Detector (To_String(Model_Filename));

   if Detector = null then
      Cv_Release_Image (Image'Access);
      return;
   end if;

   Detect_And_Draw_Objects (Image, Detector);

   Ret := Cv_Named_Window ("test", 0);
   Cv_Show_Image ("test", +Image);
   loop
      exit when Cv_Wait_Key (0) = Ascii.Esc;
   end loop;
   Cv_Release_Latent_Svm_Detector (Detector'Access);
   Cv_Release_Image (Image'Access);
   Cv_Destroy_Window ("test");
end Latentsvmdetect;
