--

with Objdetect;
use Objdetect;
with Highgui;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Core; use Core;
with Core.Operations; use Core.Operations;
with Ada.Real_Time;
use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

procedure Latentsvmdetect is
   Model_Filename : Unbounded_String := To_Unbounded_String("cat.xml");
   Image_Filename : Unbounded_String := To_Unbounded_String ("cat.jpg");

   procedure Detect_And_Draw_Objects (Image : Ipl_Image_P;
                                      Detector : Cv_Latent_Svm_Detector) is
      Storage : Cv_Mem_Storage_P := CvCreateMemStorage (0);
      Detections : Cv_Seq_P := null;
      I          : Integer := 0;
      Start, Finish : Time;
   begin
      Start := Ada.Real_Time.Clock;
      Detections := CvLatentSvmDetectObjects (Image, Detector, Storage);
      Finish := Ada.Real_Time.Clock;
      Put_Line (Ada.Real_Time.To_Duration (Finish - Start)'img);
--        Ada.Real_Time.
      null;
   end Detect_And_Draw_Objects;
begin
   null;
end Latentsvmdetect;
