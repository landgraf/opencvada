---

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Defero; use Defero;
with Venit_Subcriptio;
use Venit_Subcriptio;
with Suo_Ut;
use Suo_Ut;
with Core;
with Core.Operations;
with Highgui;
procedure Camera is

   type Mongo is new integer range 0 .. 127;
   for Mongo'Size use 7;

   type Double_Bill is
      record
         A : Mongo;
         B : Unsigned_8;
         C : Unsigned_16;
      end record;
   for Double_Bill'Size use 1500 * 8;--Mongo'Size + Unsigned_8'Size + Unsigned_16'Size;
   pragma Pack (Double_Bill);

   type Pelle_2 is array (Integer range <>) of Double_Bill;

   Pelle : aliased Pelle_2 := ((127, 129, 6168), (0, 0, 0), (24, 24, 6168), (0, 0, 0), (24, 24, 6169), (0, 0, 0));

   function Nej_Nej is
     new To_Frame_Data (Double_Bill, Pelle_2);

   Payload : Frame_Data := Nej_Nej (Pelle);
   Null_Load : Frame_Data (0 .. 0) := (others => 0);
   Raws : Raw_Ethernet_Frame_Array := Create_Raw_Frames (Payload, ((15, 240, others => 0), 10), ((others => 170), 4));
   Raws2 : Raw_Ethernet_Frame_Array := Create_Raw_Frames (Null_Load, Constant_Header => To_Frame_Header(Ping), Frame_Size => 0);
   File : File_Type;

   Image : aliased core.Ipl_Image_Ptr := Highgui.Cv_Load_Image ("C:\Users\niklas\Pictures\almost_red_painting.png");
   Image_Data : Frame_Data := Suo_Ut.Image_To_Byte (Image);
   Re_Image : aliased Core.Ipl_Image_Ptr := Byte_To_Image (Image_Data, 800,600);
begin
--     Put_Line ("frames: " & Amount_Of_Frames (Payload'Length, 8, 5)'img & Raws'Length'Img);
--     Create (File, Out_File, "test.csv");
--     for I in Raws'Range loop
--        for N in Raws (I).Payload'Range loop
--           Put (File, Raws (I).Payload (N)'Img & ", ");
--        end loop;
--        New_Line (File);
--     end loop;
--     Close (File);
--     Put_Line (Raws2'Length'Img);
--     for I in Raws2'Range loop
--        Put_Line (Raws2 (I).Length'Img);
--        for N in 0 .. Raws2 (I).Length loop
--           Put (Raws2 (I).Payload (N)'Img);
--        end loop;
--     end loop;
   Put_Line ("hallo" & Image_Data'Length'Img);

   Highgui.Cv_Save_Image ("test.png", Re_Image);

   Core.Operations.Cv_Release_Image(Image'Access);

end Camera;
