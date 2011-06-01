---

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Text_Io; use Ada.Text_Io;
with Raw_Frame_Toolkit; use Raw_Frame_Toolkit;
with Imperium_Protocol; use Imperium_Protocol;
with OpenCvAda_Camera_API; use OpenCvAda_Camera_API;
with Core;
with Core.Operations;
with Highgui;
with Generic_Toolkit; use Generic_Toolkit;

with Ethernet_Internal; use Ethernet_Internal;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pcap; use Pcap;
with Networking; use Networking;
procedure Camera is

   type Mongo is new Integer range 0 .. 127;
   for Mongo'Size use 7;

   pragma Warnings (Off);
   type Double_Bill is
      record
         A : Mongo;
         B : Unsigned_8;
         C : Unsigned_16;
      end record;
   for Double_Bill'Size use 1500 * 8; --Mongo'Size + Unsigned_8'Size + Unsigned_16'Size;
   pragma Pack (Double_Bill);
   pragma Warnings (On);

   type Pelle_2 is array (Integer range <>) of Double_Bill;

   Pelle : aliased Pelle_2 := ((127, 129, 6168), (0, 0, 0), (24, 24, 6168), (0, 0, 0), (24, 24, 6169), (0, 0, 0));

   function Nej_Nej is
     new To_Frame_Data (Double_Bill, Pelle_2);

   function To_Frame_Header is
     new Generic_To_Generic (Image_Header, Frame_Header);

   Payload : Frame_Data := Nej_Nej (Pelle);
   Null_Load : Frame_Data (0 .. 0) := (others => 0);
   Raws2 : Raw_Ethernet_Frame_Array := Create_Raw_Frames (Null_Load, Constant_Head => Ping, Frame_Size => 46);
   File : File_Type;

   Image : aliased Core.Ipl_Image_Ptr := Highgui.Cv_Load_Image ("./WP_Emrakul_1280x1024.jpg");
   Image_Data : Frame_Data := Opencvada_Camera_Api.Image_To_Byte (Image);
   Re_Image : aliased Core.Ipl_Image_Ptr := Byte_To_Image (Image_Data, 1280, 1024);
   Spec_Header : Frame_Header := To_Frame_Header (Image_To_Header (Image));

   NIC_Names : NIC_Info_Array := Get_NICs;
   NIC : NIC_Info := Find_NIC (Nic_Names, "Intel(R) 82577LM Gigabit Network Connection");
   Clients   : Client_Info_Vector;


   Header    : Constant_Header := Create_Constant_Header (Flags  => Flag_Data,
                                                          Length => 0);

   function Generate_Data (Count : Integer) return Frame_Data is
      Data : Frame_Data (0 .. Count - 1);
   begin
      for I in Data'Range loop
         Data (I) := Unsigned_8 (I mod 255) + 1 ;
      end loop;
      return Data;
   end Generate_Data;

   Data : Frame_Data := Generate_Data (65535);
--     Data      : Frame_Data (0 .. 65535) := (others => 16#C4#);
   Frames    : Raw_Ethernet_Frame_Array := Create_Raw_Frames (Constant_Head => Header,
                                                              Data          => Data);
   Blesch : Nic_Info := Fetch_Nic (0);
begin
   for I in Nic_Names'Range loop
      Print_NIC (Nic_Names (I));
      Ethernet_Internal.Open (Nic_Names (I));
      if Nic_Names (I).Handle /= null then
         Put("NIC open");
      else
         Put("NIC failed to open");
      end if;
      New_Line (2);
      Ethernet_Internal.Close (Nic_Names (I));
   end loop;

   Put_Line ("blesch mac:" & To_String (Blesch.MAC));
   Go_Boom (0);
   Put_Line ("blesch mac:" & To_String (Blesch.MAC));
   Blesch := Fetch_Nic (0);
   Put_Line ("blesch mac:" & To_String (Blesch.MAC));

   Open (NIC);
   Clients := Discover (NIC);
   Send (NIC.Handle, NIC.MAC, Clients.First_Element.Device_Addr, Frames);
   Close (NIC);

--     Put_Line (Spec_Header.Length'Img);
--     Spec_Header.Length := 5;
   declare
      Raws : Raw_Ethernet_Frame_Array := Create_Raw_Frames (Image_Data,  To_Frame_Header (Image_To_Header (Image)), Create_Constant_Header(0,2#0000#,False,False,False,False,2#0101#,0,2#000_00000#),1001);
      Parsed_Frame : Parsed_Raw_Frame :=    From_Raw_Frame (Raws (Raws'First + 1));
   begin
      --     Put_Line ("frames: " & Amount_Of_Frames (Payload'Length, 8, 5)'img & Raws'Length'Img);
      Put_Line (Parsed_Frame.Constant_Head.Seq_No'Img &
                Parsed_Frame.Constant_Head.Length'Img &
                Parsed_Frame.Constant_Head.Options'Img &
                Parsed_Frame.Constant_Head.Flags'Img &
                Parsed_Frame.Spec_Header.Length'Img &
                Parsed_Frame.Type_Of_Frame'Img &
                Parsed_Frame.Payload_Start'Img);
      Parsed_Frame := From_Raw_Frame (Raws (Raws'First + 0));
      Put_Line (Parsed_Frame.Constant_Head.Seq_No'Img &
                Parsed_Frame.Constant_Head.Length'Img &
                parsed_Frame.Constant_Head.Options'Img &
                Parsed_Frame.Constant_Head.Flags'Img &
                Parsed_Frame.Spec_Header.Length'Img &
                Parsed_Frame.Type_Of_Frame'Img &
                Parsed_Frame.Payload_Start'Img);
      Create (File, Out_File, "test.csv");
      for I in Raws'Range loop
--           for N in Raws (I).Payload'Range loop
--              Put (File, Raws (I).Payload (N)'Img & ", ");
--           end loop;
         Put (File, Raws(I).Length'Img);
         New_Line (File);
      end loop;
      Close (File);
      Put_Line (Raws2'Length'Img);
      for I in Raws2'Range loop
         Put_Line (Raws2 (I).Length'Img);
         for N in 0 .. Raws2 (I).Length loop
            Put (Raws2 (I).Payload (N)'Img);
         end loop;
      end loop;
      Put_Line ("hallo" & Image_Data'Length'Img);

--        Highgui.Cv_Save_Image ("test.png", Re_Image);

      Core.Operations.Cv_Release_Image (Image'Access);
      Core.Operations.Cv_Release_Image (Re_Image'Access);
   end;



end Camera;
