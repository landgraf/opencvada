---

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Defero; use Defero;
procedure Camera is




   type Pcap_Buffer is array (Integer range <> ) of Unsigned_8;
   procedure Parse_Raw (Buf    : Pcap_Buffer;
                        Header : out Raw_Frame_Header;
                        Data   : out Frame_Data) is
      Data_Temp   : Frame_Data (0 .. Buf'Length - 15);
      Header_Temp : Raw_Frame_Header;
   begin
      Header_Temp.Dest := Mac_Address (Buf (0 .. 5));
      Header_Temp.Src := Mac_Address (Buf (6 .. 11));
      Header_Temp.Id := Packet_Type_Id (Buf (12 .. 13));
      Data_Temp := Frame_Data (Buf (14 .. Buf'Last));

      Header := Header_Temp;
      Data := Data_Temp;

   end Parse_Raw;

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

   Pelle : aliased Pelle_2 := ((127,129,6168), (0,0,0),(24,24,6168), (0,0,0),(24,24,6169), (0,0,0));

   function Nej_Nej is
     new To_Frame_Data (Double_Bill, Pelle_2);

   type Raw_Ethernet_Frame is
      record
         Raw_Header : Raw_Frame_Header;
         Payload    : Frame_Data (0 .. 1499) := (others => 0);
         Length     : Integer := 1500;
      end record;

   type Raw_Ethernet_Frame_Array is array (Integer range <>) of Raw_Ethernet_Frame;

   type Header_Version is new Unsigned_8 range 0 .. 15;
   type Header_Length is new Unsigned_8 range 0 .. 15;
   type Header_Bit is new Integer range 0 .. 1;
   type Header_Flags is new Unsigned_8 range 0 .. 15;
   type Header_Sequence is new Unsigned_16;
   type Header_Options is new Unsigned_8;
   type Header_Data is array (Integer range 0 .. 14) of Unsigned_8;

   type Constant_Header is
      record
         Version : Header_Version;
         Length  : Header_Length;
         Ack     : Header_Bit;
         Nak     : Header_Bit;
         Eof     : Header_Bit;
         Req     : Header_Bit;
         Flags   : Header_Flags;
         Seq_No  : Header_Sequence;
         Options : Header_Options;
         Data    : Header_Data;
      end record;
   for Constant_Header use
      record
         Version at 0 range 0 .. 3;
         Length at 0 range 4 .. 7;
         Ack at 0 range 8 .. 8;
         Nak at 0 range 9 .. 9;
         Eof at 0 range 10 .. 10;
         Req at 0 range 11 .. 11;
         Flags at 0 range 12 .. 15;
         Seq_No at 0 range 16 .. 31;
         Options at 0 range 32 .. 39;
         Data at 0 range 40 .. 159;
      end record;
   for Constant_Header'Size use 160;

   type Frame_Header is
      record
         Data : Frame_Data(0 .. 19);
         Length : Integer := 4;
      end record;

   function To_Frame_Header (Src : Constant_Header) return Frame_Header is
      Dest : Frame_Header;
      Temp : Frame_Data(0 .. 19);
      for Temp'Address use Src'Address;
   begin
      Dest.Data := Temp;
      Dest.Length := Integer (Src.Length) + 4;
      if Src.Flags = 2#1111# or Src.Flags = 2#0101# then
         Dest.Length := Dest.Length + 1;
      end if;
      return Dest;
   end To_Frame_Header;

   function To_Constant_Header (Src    : Frame_Data;
                                Offset : Integer := 0) return Constant_Header is
      Src_Temp : Frame_Data (0 .. 19) := Src (Offset .. Offset+19);
      Dest : Constant_Header;
      for Dest'Address use Src_Temp'Address;
   begin
      return Dest;
   end To_Constant_Header;
   -- in bytes please
   function Amount_Of_Frames (Data_Bytes           : Integer;
                              Spec_Header_Size     : Integer;
                              Constant_Header_Size : Integer;
                              Frame_Size           : Integer := 1500) return Integer is
      Max_Data_Per_Frame : Integer := Frame_Size - Constant_Header_Size;
      Total_Data         : Integer := Data_Bytes + Spec_Header_Size;
      Ret_Val            : Integer := 0;
   begin
      Ret_Val := Total_Data / Max_Data_Per_Frame;
      if (Total_Data mod Max_Data_Per_Frame) > 0 then
         Ret_Val := Ret_Val + 1;
      end if;
      return Ret_Val;
   end Amount_Of_Frames;

   function Create_Raw_Frames (Data            : Frame_Data;
                               Spec_Header     : Frame_Header;
                               Constant_Header : Frame_Header;
                               Frame_Size      : Integer := 1500) return Raw_Ethernet_Frame_Array is
      Frames : Raw_Ethernet_Frame_Array (0 .. Amount_Of_Frames (Data'Length, Spec_Header.Length, Constant_Header.Length) -1);
      Spec_Set : Boolean := False;
      Next_Data : Integer := 0;
      Next_Pos  : Integer := 0;
      Header_Length : Integer := 0;
      Test_Val : Integer;
   begin
      for I in Frames'Range loop
         Next_Pos := 0;
         Header_Length := 0;
         Frames (I).Payload (Next_Pos .. Constant_Header.Length - 1) := Constant_Header.Data (0 .. Constant_Header.Length - 1);
         Next_Pos := Constant_Header.Length;
         Header_Length := Constant_Header.Length;
         if not spec_Set then
            Frames (I).Payload (Next_Pos .. (Constant_Header.Length + Spec_Header.Length) - 1) := Spec_Header.Data (0 .. Spec_Header.Length - 1);
            Next_Pos := Constant_Header.Length + Spec_Header.Length;
            Header_Length := Header_Length + Spec_Header.Length;
            Spec_Set := True;
         end if;
         if not (I = Frames'Length - 1) then
            Test_Val := Next_Data - Header_Length + Frame_Size;
            Frames (I).Payload (Next_Pos .. Frames (I).Payload'Last) := Data (Next_Data .. Test_Val -1);
            Next_Data := (Next_Data + (Frame_Size - Header_Length));
         else
            Frames (I).Payload (Next_Pos .. Next_Pos + Data'Last - Next_Data ) := Data (Next_Data .. Data'Last);
            Next_Data := Data'Last + 1;
         end if;
      end loop;
      return Frames;
   end Create_Raw_Frames;

   Payload : Frame_Data := Nej_Nej (Pelle, Pelle'Length);
   Raws : Raw_Ethernet_Frame_Array := Create_Raw_Frames (Payload, ((15, 240, others => 0), 2), ((240, 15, others => 0), 2));
   File : File_Type;
begin
   Put_Line ("frames: " & Amount_Of_Frames (Payload'Length, 8, 5)'img & Raws'Length'Img);
   Create (File, Out_File, "test.csv");
   for I in Raws'Range loop
      for N in Raws (I).Payload'Range loop
         Put (File, Raws (I).Payload (N)'Img & ", ");
      end loop;
      New_Line (File);
   end loop;
   Close (File);
end Camera;
