
with Pcap; use Pcap;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
use Pcap;
with System; use System;

procedure Packet_Sniffer is
   procedure Packet_Printer (User          : System.Address;
                             Packet_Header : Pcap_Packet_Header_Ptr;
                             Packet_Data   : System.Address);
   pragma Convention (C, Packet_Printer);

   procedure Packet_Printer (User          : System.Address;
                             Packet_Header : Pcap_Packet_Header_Ptr;
                             Packet_Data   : System.Address) is
      Packet : Byte_Array (0 .. Integer (Packet_Header.all.Length) - 1);
      for Packet'Address use Packet_Data;
   begin

      Put_Line ("" & Packet_Header.all.Length'Img & Packet'Length'Img);
      for I in 0 .. Integer (Packet_Header.all.Length) - 1 loop
         Put (Packet (I)'Img);
      end loop;
      New_Line;
      New_Line;
   end Packet_Printer;

   procedure Read_Packet (Handle : Pcap_Ptr;
                          Packet : in out Byte_Array_Ptr;
                          Header : in out Pcap_Packet_Header;
                          Print  : Boolean := False) is
      H : aliased Pcap_Packet_Header;
   begin
      Packet := Pcap_Next (Handle, H'Access);

      Header := H;

      if Print = True and Packet /= null then
         Put_Line (Header.Length'Img);
         Put ("Dest:");
         for I in 1 .. 6 loop
            Put (Packet.all (I)'Img);
         end loop;
         Put (Ascii.Ht & "Src.:");
         for I in 7 .. 12 loop
            Put (Packet.all (I)'Img);
         end loop;
         New_Line;
         for I in 13 .. Packet.all'Length loop
            Put (Packet.all (I)'Img);
         end loop;
         New_Line;
         New_Line;
      end if;
   end Read_Packet;

   procedure Reply_Packet (Handle : Pcap_Ptr; Packet : Byte_Array_Ptr) is
      Temp_Mac : constant Byte_Array (1 .. 6) := Packet.all (1 .. 6);
      Ret      : Integer;
   begin
      Packet.all (1 .. 6) := Packet.all (7 .. 12);
      Packet.all (7 .. 12) := Temp_Mac;

      Ret := Pcap_Send_Packet (Handle, Packet.all, Packet.all'Length);
   end Reply_Packet;

   All_Devs, D : aliased Pcap_If_Ptr;
   Name        : constant String := "\Device\NPF_{D74653A5-74D9-44AD-8A4D-683012792F8F}";
   pragma Warnings (Off);
   Err_Buf     : Pcap_Error_String;
   pragma Warnings (On);
   Ret         : Integer;
   Handle      : Pcap_Ptr := null;

   Packet      : aliased Byte_Array_Ptr;
   Header      : aliased Pcap_Packet_Header;
   Stats       : Pcap_Stat_Ptr;
   Stats_Size  : aliased Integer;
begin
   Ret := Pcap_Find_All_Devs (All_Devs'Access, Err_Buf);

   D := All_Devs;
   while D /= null loop
      Put_Line ("" & Value (D.all.Name) & Ascii.Ht & Value (D.all.Description));
      D := D.all.Next;
   end loop;

   Handle := Pcap_Open_Live (Name, 65535, 1, 1000, Err_Buf);

   loop
      Read_Packet (Handle, Packet, Header, False);
      if Packet /= null then
         Reply_Packet (Handle, Packet);
      end if;

      Stats := Pcap_Stats_Ex (Handle, Stats_Size'Access);

      Put ("RX:" & Integer (Stats.all.Received)'Img);
      Put (" DP:" & Integer (Stats.all.Dropped)'Img);
      New_Line;

--        if Packet /= null then
--           Reply_Packet (Handle, Packet);
--        end if;
   end loop;
end Packet_Sniffer;
