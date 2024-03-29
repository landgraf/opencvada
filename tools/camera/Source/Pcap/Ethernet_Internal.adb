

package body Ethernet_Internal is
   function Get_NICs return NIC_Info_Array is
      All_Devs  : aliased Pcap_If_Ptr;
      It        : Pcap_If_Ptr;
      Err_Buf   : Pcap_Error_String := (others => Ascii.Nul);
      NIC_Count : Integer := Pcap_Find_All_Devs (All_Devs'Access, Err_Buf);
      Names     : NIC_Info_Array (1 .. NIC_Count);
      Index     : Integer := 0;
   begin
      It := All_Devs;
      while It /= null loop
         Index := Index + 1;
         Names (Index).Name := Value (It.all.Name) (13 .. 50);
         Names (Index).Desc := To_Unbounded_String (Value (It.all.Description));
         Names (Index).MAC := Get_MAC (Names (Index).Name, To_String (Names (Index).Desc));
         Names (Index).Clients.Append (Create_Broadcast_Client (Names (Index)));

         It := It.all.Next;
      end loop;
      return Names;
   end Get_NICs;

   procedure Get_NICs is
      All_Devs  : aliased Pcap_If_Ptr;
      It        : Pcap_If_Ptr;
      Err_Buf   : Pcap_Error_String := (others => Ascii.Nul);
      NIC_Count : Integer := Pcap_Find_All_Devs (All_Devs'Access, Err_Buf);
      NIC       : NIC_Info;
   begin
      It := All_Devs;
      while It /= null loop
         NIC.Name := Value (It.all.Name) (13 .. 50);
         NIC.Desc := To_Unbounded_String (Value (It.all.Description));
         NIC.MAC := Get_MAC (Nic.Name, To_String (Nic.Desc));
         NIC.Clients.Append (Create_Broadcast_Client (NIC));

         Network_Interfaces.Append (NIC);
         It := It.Next;
      end loop;
   end Get_NICs;

   function Fetch_NIC (Index : Integer) return NIC_Info is
   begin
      return Network_Interfaces.Element (Index);
   end Fetch_NIC;

   procedure Go_Boom (Index : Integer) is
      procedure Go (Element : in out NIC_Info) is
      begin
         Element.MAC (Element.MAC'First) := 16#FF#;
      end Go;
   begin
      Network_Interfaces.Update_Element (Index, Go'Access);
   end Go_Boom;

   function Find_NIC (Query : String) return Integer is
      Result : Integer := -1;
   begin
      for I in Network_Interfaces.First_Index .. Network_Interfaces.Last_Index loop
         if Network_Interfaces.Element (I).Name = Query or To_String (Network_Interfaces.Element (I).Desc) = Query then
            Result := I;
         end if;
      end loop;
      return Result;
   end Find_NIC;

   function Find_NIC (Nics  : NIC_Info_Array;
                      Query : String)
                      return Integer is
      Result : Integer := -1;
   begin
      for I in Nics'Range loop
         if Nics (I).Name = Query or To_String (Nics (I).Desc) = Query then
            Result := I;
            exit;
         end if;
      end loop;

      return Result;
   end Find_NIC;

   function Find_NIC (Nics  : NIC_Info_Array;
                      Query : String)
                      return NIC_Info is
      NIC : Nic_Info;
   begin
      for I in Nics'Range loop
         if Nics (I).Name = Query or To_String (Nics (I).Desc) = Query then
            NIC := Nics (I);
            exit;
         end if;
      end loop;

      return NIC;
   end Find_NIC;

   function Discover (Nic : NIC_Info) return RFT.Client_Info_Vector is
      Clients   : RFT.Client_Info_Vector;
      Header    : Constant_Header := Create_Constant_Header (Flags  => Flag_Handshake,
                                                             Length => 4,
                                                             Req    => True,
                                                             Eof    => True,
                                                             Data   => (2#01101101#, others => 16#00#));

      Data      : Frame_Data (0 .. 100) := (others => 16#B7#);
      Frames    : Raw_Ethernet_Frame_Array := Create_Raw_Frames (Constant_Head => Header,
                                                                 Data          => Data);
      Parsed_Frame : Parsed_Raw_Frame :=    From_Raw_Frame (Frames (Frames'First));
      Ret       : Integer;
      Broadcast : Mac_Address := (others => 16#FF#);
      Source    : Mac_Address := Nic.MAC;
      Broadcast_Info : Client_Info := Create_Broadcast_Client (Nic);
   begin
      if Nic.Handle = null then
         raise Pcap_Handle_Null;
      end if;

      Clients.Append (Broadcast_Info);

      Ret := Pcap.Pcap_Send_Packet (Nic.Handle,
                                    To_Byte_Array (Broadcast, Source, Frames (Frames'First)),
                                    Frames (Frames'First).Length + 14);

      return Clients;
   end Discover;

--     procedure Send (NIC    : String;
--                     Device : String;
--                     Frame  : Raw_Ehternet_Frame) is
--        Result : Integer;
--     begin
--        Result := Pcap.Pcap_Send_Packet (Network_Interfaces.Element (Find_NIC (NIC)).Handle,
--                                         To_Byte_Array(
--     end Send;

   procedure Send (Handle : Pcap_Ptr;
                   Source : Mac_Address;
                   Dest   : Mac_Address;
                   Frame  : Raw_Ethernet_Frame) is
      Result : Integer;
   begin
      Result := Pcap.Pcap_Send_Packet (Handle, To_Byte_Array (Dest, Source, Frame), Frame.Length + 14);
   end Send;

   procedure Send (Handle : Pcap_Ptr;
                   Source : Mac_Address;
                   Dest   : Mac_Address;
                   Frames : Raw_Ethernet_Frame_Array) is
   begin
      for I in Frames'Range loop
         Put_Line ("Sending frame #" & I'Img);
         Send (Handle, Source, Dest, Frames (I));
      end loop;
   end Send;

   procedure Close (Nic : in out NIC_Info) is
   begin
      Pcap_Close (Nic.Handle);
      Nic.Handle := null;
   end Close;

   procedure Open (Nic : in out NIC_Info) is
      Err_Buf : Pcap_Error_String := (others => Ascii.Nul);
   begin
      Nic.Handle := Pcap_Open_Live (Pcap_Nic_Name_Prefix & Nic.Name, 65535, 1, 1000, Err_Buf);
   end Open;

   function Handshake (Nic : NIC_Info) return RFT.Client_Info is
      pragma Warnings (Off);
      Client : RFT.Client_Info;
      pragma Warnings (On);
   begin
      return Client;
   end Handshake;

   procedure Print_NIC (Nic : NIC_Info) is
   begin
      Put_Line ("Name: " & Nic.Name);
      Put_Line ("Desc: " & To_String (Nic.Desc));
      Put ("MAC: ");
      for I in Nic.MAC'Range loop
         Put (To_Hex (Nic.Mac (I)));
         if I < Nic.MAC'Last then
            Put ("-");
         end if;
      end loop;
      New_Line;
   end Print_NIC;

   procedure Print_NICs is
   begin
      for I in Network_Interfaces.First_Index .. Network_Interfaces.Last_Index loop
         Print_NIC (Network_Interfaces.Element (I));
      end loop;
   end Print_NICs;

   function Create_Broadcast_Client (Source : Mac_Address) return RFT.Client_Info is
   begin
      return Create_Client_Info (Client_Addr     => (others => 16#FF#),
                                 NIC_Addr        => Source,
                                 Client_Dev_Name => To_Unbounded_String ("FF-FF-FF-FF-FF-FF"));
   end Create_Broadcast_Client;

   function Create_Broadcast_Client (Nic : NIC_Info) return RFT.Client_Info is
   begin
      return Create_Client_Info (Client_Addr     => (others => 16#FF#),
                                 NIC_Addr        => Nic.MAC,
                                 Client_Dev_Name => To_Unbounded_String ("Broadcast"));
   end Create_Broadcast_Client;

   function Create_Client_Info (Dev_Type        : RFT.Device_Type := 0;
                                Dev_Data_Types  : RFT.Data_Type_Array := (others => False);
                                Client_Prot_Ver : RFT.Protocol_Version := 0;
                                Client_Dev_Name : Unbounded_String := Null_Unbounded_String;
                                Client_Addr     : Mac_Address := (others => 16#00#);
                                NIC_Addr        : Mac_Address := (others => 16#00#))
                                return RFT.Client_Info is
      Client : RFT.Client_Info;
   begin
      Client.Prot_Ver := Client_Prot_Ver;
      Client.Device_Name := Client_Dev_Name;
      Client.Device_Addr := Client_Addr;
      Client.Device.Dev_Type := Dev_Type;
      Client.Device.Data_Types := Dev_Data_Types;
      Client.Device.Raw_Header.Dest := Client_Addr;
      Client.Device.Raw_Header.Src := NIC_Addr;

      return Client;
   end Create_Client_Info;

   function To_Hex (Value : Unsigned_8) return Hex_Byte is
      Hex    : Hex_Byte;
      First  : Unsigned_8 := Shift_Right (Value, 4);
      Second : Unsigned_8 := Value and Unsigned_8 (16#F#);
   begin
      if First < 10 then
         Hex (1) := Character'Val (First + Character'Pos ('0'));
      else
         Hex (1) := Character'Val (First + Character'Pos ('A') - 10);
      end if;

      if Second < 10 then
         Hex (2) := Character'Val (Second + Character'Pos ('0'));
      else
         Hex (2) := Character'Val (Second + Character'Pos ('A') - 10);
      end if;

      return Hex;
   end To_Hex;

   function To_String (Mac : Mac_Address) return String is
      Str : String (1 .. 17);
   begin
      Str (1 .. 2) := To_Hex (Mac (Mac'First));
      Str (3) := '-';
      Str (4 .. 5) := To_Hex (Mac (Mac'First + 1));
      Str (6) := '-';
      Str (7 .. 8) := To_Hex (Mac (Mac'First + 2));
      Str (9) := '-';
      Str (10 .. 11) := To_Hex (Mac (Mac'First + 3));
      Str (12) := '-';
      Str (13 .. 14) := To_Hex (Mac (Mac'First + 4));
      Str (15) := '-';
      Str (16 .. 17) := To_Hex (Mac (Mac'First + 5));

      return Str;
   end To_String;
begin
   Get_NICs;
end Ethernet_Internal;
