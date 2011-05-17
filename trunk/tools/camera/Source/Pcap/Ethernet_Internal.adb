
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

         It := It.all.Next;
      end loop;
      return Names;
   end Get_NICs;

   function Discover (Nic : NIC_Info) return RFT.Client_Info_Array is
      Clients : RFT.Client_Info_Array (1 .. 0);
   begin
      return Clients;
   end Discover;

   procedure Close (Nic : in out NIC_Info) is
   begin
      Pcap_Close (Nic.Handle);
   end Close;

   procedure Open (Nic : in out NIC_Info) is
      Err_Buf : Pcap_Error_String := (others => Ascii.Nul);
   begin
      Nic.Handle := Pcap_Open_Live (Pcap_Nic_Name_Prefix & Nic.Name, 65535, 1, 1000, Err_Buf);
   end Open;

   function Handshake (Nic : NIC_Info) return RFT.Client_Info is
      Client : RFT.Client_Info;
   begin
      return Client;
   end Handshake;

   procedure Print_NIC (Nic : NIC_Info) is
   begin
      Put_Line ("Name:" & Nic.Name);
      Put_Line ("Desc:" & To_String (Nic.Desc));
      Put ("MAC:");
      for I in Nic.MAC'Range loop
         Put (Item => Integer (Nic.MAC (I)), Base => 16);
      end loop;
      New_Line;
   end Print_NIC;

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
end Ethernet_Internal;
