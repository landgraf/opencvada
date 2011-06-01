
with Pcap; use Pcap;
with Raw_Frame_Toolkit;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces; use Interfaces;
with Imperium_Protocol; use Imperium_Protocol;
with Raw_Frame_Toolkit; use Raw_Frame_Toolkit;
with Ada.Containers.Vectors;

package Ethernet_Internal is
   package RFT renames Raw_Frame_Toolkit;

   Pcap_Handle_Null : exception;

   NIC_Name_Length : constant Integer := 38;

   -- Used to store the name of a NIC with the form:
   -- {FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF}
   subtype NIC_Name is String (1 .. NIC_Name_Length);
   type NIC_Name_Array is array (Integer range <>) of NIC_Name;

   -- Information about a NIC, hold the MAC Address, the NIC name and
   -- the description of the NIC. These records are populated by calling
   -- the Get_NICs function.
   type NIC_Info is record
      Handle  : Pcap_Ptr := null;
      MAC     : Mac_Address := (others => 16#00#);
      Name    : NIC_Name := (others => Ascii.Nul);
      Desc    : Unbounded_String := Null_Unbounded_String;
      Clients : RFT.Client_Info_Vector;
   end record;
   type NIC_Info_Ptr is access all NIC_Info;
   type NIC_Info_Array is array (Integer range <>) of aliased NIC_Info;

   subtype NIC_Index is Natural;
   package NIC_Info_Vector_Pkg is new Ada.Containers.Vectors (NIC_Index, NIC_Info);
   subtype NIC_Info_Vector is NIC_Info_Vector_Pkg.Vector;

   -- Grabs the MAC address, physical name and description from each Pcap
   -- compatible NIC.
   -- *** Obsolete ***
   function Get_NICs return NIC_Info_Array;

   -- Grabs the MAC address, physical name and description from each Pcap
   -- compatible NIC.
   procedure Get_NICs;

   function Fetch_NIC (Index : Integer) return NIC_Info;
   procedure Go_Boom (Index : Integer);

   function Find_NIC (Query : String) return Integer;

   function Find_NIC (Nics  : NIC_Info_Array;
                      Query : String)
                      return Integer;

   function Find_NIC (Nics  : NIC_Info_Array;
                      Query : String)
                      return NIC_Info;

   -- Broadcasts a handshake frame and generates a list of possible devices to
   -- communicate with along with the devices information.
   function Discover (Nic : NIC_Info) return RFT.Client_Info_Vector;

   -- Close a handle to a NIC.
   -- *** Deprecated ***
   procedure Close (Nic : in out NIC_Info);

   -- Open a handle to a NIC.
   -- *** Deprecated ***
   procedure Open (Nic : in out NIC_Info);

--     procedure Send (NIC    : String;
--                     Device : String;
--                     Frame  : Raw_Ehternet_Frame);

   -- Sends a single frame to the destination MAC address
   procedure Send (Handle : Pcap_Ptr;
                   Source : Mac_Address;
                   Dest   : Mac_Address;
                   Frame  : Raw_Ethernet_Frame);

   -- Sends a series of frames to the destination MAC address.
   procedure Send (Handle : Pcap_Ptr;
                   Source : Mac_Address;
                   Dest   : Mac_Address;
                   Frames : Raw_Ethernet_Frame_Array);


   -- Prints out information about a NIC.
   -- output has the form:
   -- Name: <Hardware name of the NIC>
   -- Desc: <Description from the NIC>
   -- MAC: <MAC address of the NIC>
   procedure Print_NIC (Nic : NIC_Info);
   procedure Print_NICs;
   function To_String (Mac : Mac_Address) return String;
private
   Network_Interfaces : NIC_Info_Vector;

   Pcap_Nic_Name_Prefix : constant String := "\Device\NPF_";

   function Handshake (Nic : NIC_Info) return RFT.Client_Info;

   function Create_Client_Info (Dev_Type        : RFT.Device_Type := 0;
                                Dev_Data_Types  : RFT.Data_Type_Array := (others => False);
                                Client_Prot_Ver : RFT.Protocol_Version := 0;
                                Client_Dev_Name : Unbounded_String := Null_Unbounded_String;
                                Client_Addr     : Mac_Address := (others => 16#00#);
                                NIC_Addr        : Mac_Address := (others => 16#00#))
                                return RFT.Client_Info;

   function Create_Broadcast_Client (Source : Mac_Address) return RFT.Client_Info;

   -- *** Deprecated ***
   function Create_Broadcast_Client (Nic : NIC_Info) return RFT.Client_Info;

   subtype Hex_Byte is String (1 .. 2);
   function To_Hex (Value : Unsigned_8) return Hex_Byte;

end Ethernet_Internal;
