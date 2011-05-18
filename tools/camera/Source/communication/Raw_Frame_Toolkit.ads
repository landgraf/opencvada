--
--
--
--
--
with Interfaces; use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Imperium_Protocol; use Imperium_Protocol;
with Pcap; use Pcap;
with Ada.Containers.Vectors;

-- Communication package Defero
package Raw_Frame_Toolkit is
--

   -------------------------
   -- Raw Ethernet header --
   -------------------------
--     type Mac_Address is array (Integer range 0 .. 5) of Unsigned_8;
   type Packet_Type_Id is array (Integer range 0 .. 1) of Unsigned_8;

   type Raw_Frame_Header is
      record
         Dest : Mac_Address;
         Src  : Mac_Address;
         Id   : Packet_Type_Id;
      end record;

   ---------------------------
   -- Raw Ethernet frame structure
   -----------------------------------------------------
   type Raw_Ethernet_Frame is
      record
         Raw_Header : Raw_Frame_Header;
         Payload    : Frame_Data (0 .. 1499) := (others => 0);
         Length     : Integer := 1500;
      end record;

   type Raw_Ethernet_Frame_Array is array (Integer range <>) of Raw_Ethernet_Frame;

   -------------------------
   -- Frame_Data functions
   -------------------------

   --* Converts a generic array into the sendable Frame_Data type.
   generic
      type T is private;
      type T_Array is array (Integer range <>) of T;
   function To_Frame_Data (Buf : T_Array) return Frame_Data;

   generic
      type T is private;
      type T_Array is array (Integer range <>) of T;
   function From_Frame_Data (Buf : Frame_Data) return T_Array;

   -------------------------
   -- Raw Ethernet frame functions
   --------------------------

   --* P
   procedure Parse_Raw (Buf    : Frame_Data;
                        Header : out Raw_Frame_Header;
                        Data   : out Frame_Data);

   ------------------------------------
   -- Implementation specific
   -------------------------------------

   function To_Byte_Array (Dest  : Mac_Address;
                           Src   : Mac_Address;
                           Frame : Raw_Ethernet_Frame)
                           return Byte_Array;

   --* Creates a series of Raw Ethernet frames from a frame_data structure
   function Create_Raw_Frames (Data                 : Frame_Data;
                               Spec_Header          : Frame_Header := ((others => 0), Length => 0);
                               Constant_Head        : Constant_Header;
                               Frame_Size           : Integer := 1500) return Raw_Ethernet_Frame_Array;

   --
   type Parsed_Raw_Frame is
      record
         Constant_Head : Constant_Header;
         Type_Of_Frame : Spec_Frames_List;
         Spec_Header   : Frame_Header;
         Payload_Start : Integer;
         Raw_Frame     : Raw_Ethernet_Frame;
      end record;

   function From_Raw_Frame (Src : Raw_Ethernet_Frame) return Parsed_Raw_Frame;

   -----------------------------------------------------------------------------
   -- Raw Ethernet function stuff
   -----------------------------------------------------------------------------
   -- Describes what type of device this is.
   type Device_Type is range 0 .. 65535;
   for Device_Type'Size use 16;

   type Data_Type is range 0 .. 255;
   for Data_Type'Size use 8;

   -- Describes which data types the device can process.
   type Data_Type_Array is array (Data_Type range 0 .. 255) of aliased Boolean;

   type Protocol_Version is range 0 .. 15;
   for Protocol_Version'Size use 4;

   type Device_Info is record
      Raw_Header : Raw_Frame_Header;
      Dev_Type   : Device_Type := 0;
      Data_Types : Data_Type_Array := (others => False);
   end record;

   type Client_Info is record
      Device      : Device_Info;
      Prot_Ver    : Protocol_Version := 0;
      Device_Name : Unbounded_String := Null_Unbounded_String;
      Device_Addr : Mac_Address := (others => 16#00#);
   end record;

   package Client_Info_Vector_Pkg is new Ada.Containers.Vectors (Natural, Client_Info);
   subtype Client_Info_Vector is Client_Info_Vector_Pkg.Vector;

   type Client_Info_Array is array (Integer range <>) of Client_Info;

   function Is_Supported_Data_Type (T      : Data_Type;
                                    Device : Device_Info)
                                    return Boolean;

--     function Discover (Handle : Pcap_Ptr)
--                        return Client_Info_Array;



   function To_Frame_Header (Src : Constant_Header) return Frame_Header;
private
--     function Create_Broadcast_Client_Info return Client_Info;

   --* Amount of Raw Ethernet frames needed to send Data
   function Amount_Of_Frames (Data_Bytes           : Integer;
                              Spec_Header_Size     : Integer;
                              Constant_Header_Size : Integer;
                              Frame_Size           : Integer := 1500) return Integer;

end Raw_Frame_Toolkit;
