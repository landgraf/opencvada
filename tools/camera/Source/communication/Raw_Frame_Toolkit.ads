--
--
--
--
--
with Interfaces; use Interfaces;
with Imperium_Protocol;
use Imperium_Protocol;
-- Communication package Defero
package Raw_Frame_Toolkit is
--

   -------------------------
   -- Raw Ethernet header --
   -------------------------
   type Mac_Address is array (Integer range 0 .. 5) of Unsigned_8;
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

   --* Creates a series of Raw Ethernet frames from a frame_data structure
   function Create_Raw_Frames (Data                 : Frame_Data;
                               Spec_Header          : Frame_Header := ((others => 0), Length => 0);
                               Constant_Head        : Constant_Header;
                               Frame_Size           : Integer := 1500) return Raw_Ethernet_Frame_Array;

   type Spec_Frames_List is (Array_Frame, Config_Frame, Image_Frame, Matrix_Frame, Memory_Frame, Control_Frame, Other, Not_A_Frame);

   --
   type Disctribuebantur_Raw_Frame is
      record
         Constant_Head : Constant_Header;
         Type_Of_Frame : Spec_Frames_List;
         Spec_Header   : Frame_Header;
         Payload_Start : Integer;
         Raw_Frame     : Raw_Ethernet_Frame;
      end record;

   function From_Raw_Frame (Src : Raw_Ethernet_Frame) return Disctribuebantur_Raw_Frame;

   -----------------------------------------------------------------------------
   --
   -----------------------------------------------------------------------------
      --* Converts Constant_Header to Frame_Header
   function To_Frame_Header (Src : Constant_Header) return Frame_Header;

   --* Converts Frame_header to Constant_Header
   function To_Constant_Header (Src    : Frame_Data;
                                Offset : Integer := 0) return Constant_Header;

--     function To_Frame_Header (Src : Image_Header) return Frame_Header;

   function To_Image_Header (Src    : Frame_Data;
                             Offset : Integer := 0)
                             return Image_Header;

private

   --* Amount of Raw Ethernet frames needed to send Data
   function Amount_Of_Frames (Data_Bytes           : Integer;
                              Spec_Header_Size     : Integer;
                              Constant_Header_Size : Integer;
                              Frame_Size           : Integer := 1500) return Integer;

end Raw_Frame_Toolkit;
