--
--
--
--
--
with Interfaces; use Interfaces;
with Venit_Subcriptio;
use Venit_Subcriptio;
-- Communication package Defero
package Defero is
--

   -------------------------
   -- Raw Data --
   -------------------------
   type Frame_Data is array (Integer range <> ) of Unsigned_8;

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
   -- Extra header for frames
   type Frame_Header is
      record
         Data : Frame_Data(0 .. 19);
         Length : Integer := 4;
      end record;

   --* Creates a series of Raw Ethernet frames from a frame_data structure
   function Create_Raw_Frames (Data                 : Frame_Data;
                               Spec_Header          : Frame_Header := ((others => 0), Length => 0);
                               Constant_Head        : Constant_Header;
                               Frame_Size           : Integer := 1500) return Raw_Ethernet_Frame_Array;

   -----------------------------------------------------------------------------
   --
   -----------------------------------------------------------------------------
      --* Converts Constant_Header to Frame_Header
   function To_Frame_Header (Src : Constant_Header) return Frame_Header;

   --* Converts Frame_header to Constant_Header
   function To_Constant_Header (Src    : Frame_Data;
                                Offset : Integer := 0) return Constant_Header;
private

   --* Amount of Raw Ethernet frames needed to send Data
   function Amount_Of_Frames (Data_Bytes           : Integer;
                              Spec_Header_Size     : Integer;
                              Constant_Header_Size : Integer;
                              Frame_Size           : Integer := 1500) return Integer;

end Defero;
