--
--
--
--
--
with Interfaces; use Interfaces;

-- Communication package Defero
package Defero is
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


   -------------------------
   -- Raw Data --
   -------------------------
   type Frame_Data is array (Integer range <> ) of Unsigned_8;

   -------------------------
   --
   -------------------------
   generic
      type T is private;
      type T_Array is array (Integer range <>) of T;
   function To_Frame_Data (Buf           : T_Array;
                           Length        : Integer;
                           Header_Length : Integer := 0) return Frame_Data;

end Defero;
