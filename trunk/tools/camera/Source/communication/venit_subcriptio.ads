--
with Defero;
use Defero;
with Interfaces; use Interfaces;
package Venit_Subcriptio is
--

   -----------------------------------------------------------------------------
   -- Constant header
   -----------------------------------------------------------------------------
   subtype Header_Version is Unsigned_8 range 0 .. 15;
   subtype Header_Length is Unsigned_8 range 0 .. 15;
   subtype Header_Bit is Integer range 0 .. 1;
   subtype Header_Flags is Unsigned_8 range 0 .. 15;
   subtype Header_Sequence is Unsigned_16;
   subtype Header_Options is Unsigned_8;
   type Header_Data is array (Integer range 0 .. 14) of Unsigned_8;

   subtype Header_Reserved is Integer;
   subtype Header_Columns is Integer range 0 .. 4095;
   subtype Header_Rows is Integer range 0 .. 4095;
   subtype Header_Color_Depth is Integer range 0 .. 15;
   subtype Header_Elem_Size is Integer range 0 .. 8191;
   subtype Header_Padding is Integer range 0 .. 7;
   subtype Header_Elements is Integer range 0 .. 16777215;
   subtype Header_Reg_Count is Integer range 0 .. 65535;
   subtype Header_Reg_Size is Integer range 0 .. 255;
   subtype Header_Addr_Size is Integer range 0 .. 255;
   subtype Header_Mem_Addr is Integer;

   Const_Header_Min_Length : constant := 4;
   Const_Header_Opt_Lenght : constant := 1;


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

   --* Converts Constant_Header to Frame_Header
   function To_Frame_Header (Src : Constant_Header) return Frame_Header;

   --* Converts Frame_header to Constant_Header
   function To_Constant_Header (Src    : Frame_Data;
                                Offset : Integer := 0) return Constant_Header;

   -----------------------------------------------------------------------------
   -- Specific constant header creators
   -----------------------------------------------------------------------------
   function Ping (Version : Integer := 0;
                  Ack     : Boolean := False;
                  Req     : Boolean := True) return Constant_Header;
--     function Handshake return Constant_Header;
--     function Subscription return Constant_Header;
--     function Data return Constant_Header;

end Venit_Subcriptio;
   -----------------------------------------------------------------------------
   -- Image header
   -----------------------------------------------------------------------------
   type Image_Header is record
      Columns  : Header_Columns;
      Rows     : Header_Rows;
      Depth    : Header_Color_Depth;
      Origin   : Header_Bit;
      Float    : Header_Bit;
      Reserved : Header_Reserved;
   end record;

   for Image_Header use record
      Columns at 0 range 0 .. 11;
      Rows at 0 range 12 .. 23;
      Depth at 0 range 24 .. 27;
      Origin at 0 range 28 .. 28;
      Float at 0 range 29 .. 29;
      Reserved at 0 range 30 .. 39;
   end record;

   for Image_Header'Size use 40;

   -----------------------------------------------------------------------------
   -- Matrix header
   -----------------------------------------------------------------------------
   type Matrix_Header is record
      Columns   : Header_Columns;
      Rows      : Header_Rows;
      Float     : Header_Bit;
      Signed    : Header_Bit;
      Reserved  : Header_Reserved;
      Elem_Size : Header_Elem_Size;
      Padding   : Header_Padding;
   end record;

   for Matrix_Header use record
      Columns at 0 range 0 .. 11;
      Rows at 0 range 12 .. 23;
      Float at 0 range 24 .. 24;
      Signed at 0 range 25 .. 25;
      Reserved at 0 range 26 .. 31;
      Elem_Size at 0 range 32 .. 44;
      Padding at 0 range 45 .. 47;
   end record;

   for Matrix_Header'Size use 48;

   -----------------------------------------------------------------------------
   -- Array header
   -----------------------------------------------------------------------------
   type Array_Header is record
      Elements  : Header_Elements;
      Float     : Header_Bit;
      Signed    : Header_Bit;
      Reserved  : Header_Reserved;
      Elem_Size : Header_Elem_Size;
      Padding   : Header_Padding;
   end record;

   for Array_Header use record
      Elements at 0 range 0 .. 23;
      Float at 0 range 24 .. 24;
      Signed at 0 range 25 .. 25;
      Reserved at 0 range 26 .. 31;
      Elem_Size at 0 range 32 .. 44;
      Padding at 0 range 45 .. 47;
   end record;

   for Array_Header'Size use 48;

   -----------------------------------------------------------------------------
   -- Configuration header
   -----------------------------------------------------------------------------
   type Config_Header is record
      Reg_Count : Header_Reg_Count;
      Reg_Size  : Header_Reg_Size;
      Addr_Size : Header_Addr_Size;
      Reserved  : Header_Reserved;
   end record;

   for Config_Header use record
      Reg_Count at 0 range 0 .. 15;
      Reg_Size at 0 range 16 .. 23;
      Addr_Size at 0 range 24 .. 31;
      Reserved at 0 range 32 .. 39;
   end record;

   for Config_Header'Size use 40;

   -----------------------------------------------------------------------------
   -- Memory header
   -----------------------------------------------------------------------------
   type Memory_Header is record
      Start_Addr : Header_Mem_Addr;
      End_Addr   : Header_Mem_Addr;
      Reserved   : Header_Reserved;
   end record;

   for Memory_Header use record
      Start_Addr at 0 range 0 .. 31;
      End_Addr at 0 range 32 .. 63;
      Reserved at 0 range 64 .. 71;
   end record;

   for Memory_Header'Size use 72;
end Venit_Subciptio;
