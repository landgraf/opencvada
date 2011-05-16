--
with Interfaces; use Interfaces;
with Generic_Toolkit; use Generic_Toolkit;
package Imperium_Protocol is
--

   -------------------------
   -- Raw Data --
   -------------------------
   type Frame_Data is array (Integer range <> ) of Unsigned_8;

   -----------------------------------------------------------------------------
   -- Extra header for frames
   -----------------------------------------------------------------------------
   type Frame_Header is
      record
         Data   : Frame_Data (0 .. 19);
         Length : Integer := 5;
      end record;

   -----------------------------------------------------------------------------
   -- Headers, sizes and things
   -----------------------------------------------------------------------------
   type Spec_Frames_List is (Array_Frame, Config_Frame, Image_Frame, Matrix_Frame, Memory_Frame, Control_Frame, Other, Not_A_Frame);
   -- change me, if you change something!
   Size_Of_Headers : array (Spec_Frames_List'Range) of Integer := (6,5,5,6,9,0,0,-1);

   -----------------------------------------------------------------------------
   -- Constant header
   -----------------------------------------------------------------------------
   subtype Header_Version is Unsigned_8 range 0 .. 15;
   subtype Header_Length is Unsigned_8 range 0 .. 15;
   subtype Header_Bit is Boolean;
   subtype Header_Flags is Unsigned_8 range 0 .. 15;
   subtype Header_Sequence is Unsigned_16;
   subtype Header_Package_Sequence is Unsigned_8;
   subtype Header_Options is Unsigned_8;
   type Header_Data is array (Integer range 0 .. 14) of Unsigned_8;
   Null_Header_Data : constant Header_Data := (others => 0);

   type Header_Reserved is array (Integer range <>) of Header_Bit;
   for Header_Reserved'Component_Size use 1;
   subtype Header_Columns is Integer range 0 .. 4095;
   subtype Header_Rows is Integer range 0 .. 4095;
   subtype Header_Color_Depth is Integer range 0 .. 15;
   subtype Header_Elem_Size is Integer range 0 .. 8191;
   subtype Header_Padding is Integer range 0 .. 7;
   subtype Header_Elements is Integer range 0 .. 16777215;
   subtype Header_Reg_Count is Integer range 0 .. 65535;
   subtype Header_Reg_Size is Integer range 0 .. 255;
   subtype Header_Addr_Size is Integer range 0 .. 255;
   type Header_Mem_Addr is array (Integer range 0 .. 0) of Integer;
   for Header_Mem_Addr'Component_Size use 32;
   for Header_Mem_Addr'Size use 32;


   Const_Header_Min_Length : constant := 4;
   Const_Header_Opt_Length : constant := 1;

   Image_Header_Size : constant := 5;
   Matrix_Header_Size : constant := 6;
   Array_Header_Size : constant := 6;
   Config_Header_Size : constant := 5;
   Memory_Header_Size : constant := 9;


   Color_1_Bit  : constant Header_Color_Depth := 2#0000#; -- 1 bit B/W
   Gray_8_Bit   : constant Header_Color_Depth := 2#0001#; -- 8 bit grayscale (8)
   Color_8_Bit  : constant Header_Color_Depth := 2#0010#; -- 8 bit color (3, 3, 2)
   Color_15_Bit : constant Header_Color_Depth := 2#0011#; -- 15 bit color (5, 5, 5)
   Color_24_Bit : constant Header_Color_Depth := 2#0100#; -- 24 bit color (8, 8, 8)
   Color_30_Bit : constant Header_Color_Depth := 2#0101#; -- 30 bit color (10, 10, 10)
   Color_36_Bit : constant Header_Color_Depth := 2#0110#; -- 36 bit color (12, 12, 12)
   Color_48_Bit : constant Header_Color_Depth := 2#0111#; -- 48 bit color (16, 16, 16)

   type Constant_Header is
      record
         Version     : Header_Version;
         Length      : Header_Length;
         Ack         : Header_Bit;
         Nak         : Header_Bit;
         Eof         : Header_Bit;
         Req         : Header_Bit;
         Flags       : Header_Flags;
         Seq_No      : Header_Sequence;
         Package_Seq : Header_Package_Sequence;
         Options     : Header_Options;
         Data        : Header_Data;
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
         Package_Seq at 0 range 32 .. 39;
         Options at 0 range 40 .. 47;
         Data at 0 range 48 .. 167;
      end record;

   for Constant_Header'Size use 168;
   for Constant_Header'Alignment use 1;

   function Create_Constant_Header (Version     : Header_Version := 0;
                                    Length      : Header_Length := 0;
                                    Ack         : Header_Bit := False;
                                    Nak         : Header_Bit := False;
                                    Eof         : Header_Bit := False;
                                    Req         : Header_Bit := False;
                                    Flags       : Header_Flags := 2#0000#;
                                    Seq_No      : Header_Sequence := 0;
                                    Package_Seq : Header_Package_Sequence := 0;
                                    Options     : Header_Options := 0;
                                    Data        : Header_Data := Null_Header_Data)
                                    return Constant_Header;

   -----------------------------------------------------------------------------
   -- Specific constant header creators
   -----------------------------------------------------------------------------
   function Ping (Version : Integer := 0;
                  Ack     : Boolean := False;
                  Req     : Boolean := True) return Constant_Header;
   --     function Handshake return Constant_Header;
   --     function Subscription return Constant_Header;
   --     function Data return Constant_Header;

   -----------------------------------------------------------------------------
   -- Image header
   -----------------------------------------------------------------------------
   type Image_Header is record
      Columns  : Header_Columns;
      Rows     : Header_Rows;
      Depth    : Header_Color_Depth;
      Origin   : Header_Bit;
      Float    : Header_Bit;
      Reserved : Header_Reserved (1 .. 10);
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
   for Image_Header'Alignment use 1;

   function Create_Image_Header (Cols   : Header_Columns;
                                 Rows   : Header_Rows;
                                 Depth  : Header_Color_Depth := Color_36_Bit;
                                 Origin : Header_Bit := False;
                                 Float  : Header_Bit := False)
                                 return Image_Header;

   -----------------------------------------------------------------------------
   -- Matrix header
   -----------------------------------------------------------------------------
   type Matrix_Header is record
      Columns   : Header_Columns;
      Rows      : Header_Rows;
      Float     : Header_Bit;
      Signed    : Header_Bit;
      Reserved  : Header_Reserved (1 .. 6);
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
   for Matrix_Header'Alignment use 1;

   function Create_Matrix_Header (Cols      : Header_Columns;
                                  Rows      : Header_Rows;
                                  Elem_Size : Header_Elem_Size;
                                  Padding   : Header_Padding;
                                  Float     : Header_Bit := False;
                                  Signed    : Header_Bit := False)
                                  return Matrix_Header;
   -----------------------------------------------------------------------------
   -- Array header
   -----------------------------------------------------------------------------
   type Array_Header is record
      Elements  : Header_Elements;
      Float     : Header_Bit;
      Signed    : Header_Bit;
      Reserved  : Header_Reserved (1 .. 6);
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
   for Array_Header'Alignment use 1;

   function Create_Array_Header (Elements  : Header_Elements;
                                 Elem_Size : Header_Elem_Size;
                                 Padding   : Header_Padding;
                                 Float     : Header_Bit := False;
                                 Signed    : Header_Bit := False)
                                 return Array_Header;

   -----------------------------------------------------------------------------
   -- Configuration header
   -----------------------------------------------------------------------------
   type Config_Header is record
      Reg_Count : Header_Reg_Count;
      Reg_Size  : Header_Reg_Size;
      Addr_Size : Header_Addr_Size;
      Reserved  : Header_Reserved (1 .. 8);
   end record;

   for Config_Header use record
      Reg_Count at 0 range 0 .. 15;
      Reg_Size at 0 range 16 .. 23;
      Addr_Size at 0 range 24 .. 31;
      Reserved at 0 range 32 .. 39;
   end record;

   for Config_Header'Size use 40;
   for Config_Header'Alignment use 1;

   function Create_Config_Header (Reg_Count : Header_Reg_Count;
                                  Reg_Size  : Header_Reg_Size;
                                  Addr_Size : Header_Addr_Size)
                                  return Config_Header;

   -----------------------------------------------------------------------------
   -- Memory header
   -----------------------------------------------------------------------------
   type Memory_Header is record
      Start_Addr : Header_Mem_Addr;
      End_Addr   : Header_Mem_Addr;
      Reserved   : Header_Reserved (0 .. 7);
   end record;

   for Memory_Header use record
      Start_Addr at 0 range 0 .. 31;
      End_Addr at 0 range 32 .. 63;
      Reserved at 0 range 64 .. 71;
   end record;
   for Memory_Header'Size use 72;
   for Memory_Header'Alignment use 1;

   function Create_Memory_Header (Start_Addr : Header_Mem_Addr;
                                  End_Addr   : Header_Mem_Addr)
                                  return Memory_Header;


   ---
   function Array_To_Frame_Header is
     new Generic_To_Generic (Array_Header, Frame_Header);
   function Config_To_Frame_Header is
     new Generic_To_Generic (Config_Header, Frame_Header);
   function Constant_To_Frame_Header is
     new Generic_To_Generic (Constant_Header, Frame_Header);
   function Image_To_Frame_Header is
     new Generic_To_Generic (Image_Header, Frame_Header);
   function Matrix_To_Frame_Header is
     new Generic_To_Generic (Matrix_Header, Frame_Header);
   function Memory_To_Frame_Header is
     new Generic_To_Generic (Memory_Header, Frame_Header);

   function Frame_To_Array_Header is
     new Generic_To_Generic (Frame_Header, Array_Header);
   function Frame_To_Config_Header is
     new Generic_To_Generic (Frame_Header, Config_Header);
   function Frame_To_Constant_Header is
     new Generic_To_Generic (Frame_Header, Constant_Header);
   function Frame_To_Image_Header is
     new Generic_To_Generic (Frame_Header, Image_Header);
   function Frame_To_Matrix_Header is
     new Generic_To_Generic (Frame_Header, Matrix_Header);
   function Frame_To_Memory_Header is
     new Generic_To_Generic (Frame_Header, Memory_Header);
end Imperium_Protocol;
