--
with Defero;
use Defero;
with Interfaces; use Interfaces;
package Venit_Subcriptio is
--

   -----------------------------------------------------------------------------
   -- Constant header
   -----------------------------------------------------------------------------
   type Header_Version is new Unsigned_8 range 0 .. 15;
   type Header_Length is new Unsigned_8 range 0 .. 15;
   type Header_Bit is new Integer range 0 .. 1;
   type Header_Flags is new Unsigned_8 range 0 .. 15;
   type Header_Sequence is new Unsigned_16;
   type Header_Options is new Unsigned_8;
   type Header_Data is array (Integer range 0 .. 14) of Unsigned_8;

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
