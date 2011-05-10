--
package body Venit_Subcriptio is
--

   --
   -- Converts Constant_Header to defero Frame_Header
   function To_Frame_Header (Src : Constant_Header) return Frame_Header is
      Dest : Frame_Header;
      Temp : Frame_Data (0 .. 19);
      for Temp'Address use Src'Address;
   begin
      Dest.Data := Temp;
      Dest.Length := Integer (Src.Length) + 4;
      if Src.Flags = 2#1111# or Src.Flags = 2#0101# then
         Dest.Length := Dest.Length + 1;
      end if;
      return Dest;
   end To_Frame_Header;

   -- Converts defero Frame_header to Constant_Header
   function To_Constant_Header (Src    : Frame_Data;
                                Offset : Integer := 0) return Constant_Header is
      Src_Temp : Frame_Data (0 .. 19) := Src (Offset .. Offset + 19);
      Dest     : Constant_Header;
      for Dest'Address use Src_Temp'Address;
   begin
      return Dest;
   end To_Constant_Header;

   --- Creates a ping constant header
   function Ping (Version : Integer := 0;
                  Ack     : Boolean := False;
                  Req     : Boolean := True) return Constant_Header is
      Temp : Constant_Header;
   begin
      Temp.Version := Header_Version (Version);
      Temp.Ack := Boolean'Pos(Ack);
      Temp.Req := Boolean'Pos (Req);
      Temp.Eof := 1;
      Temp.Flags := 2#0001#;
      return Temp;
   end Ping;
end Venit_Subcriptio;
