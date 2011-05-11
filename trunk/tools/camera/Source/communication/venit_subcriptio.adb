--
package body Venit_Subcriptio is
--
   --- Creates a ping constant header
   function Ping (Version : Integer := 0;
                  Ack     : Boolean := False;
                  Req     : Boolean := True) return Constant_Header is
      Temp : Constant_Header;
   begin
      Temp.Version := Header_Version (Version);
      Temp.Ack := Boolean'Pos (Ack);
      Temp.Req := Boolean'Pos (Req);
      Temp.Eof := 1;
      Temp.Flags := 2#0001#;
      return Temp;
   end Ping;

   function Create_Image_Header (Cols   : Header_Columns;
                                 Rows   : Header_Rows;
                                 Depth  : Header_Color_Depth := Color_36_Bit;
                                 Origin : Header_Bit := False;
                                 Float  : Header_Bit := False)
                                 return Image_Header is
      Header : Image_Header;
   begin
      Header.Columns := Cols;
      Header.Rows := Rows;
      Header.Depth := Depth;
      Header.Origin := Origin;
      Header.Float := Float;
--        Header.Reserved := (others => False);

      return Header;
   end Create_Image_Header;

   function To_Frame_Header (Src : Image_Header) return Frame_Header is
      Dest : Frame_Header;
      Temp : Frame_Data (0 .. 19);
      for Temp'Address use Src'Address;
   begin
      Dest.Data := Temp;
      Dest.Length := Image_Header_Size;
      return Dest;
   end To_Frame_Header;

   function To_Image_Heaeder (Src    : Frame_Header;
                              Offset : Integer := 0)
                              return Image_Header is
      Temp : Frame_Data (1 .. Image_Header_Size) := Src (Offset .. Offset + Image_Header_Size);
      Dest : Image_Header;
      for Dest'Address use Temp'Address;
   begin
      return Dest;
   end To_Image_Header;
end Venit_Subcriptio;
