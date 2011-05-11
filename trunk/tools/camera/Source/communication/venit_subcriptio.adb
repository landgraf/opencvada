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
      Temp.Length := 2#0000#;
      Temp.Ack := Ack;
      Temp.Nak := False;
      Temp.Req := Req;
      Temp.Eof := True;
      Temp.Flags := 2#0001#;
      Temp.Options := 2#0000_0000#;
      Temp.Data := (others => 2#0000_0000#);
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
end Venit_Subcriptio;
