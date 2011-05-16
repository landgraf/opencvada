--
package body Imperium_Protocol is
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

   function Create_Matrix_Header (Cols      : Header_Columns;
                                  Rows      : Header_Rows;
                                  Elem_Size : Header_Elem_Size;
                                  Padding   : Header_Padding;
                                  Float     : Header_Bit := False;
                                  Signed    : Header_Bit := False)
                                  return Matrix_Header is
      Header : Matrix_Header;
   begin
      Header.Columns := Cols;
      Header.Rows := Rows;
      Header.Elem_Size := Elem_Size;
      Header.Padding := Padding;
      Header.Float := Float;
      Header.Signed := Signed;
--        Header.Reserved := (others => False);

      return Header;
   end Create_Matrix_Header;

   function Create_Array_Header (Elements  : Header_Elements;
                                 Elem_Size : Header_Elem_Size;
                                 Padding   : Header_Padding;
                                 Float     : Header_Bit := False;
                                 Signed    : Header_Bit := False)
                                 return Array_Header is
      Header : Array_Header;
   begin
      Header.Elements := Elements;
      Header.Elem_Size := Elem_Size;
      Header.Padding := Padding;
      Header.Float := Float;
      Header.Signed := Signed;
--        Header.Reserved := (others => False);

      return Header;
   end Create_Array_Header;

   function Create_Config_Header (Reg_Count : Header_Reg_Count;
                                  Reg_Size  : Header_Reg_Size;
                                  Addr_Size : Header_Addr_Size)
                                  return Config_Header is
      Header : Config_Header;
   begin
      Header.Reg_Count := Reg_Count;
      Header.Reg_Size := Reg_Size;
      Header.Addr_Size := Addr_Size;
--        Header.Reserved := (others => False);

      return Header;
   end Create_Config_Header;

   function Create_Memory_Header (Start_Addr : Header_Mem_Addr;
                                  End_Addr   : Header_Mem_Addr)
                                  return Memory_Header is
      Header : Memory_Header;
   begin
      Header.Start_Addr := Start_Addr;
      Header.End_Addr := End_Addr;
--        Header.Reserved := (others => False);

      return Header;
   end Create_Memory_Header;

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
                                    return Constant_Header is
      Header : Constant_Header;
   begin
      Header.Version := Version;
      Header.Length := Length;
      Header.Ack := Ack;
      Header.Nak := Nak;
      Header.Eof := Eof;
      Header.Req := Req;
      Header.Flags := Flags;
      Header.Seq_No := Seq_No;
      Header.Package_Seq := Package_Seq;
      Header.Options := Options;
      Header.Data := Data;

      return Header;
   end Create_Constant_Header;


end Imperium_Protocol;
