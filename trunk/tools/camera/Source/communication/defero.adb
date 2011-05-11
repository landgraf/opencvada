with Ada.Text_Io; use Ada.Text_Io;
package body Defero is
--

   -- Function for converting a generic type into a byte array.
   function To_Frame_Data (Buf : T_Array) return Frame_Data is
      Bits : Integer := (T'Size * Buf'Length);

      type Bit is new Integer range 0 .. 1;
      for Bit'Size use 1;
      type Bit_Array is array (Integer range 0 .. Bits - 1) of Bit;
      for Bit_Array'Component_Size use 1;

      Temp_Bytes_Array : Bit_Array;

      for Temp_Bytes_Array'Address use Buf'Address;
      pragma Import (Ada, Temp_Bytes_Array);

      -- Length of array in bytes.
      function Byte_Length (Length : Integer) return Integer is
         function Find_Padding return Integer is
         begin
            if (Length mod 8) = 0 then
               return 1;
            else
               return 0;
            end if;
         end Find_Padding;
      begin
         return ((Length / 8) - Find_Padding);
      end Byte_Length;

      -- Convert a bit array into a byte array.
      function To_Byte (Src : Bit_Array) return Frame_Data is
         Dest : Frame_Data (0 .. Byte_Length (Src'Length));
         for Dest'Address use Src'Address;
         pragma Import (Ada, Dest);
      begin
         return Dest;
      end To_Byte;

      Temp_Bytes       : Frame_Data (0 .. Byte_Length (Temp_Bytes_Array'Length));
   begin
      Temp_Bytes := To_Byte (Temp_Bytes_Array);
      return Temp_Bytes;
   end To_Frame_Data;

   function From_Frame_Data (Buf : Frame_Data) return T_Array is
      Bits : Integer := (8 * Buf'Length);
      type Bit is new Integer range 0 .. 1;
      for Bit'Size use 1;
      type Bit_Array is array (Integer range 0 .. Bits - 1) of Bit;
      for Bit_Array'Component_Size use 1;

      Temp : Bit_Array;
      for Temp'Address use Buf'Address;
      pragma Import (Ada, Temp);

      type Fixed_Bit_Array is array (Integer range 0 .. Bits - 1) of Bit;
      Temp_Fixed : Bit_Array := Temp;
      for Temp_Fixed'Alignment use 4;
      Dest : T_Array (0 .. (Bits / T'Size)-1);
      for Dest'Address use Temp_Fixed'Address;
      pragma Import (Ada, Dest);
   begin
      Put_Line (Dest'Length'Img);
      return Dest;
   end From_Frame_Data;

   --
   procedure Parse_Raw (Buf    : Frame_Data;
                        Header : out Raw_Frame_Header;
                        Data   : out Frame_Data) is
      Data_Temp   : Frame_Data (0 .. Buf'Length - 15);
      Header_Temp : Raw_Frame_Header;
   begin
      Header_Temp.Dest := Mac_Address (Buf (0 .. 5));
      Header_Temp.Src := Mac_Address (Buf (6 .. 11));
      Header_Temp.Id := Packet_Type_Id (Buf (12 .. 13));
      Data_Temp := Frame_Data (Buf (14 .. Buf'Last));

      Header := Header_Temp;
      Data := Data_Temp;
   end Parse_Raw;

   -- Amount of Raw Ethernet frames needed to send Data
   -- in bytes please
   function Amount_Of_Frames (Data_Bytes           : Integer;
                              Spec_Header_Size     : Integer;
                              Constant_Header_Size : Integer;
                              Frame_Size           : Integer := 1500) return Integer is
      Max_Data_Per_Frame : Integer := Frame_Size - Constant_Header_Size;
      Total_Data         : Integer := Data_Bytes + Spec_Header_Size;
      Ret_Val            : Integer := 0;
   begin
      Ret_Val := Total_Data / Max_Data_Per_Frame;
      if (Total_Data mod Max_Data_Per_Frame) > 0 then
         Ret_Val := Ret_Val + 1;
      end if;
      return Ret_Val;
   end Amount_Of_Frames;

   -- Creates a raw frame
   function Create_Raw_Frames (Data            : Frame_Data;
                               Spec_Header     : Frame_Header := ((others => 0), Length => 0);
                               Constant_Head   : Constant_Header;
                               Frame_Size      : Integer := 1500) return Raw_Ethernet_Frame_Array is
      Frames        : Raw_Ethernet_Frame_Array (0 .. Amount_Of_Frames (Data'Length, Spec_Header.Length, To_Frame_Header (Constant_Head).Length) -1);
      Spec_Set      : Boolean := False;
      Next_Data     : Integer := 0;
      Next_Pos      : Integer := 0;
      Header_Length : Integer := 0;
      Test_Val      : Integer;

      C_Head        : Constant_Header := Constant_Head;
   begin
      for I in Frames'Range loop
         Next_Pos := 0;
         Header_Length := 0;
         C_Head.Seq_No := Unsigned_16 (I);
         Put_Line(C_Head.Seq_No'Img);
         Frames (I).Payload (Next_Pos .. To_Frame_Header (C_Head).Length - 1) := To_Frame_Header (C_Head).Data (0 .. To_Frame_Header (C_Head).Length - 1);
         Next_Pos := To_Frame_Header (C_Head).Length;
         Header_Length := To_Frame_Header (C_Head).Length;
         if not Spec_Set then
            Frames (I).Payload (Next_Pos .. (Header_Length + Spec_Header.Length) - 1) := Spec_Header.Data (0 .. Spec_Header.Length - 1);
            Next_Pos := Header_Length + Spec_Header.Length;
            Header_Length := Header_Length + Spec_Header.Length;
            Spec_Set := True;
         end if;
         if not (I = Frames'Length - 1) then
            Test_Val := Next_Data - Header_Length + Frame_Size;
            Frames (I).Payload (Next_Pos .. Frames (I).Payload'Last) := Data (Next_Data .. Test_Val - 1);
            Next_Data := (Next_Data + (Frame_Size - Header_Length));
         else
            Frames (I).Payload (Next_Pos .. Next_Pos + Data'Last - Next_Data ) := Data (Next_Data .. Data'Last);
            Frames (I).Length := 0;
            if (Data'Last - Next_Data + Header_Length) >= 46 then
               Frames (I).Length := Frames (I).Length + (Data'Last - Next_Data + Header_Length);
            else
               Frames (I).Length := Frames (I).Length + 46;
            end if;
            Next_Data := Data'Last + 1;
         end if;
      end loop;
      return Frames;
   end Create_Raw_Frames;

   --
   -- Converts Constant_Header to defero Frame_Header
   function To_Frame_Header (Src : Constant_Header) return Frame_Header is
      Dest : Frame_Header;
      Temp : Frame_Data (0 .. 19);
      for Temp'Address use Src'Address;
   begin
      Dest.Data := Temp;
      Dest.Length := Integer (Src.Length) + Const_Header_Min_Length;
      if Src.Flags = 2#1111# or Src.Flags = 2#0101# then
         Dest.Length := Dest.Length + Const_Header_Opt_Length;
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



   function To_Frame_Header (Src : Image_Header) return Frame_Header is
      Dest : Frame_Header;
      Temp : Frame_Data (1 .. 5);
      for Temp'Address use Src'Address;
   begin
      Dest.Data (0 .. 4) := Temp;
      Dest.Length := Image_Header_Size;
      return Dest;
   end To_Frame_Header;

   function To_Image_Header (Src    : Frame_Data;
                             Offset : Integer := 0)
                             return Image_Header is
      Temp : Frame_Data (0 .. Image_Header_Size - 1) := Src (Offset .. Offset + Image_Header_Size - 1);
      Dest : Image_Header;
      for Dest'Address use Temp'Address;
   begin
      return Dest;
   end To_Image_Header;
end Defero;
