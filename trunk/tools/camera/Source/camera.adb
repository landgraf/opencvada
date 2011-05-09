---

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
procedure Camera is

   type Mac_Address is array (Integer range 0 .. 5) of Unsigned_8;
   type Packet_Type_Id is array (Integer range 0 .. 1) of Unsigned_8;

   type Raw_Frame_Header is
      record
         Dest : Mac_Address;
         Src  : Mac_Address;
         Id   : Packet_Type_Id;
      end record;

   type Frame_Data is array (Integer range <> ) of Unsigned_8;
   type Pcap_Buffer is array (Integer range <> ) of Unsigned_8;

   generic
      type T is private;
      type T_Array is array (Integer range <>) of T;
   function Ultra_Procedure (Buf           : T_Array;
                              Length        : Integer;
                              Header_Length : Integer := 0) return Frame_Data;

   function Ultra_Procedure (Buf           : T_Array;
                              Length        : Integer;
                              Header_Length : Integer := 0) return Frame_Data is
      Frames           : Integer := ((T'Size * Length) / 8) / 1500 - Header_Length;
      Rest             : Integer := ((T'Size * Length) / 8) mod 1500 - Header_Length;
      Bits             : Integer := (T'Size * Length);

      type Bit is new Integer range 0 .. 1;
      for Bit'Size use 1;
      type Bit_Array is array (Integer range 0 .. Bits - 1) of Bit;
      for Bit_Array'Component_Size use 1;

      Temp_Bytes_Array : Bit_Array;
      for Temp_Bytes_Array'Address use Buf'Address;
      pragma Import (Ada,Temp_Bytes_Array);

      function Byte_Length(Length : Integer) return Integer is
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

      function To_Byte (Src : Bit_Array) return Frame_Data is
         Dest : Frame_Data (0 .. Byte_Length(Src'Length));
         for Dest'Address use Src'Address;
         pragma Import (Ada, Dest);
      begin
         return Dest;
      end To_Byte;

      Temp_Bytes       : Frame_Data (0 .. Byte_Length (Temp_Bytes_Array'Length));
   begin
      Put(Bits'Img);
      for I in Temp_Bytes_Array'Range loop
         if (I mod T'Size) = 0 then
            New_Line;
         end if;
         Put (Temp_Bytes_Array (I)'Img);
      end loop;

      Temp_Bytes := To_Byte (Temp_Bytes_Array);
      for I in Temp_Bytes'Range loop
         if (I mod (T'Size/8)) = 0 then
            New_Line;
         end if;
         Put (Temp_Bytes (I)'Img);
      end loop;

      return Temp_Bytes;
   end Ultra_Procedure;

   procedure Parse_Raw (Buf    : Pcap_Buffer;
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

   type Mongo is new integer range 0 .. 127;
   for Mongo'Size use 7;

   type Double_Bill is
      record
         A : Mongo;
         B : Unsigned_8;
         C : Unsigned_16;
      end record;
   for Double_Bill'Size use 1500 * 8;--Mongo'Size + Unsigned_8'Size + Unsigned_16'Size;
   pragma Pack (Double_Bill);

   type Pelle_2 is array (Integer range <>) of Double_Bill;

   Pelle : aliased Pelle_2 := ((127,129,6168), (0,0,0),(24,24,6168), (0,0,0),(24,24,6169), (0,0,0));

   function Nej_Nej is
     new Ultra_Procedure (Double_Bill, Pelle_2);

   type Raw_Ethernet_Frame is
      record
         Raw_Header : Raw_Frame_Header;
         Payload    : Frame_Data (0 .. 1499) := (others => 0);
         Length     : Integer := 1500;
      end record;

   type Raw_Ethernet_Frame_Array is array (Integer range <>) of Raw_Ethernet_Frame;

--     function Create_Raw_Ethernet_Payloads (Src             : Frame_Data;
--                                            Constant_Header : Frame_Data;
--                                            First_Header    : Frame_Data) return Raw_Ethernet_Frame_Array is
--        Max_Data : constant Integer := 1500 - Constant_Header'Length;
--        -- Finds the length of the current frame...
--        function Frame_Length (Length : Integer) return Integer is
--           function Find_Padding return Integer is
--           begin
--              if (Length mod Max_Data) = 0 then
--                 return 1;
--              else
--                 return 0;
--              end if;
--           end Find_Padding;
--        begin
--           return ((Length / Max_Data) - Find_Padding);
--        end Frame_Length;
--
--        Dest : Raw_Ethernet_Frame_Array (0 .. Frame_Length (Src'Length));
--        Counter : Integer := 0;
--     begin
--        Dest (0).Header := 1;
--        for N in Dest'Range loop
--           for P in Dest (N).Payload'Range loop
--              Dest (N).Payload (P) := Src (Counter);
--              Counter := Counter + 1;
--              exit when Counter > Src'Length;
--           end loop;
--        end loop;
--        return Dest;
--     end Create_Raw_Ethernet_Payloads;



   type Frame_Header is
      record
         Data : Frame_Data(0 .. 10);
         Length : Integer;
      end record;

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

   function Create_Raw_Frames (Data            : Frame_Data;
                               Spec_Header     : Frame_Header;
                               Constant_Header : Frame_Header;
                               Frame_Size      : Integer := 1500) return Raw_Ethernet_Frame_Array is
      Frames : Raw_Ethernet_Frame_Array (0 .. Amount_Of_Frames (Data'Length, Spec_Header.Length, Constant_Header.Length));
      Spec_Set : Boolean := False;
      Next_Data : Integer := 0;
      Next_Pos  : Integer := 0;
   begin
      for I in Frames'Range loop
         Next_Pos := 0;
         Frames (I).Payload (Next_Pos .. Constant_Header.Length - 1) := Constant_Header.Data (0 .. Constant_Header.Length - 1);
         Next_Pos := Constant_Header.Length;
         if not spec_Set then
            Frames (I).Payload (Next_Pos .. (Constant_Header.Length + Spec_Header.Length) - 1) := Spec_Header.Data (0 .. Spec_Header.Length);
            Next_Pos := Constant_Header.Length + Spec_Header.Length;
            Spec_Set := True;
         end if;
         if not (I = Frames'Length - 1) then
            Frames (I).Payload (Next_Pos .. Frame_Size - 1) := Data (Next_Data .. Frame_Size - ((Constant_Header.Length + Spec_Header.Length) - 1));
            Next_Data := Frame_Size - ((Constant_Header.Length + Spec_Header.Length) - 1);
         else
            Frames (I).Payload (Next_Pos .. Frame_Size - 1) := Data (Next_Data .. Data'Last);
            Next_Data := -1;
         end if;
      end loop;
      return Frames;
   end Create_Raw_Frames;

   Payload : Frame_Data := Nej_Nej (Pelle, Pelle'Length);
begin
   Put_Line("frames: " & Amount_Of_Frames(Payload'Length,8,5)'img);
end Camera;
