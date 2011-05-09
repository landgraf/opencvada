package body Defero is

-- Function for converting a generic type into a byte array.
   function To_Frame_Data (Buf           : T_Array;
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
end Defero;
