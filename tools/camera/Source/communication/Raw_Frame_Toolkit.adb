with Ada.Text_Io; use Ada.Text_Io;
with Generic_Toolkit;
package body Raw_Frame_Toolkit is
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
      Frames_Needs_To_Be_Atleast_46_Bytes_And_You_Smell : exception;
   begin
      --        Put_Line(Max_Data_Per_Frame'Img & " = " & Frame_Size'Img & " - " & Constant_Header_Size'Img);
      if Max_Data_Per_Frame < 1 then
         raise Frames_Needs_To_Be_Atleast_46_Bytes_And_You_Smell;
      end if;
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
      -- DONT LOOK AT ME!
      function Im_Tired_Of_This_Shit return Integer is
         Temp : Integer := To_Frame_Header (Constant_Head).Length;
      begin
         Put_Line (Frame_Size'Img & Temp'Img & Spec_Header.Length'Img & Data'Length'Img);
         if Constant_Head.Flags = 2#0101# then
            Temp := Temp + 1;
         end if;
         return Amount_Of_Frames (Data'Length, Spec_Header.Length, Temp,Frame_Size) -1;
      end Im_Tired_Of_This_Shit;
      -- code starts here promise
      Frames        : Raw_Ethernet_Frame_Array (0 .. Im_Tired_Of_This_Shit);
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
         -- data headers are bigger
         -- who ever got this idea is gonna get it i promise!
         if C_Head.Flags = 2#0101# then
            Frames (I).Payload (Next_Pos .. To_Frame_Header (C_Head).Length) := To_Frame_Header (C_Head).Data (0 .. To_Frame_Header (C_Head).Length);
            Next_Pos := To_Frame_Header (C_Head).Length + 1;
            Header_Length := To_Frame_Header (C_Head).Length + 1;
--              Put_Line("c_head:" & Next_Pos'Img & Header_Length'Img);
         else
            Frames (I).Payload (Next_Pos .. To_Frame_Header (C_Head).Length - 1) := To_Frame_Header (C_Head).Data (0 .. To_Frame_Header (C_Head).Length - 1);
            Next_Pos := To_Frame_Header (C_Head).Length;
            Header_Length := To_Frame_Header (C_Head).Length;
         end if;

         if not Spec_Set then
            Frames (I).Payload (Next_Pos .. (Header_Length + Spec_Header.Length) - 1) := Spec_Header.Data (0 .. Spec_Header.Length - 1);
            Next_Pos := Header_Length + Spec_Header.Length;
            Header_Length := Header_Length + Spec_Header.Length;
            Spec_Set := True;
         end if;
         if not (I = Frames'Length - 1) then
            Test_Val := Next_Data - Header_Length + Frame_Size;
            Frames (I).Payload (Next_Pos .. Frame_Size-1) := Data (Next_Data .. Test_Val - 1);
            Next_Data := (Next_Data + (Frame_Size - Header_Length));
            Frames (I).Length := Frame_Size;
         else
            declare
               Temp_Int : Integer := (Next_Pos +  Data'Last - Next_Data) - Next_Pos;
               Temp_Int2 : Integer := Data'Last - Next_Data;
            begin
               Put_Line (Temp_Int'Img & Temp_Int2'Img & Next_Data'Img & Frames'Length'Img);
            end;
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

   -- Converts Constant_Header to defero Frame_Header
   function To_Frame_Header (Src : Constant_Header) return Frame_Header is
      Dest : Frame_Header;
      Temp : Frame_Data (0 .. 19);
      for Temp'Address use Src'Address;
   begin
      Dest.Data := Temp;
      Dest.Length := Integer (Src.Length) + Const_Header_Size;
      if Src.Flags = 2#1111# or Src.Flags = 2#0101# then
         Dest.Length := Dest.Length;
      end if;
      return Dest;
   end To_Frame_Header;

-- converts a raw_ethernet_frame to a parsed one with more information
   function From_Raw_Frame (Src : Raw_Ethernet_Frame) return Parsed_Raw_Frame is
      Temp : Parsed_Raw_Frame;
      Length : Integer := 5;
   begin
      Temp.Raw_Frame := Src;

      if Temp.Raw_Frame.Length >= Length then
         --get constant header
         declare
            Temp_Length : Integer := Integer(shift_Right (Src.Payload (Src.Payload'First + 0), 4));
         begin
            Length := Length + Temp_Length;
         end;
         if not (Temp.Raw_Frame.Length >= Length) then
            goto Is_Not_A_Frame;
         end if;
         -- create constant header
         declare
            Temp_Header : Frame_Header;
         begin
            Temp_Header.Length := Length;
            for I in Integer range 0 .. Length - 1 loop
               Temp_Header.Data (I) := Src.Payload (Src.Payload'First + I);
            end loop;
--              Put_Line (Temp_Header.Data (1)'Img & Length'Img);
            Temp.Constant_Head := Frame_To_Constant_Header (Temp_Header,5);
--              Put_Line (Temp.Constant_Head.Flags 'Img);
         end;
         -- what type of frame do we have?
         case (temp.Constant_Head.Flags) is
            --(Array_Frame, Config_Frame, Image_Frame, Matrix_Frame, Memory_Frame, Control_Frame, Other, Not_A_Frame);
            when 2#0000# =>
               -- undefined /at this stage at least
               Temp.Type_Of_Frame := Other;
            when 2#0001# | 2#0011# | 2#0100# =>
               -- control frames
               Temp.Type_Of_Frame := Control_Frame;
            when 2#0010# =>
               -- config frame
               Temp.Type_Of_Frame := Config_Frame;
            when 2#0101# =>
               -- data frame headers are longer
               Temp.Constant_Head.Options := Src.Payload (Src.Payload'First + Length);
               Length := Length + 1;
               -- data frame *find out more*
               declare
                  Temp_Option : Unsigned_8 := 2#0000_0000#;
               begin
                  Temp_Option := Shift_Left (Temp.Constant_Head.Options, 2);
                  Temp_Option := Shift_Right (Temp_Option, 2);
                  case Temp_Option is
                     when 2#000_00000# =>
                        --image
                        Temp.Type_Of_Frame := Image_Frame;
                     when 2#0000_0001# .. 2#0000_1111# =>
                        -- matrix
                        Temp.Type_Of_Frame := Matrix_Frame;
                     when 2#0001_0000# .. 2#0001_1111# =>
                        -- array
                        Temp.Type_Of_Frame := Array_Frame;
                     when others =>
                        goto Is_Not_A_Frame;
                  end case;
               end;
            when 2#0110# =>
               -- memory frame
               Temp.Type_Of_Frame := Memory_Frame;
            when 2#1111# =>
               Temp.Type_Of_Frame := Other;
               -- could look at option here to find out what we are doing.
            when others =>
               -- if we get here add your own case!
               -- or we are parsing something else as our frame that will be bad!
               goto Is_Not_A_Frame;
         end case;
         -- we know know the frame type check for specification header
         if Temp.Constant_Head.Seq_No = 0 then
            -- we have a specification header!
            if not (Size_Of_Headers (Temp.Type_Of_Frame) = 0) then
               -- we have a spec header!
               Temp.Spec_Header.Length := Size_Of_Headers (Temp.Type_Of_Frame); -- set length
--                 Put_Line("spec_length" & Size_Of_Headers (Temp.Type_Of_Frame)'img & Temp.Type_Of_Frame'Img);
               Temp.Spec_Header.Data (0 .. Temp.Spec_Header.Length - 1) := Temp.Raw_Frame.Payload (Temp.Raw_Frame.Payload'First + Length .. (Temp.Raw_Frame.Payload'First + Temp.Spec_Header.Length + Length) -1);
               Temp.Payload_Start := (Temp.Raw_Frame.Payload'First + Temp.Spec_Header.Length + Length);
            elsif (Size_Of_Headers (Temp.Type_Of_Frame) = -1) then
               goto Is_Not_A_Frame;
            else
               -- no spec header for this type
               Temp.Spec_Header.Length := 0;
               Temp.Payload_Start := Length;
               null;
            end if;
         else
            Temp.Payload_Start := Length; -- should be after the constant header...
         end if;
         --           We Are Done Now
         return Temp;
      else
         goto Is_Not_A_Frame;
      end if;
      -- not a real frame
      << Is_Not_A_Frame >>
      Temp.Type_Of_Frame := Not_A_Frame;
      return Temp;
   end From_Raw_Frame;

   -----------------------------------------------------------------------------
   -- Raw Ethernet function stuff
   -----------------------------------------------------------------------------
--     function Discover (Handle : Pcap_Ptr)
--                        return Client_Info_Array is
--     begin
--        null;
--        -- Broadcast handshake frame
--        -- While reply pending
--        --   If reply is ack handshake
--        --     Create new client info
--        --
--     end Discover;
--
--     function Create_Broadcast_Client_Info return Client_Info is
--        Client : Client_Info;
--     begin
--        Client.Device.Raw_Header.Dest := (others => 16#FF#);
--  --        Client.Device.Raw_Header.Src :=
--     end Create_Broadcast_Client_Info;

   function Is_Supported_Data_Type (T      : Data_Type;
                                    Device : Device_Info)
                                    return Boolean is
   begin
      return Device.Data_Types(T);
   end Is_Supported_Data_Type;

   function To_Byte_Array (Dest  : Mac_Address;
                           Src   : Mac_Address;
                           Frame : Raw_Ethernet_Frame)
                           return Byte_Array is
      Data : Byte_Array (0 .. 13 + Frame.Length);
   begin
      Data (0 .. 5) := Byte_Array (Dest);
      Data (6 .. 11) := Byte_Array (Src);
      Data (12) := Shift_Right (Unsigned_8 (Frame.Length), 8);
      Data (13) := Unsigned_8 (Frame.Length) and 16#FF#;
      Data (14 .. Data'Last) := Frame.Payload (Frame.Payload'First .. Frame.Length - 1);

      return Data;
   end To_Byte_Array;
end Raw_Frame_Toolkit;
