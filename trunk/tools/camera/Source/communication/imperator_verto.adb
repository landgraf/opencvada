--
--
--
with Interfaces; use Interfaces;
package body Imperator_Verto is
--

   --
   -- used for converting headers
   function Generic_To_Generic (Source : Source_T;
                                Length : Integer := Source_T'Size / 8) return Destination_T is
   --

      -----------------------------------------------------------------------------
      -- Temporary types ignore...
      -------------------------
      type Frame_Data is array (Integer range <> ) of Unsigned_8;
      type Frame_Header is
         record
            Data   : Frame_Data (0 .. 19);
            Length : Integer := 5;
         end record;
      -----------------------------------------------------------------------------
      Size : constant integer := Destination_T'Size;
      Temp_Source : Frame_Data (0 .. Length - 1);
      for Temp_Source'Address use Source'Address;
      Temp_Dest   : Frame_Data (0 .. (Destination_T'Size / 8) -1) := (others => 2#0000_0000#);
      Destination : Destination_T;
      for Destination'Address use Temp_Dest'Address;
      pragma Import(Ada,Destination);
   begin
      if (Destination_T'Size / 8) >= Length then
         for I in Integer range 0 .. Length - 1 loop
            Temp_Dest (I) := Temp_Source (I);
         end loop;
      end if;

      if ((Destination_T'Size / 8) - (32 / 8)) >= Length and Destination_T'Size = Frame_Header'Size then
         declare
            Length_Bytes : Frame_Data (0 .. 3);
            for Length_Bytes'Address use Length'Address;
            Counter      : Integer := 0;
         begin
            for I in Integer range (Destination_T'Size / 8) - 4 .. ((Destination_T'Size / 8) - 1) loop
               Temp_Dest (I) := Length_Bytes (Counter);
               Counter := Counter + 1;
            end loop;
         end;
      end if;
      return Destination;
   end Generic_To_Generic;
end Imperator_Verto;
