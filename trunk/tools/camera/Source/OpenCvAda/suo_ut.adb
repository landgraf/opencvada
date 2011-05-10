--
with Interfaces; use Interfaces;
with Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Defero; use Defero;
--
package body Suo_Ut is
--

   function Byte_To_Image (Src    : Frame_Data;
                           Width  : Integer;
                           Height : Integer) return Ipl_Image_Ptr is
      type Internal_12 is new Unsigned_16 range 0 .. 4095;
      for Internal_12'Size use 12;
      type Internal is array (Integer range <>) of Internal_12; --(Integer range 0 .. ) of Internal_12;
      for Internal'Component_Size use 12;

      function Frame_Data_To_Internal is
        new From_Frame_Data (Internal_12, Internal);

      Temp : Internal := Frame_Data_To_Internal (Src);
      Dest : Ipl_Image_Ptr := Core.Operations.cv_Create_Image (Cv_Create_Size (Width, Height), Core.Ipl_Depth_8u, 3);

      Data : Core.Cv_8u_Array (0 .. ((Dest.all.Width * Dest.all.Height) * Dest.all.N_Channels)-1) := Core.Cv_8u_Pointer_Pkg.Value (Dest.all.Image_Data, Interfaces.C.Ptrdiff_T ((Dest.all.Width * Dest.all.Height) * Dest.all.N_Channels));
   begin
      for I in Data'Range loop
         Data (I) := Unsigned_8 (Shift_Right (Temp (I), 4));
      end loop;
      Put (Temp (0)'Img & Data (0)'Img);
      Core.Cv_8u_Pointer_Pkg.Copy_Array (Data (0)'Unchecked_Access, Dest.Image_Data, Data'Length);
      --Dest.Image_Data := Data (0)'Access;
      return Dest;
   end Byte_To_Image;

   -- converts ipl image to byte array
   -- each element is now 12 bit
   function Image_To_Byte (Src : Ipl_Image_Ptr) return Defero.Frame_Data is
      type Internal_12 is new Unsigned_16 range 0 .. 4095;
      for Internal_12'Size use 12;
      type Internal is array (integer range <>) of Internal_12;--(Integer range 0 .. ) of Internal_12;
      for Internal'Component_Size use 12;
--        Dest : Defero.Frame_Data (0 .. ((Src.all.Width * Src.all.Height) * 3 * Src.all.N_Channels) - 1);
      Temp : Internal(0 .. ((Src.all.Width * Src.all.Height) * Src.all.N_Channels)-1);
--        for Dest'Address use Temp'Address;
--        pragma Import (Ada, Dest);
      Data : Cv_Void_Ptr := To_Void_Ptr (Src.all.Image_Data);

      function Internal_To_Frame_Data is
        new To_Frame_Data (Internal_12, Internal);

   begin
      Put_Line("imagE_to_byte");
      case Src.Depth is
         when Ipl_Depth_1u =>
            null;
         when Ipl_Depth_8u | Ipl_Depth_8s =>
            --
            Put_Line ("Ipl_Depth_8u | Ipl_Depth_8s");
            declare
               Data : Core.Cv_8u_Array (0 .. ((Src.all.Width * Src.all.Height) * Src.all.N_Channels)-1) := Core.Cv_8u_Pointer_Pkg.Value (Src.all.Image_Data, Interfaces.C.Ptrdiff_T ((Src.all.Width * Src.all.Height) * Src.all.N_Channels));
            begin
               for I in Temp'Range loop
                  Temp (I) :=  Internal_12 (Shift_Left (Unsigned_16 (Data (I)), 4));
               end loop;
               Put_Line (Temp (0)'Img & Data(0)'img);
               Put_Line (Temp (1)'Img & Data(1)'img);
               Put_Line (Temp (2)'Img & Data (2)'img);
--

            end;
         when Ipl_Depth_16u | Ipl_Depth_16s =>
            null;
         when Ipl_Depth_32f | Ipl_Depth_32s =>
            null;
         when others =>
            null;
      end case;
      declare
         Dest : Frame_Data := Internal_To_Frame_Data (Temp);
      begin
         Put_Line (Dest (0)'img & Dest (1)'img & Dest (2)'img);
         return Dest;
      end;
   end Image_To_Byte;


end Suo_Ut;
