--
--
--
with Defero; use Defero;
with Venit_Subcriptio; use Venit_Subcriptio;
package Imperator_Verto is
--

   function To_Test is
     new Generic_To_Generic (Image_Header, Frame_Header);

   function Array_To_Frame_Header is
     new Generic_To_Generic (Array_Header, Frame_Header);
   function Config_To_Frame_Header is
     new Generic_To_Generic (Config_Header, Frame_Header);
   function Constant_To_Frame_Header is
     new Generic_To_Generic (Constant_Header, Frame_Header);
   function Image_To_Frame_Header is
     new Generic_To_Generic (Image_Header, Frame_Header);
   function Matrix_To_Frame_Header is
     new Generic_To_Generic (Matrix_Header, Frame_Header);
   function Memory_To_Frame_Header is
     new Generic_To_Generic (Memory_Header, Frame_Header);

   function Frame_To_Array_Header is
     new Generic_To_Generic (Frame_Header, Array_Header);
   function Frame_To_Config_Header is
     new Generic_To_Generic (Frame_Header, Config_Header);
   function Frame_To_Constant_Header is
     new Generic_To_Generic (Frame_Header, Constant_Header);
   function Frame_To_Image_Header is
     new Generic_To_Generic (Frame_Header, Image_Header);
   function Frame_To_Matrix_Header is
     new Generic_To_Generic (Frame_Header, Matrix_Header);
   function Frame_To_Memory_Header is
     new Generic_To_Generic (Frame_Header, Memory_Header);
end Imperator_Verto;
