--
with Core; use Core;
with Core.Operations; use Core.Operations;
with Defero; use Defero;
with Venit_Subcriptio; use Venit_Subcriptio;
package Suo_Ut is
--

   type Suo_Ut_Type is new Integer range 0 .. 1;

   -----------------------------------------------------------------------------
   -- Images
   -----------------------------------------------------------------------------
   function Image_To_Byte (Src : Ipl_Image_Ptr) return Defero.Frame_Data;
   function Byte_To_Image (Src    : Frame_Data;
                           Width  : Integer;
                           Height : Integer) return Ipl_Image_Ptr;

   function Image_To_Header (Src : Ipl_Image_Ptr) return Image_Header;
   -----------------------------------------------------------------------------
   -- Other
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Generics
   -----------------------------------------------------------------------------
end Suo_Ut;
