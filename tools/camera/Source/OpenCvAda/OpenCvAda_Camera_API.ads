--
with Core; use Core;
with Core.Operations; use Core.Operations;
with Raw_Frame_Toolkit; use Raw_Frame_Toolkit;
with Imperium_Protocol; use Imperium_Protocol;
package OpenCvAda_Camera_API is
--

   -----------------------------------------------------------------------------
   -- Images
   -----------------------------------------------------------------------------
   function Image_To_Byte (Src : Ipl_Image_Ptr) return Imperium_Protocol.Frame_Data;
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
end OpenCvAda_Camera_API;
