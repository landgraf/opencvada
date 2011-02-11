-----------------------------------------------------------------------
-- Ada bindings for OpenCV 2.1.1 (from SVN 3 October 2010, rev. 3703)
-- Developed as a master thesis project at Mälardalens Högskola
-- OpenCV: http://opencv.willowgarage.com/
-- Ada bindings : http://not_available.nope/
-- License @ ./LICENSE (BSD license)
-----------------------------------------------------------------------

--Contact--------------------------------------------------------------
-- Lars Cederholm, Niklas Pettersson
-- Mälardalens Högskola, http://www.mdh.se/
-- [lcm06001,npn06002]@student.mdh.se
-----------------------------------------------------------------------

--File-Info-------------------------------------------------------------
-- legacy.adb - legacy.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Legacy is
   --
   procedure Cv_Count_Obs (Roi       : Cv_Size;
                           Win       : Cv_Size;
                           Delta_Obs : Cv_Size;
                           Num_Obs   : out Cv_Size) is
   begin
      Num_Obs.Width := (Roi.Width - Win.Width + Delta_Obs.Width) / Delta_Obs.Width;
      Num_Obs.Height := (Roi.Height - Win.Height + Delta_Obs.Height) / Delta_Obs.Height;
   end Cv_Count_Obs;
   --
   function Ipl_Width (Img : Ipl_Image_Ptr)
                      return Integer is
   begin
      if Img = null then
         return 0;
      elsif Img.all.Roi = null then
         return Img.all.Width;
      else
         return Img.all.Roi.all.Width;
      end if;
   end Ipl_Width;

   --
   function Ipl_Height (Img : Ipl_Image_Ptr)
                       return Integer is
   begin
      if Img = null then
         return 0;
      elsif Img.all.Roi = null then
         return Img.all.Height;
      else
         return Img.all.Roi.all.Height;
      end if;
   end Ipl_Height;

end Legacy;
