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
   procedure CV_COUNT_OBS (Roi       : Cv_Size;
                           Win       : Cv_Size;
                           Delta_Obs : Cv_Size;
                           Num_Obs   : out Cv_Size) is
   begin
      Num_Obs.Width := (Roi.Width - Win.Width + Delta_Obs.Width) / Delta_Obs.Width;
      Num_Obs.Height := (Roi.Height - Win.Height + Delta_Obs.Height) / Delta_Obs.Height;
   end CV_COUNT_OBS;

   function CV_CURRENT_INT (Reader : Cv_Seq_Reader_P)
                            return Integer is
   begin
      return Character'Pos (From_Arr (Value (Reader.Ptr) (1)));
   end CV_CURRENT_INT;

   function CV_PREV_INT (Reader : Cv_Seq_Reader_P)
                         return Integer is
   begin
      return Character'Pos (From_Arr (Value (Reader.PrevElem) (1)));
   end CV_PREV_INT;

   function IplWidth (Img : Ipl_Image_P)
                      return Integer is
   begin
      if Img = null then
         return 0;
      elsif Img.ROI = null then
         return Img.Width;
      else
         return Img.ROI.Width;
      end if;
   end IplWidth;

   function IplHeight (Img : Ipl_Image_P)
                       return Integer is
   begin
      if Img = null then
         return 0;
      elsif Img.ROI = null then
         return Img.Height;
      else
         return Img.ROI.Height;
      end if;
   end IplHeight;

--     package body Class_CvCalibFilter is
--        function GetCameraCount (This : access CvCalibFilter) return Integer is
--        begin
--           return This.Camera_Count;
--        end GetCameraCount;
--
--        function IsCalibrated (This : access CvCalibFilter) return Cv_Bool is
--        begin
--           return This.Is_Calibrated;
--        end IsCalibrated;
--     end Class_CvCalibFilter;
end Legacy;
