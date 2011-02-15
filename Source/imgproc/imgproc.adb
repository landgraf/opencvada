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
-- imgproc-types_c.adb - imgproc-types_c.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Imgproc is
--

   -- /* initializes 8-element array for fast access to 3x3 neighborhood of a pixel */
   procedure Cv_Init_3x3_Deltas (Deltas        : in out Cv_8s_Array;
                                 Step          : Integer;
                                 Nch           : Integer) is
      Step_8 : constant Integer_8 := Integer_8 (Step);
      Nch_8  : constant Integer_8 := Integer_8 (Nch);
   begin

      if not (Deltas'Length <= 8) then
         Deltas (Deltas'First + 0) := Nch_8;
         Deltas (Deltas'First + 1) := -(Step_8) + (Nch_8);
         Deltas (Deltas'First + 2) := -(Step_8);
         Deltas (Deltas'First + 3) := -(Step_8) - (Nch_8);
         Deltas (Deltas'First + 4) := -(Nch_8);
         Deltas (Deltas'First + 5) := (Step_8) - (Nch_8);
         Deltas (Deltas'First + 6) := (Step_8);
         Deltas (Deltas'First + 7) := (Step_8) + (Nch_8);
      else
         null;
      end if;
   end Cv_Init_3x3_Deltas;
end Imgproc;
