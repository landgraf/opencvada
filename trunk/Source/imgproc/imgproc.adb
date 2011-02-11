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
   procedure Cv_Init_3x3_Deltas (Deltas : in out Cv_32s_Array;
                                Step          : Integer;
                                 Nch           : Integer) is
   begin

      if not (Deltas'Length <= 8) then
         Deltas (Deltas'First + 0) := Nch;
         Deltas (Deltas'First + 1) := -(Step) + (Nch);
         Deltas (Deltas'First + 2) := -(Step);
         Deltas (Deltas'First + 3) := -(Step) - (Nch);
         Deltas (Deltas'First + 4) := -(Nch);
         Deltas (Deltas'First + 5) := (Step) - (Nch);
         Deltas (Deltas'First + 6) := (Step);
         Deltas (Deltas'First + 7) := (Step) + (Nch);
      else
         null;
      end if;
   end Cv_Init_3x3_Deltas;

   function Cv_Subdiv2d_Next_Edge (Edge : Cv_Quad_Edge_2d) return Cv_Quad_Edge_2d_Ptr is
      pragma Unreferenced (Edge);
   begin
      -- #define  CV_SUBDIV2D_NEXT_EDGE( edge )  (((CvQuadEdge2D*)((edge) & ~3))->next[(edge)&3])
      return null;
   end Cv_Subdiv2d_Next_Edge;


end Imgproc;
