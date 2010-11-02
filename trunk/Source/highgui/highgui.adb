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
-- highgui.adb - highgui.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

package body Highgui is

   function CreateFileSettings ( Compression     : Compression_Type;
                                Compression_Rate : Integer;
                                Not_Used         : Integer := 0) return File_Settings is
   begin
      return File_Settings'( Compression, Compression_Rate, Not_Used );
   end CreateFileSettings;

   procedure Nulled is
   begin null; end Nulled;
end Highgui;
