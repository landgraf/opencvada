with Interfaces; use Interfaces;
with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;
with Core_Types_C;
use Core_Types_C;
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
-- core.ads - core.hpp
-- Comments, Information, Other
-----------------------------------------------------------------------

with Interfaces.C.Strings; use Interfaces.C.Strings;

package Core is
--
   ----------------------------------------
   -- Ada stuff
   -----------------------------------------

   -----------------------------------------------------------------------------
   -- Class Mat
   -----------------------------------------------------------------------------
   -- Wrapper for Core.Class_Mat.Mat, Do not use directly!
   type Interface_Mat is tagged record
      Flags      : Integer;
      Rows, Cols : Integer;
      Step       : Interfaces.C.Size_T;
      Data       : Cv_32U_Array_P;
      Ref_Count  : Cv_32U_Array_P;
      Data_Start : Cv_32U_Array_P;
      Data_End   : Cv_32U_Array_P;
   end record;
   pragma Convention (C_Pass_By_Copy, Interface_Mat);
   type Interface_Mat_P is access Interface_Mat;

   type String_CPP is record


      Str      : Chars_Ptr;
      Size     : Short;
      Capacity : Short;
   end record;
   pragma Convention(C_Pass_By_Copy, String_CPP);

private
   procedure Nulled;
end Core;
