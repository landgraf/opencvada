--

with Ada.Unchecked_Conversion;
generic
   type Data_T is private;
   type Data_T_Array is array (Integer range <>) of Data_T;
package Core.Class_Mat is
   type Data_T_P is access Data_T;
   type Data_T_Array_P is access Data_T_Array;

   type Mat is tagged limited record
      Flags      : Integer;
      Rows, Cols : Integer;
      Step       : Interfaces.C.Size_T;
      Data       : Data_T_Array_P;
      Ref_Count  : Cv_32U_Array_P;
      Data_Start : Data_T_P;
      Data_End   : Data_T_P;
   end record;
   pragma Import (CPP, Mat);
   type Mat_P is access all Mat;

   function New_Mat return Mat;
   --        function New_Mat (Rows     : Integer;
   --                          Cols     : Integer;
   --                          Mat_Type : Integer) return Mat;
   pragma CPP_Constructor (New_Mat, "_ZN2cv3MatC1Ev");

   function New_Mat (Image      : Ipl_Image_P;
                     Copy_Data  : Boolean := False) return Mat;
   pragma CPP_Constructor (New_Mat, "_ZN2cv3MatC2EPK9_IplImageb");
   procedure Delete_Mat (This : access Mat);
   pragma Import (CPP, Delete_Mat, "_ZN2cv3MatD1Ev");

   function Im_Read (File_Name : String_CPP;
                     Flags     : Integer := 1) return Mat;
   pragma CPP_Constructor (Im_Read, "_ZN2cv6imreadERKSsi");

   -----------------------------------------------------------------------------
   -- Ada Stuff
   -----------------------------------------------------------------------------
   pragma Warnings (Off);
   function "+" is
     new Ada.Unchecked_Conversion (Source => Mat_P,
                                   Target => Core.Interface_Mat_P);
      function "+" is
     new Ada.Unchecked_Conversion (Source => Core.Interface_Mat_P,
                                   Target => Mat_P);
--     function To_Interface is
--       new Ada.Unchecked_Conversion (Source => Mat,
--                                     Target => Core.Interface_Mat);
   pragma Warnings (On);
private



end Core.Class_Mat;


