-- Hej!!

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Debug; use Debug;

package Toolkit is
   type Time_Record is
      record
         Total      : Time_Span;
         Min        : Time_Span;
         Max        : Time_Span;
         Avg        : Time_Span;
         --
         Execs      : Integer := 0;
         -- for debug
         Start_Time : Time;
         End_Time   : Time;
      end record;

   function Run_App (App    : String;
                     Args   : Argument_List;
                     Timer  : access Time_Record) return Integer;
   function Run_Apps (App : String;
                      Args  : Argument_List;
                      Timer : access Time_Record;
                      Execs : Integer := 1;
                      Debug : Boolean := False) return Integer;

   function Get_Arg_List (Args : String) return Argument_List;

   function To_String (Timer : Time_Record) return String;
end Toolkit;
