--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.fixed;
package body Toolkit is
   function Run_App (App    : String;
                     Args   : Argument_List;
                     Timer  : access Time_Record) return Integer is
      Success : Boolean := False;
      App_Time : Time_Span;
   begin
      Timer.all.Start_Time := Clock;
      Gnat.OS_Lib.Spawn (App, Args, Success);
      Timer.all.End_Time := Clock;
      App_Time := Timer.all.End_Time - Timer.all.Start_Time;
      if Timer.all.Min > App_Time then
         Timer.all.Min := App_Time;
      end if;
      if Timer.all.Max < App_Time then
         Timer.all.Max := App_Time;
      end if;

      Timer.all.Execs := Timer.all.Execs + 1;
      Timer.all.Total := Timer.all.Total + App_Time;
      -- add timer stuffs
      return Boolean'Pos(Success);
   end Run_App;

   function To_String (Timer : Time_Record) return String is
   begin
      return To_Duration(Timer.Total)'Img & "," & To_Duration(Timer.Min)'Img & "," & To_Duration(Timer.Max)'Img & "," & To_Duration(Timer.Avg)'Img & "," & Timer.Execs'Img;
   end To_String;

   function Run_Apps (App   : String;
                      Args  : Argument_List;
                      Timer : access Time_Record;
                      Execs : Integer := 1;
                      Debug : Boolean := False) return Integer is
      Ret : Integer := 0;
   begin
      for I in Integer range 1 .. Execs
      loop
         Ret := Ret + Run_App (App, Args, Timer);
--           Put_Debug(App & "," & To_String(Timer.all));
      end loop;
      Timer.all.Avg := Timer.all.Total / Timer.all.Execs;
      Put_Debug(App & "," & To_String(Timer.all));
      return Ret - Execs;
   end Run_Apps;

   function Get_Arg_List (Args : String) return Argument_List is
      N_Args : Integer := Ada.Strings.Fixed.Count (Args, " ") + 1;
      Ret    : Argument_List (1 .. N_Args) ;
      U_Args : Unbounded_String := To_Unbounded_String (Args);
      Strs   : array (Integer range 1 .. N_Args) of Unbounded_String;
      Pos    : Integer := 1;
   begin
      Put_Line(Args);
      U_Args := Unbounded_Slice(U_Args,1,Args'Length);
      for I in Integer range 1 .. N_Args
      loop
         if Ada.Strings.Unbounded.Count (U_Args, " ") > 0 then
            Strs (I) := Ada.Strings.Unbounded.Unbounded_Slice (U_Args, Pos, Ada.Strings.Unbounded.Index (U_Args, " ")-1);
         else
            Strs (I) := Ada.Strings.Unbounded.Unbounded_Slice (U_Args, Pos, Length (U_Args));
         end if;
         Pos := Ada.Strings.Unbounded.Index (U_Args, " ") + 1;
         U_Args := To_Unbounded_String (Slice (U_Args, Pos, Length (U_Args)));
         Pos := 1;
      end loop;

      for I in Strs'Range
      loop
         Put_Line (To_String(Strs (I)));
         Ret(I) := new String'(To_String(Strs (I)));
      end loop;
      return Ret;
   end Get_Arg_List;

end Toolkit;
