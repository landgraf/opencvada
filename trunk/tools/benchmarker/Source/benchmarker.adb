--

with Ada.Real_Time; use Ada.Real_Time;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;

-- mine
with Debug; use Debug;
with Toolkit; use Toolkit;
-- flera gånger ,
-- total tid,
-- min tid
-- max tid
-- avg tid
--

-- exkeverings metoder:
-- 1: app ("args") (#) (output.csv)
-- 2: input.list (output.csv)
-- list file format:
-- app "(args)" #
procedure Benchmarker is
   Debug        : constant Boolean := True;
   App          : Unbounded_String := To_Unbounded_String ("");
   App_Args     : Unbounded_String := To_Unbounded_String ("");
   Output_File  : Unbounded_String := To_Unbounded_String ("output.csv");
   O_File       : File_Type;
   L_File       : File_Type;
   Execs        : Integer := 1;
   Ret          : Integer;
   Line         : String (1 .. 100);
   Line_Last    : Integer := 0;
   Out_Time     : access Time_Record := new Time_Record'(Execs => 0, Start_Time => Clock, End_Time => Clock, Min => Time_Span_Last, others => Time_Span_Zero);
begin
   if Argument_Count /= 0 then
      if Ada.Strings.Fixed.Count (Argument (1), ".list") > 0 then
         -- we have a .list file
         Put_Debug ("list: Read from file", Debug);
         Open (L_File, In_File, Argument (1));
         -- find the output file name...
         if Argument_Count > 1 then
            for I in Integer range 2 .. Argument_Count
            loop
               if Ada.Strings.Fixed.Count (Argument (I), ".csv") > 0 then
                  Output_File := To_Unbounded_String (Argument (I));
               else
                  Put_Debug ("list: No output file, more then 1 Argument", Debug);
               end if;
            end loop;
         else
            -- no specified out put file
            Put_Debug ("list: No output file", Debug);
         end if;
         if not GNAT.OS_Lib.Is_Writable_File (To_String (Output_File)) then
            Create (O_File, Out_File, To_String (Output_File));
         else
            Ada.Text_IO.Open (O_File, Append_File, To_String (Output_File));
         end if;
         loop
            exit when Ada.Text_IO.End_Of_File (L_File);
            Ada.Text_IO.Get_Line (L_File, Line, Line_Last);
            Put_Debug (Line (1 .. Line_Last) & "," & Line_Last'Img, Debug);
            App_Parse :
            declare
               Next_Space : Integer := Index (To_Unbounded_String (Line), " ");
               Next_Comment : Integer := Index (To_Unbounded_String (Line), "" & '"');
               Second_Comment : Integer := Next_Comment + Index (To_Unbounded_String (Line (Next_Comment+1 .. Line_Last)), "" & '"');
            begin
               Put (Next_Comment'Img);
               Put (Second_Comment'Img);
               Put (Line (Next_Comment+1 .. Line_Last));
               App := Unbounded_Slice (To_Unbounded_String (Line), 1, Next_Space);
               App_Args := Unbounded_Slice (To_Unbounded_String (Line),
                                                       Next_Comment,
                                            Second_Comment);
               Put_Line(Line (Second_Comment + 1 .. Line_Last));
               if Index (To_Unbounded_String (Line (Second_Comment + 1 .. Line_Last)), " ") > 0 then
                  Execs := Integer'Value((Line (Second_Comment + 1 .. Line_Last)));
               end if;
               Put_Debug (To_String (App), Debug);
               Put_Debug (To_String (App_Args), Debug);
               Ret := Run_Apps (To_String (App), Get_Arg_List (To_String (App_Args)), Out_Time, Execs, Debug);
               Put_Line (O_File, To_String (App) & "," & To_String (App_Args) & "," & To_String (Out_Time.all));
            end App_Parse;
            Execs := 1;
            Out_Time := new Time_Record'(Execs => 0, Start_Time => Clock, End_Time => Clock, Min => Time_Span_Last, others => Time_Span_Zero);
         end loop;
      else
         -- 1 application execution
         Put_Debug ("app: exec one application", Debug);
         if Argument_Count >= 1 then
            App := App & Argument (1);
         end if;
         if Argument_Count >= 2 then
            App_Args := App_Args & Argument (2);
         end if;
         if Argument_Count >= 3 then
            Execs := Integer'Value (Argument (3));
         end if;
         if Argument_Count >= 4 then
            if Ada.Strings.Fixed.Count (Argument (4), ".csv") > 0 then
               Output_File := To_Unbounded_String (Argument (4));
            end if;
         end if;

         if not GNAT.OS_Lib.Is_Writable_File (To_String (Output_File)) then
            Create (O_File, Out_File, To_String (Output_File));
         else
            Ada.Text_IO.Open (O_File, Append_File, To_String (Output_File));
         end if;

         Ret := Run_Apps (To_String (App), Get_Arg_List (To_String (App_Args)), Out_Time, Execs, Debug);
         Put_Line (O_File, To_String (App) & "," & To_String (App_Args) & "," & To_String (Out_Time.all));
      end if;
      -- debug, print all arguments
      if Debug then
         for I in Integer range 1 .. Argument_Count
         loop
            Put_Line ("Arg " & I'Img & ":" & Argument (I));
         end loop;
      end if;


   end if;
end Benchmarker;

