
with Ada.Exceptions; use Ada.Exceptions;
with Generic_Raw_Socket;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Sequential_IO;
procedure Test is
   subtype Str46 is String (1 .. 1500);

   procedure Put (S : Str46) is
   begin
      Put_Line (String (S));
   end Put;

   package Rs is new Generic_Raw_Socket (Str46, Put);
   package Dio is new Ada.Sequential_IO(Str46);
   Data : Str46;
   S    : Rs.Socket_Type;
   Len  : Integer;
   Count : Natural := 0;
   Start, Curr : Ada.Real_Time.Time;
   Diff        : Ada.Real_Time.Time_Span;
   Seconds     : Float;
   Src : Rs.Addr_Type := Rs.Null_Addr;
   Fil : Dio.File_Type;
   Filnamn : String := "data_file";
   Val : Natural := 1;
begin

   S := Rs.Create_Socket ("eth0");
   Start := Clock;
   Rs.Start_Tasked_Reading (S, Rs.DEFAULT_DATA_PKG);
--     DIO.Create (Fil, DIO.Out_File, "data_file0.bin");
   loop
      Rs.Get (Src, Data, Len);
--        Dio.Write (Fil,Data);
      if Len > 0 then
--           Put_Line ("Len=" & Len'Img & " "  & Data);
         Count := Count + 1;
         if Count mod 1_000_000 = 0 then
--              Dio.Close (Fil);
--              declare
--                 Str : String := Val'Img;
--              begin
--                 Dio.Create (Fil, DIO.Out_File, Filnamn & Str (Str'First + 1 .. Str'Last) & ".bin");
--              end;
            Val := Val + 1;
            Curr := Clock;
            Diff := Curr - Start;
            Start := Curr;
            Seconds := Float (To_Duration (Diff));
            Ada.Text_IO.Put ("Pkgs/sec: ");
            Put (100_000.0 / Seconds, 4, 2, 0);
            begin
               Ada.Text_IO.Put (String (" Fail: " & Rs.Fail'Img));
            exception
               when E : others =>
                  Put_Line (Exception_Information (E));
            end;

            New_Line;
            Rs.Fail := 0;
         end if;
         if Count = 300_000 then
            Put_Line ("Received all packages");
            exit;
         end if;
         --           Put_Line ("------------------------------------------");
      end if;

--        delay 0.1;
   end loop;
end Test;
