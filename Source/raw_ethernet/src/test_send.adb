
--  with Raw_Socket; use Raw_Socket;
with Generic_Raw_Socket;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Send is
   subtype Str46 is String (1 .. 1500);
   procedure Put (S : Str46) is
   begin
      null;
   end Put;
   package Str_Sock is new Generic_Raw_Socket (Str46, Put);
   use Str_Sock;

   Sock : Socket_Type;
   D    : Str46 := (others => ' ');
   Res : Integer;
begin
   D(1..50) := "01234567899876543210012345678998765432105263411234";
   Sock := Create_Socket ("eth0");

   Put_Line ("Sending: " & String(D(1..50)) & "....");
   delay 0.5;
   for I in 1 .. 300_000 loop
   --        Transmit (Sock, Dst => Get_Addr(Sock), Data => D, Length => 4);
      loop
         Transmit (Sock,
                   Dst  => ( 16#0#, 16#19#, 16#B9#, 16#79#, 16#AA#, 16#8F#),
                   Data => D,
                   Res  => Res);
         exit when Res > 0;
         Ada.Text_IO.Put('.');
      end loop;
      delay 0.000_06;
   end loop;
   Put_Line ("Done");
end Test_Send;
