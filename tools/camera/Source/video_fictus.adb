with Ada.Text_Io; use Ada.Text_Io;
with Ada.Numerics.Discrete_Random;
with Imperium_Plures_Supplicium;
use Imperium_Plures_Supplicium;
--
--
--
--
procedure Video_Fictus is
   protected type Single_Frame_Buffer is
      procedure Get (Item : out Integer);
      procedure Set ( Item : Integer);
      function Empty return Boolean;
   private
      Internal_Buffer    : Integer := 0;
      Is_Empty           : Boolean := True;
   end Single_Frame_Buffer;

   protected body Single_Frame_Buffer is
      procedure Get (Item : out Integer) is
      begin
         Is_Empty := True;
         Item := Internal_Buffer;
      end Get;

      procedure Set (Item : Integer) is
      begin
         Internal_Buffer := Item;
         Is_Empty := False;
      end Set;

      function Empty return Boolean is
      begin
         return Is_Empty;
      end Empty;
   end Single_Frame_Buffer;


--     package Integer_Buffer is new Imperium_Plures_Supplicium (Integer);
--     use Integer_Buffer;
   Buffer : Semaphore;
   Loop_Exit : Boolean := False;

   task Receiver;
   task body Receiver is
      type Rand_Int is new Integer range 0 .. 42;
      package Random_Integer is new Ada.Numerics.Discrete_Random (Rand_Int);
      Seed : Random_Integer.Generator;
      Received : Integer;
      Done : Boolean := False;
   begin
      Random_Integer.Reset (Seed);
      loop
         Done := False;
         Received := Integer (Random_Integer.Random (Seed));
         if (Received mod 10) = 0 then
            Done := True;
         end if;
         exit when Received = 42;
         delay 0.2;
      end loop;

   end Receiver;

   Data : Integer := 0;
begin
   loop
      exit when Loop_Exit;
      Put_Line (Data'Img);
   end loop;

end Video_Fictus;
