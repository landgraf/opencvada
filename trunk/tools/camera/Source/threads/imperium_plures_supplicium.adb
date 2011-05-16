--
--
--
package body Imperium_Plures_Supplicium is
   -- Semaphore
   protected body Semaphore is
      procedure Unlock is
      begin
         Unlocked := True;
      end Unlock;

      entry Lock
        when Unlocked is
      begin
         Unlocked := False;
      end Lock;

      function Is_Locked return Boolean is
      begin
         return not Unlocked;
      end Is_Locked;
   end Semaphore;

   --
   protected body Buffer is
      procedure Get (Item : out Raw_Ethernet_Frame) is
      begin
         Item := Buffer;
         Empty := True;
      end Get;

      procedure Set (Item : in Raw_Ethernet_Frame) is
      begin
         Buffer := Item;
         Empty := False;
      end Set;
      function Is_Empty return Boolean is
      begin
         return Empty;
      end Is_Empty;
   end Buffer;


   protected body Package_Buffer is
      procedure Have_Full_Package (Buffer    : in Buffer_Type;
                                   Position  : out Integer;
                                   Have_Full : out boolean) is
      begin
         if (Buffer_Lengths (Buffer) > -1) then
            case Buffer is
               when Config =>
                  --                    Config_Buffer(Buffer_Lengths (Buffer));
                  null;
               when Control =>
                  null;
               when Data =>
                  null;
            end case;
         else
            Position := -1;
            Have_Full := False;
         end if;
      end Have_Full_Package;
--        procedure Have_Full_Package (Buffer    : in Buffer_Type;
--                                     Have_Full : Boolean);
--        procedure Have_Full_Package (Have_Full : Boolean);
   end Package_Buffer;
end Imperium_Plures_Supplicium;
