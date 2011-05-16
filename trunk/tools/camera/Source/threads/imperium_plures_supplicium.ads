--
--
--
with Ada.Containers.Vectors;
with Raw_Frame_Toolkit; use Raw_Frame_Toolkit;

package Imperium_Plures_Supplicium is
   -- Semaphore
   protected type Semaphore is
      procedure Unlock;
      function Is_Locked return Boolean;
      entry Lock;
   private
      Unlocked : Boolean := True;
   end Semaphore;

   protected type Buffer is
      procedure Get (Item : out Raw_Ethernet_Frame);
      procedure Set (Item : in Raw_Ethernet_Frame);
      function Is_Empty return Boolean;
   private
      Buffer : Raw_Ethernet_Frame;
      Empty  : Boolean := True;
   end Buffer;


   type Buffer_Index is new Integer range 0 .. 32**2;
   subtype Max_Raw_Frame_Array is Raw_Ethernet_Frame_Array(0 .. 65535);
   package Raw_Vector is
     new Ada.Containers.Vectors (Buffer_Index, Max_Raw_Frame_Array);
   use Raw_Vector;

   type Buffer_Type is (Config, Control, Data);
   type Buffer_Length_Array is array (Buffer_Type) of Integer;

   protected type Package_Buffer is
--        procedure Create_Package (Position : out Integer);
--        procedure Add_Frame (Item     : in Raw_Ethernet_Frame;
--                             Position : in Integer;
--                             Done     : Boolean := False);
--        procedure Remove_Package (Frame_Package : out Raw_Ethernet_Frame_Array);
      procedure Have_Full_Package (Buffer    : in Buffer_Type;
                                   Position  : out Integer;
                                   Have_Full : out boolean);
--        procedure Have_Full_Package (Buffer    : in Buffer_Type;
--                                     Have_Full : Boolean);
--        procedure Have_Full_Package (Have_Full : Boolean);
   private
      Control_Buffer : Max_Raw_Frame_Array;
      Config_Buffer  : Max_Raw_Frame_Array;
      Data_Buffer    : Raw_Vector.Vector;

      Buffer_Lengths : Buffer_Length_Array := (others => -1);

      -- internal functions/procedures

   end Package_Buffer;

end Imperium_Plures_Supplicium;
