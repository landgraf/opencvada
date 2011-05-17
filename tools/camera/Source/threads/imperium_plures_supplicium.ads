--
--
--
with Ada.Containers.Vectors;
with Raw_Frame_Toolkit; use Raw_Frame_Toolkit;
with Ada.Exceptions;
with Interfaces; use Interfaces;
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

   -----------------------------------------------------------------------------
   --
   -----------------------------------------------------------------------------

   type Buffer_Index is new Natural;
   type Max_Parsed_Frame_Array is array (Integer range 0 .. 65535) of Parsed_Raw_Frame;
   type Vecotor_Parsed_Frame is
      record
         Buffer : Max_Parsed_Frame_Array;
         Length : Integer := -1;
      end record;
   package Parsed_Vector is
     new Ada.Containers.Vectors (Buffer_Index, Vecotor_Parsed_Frame);
   use Parsed_Vector;

   type Buffer_Type is (Config, Control, Data, Memory);
   type Buffer_Length_Array is array (Buffer_Type) of Integer;
   type Buffer_Array is array (Buffer_Type) of Max_Parsed_Frame_Array;


   -----------------------------------------------------------------------------
   -- Buffer exceptions
   -----------------------------------------------------------------------------

   Package_Buffer_Overflow : exception;
   Package_Buffer_Logic : exception;
   Package_Buffer_Cant_Add : exception;

   protected type Package_Buffer is
--        procedure Create_Package (Position : out Integer);
--        procedure Add_Frame (Item     : in Raw_Ethernet_Frame;
--                             Position : in Integer;
--                             Done     : Boolean := False);
   --        procedure Remove_Package (Frame_Package : out Raw_Ethernet_Frame_Array);
      procedure Have_Full_Package (Buffer           : in Buffer_Type;
                                   Vector_Position  : out Integer;
                                   Have_Full        : out Boolean);
      procedure Have_Full_Package (Buffer    : in Buffer_Type;
                                   Have_Full : out Boolean);
      -- Maybe not
      procedure Have_Full_Package (Have_Full      : out Boolean;
                                   Number_Of_Full : out Integer);
      function Have_Full_Package return Boolean;

      --------------------------------------------------------------------------
      -- Frame control
      --------------------------------------------------------------------------
      -- Adds frame to last spot in specified buffer.
      procedure Add_Frame (Buffer          : in Buffer_Type;
                           Frame           : in Parsed_Raw_Frame);
      -- Figures out by itself where to add the frame.
      procedure Smart_Add_Frame (Frame : in Parsed_Raw_Frame);
      -- Adds a frame to a very specific position /needs two maybe/
      -- Should not look at any thing just add the frame to the specified position.
      -- Used for testing...
      procedure Stupid_Add_Frame (Buffer          : in Buffer_Type;
                                  Frame           : in Parsed_Raw_Frame;
                                  Position        : in Integer;
                                  Vector_Position : in Integer);
      -- Adds a series of frames.
--        procedure Batch_Add_Frame;
   private
      --------------------------------------------------------------------------
      -- Internal function / procedures
      --------------------------------------------------------------------------
      procedure Update_Full_Package (Eof    : in Boolean;
                                     Remove : in Boolean);
      --------------------------------------------------------------------------
      -- Internal data structure
      --------------------------------------------------------------------------
      -- Stores Data messages.
      Data_Buffer    : Parsed_Vector.Vector;
      -- Stores control,config and memory arrays, and an unsed data array
      C_C_M_Buffer : Buffer_Array;
      -- Lengths of buffers
      Buffer_Lengths : Buffer_Length_Array := (others => -1);
      -- Tracks finished packages
      Full_Package_Exists : Boolean := False;
      Number_Of_Full_Packages : Integer := 0;
   end Package_Buffer;

end Imperium_Plures_Supplicium;
