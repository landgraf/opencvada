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
   --

      ------------------------------------------------------------------------
      procedure Have_Full_Package (Buffer           : in Buffer_Type;
                                   Vector_Position  : out Integer;
                                   Have_Full        : out Boolean) is
      begin
         if (Buffer_Lengths (Buffer) > -1) and
           (Number_Of_Full_Packages > 0) and
           (Full_Package_Exists = True) then
            case Buffer is
               when Config | Control | Memory =>
                  if (C_C_M_Buffer (Buffer) (Buffer_Lengths (Buffer)).Constant_Head.Eof) = True then
                     Have_Full := True;
                     Vector_Position := 0;
                  else
                     Have_Full := False;
                     Vector_Position := 0;
                  end if;
                  return;
               when Data =>
                  Have_Full := False;
                  Vector_Position := 0;
                  -- match first possible full package
                  -- should be comparable to first in first out (maybe)
                  for I in Buffer_Index range Parsed_Vector.First_Index (Data_Buffer) .. Parsed_Vector.Last_Index (Data_Buffer) loop
                     declare
                        Temp : Vector_Parsed_Frame := Parsed_Vector.Element (Data_Buffer, I);
                     begin
                        if Temp.Buffer (Temp.Length).Constant_Head.Eof = True then
                           Have_Full := True;
                           Vector_Position := Integer (I);
                           return;
                        end if;
                     end;
                  end loop;
                  return;
            end case;
         else
            Vector_Position := -1;
            Have_Full := False;
         end if;
      end Have_Full_Package;
      --------------------------------------------------------------------------
      procedure Have_Full_Package (Buffer    : in Buffer_Type;
                                   Have_Full : out Boolean) is
         Throw_Away : Integer;
      begin
         Have_Full_Package (Buffer, Throw_Away, Have_Full);
      end Have_Full_Package;
      --------------------------------------------------------------------------
      procedure Have_Full_Package (Have_Full      : out Boolean;
                                   Number_Of_Full : out Integer) is
      begin
         Have_Full := Full_Package_Exists;
         Number_Of_Full := Number_Of_Full_Packages;
      end Have_Full_Package;
      --------------------------------------------------------------------------
      function Have_Full_Package return Boolean is
      begin
         return Full_Package_Exists;
      end Have_Full_Package;

      --------------------------------------------------------------------------
      -- Frame Control things
      --------------------------------------------------------------------------
      -- updates amount of full packes
      procedure Update_Full_Package (Eof    : in Boolean;
                                     Remove : in Boolean) is
      begin
         case Remove is
            when False =>
               if Eof = True then
                  -- this package is full used for quick access to finished packages
                  Full_Package_Exists := True;
                  Number_Of_Full_Packages := Number_Of_Full_Packages + 1;
               end if;
            when True =>
               -- Remove One From Full Packages and if No More Set variable
               Number_Of_Full_Packages := Number_Of_Full_Packages - 1;
               if Number_Of_Full_Packages < 1 then
                  Full_Package_Exists := False;
               end if;
               if Number_Of_Full_Packages < 0 then
                  -- should never happen used for debuging
                  Number_Of_Full_Packages := 0;
                  raise Package_Buffer_Logic;
               end if;
         end case;
      end Update_Full_Package;

      procedure Package_Exists (Frame           : in Parsed_Raw_Frame;
                                Vector_Position : out Integer;
                                Exists          : out Boolean) is
         Vec_Pos : Integer := -1;
      begin
         if not (Parsed_Vector.Is_Empty (Data_Buffer)) then
            -- empty
            null;
         else
            for I in Buffer_Index range Parsed_Vector.First_Index (Data_Buffer) .. Parsed_Vector.Last_Index (Data_Buffer) loop
               -- look for the package
               declare
                  Temp : Vector_Parsed_Frame := Parsed_Vector.Element (Data_Buffer, I);
               begin
                  if Temp.Length >= 0 then
                     if Temp.Buffer (0).Constant_Head.Package_Seq = Frame.Constant_Head.Package_Seq then
                        --                          We Have A Match
                        Vector_Position := Integer (I);
                        Exists := True;
                        return;
                     end if;
                  end if;
               end;
               null;
            end loop;
         end if;
         --No Match new package Should Be Added
         Vector_Position := -1;
         Exists :=  False;
         return;
      end Package_Exists;

      -- Adds frame to last spot in specified buffer.
      procedure Add_Frame (Buffer          : in Buffer_Type;
                           Frame           : in Parsed_Raw_Frame) is
         Length : Integer := Buffer_Lengths (Buffer) + 1;
      begin
         case Buffer is
         --type Buffer_Type is (Config, Control, Data, Memory);
            when Config | Control | Memory =>
               if Length > C_C_M_Buffer (Buffer)'Last then
                  -- Should not happen
                  -- this is a major fault in code
                  -- and or several devices using same address
                  raise Package_Buffer_Overflow; -- eh what
               elsif Buffer_Lengths (Buffer) < 0 then
                  -- not a package
                  -- first frame in a new package
                  C_C_M_Buffer (Buffer) (0) := Frame;
                  Update_Full_Package (Eof => Frame.Constant_Head.Eof, Remove => False);
                  Buffer_Lengths (Buffer) := 0; -- first frame
               elsif Frame.Constant_Head.Package_Seq /= C_C_M_Buffer (Buffer) (Length - 1).Constant_Head.Package_Seq then
                  -- wrong package
                  -- we got a new package HOLD!
                  raise Package_Buffer_Cant_Add;
               elsif not (Integer (Frame.Constant_Head.Seq_No) > Length) then
                  -- package problem
                  raise Package_Buffer_Logic;
               else
                  C_C_M_Buffer (Buffer) (Length) := Frame;
                  Update_Full_Package (Eof => Frame.Constant_Head.Eof, Remove => False);
                  Buffer_Lengths (Buffer) := Length; -- update to point at this frame
               end if;
            when Data =>
               declare
                  Package_Pre_Existing : Boolean := False;
                  Position             : Integer := -1;
               begin
                  Package_Exists (Frame, Position, Package_Pre_Existing);
                  if Package_Pre_Existing then
                     -- put it into the vector
                     if not (Parsed_Vector.Element (Data_Buffer, Buffer_Index (Position)).Length < Integer (Frame.Constant_Head.Seq_No)) then
                        -- old stuff
                        raise Package_Buffer_Logic;
                     else
                        -- Should Add To Vector Now
                        declare
                           Temp : Vector_Parsed_Frame := Parsed_Vector.Element (Data_Buffer, Buffer_Index (Position));
                        begin
                           Temp.Length := Temp.Length + 1;
                           Temp.Buffer (Temp.Length) := Frame;
                           Parsed_Vector.Replace_Element (Data_Buffer, Buffer_Index (Position), Temp);
                           Update_Full_Package (Frame.Constant_Head.Eof, False);
                        end;
                     end if;
                  else
                     -- create a new vector post
                     declare
                        New_Element : Vector_Parsed_Frame;
                     begin
                        New_Element.Buffer (0) := Frame;
                        New_Element.Length := 0;
                        Parsed_Vector.Append (Container => Data_Buffer,
                                              New_Item  => New_Element);
                        Update_Full_Package (Frame.Constant_Head.Eof, False);
                     end;
                  end if;
               end;
         end case;
      end Add_Frame;

      -- Figures out by itself where to add the frame.
      -- Not as smart as it sounds :(
      procedure Smart_Add_Frame (Frame    : in Parsed_Raw_Frame;
                                 Is_Added : out Boolean) is
      begin
         Is_Added := True;
         Add_Frame (Spec_Frame_List_To_Buffer_Type (Frame.Type_Of_Frame), Frame);
      exception
         when Not_A_Buffer_Type | Package_Buffer_Cant_Add | Package_Buffer_Overflow | Package_Buffer_Logic =>
            Is_Added := False;
      end Smart_Add_Frame;

      -- returns if full...
      -- returns first available full data buffer...
      -- removes in buffer_type order...
      procedure Smart_Remove_Frames (Frames : out Parsed_Frame_Array) is
         Vector_Position : Integer;
         Is_Full         : Boolean;
      begin
         for I in Buffer_Type loop
            Have_Full_Package (I, Vector_Position, Is_Full);
            if (Is_Full and I = Data) then
               --                 Remove_From_Vector
               Remove_Frames (I, Vector_Position, Frames);
               return;
            elsif (Is_Full) then
               Remove_Frames (I, Frames);
               return;
            else
               raise Nothing_To_See_Here_Move_Along; -- rename me
            end if;
         end loop;
      exception
            -- no full things..
         when Nothing_To_See_Here_Move_Along =>
            declare
               Empty : Parsed_Frame_Array (1 .. 0);
            begin
               Frames := Empty;
               return;
            end;
      end Smart_Remove_Frames;

      -- removes a frame from the specific buffer
      procedure Remove_Frames (Buffer : in Buffer_Type;
                               Frames : out Parsed_Frame_Array) is
         Length : Integer := Buffer_Lengths (Buffer);
         Is_Full : Boolean;
      begin
         Have_Full_Package (Buffer, Is_Full);
         if (Length = -1 or (not Is_Full)) and not (Buffer = Data) then
            raise Nothing_To_See_Here_Move_Along;
         else
         case Buffer is
            when Config | Control | Memory =>
               declare
                  Temp : Parsed_Frame_Array (0 .. Length);
               begin
                  Temp := Parsed_Frame_Array (C_C_M_Buffer (Buffer) (0 .. Length));
                  Update_Full_Package (True, True);
                  Buffer_Lengths (Buffer) := -1;
                  return;
               end;
            when Data =>
               declare
                  Vec_Position : Integer;
               begin
                  Have_Full_Package (Buffer, Vec_Position, Is_Full);
                  if not Is_Full then
                     raise Nothing_To_See_Here_Move_Along;
                  else
                     Remove_Frames (Buffer, Vec_Position, Frames);
                     return;
                  end if;
               end;
            end case;
         end if;
      end Remove_Frames;

      procedure Remove_Frames (Buffer          : in Buffer_Type;
                               Vector_Position : in Integer;
                               Frames          : out Parsed_Frame_Array;
                               Unsafe          : in Boolean := False) is
         Temp_Pos : Integer;
         Is_Full  : Boolean;
         Temp     : Vector_Parsed_Frame;
      begin
         case Buffer is
            when Config | Control | Memory =>
               Remove_Frames (Buffer, Frames);
               return;
            when Data =>
               Have_Full_Package (Buffer, Temp_Pos, Is_Full);
               if (Is_Full and Vector_Position = Temp_Pos) or Unsafe then
                  -- remove_frame
                  Temp := Parsed_Vector.Element (Data_Buffer, Vector_Position);
                  declare
                     Out_Frame : Parsed_Frame_Array (0 .. Temp.Length);
                  begin
                     null;
                  end;
                  Update_Full_Package (True, True);
               else
                  raise Nothing_To_See_Here_Move_Along;
               end if;
         end case;
      end Remove_Frames;

      -- Adds a frame to a very specific position /needs two maybe/
      -- Should not look at any thing just add the frame to the specified position.
      -- Used for testing...
      procedure Stupid_Add_Frame (Buffer          : in Buffer_Type;
                                  Frame           : in Parsed_Raw_Frame;
                                  Position        : in Integer;
                                  Vector_Position : in Integer) is
      begin
         case Buffer is
            when Config | Control | Memory =>
               C_C_M_Buffer (Buffer) (Position) := Frame;
               Update_Full_Package (Frame.Constant_Head.Eof, False);
               Buffer_Lengths (Buffer) := Position; -- update to point at this frame
            when Data =>
               declare
                  Temp : Vector_Parsed_Frame := Parsed_Vector.Element (Data_Buffer, Buffer_Index (Vector_Position));
               begin
                  Temp.Length := Position;
                  Temp.Buffer (Temp.Length) := Frame;
                  Parsed_Vector.Replace_Element (Data_Buffer, Buffer_Index (Vector_Position), Temp);
                  Update_Full_Package (Frame.Constant_Head.Eof, False);
               end;
         end case;
      end Stupid_Add_Frame;
   end Package_Buffer;

   -- Converts Spec_Frames_List to Buffer_Type
   function Spec_Frame_List_To_Buffer_Type (Src : Spec_Frames_List) return Buffer_Type is
   begin
      -- (Array_Frame, Config_Frame, Image_Frame, Matrix_Frame, Memory_Frame, Control_Frame, Other, Not_A_Frame);
      case Src is
         when Array_Frame | Image_Frame | Matrix_Frame =>
            return Data;
         when Config_Frame =>
            return Config;
         when Control_Frame =>
            return Control;
         when Memory_Frame =>
            return Memory;
         when others =>
            raise Not_A_Buffer_Type;
      end case;
   end Spec_Frame_List_To_Buffer_Type;
end Imperium_Plures_Supplicium;
