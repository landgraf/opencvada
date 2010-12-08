---

with Parser; use Parser;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Command_Line;
procedure Main is
   Position : Smaller_Integer := 1;
   Str      : String (1 .. 10_000);
   Str_2    : aliased Unbounded_String := To_Unbounded_String("");


   Decl     : Decleration;
   Last     : Integer;

   Superfilen, Cppfil, Hppfil : File_Type;
begin

   if Ada.Command_Line.Argument_Count <= 0 then
      return;
   else
      Open (Superfilen, In_File,  Ada.Command_Line.Argument (1)); --"highgui.hpp_itp.txt";
      if Ada.Command_Line.Argument_Count = 3 then
         Open (Cppfil, Out_File, Ada.Command_Line.Argument (2));
         Open (Hppfil, Out_File, Ada.Command_Line.Argument (3));
      else
         Create (Cppfil, Out_File, Ada.Command_Line.Argument (1) & ".cpp");
         Create (Hppfil, Out_File, Ada.Command_Line.Argument (1) & ".hpp");
      end if;
   end if;

   while not End_Of_File (Superfilen) loop
      Get_Line (Superfilen, Str, Last);
      if Last = 0 then
         null;
      else
         Str_2 := To_Unbounded_String (Str (1 .. Last));

         loop
            exit when Ada.Strings.Unbounded.Count (Str_2, ";") /= 0;
            Get_Line (Superfilen, Str, Last);
            if not (Last = 0) then
               Str_2 := Str_2 & To_Unbounded_String (Str (1 .. Last));
            end if;
         end loop;

         Decl := CreateDecleration (To_String (Str_2));
         Put_Line (HPpfil, To_String (PutDecleration (Decl,False)));
         Put_Line (Cppfil, To_String (PutDecleration (Decl,True)));
         --Put_Line ("{");
         Put_Line (Cppfil, To_String (CreateFunctionCall (Decl)));
         Put_Line(Cppfil, "}");
      end if;
     Str_2 := To_Unbounded_String("");
   end loop;




end Main;
