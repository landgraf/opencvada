---
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
package body Parser is
   procedure Nulled is begin null; end Nulled;

   -- Find a word that is also a C token.
   function GetWord (Src   : String;
                     Start : Integer) return Word is
      End_Of_Word : Integer := Start;
      Current     : Integer := Start;
      Done        : Boolean := False;
      Ret         : Word;
      Next        : Integer := 0;
      Skip        : Boolean := False;
      New_Start   : Integer := Start;
   begin
      loop
         GetCharacter (Src, Current, Skip, Done);
         if Skip and Current = New_Start then
            -- we start at a space skip this
            New_Start := New_Start + 1;
            End_Of_Word := Start + 1;
            Current := Current + 1;
         else
            if Done then
               if Skip then
                  -- found a space or something equal evil skip it
                  Next := Current + 1;
                  Current := Current - 1;
               else
                  -- found a delimiter
                  Next :=  Current;
                  if not (Current = New_Start ) then
                     Current := Current - 1;
                  end if;
               end if;
               exit;
            else
               -- what's next?
               Current := Current + 1;
            end if;
         end if;
      end loop;
      End_Of_Word := Current;
      Ret := (Ada.Strings.Unbounded.To_Unbounded_String (Src (New_Start .. End_Of_Word)),
              New_Start,
              End_Of_Word,
              Next);
      if End_Of_Word = Next then
         Ret.Next := Next + 1;
      end if;
      return Ret;
   end GetWord;

   -- used with GetWord
   -- removes whitespaces and looks for delmiters
   procedure GetCharacter (Str         : String;
                           Pos         : Integer;
                           Skip        : out Boolean;
                           End_Of_Word : out Boolean) is
   begin
      Skip := False;
      if Pos <= Str'Length then
         case Str (Pos) is
         when '(' | ')' | '[' | ']' | '{' | '}' | ';' | ',' | '<' | '>' | ':' | '&' | '*' | '=' | '|' =>
            End_Of_Word := True;
         when Ascii.Ht | ' ' => --more?
            End_Of_Word := True;
            Skip := True;
         when others =>
            End_Of_Word := False;
         end case;
      else
         End_Of_Word := True;
      end if;

   end GetCharacter;

   -- Compare function for single character tokens.
   function CompareOneToken (Src : Character) return Token_Type is
   begin -- { curly [ brace ( parentheses < chevrons
      case Src is
         when '{' =>
            return Left_Curly;
         when '}' =>
            return Right_Curly;
         when '[' =>
            return Left_Brace;
         when ']' =>
            return Right_Brace;
         when '(' =>
            return Left_Parentheses;
         when ')' =>
            return Right_Parentheses;
         when '<' =>
            return Left_Chevron;
         when '>' =>
            return Right_Chevron;
         when ';' =>
            return Semicolon;
         when ':' =>
            return Colon;
         when ',' =>
            return Comma;
         when '&' =>
            return And_T;
         when '*' =>
            return Star;
         when '=' =>
            return Equal;
         when '|' =>
            return Pipe;
         when others =>
            return Name;
      end case;
   end CompareOneToken;


   -- Compare function for string type words.
   function CompareToken (Str : String) return Token_Type is
   begin
      if Str'Length = 1 then
         return CompareOneToken (Str (Str'First));
      end if;
      case Str (Str'First) is
         when 'c' =>
            if CompareString (Str, "char") then
               return Char;
            elsif CompareString (Str, "const") then
               return Const;
            else
               return Name;
            end if;
         when 'i' =>
            if CompareString (Str, "int") then
               return Int;
            else
               return Name;
            end if;
         when 'f' =>
            if CompareString (Str, "float") then
               return Float;
            else
               return Name;
            end if;
         when 'd' =>
            if CompareString (Str, "double") then
               return Double;
            else
               return Name;
            end if;
         when 'v' =>
            if CompareString (Str, "void") then
               return Void; -- vector
            elsif CompareString (Str, "vector") then
               return Vector;
            elsif CompareString (Str, "virtual") then
               return Virtual;
            else
               return Name;
            end if;
         when 'b' =>
            if CompareString (Str, "bool") then
               return Bool;
            else
               return Name;
            end if;
         when 'w' =>
            if CompareString (Str, "wchar_t") then
               return Wchar_T;
            else
               return Name;
            end if;
         when 's' =>
            if CompareString (Str, "string") then
               return String_T;
            elsif CompareString (Str, "short") then
               return Short;
            elsif CompareString (Str, "signed") then
               return Signed;
            else
               return Name;
            end if;
         when 'l' =>
            if CompareString (Str, "long") then
               return Long;
            else
               return Name;
            end if;
         when 'u' =>
            if CompareString (Str, "unsigned") then
               return Unsigned;
            else
               return Name;
            end if;
         when '_' =>
            if CompareString (Str, "_this") then
               return This;
            else
               return Name;
            end if;
         when others =>
            return Name;
      end case;
   end CompareToken;

   -- create a token from a word
   function GetToken (Src : Word) return Token is
      Ret : Token;
   begin
      Ret.T := CompareToken (To_String (Src.Str));
      Ret.Name := Src;
      --        Put_Line(Ada.Strings.Unbounded.To_String(Ret.Name.Str));
      return Ret;
   end GetToken;

   -- Compares two strings.
   function CompareString (Src : String; Token : String) return Boolean is
   begin
      if not (Src'Length = Token'Length) then
         return False;
      elsif (Src = Token) then
         return True;
      else
         return False;
      end if;
   end CompareString;

   -- Creates a new token for inserting or changeing tokens.
   function CreateToken (T : Token_Type) return Token is
   begin
      return ((Ada.Strings.Unbounded.To_Unbounded_String (" "), 0, 0, -1),
              T,
              True,
              Ada.Strings.Unbounded.To_Unbounded_String (" "));
   end CreateToken;

   -- Create a data structure for a decleration.
   function CreateDecleration (Str : String) return Decleration is
      Position   : Integer := Str'First;
      Temp_Word  : Word;
      Temp_Token : Token;

      Int_Paren  : Integer := 0;
      Params     : Boolean := False;
      Stop       : Boolean := False;
      Add_Star   : Boolean := False;
      Remove_And : Boolean := False;

      Decl       : Decleration;
      Start_Vector : Token_Vector := To_Vector (0);
      Param_Vector : Token_Vector := To_Vector (0);

      Skip       : Boolean := False;
   begin
      loop
         -- loop until end of string
         exit when Position > Str'Length;
         Temp_Word := GetWord (Str, Position);
         Temp_Token := GetToken (Temp_Word);

         Position := Temp_Token.Name.Next;

         --           Put_Line (Token_Type (Temp_Token.T)'Img & " ");
         if Remove_And and Temp_Token.T = And_T then
            null;
            Remove_And := False;
         else
            --              if Temp_Token.T = Vector then
            --                 Add_Star := True;
            --              else

            if not Params and not Stop then
               if Temp_Token.T = Left_Parentheses then
                  --Int_Paren := Int_Paren + 1;
                  Params := True;
               elsif (Token_Type'Pos (Temp_Token.T) >= Token_Type'Pos (Name)
                      or Temp_Token.T = Comma
                      or Temp_Token.T = Left_Brace or Temp_Token.T = Right_Brace
                      or Temp_Token.T = Star
                      or Temp_Token.T = And_T)
                 and not (Temp_Token.T = Virtual) -- remove virtual keyword
               then
                  -- code here

                  if Temp_Token.T = String_T then
                     Start_Vector := Start_Vector & CreateToken (Char);
                     Start_Vector := Start_Vector & CreateToken (Star);
                     Remove_And := True;
                  else
                     Start_Vector := Start_Vector & To_Vector (Temp_Token, 1);
                     if Add_Star then
                        Add_Star := False;
                        Start_Vector := Start_Vector & CreateToken (Star);
                        Remove_And := True;
                     end if;
                  end if;
               end if;

            end if;

            -- fix the parameters
            if Params then
               if Temp_Token.T = Left_Parentheses then
                  Int_Paren := Int_Paren + 1;
               elsif Temp_Token.T = Right_Parentheses then
                  Int_Paren := Int_Paren - 1;
                  if Int_Paren = 0 then
                     Stop := True;
                     Params := False;
                  end if;
               end if;

               -- skip due to =
               if Skip then
                  if (Temp_Token.T = Right_Parentheses or Temp_Token.T = Comma) and Int_Paren <= 1 then
                     Skip := False;
                  end if;
               end if;
               if not Skip then
                  if Token_Type'Pos (Temp_Token.T) >= Token_Type'Pos (Name)
                    or Temp_Token.T = Comma
                    or Temp_Token.T = Left_Brace or Temp_Token.T = Right_Brace
                    or Temp_Token.T = Star
                    or Temp_Token.T = And_T
                  then
                     if Temp_Token.T = String_T then
                        Param_Vector := Param_Vector & CreateToken (Char);
                        Param_Vector := Param_Vector & CreateToken (Star);
                        Remove_And := True;
                     else
                        Param_Vector := Param_Vector & Temp_Token;
                        if Add_Star then
                           Add_Star := False;
                           Param_Vector := Param_Vector & CreateToken (Star);
                           Remove_And := True;
                        end if;
                     end if;
                  elsif Temp_Token.T = Equal then
                     Skip := True;
                  end if;
               end if;
            end if;
            -- maybe not needed, put things after the params here
            if Stop then
               null;
            end if;
            --              end if; -- remove vector
         end if; -- remove and
      end loop;

      Decl.Start := Start_Vector;
      Decl.Params := Param_Vector;
      Decl.End_Of_Decl := Temp_Token;

      return Decl;
   end CreateDecleration;

   -- Create a string from a decleration data structure.
   function PutDecleration (Decl     : Decleration;
                            New_Last : Boolean := False) return Unbounded_String is
      S        : Unbounded_String ;
      Position : Smaller_Integer := 1;
      Temp     : Token;

   begin
      Position := First_Index (Decl.Start);

      loop
         exit when Position > Last_Index (Decl.Start);
         Temp := Element (Decl.Start, Position);


         if Temp.T = Vector then
            S := S & FixVector (Decl          => Decl,
                                Position      => Integer (Position),
                                Is_Return     => False,
                                Is_Start_Type => True) & " ";
            Position := Last_Index (Decl.Start);
         else

            if Temp.Use_Replacement then
               if Temp.T = Char then
                  S := S & "char";
               elsif Temp.T = Star then
                  S := S & "*";
               else
                  S := S & "/* ERROR */";
               end if;
               S := S & " ";
            else
               S := S & Temp.Name.Str;
               S := S & " ";
            end if;

            Position := Position + 1;
         end if;
      end loop;


      -- Start of the params section
      S := S & "(";

      Position := First_Index (Decl.Params);
      loop
         exit when Position > Last_Index (Decl.Params);
         Temp := Element (Decl.Params, Position);

         if Temp.T = Vector then
            S := S & FixVector (Decl          => Decl,
                                Position      => Integer (Position),
                                Is_Return     => False,
                                Is_Start_Type => False,
                                Is_Param      => True) & " ";
            loop
               exit when Temp.T = Name and (Element (Decl.Params, Position + 1).T = Comma or Position + 1 = Last_Index(Decl.Params));
               Position := Position + 1;
               Temp := Element (Decl.Params, Position);
            end loop;
         else

            if Temp.Use_Replacement then
               if Temp.T = Char then
                  S := S & "char";
               elsif Temp.T = Star then
                  S := S & "*";
               else
                  S := S & "/* ERROR */";
               end if;
               S := S & " ";
            else
               S := S & Temp.Name.Str;
               S := S & " ";
            end if;

            Position := Position + 1;
         end if;
      end loop;

      S := S & ")";
      -- End of params.

      -- sure!
      if New_Last then
         S := S & "{";
      else
         S := S & ";";
      end if;
      return S;
   end PutDecleration;


   function CreateFunctionCall (Decl : Decleration) return Unbounded_String is
      Temp           : Token;
      Is_Constructor : Boolean := False;
      Is_Destroy     : Boolean := False;

      Position       : Integer;
      Func_Name      : Unbounded_String;
      Function_Call  : Unbounded_String := To_Unbounded_String ("");
      End_Str        : Unbounded_String := To_Unbounded_String ("");
      Params         : Unbounded_String;

      Is_Function    : Integer := 0;
      Have_This      : Boolean := False;
      Is_String      : Boolean := False;
      Is_Vector      : Boolean := False;
      End_Call_Extra_String : Boolean := False;
   begin

      -- find the name of the call.

      Position := Integer (Last_Index (Decl.Start));
      Temp := Element (Decl.Start, Smaller_Integer (Position));
      if not (Temp.T = Name) then
         return To_Unbounded_String ("/* Error" & To_String (Temp.Name.Str) & "*/");
      else
         if Ada.Strings.Unbounded.Count (Temp.Name.Str, "new_") >= 1 then
            Is_Constructor := True;
         elsif Ada.Strings.Unbounded.Count (Temp.Name.Str, "delete_") >= 1 then
            Is_Destroy := True;
         else
            -- oh noes
            Func_Name := Temp.Name.Str;
         end if;
      end if;

      if Is_Constructor then
         -- create a constructor call
         Function_Call := Function_Call & "return new " & Slice (Temp.Name.Str, 5, Length (Temp.Name.Str));
         null;
      elsif Is_Destroy then
         -- create a destructor call
         Function_Call := Function_Call & "delete " & "_this";
         null;
      else
         -- whatever

         -- procedure or function?
         Position := Integer (First_Index (Decl.Start));
         loop
            exit when Position > Integer (Last_Index (Decl.Start));
            Temp := Element (Decl.Start, Smaller_Integer (Position));

            if Temp.T = Void then
               Is_Function := Is_Function + 1;
            elsif Temp.T = And_T or Temp.T = Star then
               Is_Function := Is_Function - 1;
            elsif Temp.T = Char and Temp.Use_Replacement then
               Is_String := True;
            elsif Temp.T = Vector then
               Function_Call := Function_Call & FixVector (Decl      => Decl,
                                                           Position  => Position,
                                                           Is_Return => True);
               Function_Call := Function_Call & "(";
               End_Str := End_Str & ")";
            end if;
            Position := Position + 1;
         end loop;

         -- create a return or not
         if Is_Function < 1 then
            -- we have a function!!! need to have a return
            Function_Call :=  "return " & Function_Call ;
            if Is_String then
               End_Call_Extra_String := True; -- appends a -
               Is_String := False;
            end if;
         end if;

         -- are we part of a class (we have a this as first parameter)
         Position := Integer (First_Index (Decl.Params));
         loop
            exit when Position > Integer (Last_Index (Decl.Params)) or Have_This;
            Temp := Element (Decl.Params, Smaller_Integer (Position));

            if Temp.T = This then
               Have_This := True;
            end if;
            Position := Position + 1;
         end loop;

         if Have_This then
            -- prepend "class"-> to function_name
            Temp := Element (Decl.Params, First_Index (Decl.Params));
            Func_Name := "_this" & "->" & Func_Name;
         end if;

      end if;
      if not Is_Destroy then
         -----------------------------------------------------------------------
         -- Parameter list
         -- are we part of a class (we have a this as first parameter)

         Params := Params & "(";

         Position := Integer (First_Index (Decl.Params));
         loop
            exit when Position > Integer (Last_Index (Decl.Params));
            Temp := Element (Decl.Params, Smaller_Integer (Position));
            if not Is_Vector then
               if Position = Integer (Last_Index (Decl.Params)) and not (Temp.T = This) then
                  Params := Params & Temp.Name.Str;
                  if Is_String then
                     Params := Params & ")";
                     Is_String := False;
                  end if;
               elsif Temp.T = This then
                  --Element (Decl.Params, Smaller_Integer (Position + 1)).T = Comma then
                  Params := Params & "/*removed _this*/";
                  Position := Position + 1;
               elsif ((Temp.T = Name) and
                     Element (Decl.Params, Smaller_Integer (Position + 1)).T = Comma)
                 or Temp.T = Comma  then
                  Params := Params & Temp.Name.Str;
                  if Is_String then
                     Params := Params & ")";
                     Is_String := False;
                  end if;
                  Params := Params & " ";
               elsif Temp.Use_Replacement then
                  if Temp.T = Char then
                     Params := Params & "string(";
                     Is_String := True;
                  else
                     null;
                  end if;
               elsif Temp.T = Vector then
                  Params := Params & FixVector (Decl, Position, False, False, False);
                  Is_Vector := True;
               else
                  Params := Params & "/*" & Temp.Name.Str & "*/";
               end if;
            elsif ((Temp.T = Name) and
                     Element (Decl.Params, Smaller_Integer (Position + 1)).T = Comma)
              or Temp.T = Comma  then
               -- done with vector;
               Params := Params & Temp.Name.Str & ")";
               if Is_String then
                  Params := Params & ")";
                  Is_String := False;
               end if;
               Is_Vector := False;
               Params := Params & " ";
               null;
            else
               null; -- skip due to vector
            end if;
            Position := Position + 1;
         end loop;
         Params := Params & ")";
      end if;

      if End_Call_Extra_String then
         Params := Params & ".c_str()";
      end if;
      Function_Call := ASCII.HT & Function_Call & Func_Name & Params & End_Str & ";";

      return Function_Call;
   end CreateFunctionCall;

   -----------------------------------------------------------------------------
   -- FixVector
   -----------------------------------------------------------------------------
   function FixVector (Decl          : Decleration;
                       Position      : Integer;
                       Is_Return     : Boolean := False;
                       Is_Start_Type : Boolean := False;
                       Is_Param      : Boolean := False) return Unbounded_String is
      function ToVector (T : Unbounded_String) return Unbounded_String is
         T2  : Unbounded_String;
         Pos : Integer := Index (T, " ");
      begin
         if Ada.Strings.Unbounded.Count (T, " ") = 0 then
            T2 := T;
         else
            T2 := Overwrite (T, Pos, "_");
         end if;
         return "vector<" & T2 & ">";
      end ToVector;

      -- creates a C_vector type
      function ToCVector (T : Unbounded_String) return Unbounded_String is
         T2  : Unbounded_String;
         Pos : Integer := Index (T, " ");
      begin
         if Ada.Strings.Unbounded.Count (T, " ") = 0 then
            T2 := T & "_vector";
         else
            T2 := Replace_Slice (T, Pos, Pos, "_") & "_vector";
         end if;
         return T2;
      end ToCVector;

      function ToVectorIn ( T : Unbounded_String) return Unbounded_String is
         T2  : Unbounded_String := To_Unbounded_String("");
         Pos : Integer := Index (T, " ");
      begin
         Put_Line("tovectorIn ");
         if Ada.Strings.Unbounded.Count (T, " ") = 0 then
            Put("no space ");
            T2 := T;
            Put("no space ");
         else
            Put("space" & To_String(T));
            T2 := Replace_Slice (T, Pos, Pos, "_")  ;
         end if;
         New_Line;
         Put (To_String (T2));
         New_Line;
         Put (To_String(T));
         return "c_vector_to_vector<" & T2  & "_vector" & ", " & T & ">";
      end ToVectorIn;

      function ToVectorOut (T : Unbounded_String) return Unbounded_String is
         T2  : Unbounded_String;
         Ts  : Unbounded_String;
         Pos : Integer := Index (T, " ");
      begin
         if Ada.Strings.Unbounded.Count (T, " ") = 0 then
            T2 := T;
         else
            Put ("we have space" & Pos'Img & To_String (T));
            --              Overwrite(T2,Pos,"_");
            T2 := Replace_Slice (T, Pos, Pos, "_");
            Put ("kasst");
         end if;
         return "vector_to_c_vector<" & T2 & "_vector" & ", " & T & ">";
      end ToVectorOut;

      Temp : Token;
      New_Position : Integer := Position;
      Temp_Str : Unbounded_String;
      Ret_Str : Unbounded_String := To_Unbounded_String ("");
   begin
      if Is_Return then
         -- function returns a vector
         New_Position := New_Position + 1;
         loop
            Temp := Element (Decl.Start, Smaller_Integer (New_Position));

            Temp_Str := Temp_Str & Temp.Name.Str;
            New_Position := New_Position + 1;
            exit when New_Position = Integer (Length (Decl.Start));
            Temp_Str := Temp_Str & " ";
         end loop;
         Ret_Str := Ret_Str & ToVectorOut (Temp_Str);
         return Ret_Str;
      end if;

      if Is_Start_Type then
         -- function returns a vector , used when creating the c function.
         New_Position := New_Position + 1;
         loop
            Temp := Element (Decl.Start, Smaller_Integer (New_Position));

            Temp_Str := Temp_Str & Temp.Name.Str;
            New_Position := New_Position + 1;
            exit when New_Position = Integer (Length (Decl.Start));
            Temp_Str := Temp_Str & " ";
         end loop;
         Ret_Str := Ret_Str & ToCVector (Temp_Str) & " *";
         return Ret_Str;
      end if;

      if Is_Param then
         -- vector is a param (in the c function)
         New_Position := New_Position + 1;
         loop
            Temp := Element (Decl.Params, Smaller_Integer (New_Position));

            Temp_Str := Temp_Str & Temp.Name.Str;
            New_Position := New_Position + 1;
            Put_Line(To_String(Temp_Str) & New_Position'Img);
            exit when New_Position = Integer (Length (Decl.Params) -1 ) or Element (Decl.Params, Smaller_Integer (New_Position+1)).T = Comma;
            Temp_Str := Temp_Str & " ";
         end loop;
         Ret_Str := Ret_Str & ToCVector (Temp_Str) & " *";
         return Ret_Str;
      end if;

      Put_Line("false,false,false");
      New_Position := New_Position + 1;
      loop
         Temp := Element (Decl.Params, Smaller_Integer (New_Position));

         Temp_Str := Temp_Str & Temp.Name.Str;
         New_Position := New_Position + 1;
         Put_Line ("false: " & To_String (Temp_Str) & New_Position'Img);
            exit when New_Position = Integer (Length (Decl.Params) -1 ) or Element (Decl.Params, Smaller_Integer (New_Position +1)).T = Comma;
         Temp_Str := Temp_Str & " ";
         Put_Line ("efter exit");
      end loop;
      Ret_Str := Ret_Str & ToVectorIn (Temp_Str) & "(";
      Put(To_String(Ret_Str));
      return Ret_Str;
   end FixVector;
end Parser;
