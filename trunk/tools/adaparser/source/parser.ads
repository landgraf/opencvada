---

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

package Parser is

   -- { curly [ brace ( parentheses < chevrons
   -- | ';' | ',' | '<' | '>' | ':' | '&'
   type Token_Type is (Left_Curly, -- crap
                       Right_Curly,
                       Left_Brace,
                       Right_Brace,
                       Left_Parentheses,
                       Right_Parentheses,
                       Left_Chevron,
                       Right_Chevron,
                       Semicolon,
                       Colon,
                       Comma,
                       And_T,
                       Star,
                       Equal,
                       Pipe,
                       Name, -- function and so one
                       Char, -- types
                       Int,
                       Float,
                       Double,
                       Void,
                       Vector,
                       Bool,
                       Wchar_T,
                       String_T,
                       Short, -- keywords
                       Long,
                       Unsigned,
                       Signed,
                       Const,
                       Virtual,
                       This,
                       Unknown);


   type Word is record
      Str         : Unbounded_String;
      Start       : Integer;
      End_Of_Word : Integer;
      Next        : Integer;
   end record;

   type Token is
      record
         Name             : Word;
         T                : Token_Type;
         Use_Replacement  : Boolean := False;
         Replacement      : Unbounded_String;
      end record;

   type Smaller_Integer is range 1 .. 255;
   package Token_Vectors is new Ada.Containers.Vectors(Smaller_Integer, Token);
   use Token_Vectors;
   subtype Token_Vector is Token_Vectors.Vector;

   type Decleration is
      record
      --           Return_Type : Token;
      --           Name        : Token;
      --           Parameters  : Token;
      --           End_Of_Decl : Token;
         Start       : Token_Vector;
         Params      : Token_Vector;
         End_Of_Decl : Token;
      end record;

   type Call is
      record
         Name : Token;
         Params : Token_Vector;
         End_Of_Call : Token;
      end record;

   function CreateDecleration (Str : String) return Decleration;

   procedure GetCharacter (Str         : String;
                           Pos         : Integer;
                           Skip        : out Boolean;
                           End_Of_Word : out Boolean);

   function GetWord (Src   : String;
                     Start : Integer) return Word;

   function CompareToken (Str : String) return Token_Type;
   function GetToken (Src : Word) return Token;
   function CompareString (Src : String; Token : String) return Boolean;

   function PutDecleration (Decl : Decleration;
                            New_Last : Boolean := False) return Unbounded_String ;

   function CreateFunctionCall (Decl : Decleration) return Unbounded_String;
   function FixVector (Decl          : Decleration;
                       Position      : Integer;
                       Is_Return     : Boolean := False;
                       Is_Start_Type : Boolean := False;
                       Is_Param      : Boolean := False) return Unbounded_String;

   procedure Nulled;
end Parser;
