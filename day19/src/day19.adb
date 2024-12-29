pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

procedure Day19 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   type Color_Variant is (White, Blue, Black, Red, Green);
   package Color_IO is new IO.Enumeration_IO (Enum => Color_Variant);

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Color_Variant
   is (case Symbol is
         when 'w' => White,
         when 'u' => Blue,
         when 'b' => Black,
         when 'r' => Red,
         when 'g' => Green,
         when others => raise Invalid_Symbol with Symbol'Image);

   function Serialize (Symbol : Color_Variant) return Character
   is (case Symbol is
         when White => 'w',
         when Blue => 'u',
         when Black => 'b',
         when Red => 'r',
         when Green => 'g');

   package Pattern_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Color_Variant);
   subtype Pattern_Vector is Pattern_Vecs.Vector;

   function "=" (Left, Right : Pattern_Vector) return Boolean
   is (Natural (Left.Length) = Natural (Right.Length)
       and then (for all Ith in Left.First_Index .. Left.Last_Index
                 => Left (Ith) = Right (Ith)));

   package Pattern_Lists is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Pattern_Vector);
   subtype Pattern_List is Pattern_Lists.Vector;

   Patterns_Possible, Designs_Desired : Pattern_List;

   function Get
     (Line : String; Position : in out Positive) return Pattern_Vector
   is
      Result : Pattern_Vector;
   begin
      while Position <= Line'Last and then Line (Position) /= ',' loop
         Result.Append (Deserialize (Line (Position)));
         Position := @ + 1;
      end loop;
      return Result;
   end Get;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      declare
         Line     : constant String := IO.Get_Line (Input);
         Position : Positive := Line'First;
      begin
         while Position <= Line'Last loop
            Patterns_Possible.Append (Get (Line, Position));
            Position := @ + 2;
         end loop;
      end;
      IO.Skip_Line (Input);
      while not IO.End_Of_File (Input) loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Position : Positive := Line'First;
         begin
            Designs_Desired.Append (Get (Line, Position));
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   procedure Put (Pattern : Pattern_Vector) is
   begin
      for Element of Pattern loop
         IO.Put (Serialize (Element));
      end loop;
   end Put;

   function Matches
     (Left : Pattern_Vector; Position : Positive; Right : Pattern_Vector)
      return Boolean
   is (Position + Natural (Right.Length) - 1 <= Natural (Left.Length)
       and then (for all Ith in Right.First_Index .. Right.Last_Index
                 => Left (Position + Ith - Right.First_Index) = Right (Ith)));

   type Pattern_Record is record
      Candidate : Pattern_Vector;
      Position  : Positive;
   end record;

   package Parse_Q_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Pattern_Record);

   package Parse_Qs is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Queue_Interfaces => Parse_Q_Interfaces);

   function Parses
     (Design : Pattern_Vector; Position : Positive := 1) return Boolean is
   begin
      if Position > Design.Last_Index then
         return True;
      end if;
      for Pattern of Patterns_Possible loop
         if Matches (Design, Position, Pattern) then
            if Parses (Design, Position + Natural (Pattern.Length)) then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Parses;

   procedure Input_Comparison is
   begin
      IO.Put_Line ("Patterns possible:");
      for Pattern of Patterns_Possible loop
         IO.Put ("   ");
         Put (Pattern);
         IO.New_Line;
      end loop;
      IO.Put_Line ("Designs desired:");
      for Design of Designs_Desired loop
         IO.Put ("   ");
         Put (Design);
         IO.New_Line;
      end loop;
   end Input_Comparison;

   procedure Match_Test is
   begin
      if Matches (Designs_Desired (1), 3, Patterns_Possible (2)) then
         IO.Put_Line ("OK");
      else
         IO.Put_Line ("NOK");
      end if;
      if Matches (Designs_Desired (6), 3, Patterns_Possible (8)) then
         IO.Put_Line ("NOK");
      else
         IO.Put_Line ("OK");
      end if;
      if Matches (Designs_Desired (6), 6, Patterns_Possible (6)) then
         IO.Put_Line ("NOK");
      else
         IO.Put_Line ("OK");
      end if;
   end Match_Test;

   Appears_Singly : array (Color_Variant) of Boolean := [others => False];

   function Is_Trivial (Design : Pattern_Vector) return Boolean
   is (for all Element of Design => Appears_Singly (Element));

   procedure Prune is
      Pruned_Patterns : Pattern_List;
   begin
      for Pattern of Patterns_Possible loop
         if Natural (Pattern.Length) = 1 then
            Appears_Singly (Pattern (Pattern.First_Index)) := True;
         end if;
      end loop;
      for Pattern of Patterns_Possible loop
         if Natural (Pattern.Length) = 1 or else (not Is_Trivial (Pattern))
         then
            Pruned_Patterns.Append (Pattern);
         end if;
      end loop;
      Patterns_Possible := Pruned_Patterns.Copy;
      for Ith
        in reverse Pruned_Patterns.First_Index .. Pruned_Patterns.Last_Index
      loop
         Patterns_Possible.Delete (Ith);
         if not Parses
                  (Pruned_Patterns (Ith), Pruned_Patterns (Ith).First_Index)
         then
            Patterns_Possible.Insert (Ith, Pruned_Patterns (Ith));
         end if;
      end loop;
   end Prune;

   procedure Part_1 is
      Result : Natural := 0;
      Ith    : Natural := 0;
   begin
      for Design of Designs_Desired loop
         Ith := @ + 1;
         IO.Put_Line ("checking design" & Ith'Image);
         if Parses (Design, Design.First_Index) then
            Result := @ + 1;
         end if;
      end loop;
      IO.Put_Line ("There are" & Result'Image & " satisfactory designs");
   end Part_1;

begin
   Read_Input;
   Prune;
   IO.Put ("The only colors not available are ");
   for Color in Color_Variant loop
      if not Appears_Singly (Color) then
         Color_IO.Put (Color, Set => IO.Lower_Case);
         IO.Put (" ");
      end if;
   end loop;
   IO.New_Line;
   IO.Put_Line
     ("After pruning," & Patterns_Possible.Length'Image & " patterns");
   for Pattern of Patterns_Possible loop
      IO.Put ("   ");
      Put (Pattern);
      IO.New_Line;
   end loop;
   Part_1;
end Day19;
