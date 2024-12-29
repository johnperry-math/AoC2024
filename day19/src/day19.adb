pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

procedure Day19 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;
   Debug         : constant Boolean := False;

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

   function "=" (Left, Right : Pattern_List) return Boolean
   is (Natural (Left.Length) = Natural (Right.Length)
       and then (for all Ith in Left.First_Index .. Left.Last_Index
                 => Left (Ith) = Right (Ith)));

   package Redirect_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Color_Variant,
        Element_Type => Pattern_List);

   Redirects : Redirect_Maps.Map;

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

   pragma Warnings (Off, "procedure ""Put"" is not referenced");
   procedure Put (Pattern : Pattern_Vector; Position : Positive := 1) is
   begin
      for Ith in Position .. Pattern.Last_Index loop
         IO.Put (Serialize (Pattern (Ith)));
      end loop;
   end Put;

   function Matches
     (Left : Pattern_Vector; Position : Positive; Right : Pattern_Vector)
      return Boolean
   is (Position + Natural (Right.Length) - 1 <= Natural (Left.Length)
       and then (for all Ith in Right.First_Index .. Right.Last_Index
                 => Left (Position + Ith - Right.First_Index) = Right (Ith)));

   Appears_Singly : array (Color_Variant) of Boolean := [others => False];

   type Value is range 0 .. 2 ** 64 - 1;

   function Arrangements (Design : Pattern_Vector) return Value is
      Results : array (Design.First_Index .. Design.Last_Index + 1) of Value :=
        [others => 0];
   begin
      Results (Design.Last_Index + 1) := 1;
      for Ith in reverse Design.First_Index .. Design.Last_Index loop
         for Pattern
           of Redirects (Design (Ith))
           when Matches (Design, Ith, Pattern)
         loop
            Results (Ith) := @ + Results (Ith + Natural (Pattern.Length));
         end loop;
      end loop;
      return Results (Design.First_Index);
   end Arrangements;

   function Is_Trivial (Design : Pattern_Vector) return Boolean
   is (for all Element of Design => Appears_Singly (Element));

   procedure Prune is
      Pruned_Patterns : Pattern_List;
      Empty           : Pattern_List;
   begin
      for Color in Color_Variant loop
         Redirects.Insert (Color, Empty.Copy);
      end loop;
      for Pattern of Patterns_Possible loop
         if Natural (Pattern.Length) = 1 then
            Appears_Singly (Pattern (Pattern.First_Index)) := True;
         end if;
      end loop;
      for Pattern of Patterns_Possible loop
         Redirects (Pattern (1)).Append (Pattern);
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
         if Arrangements (Pruned_Patterns (Ith)) = 0 then
            Patterns_Possible.Insert (Ith, Pruned_Patterns (Ith));
         end if;
      end loop;
      if Debug then
         for Color in Color_Variant loop
            Color_IO.Put (Color, Set => IO.Lower_Case);
            IO.Put_Line
              (" has" & Redirects (Color).Length'Image & " redirects");
         end loop;
         declare
            Total : Natural := 0;
         begin
            for Color in Color_Variant loop
               Total := @ + Natural (Redirects (Color).Length);
            end loop;
            IO.Put_Line ("altogether there are" & Total'Image & " patterns");
         end;
      end if;
   end Prune;

   procedure Both_Parts is
      Part_1 : Natural := 0;
      Part_2 : Value := 0;
   begin
      for Design of Designs_Desired loop
         declare
            My_Arrangements : constant Value := Arrangements (Design);
         begin
            if My_Arrangements > Value (0) then
               Part_1 := @ + 1;
               Part_2 := @ + My_Arrangements;
            end if;
         end;
      end loop;
      IO.Put_Line ("There are" & Part_1'Image & " satisfactory designs");
      IO.Put_Line ("There are" & Part_2'Image & " ways to arrange them");
   end Both_Parts;

begin
   Read_Input;
   Prune;
   if Debug then
      IO.Put ("The only color(s) not available singly? ");
      for Color in Color_Variant loop
         if not Appears_Singly (Color) then
            Color_IO.Put (Color, Set => IO.Lower_Case);
            IO.Put (" ");
         end if;
      end loop;
   end if;
   IO.New_Line;
   Both_Parts;
end Day19;
