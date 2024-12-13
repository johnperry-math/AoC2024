pragma Ada_2022;

with Ada.Text_IO;

procedure Day04 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   Dimension : constant Positive := (if Doing_Example then 10 else 140);
   subtype Indices is Positive range 1 .. Dimension;

   Word_Search : array (1 .. Dimension, 1 .. Dimension) of Character;

   procedure Read_Input is
      Input    : IO.File_Type;
      Filename : constant String :=
        (if Doing_Example then "example.txt" else "input.txt");
   begin
      IO.Open (Input, IO.In_File, Filename);
      for Row in Indices loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Col in Indices loop
               Word_Search (Row, Col) := Line (Col);
            end loop;
         end;
      end loop;
   end Read_Input;

   type Directions is (NN, EE, SS, WW, NE, SE, SW, NW);
   subtype Diff is Integer range -1 .. 1;
   type Step_Record is record
      Dx, Dy : Diff;
   end record;

   Step_Array : constant array (Directions) of Step_Record :=
     [NN => (0, -1),
      NE => (1, -1),
      EE => (1, 0),
      SE => (1, 1),
      SS => (0, 1),
      SW => (-1, 1),
      WW => (-1, 0),
      NW => (-1, -1)];

   function Spells_Xmas
     (Row, Col : Indices; Direction : Directions) return Boolean
   is (declare
         Step renames Step_Array (Direction);
       begin
         Integer (Row) + 3 * Step.Dy in Indices
         and then Integer (Col) + 3 * Step.Dx in Indices
         and then Word_Search (Row, Col) = 'X'
         and then Word_Search (Row + Step.Dy, Col + Step.Dx) = 'M'
         and then Word_Search (Row + 2 * Step.Dy, Col + 2 * Step.Dx) = 'A'
         and then Word_Search (Row + 3 * Step.Dy, Col + 3 * Step.Dx) = 'S');

   procedure Part_1 is
      Result     : Natural := 0;
      Old_Result : Natural;
   begin
      for Row in Indices loop
         for Col in Indices loop
            Old_Result := Result;
            for Direction in Directions loop
               if Spells_Xmas (Row, Col, Direction) then
                  Result := @ + 1;

               end if;
            end loop;
         end loop;
      end loop;
      IO.Put_Line ("XMAS appears" & Result'Image & " times");
   end Part_1;

   subtype Angles is Directions range NE .. NW;

   function Spells_Mas_in_X
     (Row, Col : Indices; Direction : Angles) return Boolean
   is (declare
         Step renames Step_Array (Direction);
       begin
         Integer (Row) - Step.Dy in Indices
         and then Integer (Row) + Step.Dy in Indices
         and then Integer (Col) - Step.Dx in Indices
         and then Integer (Col) + Step.Dx in Indices
         and then Word_Search (Row + Step.Dy, Col + Step.Dx) = 'M'
         and then Word_Search (Row, Col) = 'A'
         and then Word_Search (Row - Step.Dy, Col - Step.Dx) = 'S');

   procedure Part_2 is
      Result : Natural := 0;
   begin
      for Row in Indices loop
         for Col in Indices loop
            if (Spells_Mas_in_X (Row, Col, NE)
                and then Spells_Mas_in_X (Row, Col, NW))
              or else (Spells_Mas_in_X (Row, Col, NE)
                       and then Spells_Mas_in_X (Row, Col, SE))
              or else (Spells_Mas_in_X (Row, Col, SW)
                       and then Spells_Mas_in_X (Row, Col, NW))
              or else (Spells_Mas_in_X (Row, Col, SW)
                       and then Spells_Mas_in_X (Row, Col, SE))
            then
               Result := @ + 1;
            end if;
         end loop;
      end loop;
      IO.Put_Line ("MAS in X appears" & Result'Image & " times");
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day04;
