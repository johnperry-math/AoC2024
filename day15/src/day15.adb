pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;

with Common;

procedure Day15 is

   package IO renames Ada.Text_IO;

   package Motion_2D renames Common.Two_Dimensional_Motion;
   use all type Motion_2D.Direction;

   type Example_Enum is (Puzzle, Example_1, Example_2);

   Doing_Example : constant Example_Enum := Puzzle;

   Read_Example : constant Boolean :=
     (if Doing_Example = Puzzle then False else True);

   Dimension : constant Positive :=
     (case Doing_Example is
        when Puzzle => 50,
        when Example_1 => 10,
        when Example_2 => 8);

   type Object_Enumeration is (Robot, Box, Wall, Empty);

   function Serialize (Object : Object_Enumeration) return Character
   is (case Object is
         when Robot => '@',
         when Box => 'O',
         when Wall => '#',
         when Empty => '.');

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Object_Enumeration
   is (case Symbol is
         when '.' => Empty,
         when '#' => Wall,
         when 'O' => Box,
         when others => raise Invalid_Symbol with Symbol'Image);

   package Map_2D is new
     Common.Two_Dimensional_Map
       (Row_Length => Dimension,
        Col_Length => Dimension,
        Object     => Object_Enumeration);

   package Map_IO is new
     Common.Two_Dimensional_Map_IO
       (Doing_Example  => Read_Example,
        Remember_Start => Common.Remember_Start_Record'(True, Symbol => '@'),
        Map_Package    => Map_2D);

   package Movement_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Motion_2D.Direction);

   Movements : Movement_Vecs.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      Map_IO.Read_Input;
      Map_2D.Map (Map_IO.Start_Location.Row, Map_IO.Start_Location.Col) :=
        Empty;
      IO.Open
        (Input,
         IO.In_File,
         (case Doing_Example is
            when Example_1 => "example.txt",
            when Example_2 => "example 2.txt",
            when Puzzle => "input.txt"));
      IO.Skip_Line (Input, IO.Positive_Count (Dimension + 1));
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Movement of Line loop
               Movements.Append
                 (case Movement is
                    when '^' => North,
                    when 'v' => South,
                    when '>' => East,
                    when '<' => West,
                    when others => raise Invalid_Symbol with "movements");
            end loop;
         end;
      end loop;
   end Read_Input;

   function Serialize (Movement : Motion_2D.Direction) return Character
   is (case Movement is
         when North => '^',
         when South => 'v',
         when East => '>',
         when West => '<');

   procedure Draw_Map (Current_Location : Map_2D.Location_Record) is
      Map renames Map_2D.Map;
      use all type Map_2D.Location_Record;
   begin
      for Row in Map_2D.Row_Range loop
         for Col in Map_2D.Col_Range loop
            if Current_Location = (Row, Col) then
               IO.Put ('@');
            else
               IO.Put (Serialize (Map (Row, Col)));
            end if;
         end loop;
         IO.New_Line;
      end loop;
   end Draw_Map;

   procedure Part_1 is
      Map renames Map_2D.Map;
      Deltas renames Motion_2D.Deltas;
      Result     : Natural := 0;
      Curr, Next : Map_2D.Location_Record := Map_IO.Start_Location;
      Offset     : Motion_2D.Drc;
   begin
      for Movement of Movements loop
         Offset := Deltas (Movement);
         Next :=
           (Row => Curr.Row + Offset.DRow, Col => Curr.Col + Offset.DCol);
         if Map (Next.Row, Next.Col) = Empty then
            Curr := Next;
         elsif Map (Next.Row, Next.Col) = Wall then
            --  might as well be explicit about this
            null;
         else
            --  we have a box... or a sequence of boxes... can we move it/them?
            declare
               Curr_Box, Next_Box : Map_2D.Location_Record := Next;
            begin
               --  find the end of the chain of boxes
               while Map (Next_Box.Row, Next_Box.Col) = Box loop
                  Next_Box :=
                    (Row => Next_Box.Row + Offset.DRow,
                     Col => Next_Box.Col + Offset.DCol);
               end loop;
               --  optimization: it will look the same
               --  if we move only the first box to the end
               if Map (Next_Box.Row, Next_Box.Col) /= Wall then
                  Map (Next_Box.Row, Next_Box.Col) := Box;
                  Map (Curr_Box.Row, Curr_Box.Col) := Empty;
                  Curr := Next;
               end if;
            end;
         end if;
      end loop;
      for Row in Map_2D.Row_Range loop
         for Col in Map_2D.Col_Range loop
            if Map (Row, Col) = Box then
               Result := @ + 100 * (Row - 1) + (Col - 1);
            end if;
         end loop;
      end loop;
      IO.Put_Line ("Sum of GPS Coordinates is" & Result'Image);
   end Part_1;

begin
   Read_Input;
   Part_1;
end Day15;
