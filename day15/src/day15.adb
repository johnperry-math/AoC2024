pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with Common;

procedure Day15 is

   package IO renames Ada.Text_IO;

   package Motion_2D renames Common.Two_Dimensional_Motion;
   use all type Motion_2D.Direction;

   type Example_Enum is (Puzzle, Example_1, Example_2, Example_3);

   Doing_Example : constant Example_Enum := Puzzle;

   Read_Example : constant Boolean :=
     (if Doing_Example = Puzzle then False else True);

   Dimension : constant Positive :=
     (case Doing_Example is
        when Puzzle => 50,
        when Example_1 => 10,
        when Example_2 => 8,
        when Example_3 => 7);

   type Object_Enumeration is (Robot, Box_Left, Box_Right, Wall, Empty);
   Box renames Box_Left;

   function Serialize (Object : Object_Enumeration) return Character
   is (case Object is
         when Robot => '@',
         when Box_Left | Box_Right => 'O',
         when Wall => '#',
         when Empty => '.');

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Object_Enumeration
   is (case Symbol is
         when '.' => Empty,
         when '#' => Wall,
         when 'O' => Box_Left,
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
            when Example_1 => "example 1.txt",
            when Example_2 => "example 2.txt",
            when Example_3 => "example 3.txt",
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
      Map        : Map_2D.Map_Array := Map_2D.Map;
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
               Curr_Box : constant Map_2D.Location_Record := Next;
               Next_Box : Map_2D.Location_Record := Next;
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

   subtype Row_Range is Positive range 1 .. Dimension;
   subtype Col_Range is Positive range 1 .. 2 * Dimension;

   Wider_Map : array (Row_Range, Col_Range) of Object_Enumeration;

   type Wider_Location_Record is record
      Row : Row_Range;
      Col : Col_Range;
   end record;

   function "<" (First, Second : Wider_Location_Record) return Boolean
   is (First.Row < Second.Row
       or else (First.Row = Second.Row and then First.Col < Second.Col));

   package Location_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Wider_Location_Record);

   Wider_Start : Wider_Location_Record;

   function Remap_Column (Col : Col_Range) return Map_2D.Col_Range
   is (if Col mod 2 = 0 then Col / 2 else Col / 2 + 1);

   procedure Widen_Map is
      Map renames Map_2D.Map;
      Narrow_Col : Map_2D.Col_Range;
   begin
      Wider_Start :=
        (Row => Map_IO.Start_Location.Row,
         Col => Map_IO.Start_Location.Col * 2 - 1);
      for Row in Row_Range loop
         for Col in Col_Range when Col mod 2 = 1 loop
            Narrow_Col := Remap_Column (Col);
            case Map (Row, Narrow_Col) is
               when Box =>
                  Wider_Map (Row, Col) := Box_Left;
                  Wider_Map (Row, Col + 1) := Box_Right;

               when others =>
                  Wider_Map (Row, Col) := Map (Row, Narrow_Col);
                  Wider_Map (Row, Col + 1) := Map (Row, Narrow_Col);
            end case;
         end loop;
      end loop;
   end Widen_Map;

   procedure Draw_Wider_Map (Current_Location : Wider_Location_Record) is
   begin
      for Row in Row_Range loop
         for Col in Col_Range loop
            if Current_Location = (Row, Col) then
               IO.Put ('@');
            else
               IO.Put
                 (case Wider_Map (Row, Col) is
                    when Empty => '.',
                    when Box_Left => '[',
                    when Box_Right => ']',
                    when Wall => '#',
                    when Robot => '@');
            end if;
         end loop;
         IO.New_Line;
      end loop;
   end Draw_Wider_Map;

   function Has_Box (Location : Wider_Location_Record) return Boolean
   is (Wider_Map (Location.Row, Location.Col) = Box_Left
       or else Wider_Map (Location.Row, Location.Col) = Box_Right);

   Failed_Check_For_Block : exception;

   function Blocked
     (Boxes : Location_Sets.Set; Offset : Motion_2D.Drc) return Boolean
   is (for some Box of Boxes
       => Wider_Map (Box.Row + Offset.DRow, Box.Col + Offset.DCol) = Wall);

   function Locate_Boxes
     (Start : Wider_Location_Record; Offset : Motion_2D.Drc)
      return Location_Sets.Set
   is
      Next                          : Wider_Location_Record := Start;
      Result, New_Boxes, Prev_Boxes : Location_Sets.Set;
      Last_Size                     : Natural := 0;
   begin
      --  IO.Put_Line ("locating boxes" & Offset.DRow'Image & Offset.DCol'Image);
      Prev_Boxes.Include (Start);
      if Wider_Map (Start.Row, Start.Col) = Box_Left then
         Prev_Boxes.Include ((Start.Row, Start.Col + 1));
      else
         Prev_Boxes.Include ((Start.Row, Start.Col - 1));
      end if;
      Result.Union (Prev_Boxes);
      while (Last_Size < Natural (Result.Length))
        and then not Blocked (Prev_Boxes, Offset)
      loop
         Last_Size := Natural (Result.Length);
         New_Boxes.Clear;
         for Curr of Prev_Boxes loop
            Next := (Curr.Row + Offset.DRow, Curr.Col + Offset.DCol);
            case Wider_Map (Next.Row, Next.Col) is
               when Box_Left =>
                  New_Boxes.Include (Next);
                  New_Boxes.Include ((Next.Row, Next.Col + 1));

               when Box_Right =>
                  New_Boxes.Include (Next);
                  New_Boxes.Include ((Next.Row, Next.Col - 1));

               when Empty | Robot =>
                  null;

               when Wall =>
                  raise Failed_Check_For_Block
                    with Next.Row'Image & Next.Col'Image;
            end case;
         end loop;
         Prev_Boxes := New_Boxes.Copy;
         --  IO.Put_Line ("----");
         --  for Box of Prev_Boxes loop
         --     IO.Put_Line ("Box at" & Box.Row'Image & Box.Col'Image);
         --  end loop;
         Result.Union (Prev_Boxes);
      end loop;
      --  IO.Put_Line ("Boxes located");
      return Result;
   end Locate_Boxes;

   procedure Part_2 is
      Deltas renames Motion_2D.Deltas;
      Result     : Natural := 0;
      Curr, Next : Wider_Location_Record := Wider_Start;
      Offset     : Motion_2D.Drc;
   begin
      for Movement of Movements loop
         Offset := Deltas (Movement);
         Next :=
           (Row => Curr.Row + Offset.DRow, Col => Curr.Col + Offset.DCol);
         if Wider_Map (Next.Row, Next.Col) = Empty then
            Curr := Next;
         elsif Wider_Map (Next.Row, Next.Col) = Wall then
            --  might as well be explicit about this
            null;
         else
            --  we have a box... or a sequence of boxes... can we move it/them?
            declare
               Boxes : constant Location_Sets.Set :=
                 Locate_Boxes (Next, Offset);
               Row   : Row_Range;
               Col   : Col_Range;
            begin
               if not Blocked (Boxes, Offset) then
                  if Offset.DRow /= 0 then
                     Row := Next.Row;
                     while (for some Box of Boxes
                            => Box.Row = Row + Offset.DRow)
                     loop
                        Row := @ + Offset.DRow;
                        --  IO.Put_Line ("Row" & Row'Image);
                     end loop;
                     --  IO.Put_Line ("Extreme box at" & Row'Image);
                     while Row /= Curr.Row loop
                        --  IO.Put_Line ("moving row" & Row'Image);
                        for Box of Boxes loop
                           if Box.Row = Row then
                              Wider_Map (Row + Offset.DRow, Box.Col) :=
                                Wider_Map (Row, Box.Col);
                              Wider_Map (Row, Box.Col) := Empty;
                           end if;
                        end loop;
                        Row := @ - Offset.DRow;
                     end loop;
                  else
                     Col := Next.Col;
                     while (for some Box of Boxes
                            => Box.Col = Col + Offset.DCol)
                     loop
                        Col := @ + Offset.DCol;
                        --  IO.Put_Line ("Col" & Col'Image);
                     end loop;
                     --  IO.Put_Line ("Extreme box at" & Col'Image);
                     while Col /= Curr.Col loop
                        --  IO.Put_Line ("moving col" & Col'Image);
                        for Box of Boxes loop
                           if Box.Col = Col then
                              Wider_Map (Box.Row, Col + Offset.DCol) :=
                                Wider_Map (Box.Row, Col);
                              Wider_Map (Box.Row, Col) := Empty;
                           end if;
                        end loop;
                        Col := @ - Offset.DCol;
                     end loop;
                  end if;
                  Curr := Next;
               end if;
            end;
         end if;
         --  Draw_Wider_Map (Curr);
      end loop;
      for Row in Row_Range loop
         for Col in Col_Range loop
            if Wider_Map (Row, Col) = Box then
               Result := @ + 100 * (Row - 1) + (Col - 1);
            end if;
         end loop;
      end loop;
      IO.Put_Line ("Sum of GPS Coordinates is" & Result'Image);
   end Part_2;

begin
   Read_Input;
   --  Part_1;
   Widen_Map;
   Draw_Wider_Map (Wider_Start);
   Part_2;
end Day15;
