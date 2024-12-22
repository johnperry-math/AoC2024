pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with Common;

procedure Day16 is

   package IO renames Ada.Text_IO;

   package Motion_2D renames Common.Two_Dimensional_Motion;
   use all type Motion_2D.Direction;

   Doing_Example : constant Boolean := False;

   Dimension : constant Positive := (if Doing_Example then 17 else 141);

   type Object is (Start, Finish, Wall, Empty);

   package Map_2D is new
     Common.Two_Dimensional_Map
       (Row_Length => Dimension,
        Col_Length => Dimension,
        Object     => Object);
   use all type Map_2D.Location_Record;

   package Location_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Map_2D.Location_Record);

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Object
   is (case Symbol is
         when '.' => Empty,
         when '#' => Wall,
         when 'S' => Start,
         when 'E' => Finish,
         when others => raise Invalid_Symbol with Symbol'Image);

   function Serialize (O : Object) return Character
   is (case O is
         when Empty => '.',
         when Wall => '#',
         when Start => 'S',
         when Finish => 'E');

   package Map_IO is new
     Common.Two_Dimensional_Map_IO
       (Doing_Example  => Doing_Example,
        Remember_Start => Common.Remember_Start_Record'(True, Symbol => 'S'),
        Remember_End   => Common.Remember_End_Record'(True, Symbol => 'E'),
        Map_Package    => Map_2D,
        Serialize      => Serialize,
        Deserialize    => Deserialize);

   type Loc_And_Dir_Rec is record
      Location  : Map_2D.Location_Record;
      Direction : Motion_2D.Direction;
   end record;

   package Location_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Map_2D.Location_Record);

   type Loc_Dir_Score_Rec is record
      Last  : Loc_And_Dir_Rec;
      Score : Natural;
      Steps : Location_Vecs.Vector;
   end record;

   overriding
   function "=" (Left, Right : Loc_Dir_Score_Rec) return Boolean
   is (Left.Score = Right.Score
       and then Left.Last.Direction = Right.Last.Direction
       and then Left.Last.Location = Right.Last.Location);

   function "<" (Left, Right : Loc_Dir_Score_Rec) return Boolean
   is (Left.Score < Right.Score
       or else (Left.Score = Right.Score
                and then Left.Last.Direction < Right.Last.Direction)
       or else (Left.Score = Right.Score
                and then Left.Last.Direction = Right.Last.Direction
                and then Left.Last.Location < Right.Last.Location));

   package Loc_And_Score_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Loc_Dir_Score_Rec);

   package Path_Sorter is new Loc_And_Score_Vecs.Generic_Sorting;

   package Loc_And_Dir_To_Scores is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Map_2D.Location_Record,
        Element_Type => Natural);

   procedure Enqueue_If_Legal_And_Optimal
     (Path        : Loc_Dir_Score_Rec;
      To_Do       : in out Loc_And_Score_Vecs.Vector;
      Path_Scores : in out Loc_And_Dir_To_Scores.Map)
   is
      Map renames Map_2D.Map;
   begin
      if Map (Path.Last.Location.Row, Path.Last.Location.Col) = Wall then
         return;
      end if;
      if Path_Scores.Contains (Path.Last.Location) then
         if Path_Scores (Path.Last.Location) >= Path.Score
           or else Path_Scores (Path.Last.Location) = Path.Score - 1000
         then
            Path_Scores.Replace (Path.Last.Location, Path.Score);
            To_Do.Append (Path);

         end if;
      else
         Path_Scores.Insert (Path.Last.Location, Path.Score);
         To_Do.Append (Path);
      end if;
   end Enqueue_If_Legal_And_Optimal;

   procedure Print_Map (Seats : Location_Sets.Set) is
      Map renames Map_2D.Map;
   begin
      for Row in Map_2D.Row_Range loop
         for Col in Map_2D.Col_Range loop
            if Seats.Contains ((Row, Col)) then
               IO.Put ('O');
            else
               IO.Put
                 (case Map (Row, Col) is
                    when Wall => '#',
                    when Empty => '.',
                    when Start => 'S',
                    when Finish => 'E');
            end if;
         end loop;
         IO.New_Line;
      end loop;
   end Print_Map;

   procedure Advance_Straight (Path : in out Loc_Dir_Score_Rec) is
      Map renames Map_2D.Map;

      Offset : constant Motion_2D.Drc :=
        Motion_2D.Deltas (Path.Last.Direction);
      Next   : Map_2D.Location_Record := Path.Last.Location;

      New_Offset       : Motion_2D.Drc;
      Hit_Intersection : Boolean := False;
   begin
      loop
         Next := (Next.Row + Offset.DRow, Next.Col + Offset.DCol);
         exit when Map (Next.Row, Next.Col) = Wall;
         for Turn_Dir in Motion_2D.Turn_Direction when not Hit_Intersection
         loop
            New_Offset :=
              Motion_2D.Deltas
                (Motion_2D.Turn (Path.Last.Direction, Turn_Dir));
            if Map (Next.Row + New_Offset.DRow, Next.Col + New_Offset.DCol)
              /= Wall
            then
               Hit_Intersection := True;
            end if;
         end loop;
         Path.Last.Location := Next;
         Path.Steps.Append (Next);
         Path.Score := @ + 1;
         exit when Hit_Intersection;
      end loop;
   end Advance_Straight;

   procedure Parts_1_And_2 is
      Winning_Score           : Natural := Natural'Last;
      Good_Seats, First_Seats : Location_Sets.Set;

      To_Do         : Loc_And_Score_Vecs.Vector;
      Path_Scores   : Loc_And_Dir_To_Scores.Map;
      Curr, Next    : Loc_Dir_Score_Rec;
      Offset        : Motion_2D.Drc;
      New_Direction : Motion_2D.Direction;

   begin
      if not Doing_Example then
         To_Do.Append
           (Loc_Dir_Score_Rec'
              ((Location => Map_IO.Start_Location, Direction => East),
               Score => 0,
               Steps => [Map_IO.Start_Location]));
      end if;
      To_Do.Append
        (Loc_Dir_Score_Rec'
           (Last  =>
              Loc_And_Dir_Rec'
                (Location  =>
                   (Map_IO.Start_Location.Row - 1, Map_IO.Start_Location.Col),
                 Direction => North),
            Score => 1001,
            Steps =>
              [Map_IO.Start_Location,
               (Map_IO.Start_Location.Row - 1, Map_IO.Start_Location.Col)]));
      while not To_Do.Is_Empty loop
         IO.Put_Line ("Considering" & To_Do.Length'Image & " paths");
         Curr := To_Do.First_Element;
         exit when Curr.Score > Winning_Score;
         Offset := Motion_2D.Deltas (Curr.Last.Direction);
         Advance_Straight (Curr);
         if Curr.Last.Location = Map_IO.End_Location then
            for Location of Curr.Steps loop
               Good_Seats.Include (Location);
               if Winning_Score = Natural'Last then
                  First_Seats.Include (Location);
               end if;
            end loop;
            Winning_Score := Curr.Score;
         end if;
         Next :=
           (
              (
                 (Curr.Last.Location.Row + Offset.DRow,
                  Curr.Last.Location.Col + Offset.DCol),
               Curr.Last.Direction),
            Curr.Score + 1,
            Curr.Steps.Copy);
         Next.Steps.Append (Next.Last.Location);
         Enqueue_If_Legal_And_Optimal (Next, To_Do, Path_Scores);
         for Dir in Motion_2D.Turn_Direction loop
            New_Direction := Motion_2D.Turn (Curr.Last.Direction, Dir);
            Offset := Motion_2D.Deltas (New_Direction);
            Next :=
              (
                 (
                    (Curr.Last.Location.Row + Offset.DRow,
                     Curr.Last.Location.Col + Offset.DCol),
                  New_Direction),
               Curr.Score + 1001,
               Curr.Steps.Copy);
            Next.Steps.Append (Next.Last.Location);
            Enqueue_If_Legal_And_Optimal (Next, To_Do, Path_Scores);
         end loop;
         To_Do (To_Do.First_Index) := To_Do.Last_Element;
         To_Do.Delete_Last;
         Path_Sorter.Sort (To_Do);
      end loop;
      Print_Map (First_Seats);
      Print_Map (Good_Seats);
      IO.Put_Line
        ("the lowest-scoring paths pass through"
         & Winning_Score'Image
         & " tiles");
      IO.Put_Line
        ("the number of seats available on those paths?"
         & Good_Seats.Length'Image);
   end Parts_1_And_2;

begin
   Map_IO.Read_Input;
   Parts_1_And_2;
end Day16;
