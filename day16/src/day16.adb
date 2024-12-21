pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

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

   type Loc_Dir_Score_Rec is record
      Last  : Loc_And_Dir_Rec;
      Score : Natural;
   end record;

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

   function "<" (First, Second : Loc_And_Dir_Rec) return Boolean
   is (First.Direction < Second.Direction
       or else (First.Direction = Second.Direction
                and then First.Location < Second.Location));

   package Loc_And_Dir_To_Scores is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Loc_And_Dir_Rec,
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
      if Path_Scores.Contains (Path.Last) then
         if Path_Scores (Path.Last) >= Path.Score then
            Path_Scores.Replace (Path.Last, Path.Score);
            To_Do.Append (Path);
         end if;
      else
         Path_Scores.Insert (Path.Last, Path.Score);
         To_Do.Append (Path);
      end if;
   end Enqueue_If_Legal_And_Optimal;

   procedure Part_1 is
      Result : Natural := 0;

      To_Do         : Loc_And_Score_Vecs.Vector;
      Path_Scores   : Loc_And_Dir_To_Scores.Map;
      Curr, Next    : Loc_Dir_Score_Rec;
      Offset        : Motion_2D.Drc;
      New_Direction : Motion_2D.Direction;
   begin
      To_Do.Append
        (Loc_Dir_Score_Rec'
           ((Location => Map_IO.Start_Location, Direction => East),
            Score => 0));
      while not To_Do.Is_Empty loop
         IO.Put_Line ("Considering" & To_Do.Length'Image & " paths");
         Curr := To_Do.First_Element;
         Result := Curr.Score;
         exit when Curr.Last.Location = Map_IO.End_Location;
         Offset := Motion_2D.Deltas (Curr.Last.Direction);
         Next :=
           (
              (
                 (Curr.Last.Location.Row + Offset.DRow,
                  Curr.Last.Location.Col + Offset.DCol),
               Curr.Last.Direction),
            Curr.Score + 1);
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
               Curr.Score + 1001);
            Enqueue_If_Legal_And_Optimal (Next, To_Do, Path_Scores);
         end loop;
         To_Do (To_Do.First_Index) := To_Do.Last_Element;
         To_Do.Delete_Last;
         Path_Sorter.Sort (To_Do);
      end loop;
      IO.Put_Line ("the lowest-scoring path is" & Result'Image);
   end Part_1;

begin
   Map_IO.Read_Input;
   Part_1;
end Day16;
