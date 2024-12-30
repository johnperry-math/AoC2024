pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Common;

procedure Day20 is

   package IO renames Ada.Text_IO;

   package Motion_2D renames Common.Two_Dimensional_Motion;

   Offset renames Motion_2D.Deltas;
   subtype Direction is Motion_2D.Direction;
   use all type Direction;

   Doing_Example : constant Boolean := False;

   Dimension : constant Positive := (if Doing_Example then 15 else 141);

   type Object is (Wall, Empty, Start, Finish);

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Object
   is (case Symbol is
         when '#' => Wall,
         when '.' => Empty,
         when 'S' => Start,
         when 'E' => Finish,
         when others => raise Invalid_Symbol);

   function Serialize (Symbol : Object) return Character
   is (case Symbol is
         when Wall => '#',
         when Empty => '.',
         when Start => 'S',
         when Finish => 'E');

   package Map_2D is new
     Common.Two_Dimensional_Map
       (Row_Length => Dimension,
        Col_Length => Dimension,
        Object     => Object);

   subtype Location_Record is Map_2D.Location_Record;
   use all type Location_Record;

   package Map_IO is new
     Common.Two_Dimensional_Map_IO
       (Doing_Example  => Doing_Example,
        Remember_Start =>
          Common.Remember_Start_Record'(Remember => True, Symbol => 'S'),
        Remember_End   =>
          Common.Remember_End_Record'(Remember => True, Symbol => 'E'),
        Map_Package    => Map_2D);

   Baseline : Natural;

   Steps_From_Start : array (1 .. Dimension, 1 .. Dimension) of Natural :=
     [others => [others => Natural'Last]];
   Steps_To_Finish  : array (1 .. Dimension, 1 .. Dimension) of Natural :=
     [others => [others => Natural'Last]];

   Map renames Map_2D.Map;

   procedure Solve is

      type Path_Record is record
         Location : Location_Record;
         Length   : Natural;
      end record;

      package Path_Q_Interfaces is new
        Ada.Containers.Synchronized_Queue_Interfaces
          (Element_Type => Path_Record);
      package Path_Qs is new
        Ada.Containers.Unbounded_Synchronized_Queues
          (Queue_Interfaces => Path_Q_Interfaces);
      To_Do : Path_Qs.Queue;

      package Location_Sets is new
        Ada.Containers.Ordered_Sets (Element_Type => Location_Record);
      Done : Location_Sets.Set;

      Curr, Next : Path_Record;

      Start renames Map_IO.Start_Location;

   begin
      Steps_From_Start (Start.Row, Start.Col) := 0;
      To_Do.Enqueue ((Start, 0));
      while Natural (To_Do.Current_Use) > 0 loop
         To_Do.Dequeue (Curr);
         Steps_From_Start (Curr.Location.Row, Curr.Location.Col) :=
           Curr.Length;
         exit when Curr.Location = Map_IO.End_Location;
         Done.Include (Curr.Location);
         Next.Length := Curr.Length + 1;
         for Dir in Direction loop
            Next.Location := Curr.Location + Offset (Dir);
            if (Map (Next.Location.Row, Next.Location.Col) /= Wall)
              and then (not Done.Contains (Next.Location))
            then
               To_Do.Enqueue (Next);
            end if;
         end loop;
      end loop;
      Baseline := Curr.Length;
      for Row in 2 .. Dimension - 1 loop
         for Col in 2 .. Dimension - 1 loop
            if Map (Row, Col) /= Wall then
               Steps_To_Finish (Row, Col) :=
                 Curr.Length - Steps_From_Start (Row, Col);
            end if;
         end loop;
      end loop;
   end Solve;

   subtype Play_Range is Positive range 2 .. Dimension - 1;

   function Can_Cheat_At (Location : Location_Record) return Boolean
   is (Location.Row in Play_Range
       and then Location.Col in Play_Range
       and then Map (Location.Row, Location.Col) /= Wall);

   function Distance (Start, Finish : Location_Record) return Natural
   is (abs (Start.Row - Finish.Row) + abs (Start.Col - Finish.Col));

   procedure Solve_By_Cheating (Longest_Cheat : Natural := 2) is
      Ingress, Egress : Location_Record;

      type Cheat_Record is record
         Ingress, Egress : Location_Record;
         Path_Length     : Natural;
      end record;

      overriding
      function "=" (Left, Right : Cheat_Record) return Boolean
      is (Left.Ingress = Right.Ingress and then Left.Egress = Right.Egress);

      function "<" (Left, Right : Cheat_Record) return Boolean
      is (Left.Ingress < Right.Ingress
          or else (Left.Ingress = Right.Ingress
                   and then Left.Egress < Right.Egress));

      function New_Cheat
        (Ingress, Egress : Location_Record; Path_Length : Natural)
         return Cheat_Record
      is (((if Ingress < Egress then Ingress else Egress),
           (if Ingress < Egress then Egress else Ingress),
           Path_Length));

      package Cheat_Sets is new
        Ada.Containers.Ordered_Sets (Element_Type => Cheat_Record);

      Cheats : Cheat_Sets.Set;
   begin
      for Row in Play_Range loop
         for Col in Play_Range when Map (Row, Col) /= Wall loop
            Ingress := (Row, Col);
            for Row_Offset
              in -Longest_Cheat .. Longest_Cheat
              when Row + Row_Offset in Play_Range
            loop
               for Col_Offset
                 in -Longest_Cheat .. Longest_Cheat
                 when abs (Col_Offset) + abs (Row_Offset) in 1 .. Longest_Cheat
                 and then Col + Col_Offset in Play_Range
               loop
                  Egress :=
                    (Ingress.Row + Row_Offset, Ingress.Col + Col_Offset);
                  if Map (Egress.Row, Egress.Col) /= Wall then
                     if Steps_From_Start (Ingress.Row, Ingress.Col)
                       + Steps_To_Finish (Egress.Row, Egress.Col)
                       + Distance (Ingress, Egress)
                       < (if Doing_Example then Baseline else Baseline - 99)
                     then
                        Cheats.Include
                          (New_Cheat
                             (Ingress,
                              Egress,
                              Baseline
                              - (Steps_From_Start (Ingress.Row, Ingress.Col)
                                 + Steps_To_Finish (Egress.Row, Egress.Col)
                                 + Distance (Ingress, Egress))));
                     end if;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      IO.Put_Line
        ("found"
         & Cheats.Length'Image
         & " cheats of max length"
         & Longest_Cheat'Image);
      if Doing_Example then
         for Cheat of Cheats loop
            IO.Put_Line
              (Cheat.Ingress.Row'Image
               & Cheat.Ingress.Col'Image
               & " ->"
               & Cheat.Egress.Row'Image
               & Cheat.Egress.Col'Image
               & ":"
               & Cheat.Path_Length'Image);
         end loop;
      end if;
   end Solve_By_Cheating;

begin
   Map_IO.Read_Input;
   Map_2D.Map (Map_IO.Start_Location.Row, Map_IO.Start_Location.Col) := Empty;
   Map_2D.Map (Map_IO.End_Location.Row, Map_IO.End_Location.Col) := Empty;
   Solve;
   IO.Put_Line ("The baseline is" & Baseline'Image);
   Solve_By_Cheating;
   Solve_By_Cheating (20);
end Day20;
