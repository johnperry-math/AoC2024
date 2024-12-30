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

   function Solve (Map : Map_2D.Map_Array := Map_2D.Map) return Natural is

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

   begin
      To_Do.Enqueue ((Map_IO.Start_Location, 0));
      while Natural (To_Do.Current_Use) > 0 loop
         To_Do.Dequeue (Curr);
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
      return Curr.Length;
   end Solve;

   procedure Part_1 is
      Map    : Map_2D.Map_Array;
      Result : Natural := 0;
   begin
      for Row in 2 .. Dimension - 1 loop
         IO.Put_Line ("Cheating on row" & Row'Image);
         for Col in 2 .. Dimension - 1 loop
            Map := Map_2D.Map;
            if Map (Row, Col) = Wall then
               Map (Row, Col) := Empty;
               declare
                  Time : Natural := Solve (Map);
               begin
                  if Baseline - Time >= 100 then
                     Result := @ + 1;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      IO.Put_Line (Result'Image & " cheats save at least 100 picoseconds");
   end Part_1;

begin
   Map_IO.Read_Input;
   Map_2D.Map (Map_IO.Start_Location.Row, Map_IO.Start_Location.Col) := Empty;
   Map_2D.Map (Map_IO.End_Location.Row, Map_IO.End_Location.Col) := Empty;
   Baseline := Solve;
   IO.Put_Line ("The baseline is" & Baseline'Image);
   Part_1;
end Day20;
