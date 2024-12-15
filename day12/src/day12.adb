pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Common;

procedure Day12 is

   package IO renames Ada.Text_IO;
   package Motion_2D renames Common.Two_Dimensional_Motion;

   Doing_Example : constant Natural := 0;

   Dimension : constant Positive :=
     (case Doing_Example is
        when 0 => 140,
        when 1 => 4,
        when 2 => 5,
        when 3 => 10,
        when others => 6);

   subtype Index_Range is Positive range 1 .. Dimension;

   subtype Garden_Plot is Character range 'A' .. 'Z';

   Map : array (Index_Range, Index_Range) of Garden_Plot;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (case Doing_Example is
            when 0 => "input.txt",
            when others => "example" & Doing_Example'Image & ".txt"));
      for Row in Index_Range loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Col in Index_Range loop
               Map (Row, Col) := Line (Col);
            end loop;
         end;
      end loop;
   end Read_Input;

   type Location_Record is record
      Row, Col : Integer;
   end record;

   function "<" (Left, Right : Location_Record) return Boolean
   is (Left.Row < Right.Row
       or else (Left.Row = Right.Row and then Left.Col < Right.Col));

   function Min (Left, Right : Location_Record) return Location_Record
   is (if Left < Right then Left else Right);

   function Max (Left, Right : Location_Record) return Location_Record
   is (if Left < Right then Right else Left);

   type Fence_Record is record
      First, Second : Location_Record;
   end record;

   function "<" (Left, Right : Fence_Record) return Boolean
   is (Left.First < Right.First
       or else (Left.First = Right.First and then Left.Second < Right.Second));

   package Location_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Location_Record);

   package Fence_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Fence_Record);

   function In_Range
     (Row, Col : Integer; Offset : Motion_2D.Drc) return Boolean
   is (Row + Offset.DRow in Index_Range
       and then Col + Offset.DCol in Index_Range);

   package Location_Q_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Location_Record);
   package Location_Qs is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Queue_Interfaces => Location_Q_Interfaces);

   function Cost_Of_Plot
     (Start : Location_Record; All_Done : in out Location_Sets.Set)
      return Natural
   is
      Region_Locations   : Location_Sets.Set;
      Region_Fences      : Fence_Sets.Set;
      To_Do              : Location_Qs.Queue;
      Plot               : constant Garden_Plot := Map (Start.Row, Start.Col);
      Location, Neighbor : Location_Record;
      Offset             : Motion_2D.Drc;
   begin
      To_Do.Enqueue (Start);
      while Natural (To_Do.Current_Use) > 0 loop
         To_Do.Dequeue (Location);
         if not All_Done.Contains (Location) then
            All_Done.Include (Location);
            Region_Locations.Include (Location);
            for Dir in Motion_2D.Direction loop
               Offset := Motion_2D.Deltas (Dir);
               Neighbor :=
                 (Location.Row + Offset.DRow, Location.Col + Offset.DCol);
               if In_Range (Location.Row, Location.Col, Offset)
                 and then Plot
                          = Map
                              (Location.Row + Offset.DRow,
                               Location.Col + Offset.DCol)
               then
                  if not All_Done.Contains (Neighbor) then
                     To_Do.Enqueue (Neighbor);
                  end if;
               else
                  Region_Fences.Include
                    ((First  => Min (Neighbor, Location),
                      Second => Max (Neighbor, Location)));
               end if;
            end loop;
         end if;
      end loop;
      return
        Natural (Region_Locations.Length) * Natural (Region_Fences.Length);
   end Cost_Of_Plot;

   procedure Part_1 is
      Result   : Natural := 0;
      All_Done : Location_Sets.Set;
   begin
      for Row in Index_Range loop
         for Col in Index_Range when not All_Done.Contains ((Row, Col)) loop
            Result := @ + Cost_Of_Plot ((Row, Col), All_Done);
         end loop;
      end loop;
      IO.Put_Line ("The total price of fencing is" & Result'Image);
   end Part_1;

   package Fence_Q_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Fence_Record);
   package Fence_Qs is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Queue_Interfaces => Fence_Q_Interfaces);

   type Face_Enum is (To_1st, To_2nd);

   function Facing_Direction
     (Fence : Fence_Record; Plot : Garden_Plot) return Face_Enum
   is (if Fence.First.Col = Fence.Second.Col
       then
         --  this is an east-west fence
         (if Fence.First.Row in Index_Range
            and then Map (Fence.First.Row, Fence.First.Col) = Plot
          then To_1st
          else To_2nd)
       else
         -- this is a north-south fence
         (if Fence.First.Col in Index_Range
            and then Map (Fence.First.Row, Fence.First.Col) = Plot
          then To_1st
          else To_2nd));

   function Number_Of_Sides
     (Plot : Garden_Plot; Fences : Fence_Sets.Set) return Natural
   is
      Result  : Natural := 0;
      To_Do   : Fence_Qs.Queue;
      Current : Fence_Record;
      Done    : Fence_Sets.Set;

      Face                            : Face_Enum;
      First_Location, Second_Location : Location_Record;
      First_Neighbor, Second_Neighbor : Fence_Record;
   begin
      --  for each piece of fencing we encounter
      --  that isn't on a side already counted,
      --  count it as a side, then walk along the fence to record
      --  other fence pieces adjoined to it
      for Fence of Fences when not Done.Contains (Fence) loop
         --  not previously counted, so it's a new side
         Result := @ + 1;
         To_Do.Enqueue (Fence);
         -- determine which side is being "fenced in"
         Face := Facing_Direction (Fence, Plot);

         while Natural (To_Do.Current_Use) > 0 loop
            To_Do.Dequeue (Current);
            if not Done.Contains (Current) then
               Done.Include (Current);
               --  walk both ways along the fence
               --  Second_Neighbor will usually be a waste of time
               --  but we sometimes need it
               if Current.First.Col = Current.Second.Col then
                  --  this is an east-west fence
                  First_Neighbor := Current;
                  First_Neighbor.First.Col := @ + 1;
                  First_Neighbor.Second.Col := @ + 1;
                  First_Location :=
                    (if Face = To_1st then First_Neighbor.First
                     else First_Neighbor.Second);
                  Second_Neighbor.First.Col := @ - 2;
                  Second_Neighbor.Second.Col := @ - 2;
                  Second_Location :=
                    (if Face = To_1st then Second_Neighbor.First
                     else Second_Neighbor.Second);
               else
                  -- this is a north-south fence
                  First_Neighbor := Current;
                  First_Neighbor.First.Row := @ + 1;
                  First_Neighbor.Second.Row := @ + 1;
                  First_Location :=
                    (if Face = To_1st then First_Neighbor.First
                     else First_Neighbor.Second);
                  Second_Neighbor.First.Row := @ - 2;
                  Second_Neighbor.Second.Row := @ - 2;
                  Second_Location :=
                    (if Face = To_1st then Second_Neighbor.First
                     else Second_Neighbor.Second);
               end if;
               --  check if we need to keep walking
               --  in order to identify more pieces of this side
               if Fences.Contains (First_Neighbor)
                 and then Map (First_Location.Row, First_Location.Col) = Plot
                 and then (not Done.Contains (First_Neighbor))
               then
                  To_Do.Enqueue (First_Neighbor);
               end if;
               if Fences.Contains (Second_Neighbor)
                 and then Map (Second_Location.Row, Second_Location.Col) = Plot
                 and then (not Done.Contains (Second_Neighbor))
               then
                  To_Do.Enqueue (Second_Neighbor);
               end if;
            end if;
         end loop;
      end loop;
      return Result;
   end Number_Of_Sides;

   function Discounted_Cost_Of_Plot
     (Start : Location_Record; All_Done : in out Location_Sets.Set)
      return Natural
   is
      Region_Locations   : Location_Sets.Set;
      Region_Fences      : Fence_Sets.Set;
      To_Do              : Location_Qs.Queue;
      Plot               : constant Garden_Plot := Map (Start.Row, Start.Col);
      Location, Neighbor : Location_Record;
      Offset             : Motion_2D.Drc;
   begin
      To_Do.Enqueue (Start);
      while Natural (To_Do.Current_Use) > 0 loop
         To_Do.Dequeue (Location);
         if not All_Done.Contains (Location) then
            All_Done.Include (Location);
            Region_Locations.Include (Location);
            for Dir in Motion_2D.Direction loop
               Offset := Motion_2D.Deltas (Dir);
               Neighbor :=
                 (Location.Row + Offset.DRow, Location.Col + Offset.DCol);
               if In_Range (Location.Row, Location.Col, Offset)
                 and then Plot
                          = Map
                              (Location.Row + Offset.DRow,
                               Location.Col + Offset.DCol)
               then
                  if not All_Done.Contains (Neighbor) then
                     To_Do.Enqueue (Neighbor);
                  end if;
               else
                  Region_Fences.Include
                    ((First  => Min (Neighbor, Location),
                      Second => Max (Neighbor, Location)));
               end if;
            end loop;
         end if;
      end loop;
      --  IO.Put (Plot);
      return
        Natural (Region_Locations.Length)
        * Number_Of_Sides (Plot, Region_Fences);
   end Discounted_Cost_Of_Plot;

   procedure Part_2 is
      Result   : Natural := 0;
      All_Done : Location_Sets.Set;
   begin
      for Row in Index_Range loop
         for Col in Index_Range when not All_Done.Contains ((Row, Col)) loop
            Result := @ + Discounted_Cost_Of_Plot ((Row, Col), All_Done);
         end loop;
      end loop;
      IO.Put_Line ("The discounted price of fencing is" & Result'Image);
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day12;
