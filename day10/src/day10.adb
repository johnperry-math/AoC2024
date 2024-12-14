pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Common;

procedure Day10 is

   package IO renames Ada.Text_IO;

   package Motion_2D renames Common.Two_Dimensional_Motion;

   Doing_Example : constant Boolean := False;

   Dimension : constant Positive := (if Doing_Example then 8 else 40);

   subtype Index_Range is Integer range 1 .. Dimension;
   subtype Digit is Integer range 0 .. 9;

   Map : array (Index_Range, Index_Range) of Digit;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      for Row in Index_Range loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Col in Index_Range loop
               Map (Row, Col) :=
                 Character'Pos (Line (Col)) - Character'Pos ('0');
            end loop;
         end;
      end loop;
   end Read_Input;

   type Location_Record is record
      Row, Col : Index_Range;
   end record;

   function "<" (Left, Right : Location_Record) return Boolean
   is (Left.Row < Right.Row
       or else (Left.Row = Right.Row and then Left.Col < Right.Col));

   package Location_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Location_Record);

   function "=" (Left, Right : Location_Sets.Set) return Boolean
   is (Left.Is_Subset (Right) and then Right.Is_Subset (Left));

   package Trail_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Location_Record,
        Element_Type => Location_Sets.Set);

   Heads_And_Summits : Trail_Maps.Map;

   type Search_Record is record
      Trailhead : Location_Record;
      Current   : Location_Record;
   end record;

   function "<" (Left, Right : Search_Record) return Boolean
   is (Left.Trailhead < Right.Trailhead
       or else (Left.Trailhead = Right.Trailhead
                and then Left.Current < Right.Current));

   package Search_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Search_Record);

   package Search_Q_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Search_Record);
   package Search_Qs is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Queue_Interfaces => Search_Q_Interface);

   procedure Prime_Pump
     (Queue : out Search_Qs.Queue; Trails : out Trail_Maps.Map)
   is
      Trailhead : Location_Record;
      None      : Location_Sets.Set;
   begin
      for Row in Index_Range loop
         for Col in Index_Range loop
            if Map (Row, Col) = 0 then
               Trailhead := Location_Record'(Row, Col);
               Trails.Insert (Trailhead, None.Copy);
               Queue.Enqueue (Search_Record'(Trailhead, Trailhead));
            end if;
         end loop;
      end loop;
      IO.Put_Line ("found" & Queue.Current_Use'Image & " starting positions");
   end Prime_Pump;

   function Motion_Remains_In_Map
     (Location : Location_Record; Offset : Motion_2D.Drc) return Boolean
   is (Integer (Location.Row) + Offset.DRow in Index_Range
       and then Integer (Location.Col) + Offset.DCol in Index_Range);

   procedure Part_1 is
      Result : Natural := 0;
      To_Do  : Search_Qs.Queue;
      Done   : Search_Sets.Set;
      Search : Search_Record;
   begin
      Prime_Pump (To_Do, Heads_And_Summits);
      while Natural (To_Do.Current_Use) > 0 loop
         To_Do.Dequeue (Search);
         for Dir in Motion_2D.Direction loop
            declare
               Curr        : constant Location_Record := Search.Current;
               Height      : constant Digit := Map (Curr.Row, Curr.Col);
               Offset      : constant Motion_2D.Drc := Motion_2D.Deltas (Dir);
               Next        : Search_Record;
               Next_Height : Digit;
            begin
               if Motion_Remains_In_Map (Curr, Offset) then
                  Next :=
                    (Search.Trailhead,
                     (Curr.Row + Offset.DRow, Curr.Col + Offset.DCol));
                  Next_Height := Map (Next.Current.Row, Next.Current.Col);
                  if Height + 1 = Next_Height then
                     if not Done.Contains (Next) then
                        if Next_Height = 9 then
                           Heads_And_Summits (Search.Trailhead).Include
                             (Next.Current);
                        else
                           To_Do.Enqueue (Next);
                        end if;
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end loop;
      for Trail of Heads_And_Summits loop
         Result := @ + Natural (Trail.Length);
      end loop;
      IO.Put_Line ("The sum of scores on the trail map is" & Result'Image);
   end Part_1;

   function Distance (Start, Finish : Location_Record) return Natural
   is (abs (Start.Row - Finish.Row) + abs (Start.Col - Finish.Col));

   procedure Part_2 is
      Result : Natural := 0;
      To_Do  : Search_Qs.Queue;
      Search : Search_Record;
   begin
      for Cursor in Heads_And_Summits.Iterate loop
         declare
            Head : constant Location_Record := Trail_Maps.Key (Cursor);
         begin
            for Summit of Heads_And_Summits (Head) loop
               To_Do.Enqueue ((Head, Summit));
            end loop;
         end;
      end loop;
      while Natural (To_Do.Current_Use) > 0 loop
         To_Do.Dequeue (Search);
         for Dir in Motion_2D.Direction loop
            declare
               Curr        : constant Location_Record := Search.Current;
               Height      : constant Digit := Map (Curr.Row, Curr.Col);
               Offset      : constant Motion_2D.Drc := Motion_2D.Deltas (Dir);
               Next        : Search_Record;
               Next_Height : Digit;
            begin
               if Motion_Remains_In_Map (Curr, Offset) then
                  Next :=
                    (Search.Trailhead,
                     (Curr.Row + Offset.DRow, Curr.Col + Offset.DCol));
                  Next_Height := Map (Next.Current.Row, Next.Current.Col);
                  if Height - 1 = Next_Height then
                     if Next.Current = Search.Trailhead then
                        Result := @ + 1;
                     elsif Distance (Next.Trailhead, Next.Current)
                       <= Next_Height
                     then
                        To_Do.Enqueue (Next);
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end loop;
      IO.Put_Line ("The sum of trailhead ratings is" & Result'Image);
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day10;
