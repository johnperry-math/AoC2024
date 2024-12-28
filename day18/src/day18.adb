pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

with Common;

procedure Day18 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   Memory_Length : constant Positive := (if Doing_Example then 7 else 71);

   subtype Memory_Range is Natural range 0 .. Memory_Length;

   package Memory_IO is new IO.Integer_IO (Num => Memory_Range);

   package Memory_Map is new
     Common.Two_Dimensional_Map
       (Row_Start  => 0,
        Col_Start  => 0,
        Row_Length => Memory_Length,
        Col_Length => Memory_Length,
        Object     => Boolean);

   subtype Location_Record is Memory_Map.Location_Record;
   use all type Location_Record;
   type Path_Record is record
      Location : Location_Record;
      Steps    : Natural;
   end record;

   package Memory_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Location_Record);

   Byte_Positions : Memory_Vecs.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));

      while not IO.End_Of_File (Input) loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Position : Positive;
            Row, Col : Memory_Range;
         begin
            Memory_IO.Get (Line, Col, Position);
            Position := @ + 2;
            Memory_IO.Get (Line (Position .. Line'Last), Row, Position);
            Byte_Positions.Append (Location_Record'(Row, Col));
         end;
      end loop;
   end Read_Input;

   package Location_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Location_Record);

   package Location_Q_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Path_Record);
   package Location_Qs is new
     Ada.Containers.Unbounded_Synchronized_Queues (Location_Q_Interfaces);

   Blocked renames Memory_Map.Map;
   Goal : constant Location_Record := (Memory_Length - 1, Memory_Length - 1);

   procedure Initial_Setup is
   begin
      Blocked := [others => [others => False]];
      for Ith in 1 .. (if Doing_Example then 12 else 1024) loop
         Blocked (Byte_Positions (Ith).Row, Byte_Positions (Ith).Col) := True;
      end loop;
   end Initial_Setup;

   function Search_For_Exit return Path_Record is
      use all type Common.Two_Dimensional_Motion.Drc;
      Deltas renames Common.Two_Dimensional_Motion.Deltas;
      subtype Direction is Common.Two_Dimensional_Motion.Direction;

      To_Do : Location_Qs.Queue;
      Done  : Location_Sets.Set;

      Curr, Next : Path_Record;
      Start      : constant Path_Record := ((0, 0), 0);

   begin
      To_Do.Enqueue (Start);
      while Natural (To_Do.Current_Use) > 0 loop
         To_Do.Dequeue (Curr);
         exit when Curr.Location = Goal;
         Done.Include (Curr.Location);
         Next.Steps := Curr.Steps + 1;
         for Dir
           in Direction
           when Memory_Map.Can_Move (Curr.Location, Deltas (Dir))
         loop
            Next.Location := Curr.Location + Deltas (Dir);
            if (not Done.Contains (Next.Location))
              and then (not Blocked (Next.Location.Row, Next.Location.Col))
            then
               To_Do.Enqueue (Next);
               Done.Include (Next.Location);
            end if;
         end loop;
      end loop;
      return Curr;
   end Search_For_Exit;

   procedure Part_1 is
      Arrival : Path_Record := Search_For_Exit;
   begin
      if Arrival.Location = Goal then
         IO.Put_Line
           ("It took" & Arrival.Steps'Image & " steps to reach the goal.");
      else
         IO.Put_Line ("Something went terribly wrong in part 1...");
      end if;
   end Part_1;

   procedure Part_2 is
      Arrival      : Path_Record;
      Last_Success : Positive := (if Doing_Example then 12 else 1024);
      Last_Failure : Positive := Byte_Positions.Last_Index;
      Test_Value   : Positive;
      Old_Blocked  : Memory_Map.Map_Array := [others => [others => False]];
      Next_Block   : Location_Record;
   begin
      while Last_Success < Last_Failure - 1 loop
         Test_Value := (Last_Success + Last_Failure) / 2;
         Blocked := Old_Blocked;
         for Ith in 1 .. Test_Value loop
            Next_Block := Byte_Positions (Ith);
            Blocked (Next_Block.Row, Next_Block.Col) := True;
         end loop;
         Arrival := Search_For_Exit;
         if Arrival.Location = Goal then
            Last_Success := Test_Value;
         else
            Last_Failure := Test_Value;
         end if;
      end loop;
      if Arrival.Location /= Goal then
         Next_Block := Byte_Positions (Last_Failure);
         IO.Put_Line
           ("The path is blocked once the byte at"
            & Next_Block.Col'Image
            & ","
            & Next_Block.Row'Image
            & " drops");
      else
         IO.Put_Line ("Something went terribly wrong in part 2...");
      end if;
   end Part_2;

begin
   Read_Input;
   Initial_Setup;
   Part_1;
   Part_2;
end Day18;
