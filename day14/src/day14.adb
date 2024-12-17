pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;

procedure Day14 is

   package IO renames Ada.Text_IO;
   package Int_IO is new IO.Integer_IO (Num => Integer);

   Doing_Example : Boolean := False;

   Row_Size : constant Positive := (if Doing_Example then 7 else 103);
   Col_Size : constant Positive := (if Doing_Example then 11 else 101);
   subtype Row_Range is Natural range 0 .. Row_Size - 1;
   subtype Col_Range is Natural range 0 .. Col_Size - 1;

   type Robot_Record is record
      Col        : Col_Range;
      Row        : Row_Range;
      DCol, DRow : Integer;
   end record;

   package Robot_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Robot_Record);

   All_Robots : Robot_Vecs.Vector;

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
            Position : Positive := 3;
            Robot    : Robot_Record;
         begin
            Int_IO.Get (Line (Position .. Line'Last), Robot.Col, Position);
            Position := @ + 2;
            Int_IO.Get (Line (Position .. Line'Last), Robot.Row, Position);
            Position := @ + 4;
            Int_IO.Get (Line (Position .. Line'Last), Robot.DCol, Position);
            Position := @ + 2;
            Int_IO.Get (Line (Position .. Line'Last), Robot.DRow, Position);
            All_Robots.Append (Robot);
         end;
      end loop;
   end Read_Input;

   Map : array (Row_Range, Col_Range) of Integer;

   procedure Draw_Map (Robots : Robot_Vecs.Vector) is
      --  Map : array (Row_Range, Col_Range) of Natural :=
      --    [others => [others => 0]];
   begin
      Map := [others => [others => 0]];
      for Robot of Robots loop
         Map (Robot.Row, Robot.Col) := @ + 1;
      end loop;
   end Draw_Map;

   procedure Wait_1_Sec (Robots : in out Robot_Vecs.Vector) is
   begin
      for Ith in Robots.First_Index .. Robots.Last_Index loop
         declare
            Robot : Robot_Vecs.Reference_Type := Robots.Reference (Ith);
         begin
            Robot.Row := (@ + Robot.DRow) mod Row_Size;
            Robot.Col := (@ + Robot.DCol) mod Col_Size;
         end;
      end loop;
   end Wait_1_Sec;

   procedure Part_1 is
      Result : Natural;
      Robots : Robot_Vecs.Vector := All_Robots.Copy;

      Q1_Count, Q2_Count, Q3_Count, Q4_Count : Natural := 0;

      Horz_Median : constant Positive := (Row_Size + 1) / 2 - 1;
      Vert_Median : constant Positive := (Col_Size + 1) / 2 - 1;
   begin
      for Ith in Robots.First_Index .. Robots.Last_Index loop
         declare
            Robot : Robot_Vecs.Reference_Type := Robots.Reference (Ith);
         begin
            Robot.Row := (@ + 100 * Robot.DRow) mod Row_Size;
            Robot.Col := (@ + 100 * Robot.DCol) mod Col_Size;
         end;
      end loop;
      for Robot of Robots loop
         if Robot.Row < Horz_Median and then Robot.Col < Vert_Median then
            Q1_Count := @ + 1;
         elsif Robot.Row > Horz_Median and then Robot.Col < Vert_Median then
            Q2_Count := @ + 1;
         elsif Robot.Row < Horz_Median and then Robot.Col > Vert_Median then
            Q3_Count := @ + 1;
         elsif Robot.Row > Horz_Median and then Robot.Col > Vert_Median then
            Q4_Count := @ + 1;
         end if;
      end loop;
      Result := Q1_Count * Q2_Count * Q3_Count * Q4_Count;
      IO.Put_Line ("The safety factor after 100 seconds is" & Result'Image);
   end Part_1;

   procedure Part_2 is
   begin
      for Ith in 1 .. 10_000 loop
         Wait_1_Sec (All_Robots);
         Draw_Map (All_Robots);
         if (for some Row in Row_Range
             => (for some Col in 0 .. Col_Size - 12
                 => (for all Offset in 0 .. 10
                     => Map (Row, Col + Offset) /= 0)))
         then
            for Row in Row_Range loop
               for Col in Col_Range loop
                  IO.Put (if Map (Row, Col) = 0 then '.' else '#');
               end loop;
               IO.New_Line;
            end loop;
            IO.Put_Line ("Found tree after" & Ith'Image & " seconds!");
         end if;
      end loop;
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day14;
