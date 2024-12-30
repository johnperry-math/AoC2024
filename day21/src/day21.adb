pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;

procedure Day21 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   subtype Door_Rows is Positive range 1 .. 4;
   subtype Door_Cols is Positive range 1 .. 3;

   type Door_Record is record
      Row : Door_Rows;
      Col : Door_Cols;
   end record;

   Invalid_Door_Location : constant Door_Record := (4, 1);

   type Door_Key is
     (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Door_A);

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Door_Key
   is (case Symbol is
         when '0' .. '9' =>
           Door_Key'Val (Character'Pos (Symbol) - Character'Pos ('0')),
         when 'A' => Door_A,
         when others => raise Invalid_Symbol with Symbol'Image);

   type Door_Key_Array is array (1 .. 4) of Door_Key;

   Door_Key_Codes : array (1 .. 5) of Door_Key_Array;

   Door_Key_Positions : constant array (Door_Key) of Door_Record :=
     [Seven  => (1, 1),
      Eight  => (1, 2),
      Nine   => (1, 3),
      Four   => (2, 1),
      Five   => (2, 2),
      Six    => (2, 3),
      One    => (3, 1),
      Two    => (3, 2),
      Three  => (3, 3),
      Zero   => (4, 2),
      Door_A => (4, 3)];

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      for Ith in Door_Key_Codes'Range loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Jth in Door_Key_Array'Range loop
               Door_Key_Codes (Ith) (Jth) := Deserialize (Line (Jth));
            end loop;
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   type Direction_Key is (Up, Down, Left, Right, Direction_A);

   function Serialize (Direction : Direction_Key) return Character
   is (case Direction is
         when Up => '^',
         when Down => 'v',
         when Left => '<',
         when Right => '>',
         when Direction_A => 'A');

   subtype Direction_Rows is Positive range 1 .. 2;
   subtype Direction_Cols is Positive range 1 .. 3;

   type Direction_Record is record
      Row : Direction_Rows;
      Col : Direction_Cols;
   end record;

   Invalid_Direction_Location : constant Direction_Record := (1, 1);

   Direction_Key_Positions :
     constant array (Direction_Key) of Direction_Record :=
       [Up          => (1, 2),
        Down        => (2, 2),
        Left        => (2, 1),
        Right       => (2, 3),
        Direction_A => (1, 3)];

   type Keypad_Motion is record
      DRow, DCol : Integer;
   end record;

   Door_Motions      : array (Door_Key, Door_Key) of Keypad_Motion;
   Direction_Motions : array (Direction_Key, Direction_Key) of Keypad_Motion;

   procedure Setup_Motions is
   begin
      for Start_Key in Door_Key loop
         declare
            Start : constant Door_Record := Door_Key_Positions (Start_Key);
         begin
            for Finish_Key in Door_Key loop
               declare
                  Finish : constant Door_Record :=
                    Door_Key_Positions (Finish_Key);
               begin
                  Door_Motions (Start_Key, Finish_Key) :=
                    (Finish.Row - Start.Row, Finish.Col - Start.Col);
               end;
            end loop;
         end;
      end loop;
      for Start_Key in Direction_Key loop
         declare
            Start : constant Direction_Record :=
              Direction_Key_Positions (Start_Key);
         begin
            for Finish_Key in Direction_Key loop
               declare
                  Finish : constant Direction_Record :=
                    Direction_Key_Positions (Finish_Key);
               begin
                  Direction_Motions (Start_Key, Finish_Key) :=
                    (Finish.Row - Start.Row, Finish.Col - Start.Col);
               end;
            end loop;
         end;
      end loop;
   end Setup_Motions;

   package Motion_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Direction_Key);
   subtype Motion_Vector is Motion_Vecs.Vector;

   function Digit (Key : Door_Key) return Natural
   is (case Key is
         when Zero .. Nine => Door_Key'Pos (Key),
         when Door_A => raise Invalid_Symbol);

   function Numeric_Part (Code : Door_Key_Array) return Natural
   is (Digit (Code (1)) * 100 + Digit (Code (2)) * 10 + Digit (Code (3)));

   procedure Move_Doorbot
     (Start : Door_Key; Motion : Keypad_Motion; Motions : in out Motion_Vector)
   is
      Start_Position : constant Door_Record := Door_Key_Positions (Start);
   begin
      if Motion.DCol < 0 then
         --  moving left
         if Start in One .. Nine
           or else (Start = Door_A and then Motion.DCol = -1)
         then
            for Each in 1 .. abs (Motion.DCol) loop
               Motions.Append (Left);
            end loop;
            for Each in 1 .. abs (Motion.DRow) loop
               Motions.Append (if Motion.DRow < 0 then Up else Down);
            end loop;
         else
            --  0 or A, with too large a leftward motion
            for Each in 1 .. abs (Motion.DRow) loop
               Motions.Append (if Motion.DRow < 0 then Up else Down);
            end loop;
            for Each in 1 .. abs (Motion.DCol) loop
               Motions.Append (Left);
            end loop;
         end if;
      elsif Motion.DRow > 0 then
         --  moving down and not-left
         if Start_Position.Col = 1 and then Motion.DRow = Start_Position.Row
         then
            --  1, 4, or 7, with too large a downward motion
            for Each in 1 .. Motion.DCol loop
               Motions.Append (Right);
            end loop;
            for Each in 1 .. Motion.DRow loop
               Motions.Append (Down);
            end loop;
         else
            for Each in 1 .. Motion.DRow loop
               Motions.Append (Down);
            end loop;
            for Each in 1 .. Motion.DCol loop
               Motions.Append (Right);
            end loop;
         end if;
      else
         --  moving up and not-left
         for Each in 1 .. abs (Motion.DRow) loop
            Motions.Append (Up);
         end loop;
         for Each in 1 .. Motion.DCol loop
            Motions.Append (Right);
         end loop;
      end if;
   end Move_Doorbot;

   procedure Move_Dirbot
     (Start   : Direction_Key;
      Motion  : Keypad_Motion;
      Motions : in out Motion_Vector)
   is
      --  Start_Position : constant Direction_Record :=
      --    Direction_Key_Positions (Start);
   begin
      if Motion.DCol < 0 then
         --  moving left
         if (Start = Down or else Start = Right)
           or else (Start = Direction_A and then Motion.DCol = -1)
         then
            for Each in 1 .. abs (Motion.DCol) loop
               Motions.Append (Left);
            end loop;
            for Each in 1 .. abs (Motion.DRow) loop
               Motions.Append (if Motion.DRow < 0 then Up else Down);
            end loop;
         else
            --  Up or A, with too large a leftward motion
            for Each in 1 .. abs (Motion.DRow) loop
               Motions.Append (Down);
            end loop;
            for Each in 1 .. abs (Motion.DCol) loop
               Motions.Append (Left);
            end loop;
         end if;
      elsif Motion.DRow < 0 then
         --  moving up and not-left
         if Start = Left then
            for Each in 1 .. Motion.DCol loop
               Motions.Append (Right);
            end loop;
            for Each in 1 .. abs (Motion.DRow) loop
               Motions.Append (Up);
            end loop;
         else
            for Each in 1 .. abs (Motion.DRow) loop
               Motions.Append (Up);
            end loop;
            for Each in 1 .. Motion.DCol loop
               Motions.Append (Right);
            end loop;
         end if;
      else
         --  moving down and not-left
         for Each in 1 .. Motion.DRow loop
            Motions.Append (Down);
         end loop;
         for Each in 1 .. Motion.DCol loop
            Motions.Append (Right);
         end loop;
      end if;
   end Move_Dirbot;

   procedure Part_1 is
      Dirbot_1, Dirbot_2, Mebot : Motion_Vector;
      Curr_Doorbot              : Door_Key;
      Curr_Dirbot               : Direction_Key;
      Motion                    : Keypad_Motion;
      Result                    : Natural := 0;
   begin
      for Code of Door_Key_Codes loop
         Dirbot_1.Clear;
         Dirbot_2.Clear;
         Mebot.Clear;
         Curr_Doorbot := Door_A;
         for Next_Doorbot of Code loop
            Motion := Door_Motions (Curr_Doorbot, Next_Doorbot);
            Move_Doorbot (Curr_Doorbot, Motion, Dirbot_1);
            Dirbot_1.Append (Direction_A);
            Curr_Doorbot := Next_Doorbot;
         end loop;
         IO.Put_Line ("one" & Dirbot_1.Length'Image);
         for Each of Dirbot_1 loop
            IO.Put (Serialize (Each));
         end loop;
         IO.New_Line;
         Curr_Dirbot := Direction_A;
         for Next_Dirbot of Dirbot_1 loop
            Motion := Direction_Motions (Curr_Dirbot, Next_Dirbot);
            Move_Dirbot (Curr_Dirbot, Motion, Dirbot_2);
            Dirbot_2.Append (Direction_A);
            Curr_Dirbot := Next_Dirbot;
         end loop;
         IO.Put_Line ("two" & Dirbot_2.Length'Image);
         for Each of Dirbot_2 loop
            IO.Put (Serialize (Each));
         end loop;
         IO.New_Line;
         Curr_Dirbot := Direction_A;
         for Next_Dirbot of Dirbot_2 loop
            Motion := Direction_Motions (Curr_Dirbot, Next_Dirbot);
            Move_Dirbot (Curr_Dirbot, Motion, Mebot);
            Mebot.Append (Direction_A);
            Curr_Dirbot := Next_Dirbot;
         end loop;
         IO.Put_Line ("me" & Mebot.Length'Image);
         for Each of Mebot loop
            IO.Put (Serialize (Each));
         end loop;
         IO.New_Line;
         IO.Put_Line
           (Mebot.Length'Image & Natural'Image (Numeric_Part (Code)));
         Result := @ + Natural (Mebot.Length) * Numeric_Part (Code);
      end loop;
      IO.Put_Line ("total complexity is" & Result'Image);
   end Part_1;

begin
   Read_Input;
   Setup_Motions;
   Part_1;
end Day21;
