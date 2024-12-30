pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
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
      Motions : in out Motion_Vector) is
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

   function "<" (Left, Right : Motion_Vector) return Boolean is
   begin
      if Natural (Left.Length) < Natural (Right.Length) then
         return True;
      elsif Natural (Left.Length) > Natural (Right.Length) then
         return False;
      else
         for Ith in Left.First_Index .. Left.Last_Index loop
            if Left (Ith) < Right (Ith) then
               return True;
            elsif Left (Ith) > Right (Ith) then
               return False;
            end if;
         end loop;
      end if;
      return False;
   end "<";

   type Value is range 0 .. 2 ** 64 - 1;

   package Motion_Count_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Motion_Vector,
        Element_Type => Value);

   procedure Update_Motions
     (Curr_Motions : in out Motion_Count_Maps.Map;
      Motions      : Motion_Vector;
      Number       : Value)
   is
      Motion_Key : Motion_Vector;
      Ith, Jth   : Positive;
   begin
      Ith := 1;
      while Ith <= Motions.Last_Index loop
         --  Ith should never actually equal Motions.Last_Index
         if Motions (Ith) = Direction_A then
            Motion_Key.Clear;
            Motion_Key.Append (Direction_A);
            Jth := Ith;
         else
            Jth := Ith + 1;
            while Motions (Jth) /= Direction_A loop
               Jth := @ + 1;
            end loop;
            Motion_Key.Clear;
            for Kth in Ith .. Jth loop
               Motion_Key.Append (Motions (Kth));
            end loop;
         end if;
         if not Curr_Motions.Contains (Motion_Key) then
            Curr_Motions.Insert (Motion_Key, Number);
         else
            Curr_Motions.Replace
              (Motion_Key, Curr_Motions (Motion_Key) + Number);
         end if;
         Ith := Jth + 1;
      end loop;
   end Update_Motions;

   procedure Complexity_At_Depth (Depth : Positive := 3) is
      Motions                    : Motion_Vector;
      Curr_Motions, Next_Motions : Motion_Count_Maps.Map;
      Curr_Doorbot               : Door_Key;
      Curr_Press                 : Direction_Key;
      Motion                     : Keypad_Motion;
      Result                     : Value := 0;
      Ith, Jth                   : Natural;
   begin
      for Code of Door_Key_Codes loop
         Motions.Clear;
         Curr_Motions.Clear;
         --  determine doorbot's needs
         Curr_Doorbot := Door_A;
         for Next_Doorbot of Code loop
            Motion := Door_Motions (Curr_Doorbot, Next_Doorbot);
            Move_Doorbot (Curr_Doorbot, Motion, Motions);
            Motions.Append (Direction_A);
            Curr_Doorbot := Next_Doorbot;
         end loop;
         --  initialize map
         IO.Put_Line (Code'Image);
         Update_Motions (Curr_Motions, Motions, 1);
         for Cursor in Curr_Motions.Iterate loop
            for Key of Motion_Count_Maps.Key (Cursor) loop
               IO.Put (Serialize (Key));
            end loop;
            IO.Put_Line (Value'Image (Motion_Count_Maps.Element (Cursor)));
         end loop;
         IO.Put_Line ("----");
         --  propagate
         for Ith in 2 .. Depth loop
            Next_Motions.Clear;
            for Cursor in Curr_Motions.Iterate loop
               declare
                  Sequence : constant Motion_Vector :=
                    Motion_Count_Maps.Key (Cursor);
                  Number   : constant Value :=
                    Motion_Count_Maps.Element (Cursor);
               begin
                  Curr_Press := Direction_A;
                  Motions.Clear;
                  for Next_Press of Sequence loop
                     IO.Put (Serialize (Next_Press));
                     Motion := Direction_Motions (Curr_Press, Next_Press);
                     Move_Dirbot (Curr_Press, Motion, Motions);
                     Motions.Append (Direction_A);
                     Curr_Press := Next_Press;
                  end loop;
                  IO.Put (" -> ");
                  for Key of Motions loop
                     IO.Put (Serialize (Key));
                  end loop;
                  IO.New_Line;
                  Update_Motions (Next_Motions, Motions, Number);
               end;
            end loop;
            Curr_Motions := Next_Motions;
            for Cursor in Curr_Motions.Iterate loop
               for Key of Motion_Count_Maps.Key (Cursor) loop
                  IO.Put (Serialize (Key));
               end loop;
               IO.Put_Line (Value'Image (Motion_Count_Maps.Element (Cursor)));
            end loop;
            IO.Put_Line ("----");
         end loop;
         for Cursor in Curr_Motions.Iterate loop
            Result :=
              @
              + Value (Motion_Count_Maps.Key (Cursor).Length)
                * Motion_Count_Maps.Element (Cursor)
                * Value (Numeric_Part (Code));
         end loop;
         IO.Put_Line ("====");
      end loop;
      IO.Put_Line ("total complexity is" & Result'Image);
   end Complexity_At_Depth;

begin
   Read_Input;
   Setup_Motions;
   Complexity_At_Depth;
   Complexity_At_Depth (26);
end Day21;
