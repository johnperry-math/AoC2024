pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;

procedure Day24 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean  := False;
   Input_Wires   : constant Positive := (if Doing_Example then 5 else 45);
   Output_Wires  : constant Positive := (if Doing_Example then 13 else 46);

   type Logic_Variant is (And_Logic, Or_Logic, Xor_Logic);

   subtype Wire_Name is String (1 .. 3);

   package Wire_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Wire_Name);
   subtype Wire_Set is Wire_Sets.Set;

   package Wire_Value_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Wire_Name, Element_Type => Boolean);
   subtype Wire_Value_Map is Wire_Value_Maps.Map;

   Wire_Values : Wire_Value_Map;

   type Gate_Record (Action : Logic_Variant := Xor_Logic) is record
      Left_Input, Right_Input : Wire_Name;
   end record;

   package Gate_Definition_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Wire_Name, Element_Type => Gate_Record);
   subtype Gate_Definition_Map is Gate_Definition_Maps.Map;

   Gate_Definitions : Gate_Definition_Map;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input, IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      --  known wire values
      loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            exit when Line'Length = 0;
            Wire_Values.Insert
              (Line (1 .. 3), (if Line (6) = '0' then False else True));
         end;
      end loop;
      --  wire logic
      while not IO.End_Of_File (Input) loop
         declare
            Line           : constant String        := IO.Get_Line (Input);
            Logic          : constant Logic_Variant :=
              (if Line (5 .. 7) = "AND" then And_Logic
               elsif Line (5 .. 6) = "OR" then Or_Logic else Xor_Logic);
            Position_Right : constant Positive      :=
              (if Logic = Or_Logic then 8 else 9);
            Left           : constant Wire_Name     := Line (1 .. 3);
            Right          : constant Wire_Name     :=
              Line (Position_Right .. Position_Right + 2);
            Output         : constant Wire_Name     :=
              Line (Position_Right + 7 .. Position_Right + 9);
         begin
            Gate_Definitions.Insert
              (Output,
               Gate_Record'
                 (Action => Logic, Left_Input => Left, Right_Input => Right));
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   function Digit (Symbol : Natural) return Character is
     (Character'Val (Symbol + Character'Pos ('0')));

   type Value is range 0 .. 2**64 - 1;

   function Wire_From_Group_And_Index
     (Group : Character; Ith : Natural) return Wire_Name
   is
      Wire : Wire_Name := "z00";
   begin
      Wire (1) := Group;
      Wire (2) := Digit (Ith / 10);
      Wire (3) := Digit (Ith mod 10);
      return Wire;
   end Wire_From_Group_And_Index;

   function Group_Value (Group : Character) return Value is
      Wire   : Wire_Name;
      Result : Value := 0;
   begin
      for Ith in reverse 0 .. 99 loop
         Result := @ * 2;
         Wire   := Wire_From_Group_And_Index (Group, Ith);
         if Wire_Values.Contains (Wire) then
            Result := @ + (if Wire_Values (Wire) then 1 else 0);
         end if;
      end loop;
      return Result;
   end Group_Value;

   function Evaluate return Value is
      Unknowns : Wire_Set;
      Wire     : Wire_Name;
   begin
      for Cursor in Gate_Definitions.Iterate loop
         Wire := Gate_Definition_Maps.Key (Cursor);
         if not Wire_Values.Contains (Wire) then
            Unknowns.Insert (Wire);
         end if;
      end loop;
      while not Unknowns.Is_Empty loop
         --  IO.Put_Line ("have" & Unknowns.Length'Image & " unknown wires");
         for Wire of Unknowns loop
            declare
               Gate  : constant Gate_Record   := Gate_Definitions (Wire);
               Left  : constant Wire_Name     := Gate.Left_Input;
               Right : constant Wire_Name     := Gate.Right_Input;
               Logic : constant Logic_Variant := Gate.Action;
            begin
               if Wire_Values.Contains (Left)
                 and then Wire_Values.Contains (Right)
               then
                  Wire_Values.Insert
                    (Wire,
                     (case Logic is
                        when And_Logic =>
                          Wire_Values (Left) and then Wire_Values (Right),
                        when Or_Logic =>
                          Wire_Values (Left) or else Wire_Values (Right),
                        when Xor_Logic =>
                          Wire_Values (Left) xor Wire_Values (Right)));
               end if;
            end;
         end loop;
         for Cursor in Wire_Values.Iterate loop
            Wire := Wire_Value_Maps.Key (Cursor);
            Unknowns.Exclude (Wire);
         end loop;
      end loop;
      return Group_Value ('z');
   end Evaluate;

   procedure Set (Group : Character; Index : Natural; New_Value : Boolean) is
      Wire : constant Wire_Name := Wire_From_Group_And_Index (Group, Index);
   begin
      Wire_Values.Include (Wire, New_Value);
   end Set;

   pragma Warnings (Off, "procedure ""Experiment_1"" is not referenced");
   procedure Experiment_1
     (Group : Character; Index : Natural; Do_Both : Boolean := False)
   is
      Group_Name : String (1 .. 1);
   begin
      Group_Name (1) := Group;
      Wire_Values.Clear;
      for Each in 0 .. Input_Wires - 1 loop
         Set ('x', Each, False);
         Set ('y', Each, False);
      end loop;
      if Do_Both then
         Set ('x', Index, True);
         Set ('y', Index, True);
      else
         Set (Group, Index, True);
      end if;
      IO.Put_Line
        ("The result of setting " & Group_Name & Index'Image & " to True is" &
         Value'Image (Evaluate));
   end Experiment_1;

   function Successor (Logic : Logic_Variant) return Logic_Variant is
     (case Logic is when And_Logic => Or_Logic, when Or_Logic => And_Logic,
        when Xor_Logic => And_Logic);
   function Companion (Logic : Logic_Variant) return Logic_Variant is
     (case Logic is when And_Logic => And_Logic, when Or_Logic => Xor_Logic,
        when Xor_Logic => Or_Logic);

   function Input_Gate (Wire : Wire_Name) return Boolean is
     (Wire (1) = 'x' or else Wire (1) = 'y');

   function Primitive_Gate (Gate : Gate_Record) return Boolean is
     (Input_Gate (Gate.Left_Input) or else Input_Gate (Gate.Right_Input));

   function Good_First_Level (Wire : Wire_Name) return Boolean is
      Curr_Gate  : constant Gate_Record := Gate_Definitions (Wire);
      Left_Wire  : constant Wire_Name   := Curr_Gate.Left_Input;
      Right_Wire : constant Wire_Name   := Curr_Gate.Right_Input;

      Xor_Count, Or_Count : Natural := 0;
   begin
      if Curr_Gate.Action /= Xor_Logic then
         IO.Put_Line
           (Wire & " should be XOR_LOGIC but is " & Curr_Gate.Action'Image);
         return False;
      else
         declare
            Left_Gate  : constant Gate_Record := Gate_Definitions (Left_Wire);
            Right_Gate : constant Gate_Record := Gate_Definitions (Right_Wire);
         begin
            case Left_Gate.Action is
               when Or_Logic =>
                  Or_Count := @ + 1;
               when Xor_Logic =>
                  Xor_Count := @ + 1;
               when And_Logic =>
                  IO.Put_Line
                    (Left_Wire & " should be OR or XOR but is " &
                     Left_Gate.Action'Image);
            end case;
            case Right_Gate.Action is
               when Or_Logic =>
                  Or_Count := @ + 1;
               when Xor_Logic =>
                  Xor_Count := @ + 1;
               when And_Logic =>
                  IO.Put_Line
                    (Right_Wire & " should be OR or XOR but is " &
                     Right_Gate.Action'Image);
            end case;
            if Or_Count /= 1 or else Xor_Count /= 1 then
               IO.Put_Line
                 ("One of " & Left_Wire & " & " & Right_Wire &
                  " should be an OR but is not");
               return False;
            end if;
         end;
      end if;
      return True;
   end Good_First_Level;

   function Inputs_Match
     (Gate : Gate_Record; Index : Character) return Boolean is
     (Gate.Left_Input (2) = '0' and then Gate.Left_Input (3) = Index
      and then Gate.Right_Input (2) = '0'
      and then Gate.Right_Input (3) = Index);

   function Base_Wires (Left, Right : Gate_Record) return Boolean is
     ((Left.Action = And_Logic and then Inputs_Match (Left, '0')
       and then Right.Action = Xor_Logic and then Inputs_Match (Right, '1'))
      or else
      (Right.Action = And_Logic and then Inputs_Match (Right, '0')
       and then Left.Action = Xor_Logic and then Inputs_Match (Left, '1')));

   procedure Experiment_3 is
      Left_Logic, Right_Logic : Logic_Variant;
      Curr_Wire               : Wire_Name;
      Initial_Wire            : Wire_Name := "z00";
      Expected_Logic          : Logic_Variant;
   begin
      --  assuming z00 is correct (should be `x00 XOR y00`)
      --  and z01 is correct
      for Ith in 2 .. Output_Wires - 2 loop
         Initial_Wire := Wire_From_Group_And_Index ('z', Ith);
         if Good_First_Level (Initial_Wire) then
            declare
               Curr_Gate : Gate_Record := Gate_Definitions (Initial_Wire);
            begin
               Expected_Logic := And_Logic;
               Curr_Wire      :=
                 (if Gate_Definitions (Curr_Gate.Left_Input).Action = Or_Logic
                  then Curr_Gate.Left_Input
                  else Curr_Gate.Right_Input);
               Curr_Gate      := Gate_Definitions (Curr_Wire);
               loop
                  declare
                     Left_Wire   : constant Wire_Name := Curr_Gate.Left_Input;
                     Left_Child  : constant Gate_Record :=
                       Gate_Definitions (Left_Wire);
                     Right_Wire  : constant Wire_Name := Curr_Gate.Right_Input;
                     Right_Child : constant Gate_Record :=
                       Gate_Definitions (Right_Wire);
                  begin
                     --  the next line for me should exit when we meet `kvj AND bhq`
                     exit when Curr_Gate.Action = And_Logic
                       and then Base_Wires (Left_Child, Right_Child);
                     Left_Logic  := Left_Child.Action;
                     Right_Logic := Right_Child.Action;
                     if Left_Logic /= Expected_Logic
                       and then Left_Logic /= Companion (Expected_Logic)
                     then
                        IO.Put_Line
                          (Curr_Gate.Left_Input & " should be " &
                           Expected_Logic'Image & " or " &
                           Logic_Variant'Image (Companion (Expected_Logic)) &
                           " but is " & Left_Logic'Image);
                        exit;
                     elsif Right_Logic /= Expected_Logic
                       and then Right_Logic /= Companion (Expected_Logic)
                     then
                        IO.Put_Line
                          (Curr_Gate.Right_Input & " should be " &
                           Expected_Logic'Image & " or " &
                           Logic_Variant'Image (Companion (Expected_Logic)) &
                           " but is " & Right_Logic'Image);
                        exit;
                     else
                        if Left_Logic = Expected_Logic
                          and then (not Primitive_Gate (Left_Child))
                        then
                           Curr_Wire := Left_Wire;
                           Curr_Gate := Left_Child;
                        elsif Right_Logic = Expected_Logic
                          and then (not Primitive_Gate (Right_Child))
                        then
                           Curr_Wire := Right_Wire;
                           Curr_Gate := Right_Child;
                        else
                           IO.Put_Line
                             ("something is wrong with one of " & Left_Wire &
                              ", " & Right_Wire);
                        end if;
                        Expected_Logic := Successor (Expected_Logic);
                     end if;
                  end;
               end loop;
            end;
         end if;
      end loop;
   end Experiment_3;

begin
   Read_Input;
   IO.Put_Line ("Result of Part 1 is" & Value'Image (Evaluate));

   --  The changes below fix Part 2 on my input.
   --  You will need to modify them for different input.
   --  To make it work, comment out the `declare` block below,
   --  and examine the output of `Experiment_3`,
   --  which _should_ work with any input.
   --  You should see reports of wires that don't match an expected pattern.
   --  The wires that need to be swapped should show up very near each other.
   --  For example, in my case I had this first:
   --
   --      Result of Part 1 is 43942008931358
   --      z10 should be XOR_LOGIC but is AND_LOGIC
   --      vcf should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --      vcf should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --      vcf should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --      vcf should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --      vcf should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --      vcf should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --      z17 should be XOR_LOGIC but is AND_LOGIC
   --      fhg should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --
   --  This was my clue that I needed to swap z10 and vcf.
   --  It was not obvious that z17 and fhg needed to be swapped,
   --  but that proved to be the case.
   --  A little further down, I encountered this:
   --
   --      fhg should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --      dvb should be OR or XOR but is AND_LOGIC
   --      One of dvb & jsn should be an OR but is not
   --      fsq should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --
   --  This was my clue that dvb and fsq should be swapped.
   --  Finally, I encountered this:
   --
   --      fsq should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --      z39 should be XOR_LOGIC but is OR_LOGIC
   --      One of rtf & tnc should be an OR but is not
   --      something is wrong with one of rtf, tnc
   --      rtf should be AND_LOGIC or AND_LOGIC but is XOR_LOGIC
   --
   --  This was my clue that z39 should be swapped with rtf or tnc.
   --  I thought it should be rtf, but it turned out to be tnc.
   --  I looked briefly at why but have forgotten the reason
   --  & don't care to check again to explain.
   --
   --  The basic principle is that addition can be defined recursively
   --  using XOR at the top level, followed by repeating, for level i
   --  (where the first i is the previous z-index):
   --
   --  * XOR of level i, OR'd with the result of:
   --  * an AND of level i - 1, AND'd with the result of: (recurse).
   --
   --  The base level needs a little extra care.
   --  The swaps seem to be among the iterating pattern,
   --  so `Experiment_3` reports any violations of that pattern.
   --  That was enough for me to identify the swaps, correct them,
   --  and obtain the correct answer.

   declare
      Temp : Gate_Record;
      type Swap_Record is record
         Left, Right : Wire_Name;
      end record;
      Swaps : array (Positive range <>) of Swap_Record :=
        [("vcf", "z10"), ("fhg", "z17"), ("dvb", "fsq"), ("tnc", "z39")];
   begin
      for Swap of Swaps loop
         Temp := Gate_Definitions (Swap.Left);
         Gate_Definitions.Replace (Swap.Left, Gate_Definitions (Swap.Right));
         Gate_Definitions.Replace (Swap.Right, Temp);
      end loop;
   end;

   --  it's worth uncommenting the next few lines if you like;
   --  this is how i started exploring

   --  IO.Put_Line ("==== Experiment 1 w/x ====");
   --  for Ith in 0 .. Input_Wires - 1 loop
   --     Experiment_1 ('x', Ith);
   --  end loop;
   --  IO.Put_Line ("==== Experiment 1 w/y ====");
   --  for Ith in 0 .. Input_Wires - 1 loop
   --     Experiment_1 ('y', Ith);
   --  end loop;
   --  IO.Put_Line ("==== Experiment 1 w/both ====");
   --  for Ith in 0 .. Input_Wires - 1 loop
   --     Experiment_1 ('y', Ith, True);
   --  end loop;

   Experiment_3;

   IO.Put_Line ("If you see no error reports, then the swaps were correct.");
   IO.Put_Line ("Otherwise, check the swaps against the reports and retry.");

end Day24;
