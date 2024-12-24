pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

procedure Day17 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   subtype Program_Value is Natural range 0 .. 7;
   package Program_Value_IO is new IO.Integer_IO (Num => Program_Value);

   package Output_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Program_Value);

   Program_Output : Output_Vecs.Vector;

   type Value is range 0 .. 2 ** 64 - 1;
   type Value_Mod is mod 2 ** 64;

   package Value_IO is new IO.Integer_IO (Num => Value);
   package Value_Q_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Value);
   package Value_Qs is new
     Ada.Containers.Unbounded_Synchronized_Queues (Value_Q_Interfaces);

   type Register_Name is (A, B, C);

   Registers : array (Register_Name) of Value;

   package Program_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Program_Value);

   Program             : Program_Vecs.Vector;
   Instruction_Pointer : Natural;

   procedure Read_Input is
      Input    : IO.File_Type;
      Position : Positive;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      declare
         First_Line  : constant String := IO.Get_Line (Input);
         Second_Line : constant String := IO.Get_Line (Input);
         Third_Line  : constant String := IO.Get_Line (Input);
      begin
         Value_IO.Get
           (First_Line (13 .. First_Line'Last), Registers (A), Position);
         Value_IO.Get
           (Second_Line (13 .. Second_Line'Last), Registers (B), Position);
         Value_IO.Get
           (Third_Line (13 .. Third_Line'Last), Registers (C), Position);
      end;
      IO.Skip_Line (Input);
      declare
         Program_Line : constant String := IO.Get_Line (Input);
         Program_Word : Program_Value;
      begin
         Position := 10;
         while Position <= Program_Line'Last loop
            Program_Value_IO.Get
              (Program_Line (Position .. Program_Line'Last),
               Program_Word,
               Position);
            Program.Append (Program_Word);
            Position := @ + 2;
         end loop;
      end;
   end Read_Input;

   Reserved : exception;

   function Combo (Operand : Program_Value) return Value
   is (case Operand is
         when 0 .. 3 => Value (Operand),
         when 4 => Registers (A),
         when 5 => Registers (B),
         when 6 => Registers (C),
         when 7 => raise Reserved with Instruction_Pointer'Image);

   Debugging : constant Boolean := False;

   procedure Dv (Register : Register_Name; Operand : Program_Value) is
   begin
      if Debugging then
         IO.Put_Line (Register'Image & "DV" & Operand'Image);
         IO.Put_Line
           ("   "
            & Value'Image (Combo (Operand))
            & Value'Image (Value (2) ** Natural (Combo (Operand))));
      end if;
      Registers (Register) :=
        Registers (A) / (Value (2) ** Natural (Combo (Operand)));
      Instruction_Pointer := @ + 2;
   end Dv;

   procedure Bxl (Operand : Program_Value) is
   begin
      if Debugging then
         IO.Put_Line ("BXL" & Operand'Image);
         IO.Put_Line ("   " & Value'Image (Registers (B)));
      end if;
      Registers (B) := Value (Value_Mod (@) xor Value_Mod (Operand));
      Instruction_Pointer := @ + 2;
   end Bxl;

   procedure Bst (Operand : Program_Value) is
   begin
      if Debugging then
         IO.Put_Line ("BST" & Operand'Image);
         IO.Put_Line ("   " & Value'Image (Value (Combo (Operand) mod 8)));
      end if;
      Registers (B) := Value (Combo (Operand) mod 8);
      Instruction_Pointer := @ + 2;
   end Bst;

   procedure Jnz (Operand : Program_Value) is
   begin
      if Debugging then
         IO.Put_Line ("JNZ" & Operand'Image);
      end if;
      if Registers (A) = 0 then
         Instruction_Pointer := @ + 2;
      else
         Instruction_Pointer := Operand;
      end if;
   end Jnz;

   procedure Bxc (Operand : Program_Value) is
   begin
      if Debugging then
         IO.Put_Line ("BXC" & Operand'Image);
         IO.Put_Line
           ("   " & Value'Image (Registers (B)) & Value'Image (Registers (C)));
      end if;
      Registers (B) :=
        Value (Value_Mod (Registers (B)) xor Value_Mod (Registers (C)));
      Instruction_Pointer := @ + 2;
   end Bxc;

   procedure Output (Operand : Program_Value) is
   begin
      if Debugging then
         IO.Put_Line ("OUT" & Operand'Image);
      end if;
      Program_Output.Append (Program_Value (Combo (Operand) mod 8));
      --  Value_IO.Put (Program_Output.Last_Element'Image, 0);
      --  IO.Put (',');
      Instruction_Pointer := @ + 2;
   end Output;

   procedure Adv (Operand : Program_Value) is
   begin
      Dv (A, Operand);
   end Adv;

   procedure Bdv (Operand : Program_Value) is
   begin
      Dv (B, Operand);
   end Bdv;

   procedure Cdv (Operand : Program_Value) is
   begin
      Dv (C, Operand);
   end Cdv;

   procedure Interpret_Opcode is
      Opcode  : constant Program_Value := Program (Instruction_Pointer);
      Operand : constant Program_Value := Program (Instruction_Pointer + 1);
   begin
      if Debugging then
         IO.Put_Line ("-----" & Instruction_Pointer'Image & " -----");
         IO.Put_Line ("A" & Value'Image (Registers (A)));
         IO.Put_Line ("B" & Value'Image (Registers (B)));
         IO.Put_Line ("C" & Value'Image (Registers (C)));
      end if;
      case Opcode is
         when 0 =>
            Adv (Operand);

         when 1 =>
            Bxl (Operand);

         when 2 =>
            Bst (Operand);

         when 3 =>
            Jnz (Operand);

         when 4 =>
            Bxc (Operand);

         when 5 =>
            Output (Operand);

         when 6 =>
            Bdv (Operand);

         when 7 =>
            Cdv (Operand);
      end case;
   end Interpret_Opcode;

   procedure Part_1 is
   begin
      Instruction_Pointer := 0;
      loop
         exit when Instruction_Pointer > Program.Last_Index;
         Interpret_Opcode;
      end loop;
   end Part_1;

   procedure Part_2 is
      Ith, Jth     : Integer;
      To_Do        : Value_Qs.Queue;
      Digits_Found : Natural := 0;
      Base, Result : Value;
   begin
      To_Do.Enqueue (0);
      while Digits_Found < Natural (Program.Length) loop
         To_Do.Dequeue (Base);
         for Start in Base .. Base + 7 loop
            Program_Output.Clear;
            Registers (A) := Start;
            Instruction_Pointer := 0;
            loop
               exit when Instruction_Pointer > Program.Last_Index;
               Interpret_Opcode;
            end loop;
            Ith := Program.Last_Index;
            Jth := Program_Output.Last_Index;
            declare
               Correct_Digits : Natural := 0;
            begin
               loop
                  exit when Program (Ith) /= Program_Output (Jth);
                  Correct_Digits := @ + 1;
                  Ith := @ - 1;
                  Jth := @ - 1;
                  exit when Jth < 0;
               end loop;
               if Jth < 0 then
                  IO.Put_Line ("hayup:" & Start'Image);
                  To_Do.Enqueue (Start * 8);
                  Digits_Found := Correct_Digits;
                  Result := Start;
               end if;
            end;
         end loop;
      end loop;
      IO.Put_Line ("Register A should contain" & Result'Image);
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day17;
