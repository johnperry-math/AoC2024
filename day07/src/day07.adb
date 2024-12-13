pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day07 is

   package IO renames Ada.Text_IO;

   type Values is range 0 .. 2 ** 64 - 1;

   package Val_IO is new IO.Integer_IO (Num => Values);

   package Val_Vecs is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Values);

   type Unknown_Equation is record
      Test_Value : Values;
      Operands   : Val_Vecs.Vector;
   end record;

   package Equation_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Unknown_Equation);

   Equations : Equation_Vecs.Vector;

   function Get_Integer
     (Line : String; Position : in out Positive) return Values
   is
      Result : Values := 0;
   begin
      while Position <= Line'Last and then Line (Position) in '0' .. '9' loop
         Result :=
           @ * 10 + Character'Pos (Line (Position)) - Character'Pos ('0');
         Position := @ + 1;
      end loop;
      return Result;
   end Get_Integer;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line          : constant String := IO.Get_Line (Input);
            Position      : Positive := Line'First;
            Result, Value : Values;
            Operands      : Val_Vecs.Vector;
         begin
            Result := Get_Integer (Line, Position);
            Position := @ + 2;
            while Position <= Line'Last loop
               Value := Get_Integer (Line, Position);
               Position := @ + 1;
               Operands.Append (Value);
            end loop;
            Equations.Append (Unknown_Equation'(Result, Operands));
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   function Compute_Results (Operands : Val_Vecs.Vector) return Val_Vecs.Vector
   is
      New_Results, Old_Results : Val_Vecs.Vector;
   begin
      Old_Results.Append (Operands (Operands.First_Index));
      for Ith in 2 .. Operands.Last_Index loop
         New_Results.Clear;
         for Result of Old_Results loop
            New_Results.Append (Result + Operands (Ith));
            New_Results.Append (Result * Operands (Ith));
         end loop;
         Old_Results := New_Results;
      end loop;
      New_Results := Old_Results;
      return New_Results;
   end Compute_Results;

   package Bool_Vecs is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Boolean);

   Satisfactory      : Bool_Vecs.Vector;
   First_Calibration : Values := 0;

   procedure Part_1 is
   begin
      for Equation of Equations loop
         declare
            Total_Product : Values := 1;
            Total_Sum     : Values := 0;
         begin
            for Operand of Equation.Operands loop
               Total_Product := @ * Operand;
               Total_Sum := @ + Operand;
            end loop;
            if Total_Product >= Equation.Test_Value
              or else Total_Sum <= Equation.Test_Value
            then
               declare
                  Results : constant Val_Vecs.Vector :=
                    Compute_Results (Equation.Operands);
               begin
                  if (for some Result of Results
                      => Result = Equation.Test_Value)
                  then
                     First_Calibration := @ + Equation.Test_Value;
                     Satisfactory.Append (True);
                  else
                     Satisfactory.Append (False);
                  end if;
               end;
            end if;
         end;
      end loop;
      IO.Put_Line ("Total calibration result is" & First_Calibration'Image);
   end Part_1;

   function Concatenate (First, Second : Values) return Values is
      Flag   : Values := 0;
      Result : Values := First;
   begin
      while Flag < Second loop
         Result := @ * 10;
         Flag := (@ + 1) * 10 - 1;
      end loop;
      return Result + Second;
   end Concatenate;

   function Compute_Results_2
     (Operands : Val_Vecs.Vector) return Val_Vecs.Vector
   is
      New_Results, Old_Results : Val_Vecs.Vector;
   begin
      Old_Results.Append (Operands (Operands.First_Index));
      for Ith in 2 .. Operands.Last_Index loop
         New_Results.Clear;
         for Result of Old_Results loop
            New_Results.Append (Result + Operands (Ith));
            New_Results.Append (Result * Operands (Ith));
            New_Results.Append (Concatenate (Result, Operands (Ith)));
         end loop;
         Old_Results := New_Results;
      end loop;
      New_Results := Old_Results;
      return New_Results;
   end Compute_Results_2;

   procedure Part_2 is
      Second_Calibration : Values := First_Calibration;
   begin
      for Ith
        in Equations.First_Index .. Equations.Last_Index
        when not Satisfactory (Ith)
      loop
         declare
            Equation : Equation_Vecs.Constant_Reference_Type :=
              Equations.Constant_Reference (Ith);
            Results  : constant Val_Vecs.Vector :=
              Compute_Results_2 (Equation.Operands);
         begin
            if (for some Result of Results => Result = Equation.Test_Value)
            then
               Second_Calibration := @ + Equation.Test_Value;
               Satisfactory.Append (True);
            else
               Satisfactory.Append (False);
            end if;
         end;
      end loop;
      IO.Put_Line ("Total calibration result is" & Second_Calibration'Image);
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day07;
