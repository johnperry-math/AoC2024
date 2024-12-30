pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with Common;

procedure Day13 is

   package IO renames Ada.Text_IO;

   type Value is range -2 ** 50 .. 2 ** 50 - 1;

   package Int_IO is new IO.Integer_IO (Num => Value);

   package Math is new
     Common.Mathematics (Base_Type => Value, Zero => 0, One => 1);

   Doing_Example : constant Boolean := False;

   type Diophantine_System_Record is record
      AX, AY, BX, BY : Value;
      CX, CY         : Value;
   end record;

   function Solvable (System : Diophantine_System_Record) return Boolean
   is (System.CX mod Math.Gcd (System.AX, System.BX) = 0
       and then System.CY mod Math.Gcd (System.AY, System.BY) = 0);

   package Diophantine_System_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Diophantine_System_Record);

   Systems : Diophantine_System_Vecs.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      while not IO.End_Of_File (Input) loop
         declare
            First_Line  : constant String := IO.Get_Line (Input);
            Second_Line : constant String := IO.Get_Line (Input);
            Third_Line  : constant String := IO.Get_Line (Input);
            System      : Diophantine_System_Record;
            Position    : Positive := 13;
         begin
            Int_IO.Get
              (First_Line (Position .. First_Line'Last), System.AX, Position);
            Position := @ + 5;
            Int_IO.Get
              (First_Line (Position .. First_Line'Last), System.AY, Position);

            Position := 13;
            Int_IO.Get
              (Second_Line (Position .. Second_Line'Last),
               System.BX,
               Position);
            Position := @ + 5;
            Int_IO.Get
              (Second_Line (Position .. Second_Line'Last),
               System.BY,
               Position);

            Position := 10;
            Int_IO.Get
              (Third_Line (Position .. Third_Line'Last), System.CX, Position);
            Position := @ + 5;
            Int_IO.Get
              (Third_Line (Position .. Third_Line'Last), System.CY, Position);

            Systems.Append (System);

            if not IO.End_Of_File (Input) then
               IO.Skip_Line (Input);
            end if;

         end;
      end loop;
   end Read_Input;

   type Diophantine_Solution is record
      X, Y : Value;
   end record;

   function "<" (Left, Right : Diophantine_Solution) return Boolean
   is (Left.X < Right.X or else (Left.X = Right.X and then Left.Y < Right.Y));

   package Diophantine_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Diophantine_Solution);

   package Diophantine_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Diophantine_Solution);

   function Cost (Solution : Diophantine_Solution) return Value
   is (3 * Solution.X + Solution.Y);

   function All_Positive_Solutions
     (A, B, C : Value) return Diophantine_Sets.Set
   is
      Bezout   : Math.Bezout_Equation := Math.XGcd (A, B);
      Result   : Diophantine_Sets.Set;
      X        : Value := Bezout.X;
      Y        : Value := Bezout.Y;
      Gcd      : Value := Bezout.Gcd;
      Quotient : Value;
   begin
      if Gcd < 0 then
         X := @ * (-1);
         Y := @ * (-1);
         Gcd := @ * (-1);
      end if;
      if C mod Gcd = 0 then
         X := @ * C / Gcd;
         Y := @ * C / Gcd;
         if X <= 0 then
            if X = 0 and then Y <= 100 then
               Result.Insert (Diophantine_Solution'(X, Y));
            end if;
            Quotient := X / (B / Gcd);
            X := @ - Quotient * B / Gcd;
            Y := @ + Quotient * A / Gcd;
            while X < 0 loop
               X := @ + B / Gcd;
               Y := @ - A / Gcd;
            end loop;
            if Y >= 0 then
               if X <= 100 and then Y <= 100 then
                  Result.Include (Diophantine_Solution'(X, Y));
               end if;
               loop
                  X := @ + B / Gcd;
                  Y := @ - A / Gcd;
                  exit when Y < 0;
                  if X <= 100 and then Y <= 100 then
                     Result.Include (Diophantine_Solution'(X, Y));
                  end if;
               end loop;

            end if;

         else
            if Y = 0 and then X <= 100 then
               Result.Insert (Diophantine_Solution'(X, Y));
            end if;
            Quotient := Y / (A / Gcd);
            X := @ + Quotient * B / Gcd;
            Y := @ - Quotient * A / Gcd;
            while Y < 0 loop
               X := @ - B / Gcd;
               Y := @ + A / Gcd;
            end loop;
            if X >= 0 then
               Quotient := X / (B / Gcd);
               X := @ + Quotient * B / Gcd;
               Y := @ - Quotient * A / Gcd;
               if X <= 100 and then Y <= 100 then
                  Result.Include (Diophantine_Solution'(X, Y));
               end if;
               loop
                  X := @ - B / Gcd;
                  Y := @ + A / Gcd;
                  exit when X < 0;
                  if X <= 100 and then Y <= 100 then
                     Result.Include (Diophantine_Solution'(X, Y));
                  end if;
               end loop;
            end if;
         end if;
      end if;
      return Result;
   end All_Positive_Solutions;

   procedure Part_1 is
      X_Sols, Y_Sols, Common_Sols : Diophantine_Sets.Set;
      Result                      : Value := 0;
   begin
      for System of Systems loop
         X_Sols := All_Positive_Solutions (System.AX, System.BX, System.CX);
         Y_Sols := All_Positive_Solutions (System.AY, System.BY, System.CY);
         Common_Sols := X_Sols.Intersection (Y_Sols);
         if not Common_Sols.Is_Empty then
            declare
               Min_Cost     : Value := Value'Last;
               Sol_Cost     : Value;
               X_Win, Y_Win : Value;
            begin
               for Sol of Common_Sols loop
                  Sol_Cost := Cost (Sol);
                  if Sol_Cost < Min_Cost then
                     Min_Cost := Sol_Cost;
                     X_Win := Sol.X;
                     Y_Win := Sol.Y;
                  end if;
               end loop;
               Result := @ + Min_Cost;
            end;
         end if;
      end loop;
      IO.Put_Line ("The cost to win all possible prizes is" & Result'Image);
   end Part_1;

   function Cramer_It
     (System : Diophantine_System_Record) return Diophantine_Solution
   is ((
          (System.CX * System.BY - System.CY * System.BX)
          / (System.AX * System.BY - System.AY * System.BX),


          (System.AX * System.CY - System.AY * System.CX)
          / (System.AX * System.BY - System.AY * System.BX)));

   procedure Part_2 is
      Result                                       : Value := 0;
      Ith                                          : Positive := 1;
      First_Quo, First_Rem, Second_Quo, Second_Rem : Value;
      Sol                                          : Diophantine_Solution;
   begin
      for System of Systems loop
         System.CX := @ + 10_000_000_000_000;
         System.CY := @ + 10_000_000_000_000;
         First_Quo :=
           (System.CX * System.BY - System.CY * System.BX)
           / (System.AX * System.BY - System.AY * System.BX);
         First_Rem :=
           (System.CX * System.BY - System.CY * System.BX)
           mod (System.AX * System.BY - System.AY * System.BX);
         Second_Quo :=
           (System.AX * System.CY - System.AY * System.CX)
           / (System.AX * System.BY - System.AY * System.BX);
         Second_Rem :=
           (System.AX * System.CY - System.AY * System.CX)
           mod (System.AX * System.BY - System.AY * System.BX);

         if First_Rem = 0 and then Second_Rem = 0 then
            Sol := (First_Quo, Second_Quo);
            Result := @ + Cost (Sol);
         end if;
      end loop;
      IO.Put_Line ("The cost to win all possible prizes is" & Result'Image);
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day13;
