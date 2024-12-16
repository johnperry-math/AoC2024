pragma Ada_2022;

with Ada.Text_IO;

package body Common is

   package IO renames Ada.Text_IO;

   package body Two_Dimensional_Map is

      function "<" (Left, Right : Location_Record) return Boolean
      is (Left.Row < Right.Row
          or else (Left.Row = Right.Row and then Left.Col < Right.Col));

   end Two_Dimensional_Map;

   package body Two_Dimensional_Map_IO is

      Start_Position : Map_Package.Location_Record;

      Map renames Map_Package.Map;

      procedure Read_Input
        (Filename : String :=
           (if Doing_Example then "example.txt" else "input.txt"))
      is
         Input : IO.File_Type;
      begin
         IO.Open (Input, IO.In_File, Filename);
         for Row in Map'Range(1) loop
            declare
               Line : constant String := IO.Get_Line (Input);
            begin
               for Col in Map'Range(2) loop
                  Map (Row, Col) := Deserialize (Line (Col));
                  if Remember_Start.Remember
                    and then Line (Col) = Remember_Start.Symbol
                  then
                     Start_Position := (Row, Col);
                     IO.Put_Line ("Found start at" & Row'Image & Col'Image);
                  end if;
               end loop;
            end;
         end loop;
      end Read_Input;

      procedure Put_Map is
      begin
         for Row in Map'Range(1) loop
            for Col in Map'Range(2) loop
               IO.Put (Serialize (Map (Row, Col)));
            end loop;
            IO.New_Line;
         end loop;
      end Put_Map;

      function Start_Location return Map_Package.Location_Record
      is (Start_Position);

   end Two_Dimensional_Map_IO;

   package body Mathematics is

      function Gcd (A, B : Base_Type) return Base_Type is

         M    : Base_Type := A;
         N    : Base_Type := B;
         Q, R : Base_Type;

      begin

         while N /= Zero loop
            Q := M / N;
            R := M mod N;
            M := N;
            N := R;
         end loop;

         return M;

      end Gcd;

      function Lcm (A, B : Base_Type) return Base_Type
      is (A / Gcd (A, B) * B);

      function XGcd (A, B : Integer) return Bezout_Equation is
         M    : Integer := A;
         N    : Integer := B;
         Q, R : Integer;

         X_Curr       : Integer := 0;
         X_Prev       : Integer := 1;
         Y_Curr       : Integer := 1;
         Y_Prev       : Integer := 0;
         X_Tmp, Y_Tmp : Integer;
      begin
         while N /= 0 loop
            Q := M / N;
            R := M mod N;
            M := N;
            N := R;
            X_Tmp := X_Curr;
            X_Curr := X_Prev - Q * X_Curr;
            X_Prev := X_Tmp;
            Y_Tmp := Y_Curr;
            Y_Curr := Y_Prev - Q * Y_Curr;
            Y_Prev := Y_Tmp;
         end loop;
         return (M, X_Prev, Y_Prev);
      end XGcd;

   end Mathematics;

end Common;
