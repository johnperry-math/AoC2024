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
                  if Remember_Start.Remember
                    and then Line (Col) = Remember_Start.Symbol
                  then
                     Start_Position := (Row, Col);
                  else
                     Map (Row, Col) := Deserialize (Line (Col));
                  end if;
               end loop;
            end;
         end loop;
         IO.Close (Input);
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

      function XGcd (A, B : Base_Type) return Bezout_Equation is
         M    : Base_Type := A;
         N    : Base_Type := B;
         Q, R : Base_Type;

         X_Curr       : Base_Type := Zero;
         X_Prev       : Base_Type := Zero + One;
         Y_Curr       : Base_Type := Zero + One;
         Y_Prev       : Base_Type := Zero;
         X_Tmp, Y_Tmp : Base_Type;
      begin
         while N /= Zero loop
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
