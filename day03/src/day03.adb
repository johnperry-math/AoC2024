pragma Ada_2022;

with Ada.Text_IO;

procedure Day03 is

   package IO renames Ada.Text_IO;
   package Int_IO is new IO.Integer_IO (Num => Integer);

   function Get_Integer
     (Line : String; Position : in out Positive) return Integer
   is
      Result : Integer := 0;
   begin
      while Line (Position) in '0' .. '9' loop
         Result :=
           @ * 10 + Character'Pos (Line (Position)) - Character'Pos ('0');
         Position := @ + 1;
      end loop;
      return Result;
   end Get_Integer;

   procedure Parse (Do_Or_Do_Not : Boolean := False) is
      Input   : IO.File_Type;
      Result  : Natural := 0;
      Enabled : Boolean := True;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line          : constant String := IO.Get_Line (Input);
            Position      : Positive := Line'First;
            First, Second : Integer;
         begin
            while Position <= Line'Last - 7 loop
               if Do_Or_Do_Not then
                  if Line (Position .. Position + 3) = "do()" then
                     Enabled := True;
                     Position := Position + 4;
                  elsif Line (Position .. Position + 6) = "don't()" then
                     Enabled := False;
                     Position := Position + 7;
                  end if;
               end if;
               if Enabled and then Position <= Line'Last - 7 then
                  if Line (Position .. Position + 3) /= "mul(" then
                     Position := @ + 1;
                  else
                     Position := @ + 4;
                     if Line (Position) in '0' .. '9' then
                        First := Get_Integer (Line, Position);
                        if Line (Position) = ','
                          and then Line (Position + 1) in '0' .. '9'
                        then
                           Position := @ + 1;
                           Second := Get_Integer (Line, Position);
                           if Line (Position) = ')' then
                              Position := @ + 1;
                              Result := @ + First * Second;
                           end if;
                        end if;
                     end if;
                  end if;
               else
                  Position := @ + 1;
               end if;
            end loop;
         end;
      end loop;
      IO.Close (Input);
      IO.Put_Line ("Result is" & Result'Image);
   end Parse;

begin
   Parse;
   Parse (Do_Or_Do_Not => True);
end Day03;
