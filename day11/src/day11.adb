pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

procedure Day11 is

   package IO renames Ada.Text_IO;

   type Value is range 0 .. 2 ** 64 - 1;

   package Value_IO is new IO.Integer_IO (Num => Value);

   package Value_Vecs is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Value);

   Values : Value_Vecs.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      declare
         Line     : constant String := IO.Get_Line (Input);
         Position : Positive := Line'First;
         X        : Value;
      begin
         while Position <= Line'Last loop
            Value_IO.Get (Line (Position .. Line'Last), X, Position);
            Position := @ + 2;
            Values.Append (X);
         end loop;
      end;
   end Read_Input;

   procedure Part_1 is
      Curr : Value_Vecs.Vector := Values.Copy;
      Next : Value_Vecs.Vector;
   begin
      for Blink in 1 .. 25 loop
         Next.Clear;
         for Ball of Curr loop
            if Ball = 0 then
               Next.Append (1);
            else
               declare
                  Ball_Image_Untrimmed : constant String := Ball'Image;
                  Ball_Image           : constant String :=
                    Ball_Image_Untrimmed (2 .. Ball_Image_Untrimmed'Last);
                  Length               : constant Natural := Ball_Image'Length;
               begin
                  if Length mod 2 = 0 then
                     Next.Append
                       (Value'Value (Ball_Image (2 .. Length / 2 + 1)));
                     Next.Append
                       (Value'Value
                          (Ball_Image (Length / 2 + 2 .. Ball_Image'Last)));
                  else
                     Next.Append (2024 * Ball);
                  end if;
               end;
            end if;
         end loop;
         Curr := Next.Copy;
      end loop;
      IO.Put_Line ("After 25 blinks there are" & Curr.Length'Image & " balls");
   end Part_1;

   package Value_Natural_Maps is new
     Ada.Containers.Ordered_Maps (Key_Type => Value, Element_Type => Value);

   procedure Step (Map : in out Value_Natural_Maps.Map; Element, Count : Value)
   is
   begin
      if Map.Contains (Element) then
         Map.Replace (Element, Map (Element) + Count);
      else
         Map.Include (Element, Count);
      end if;
   end Step;

   procedure Part_2 is
      Curr, Next : Value_Natural_Maps.Map;
      Result     : Value := 0;
   begin
      for Element of Values loop
         Step (Curr, Element, 1);
      end loop;
      for Blink in 1 .. 75 loop
         Next.Clear;
         for Cursor in Curr.Iterate loop
            declare
               Ball  : Value := Value_Natural_Maps.Key (Cursor);
               Count : Value := Value_Natural_Maps.Element (Cursor);
            begin
               if Ball = 0 then
                  Step (Next, 1, Count);
               else
                  declare
                     Ball_Image_Untrimmed : constant String := Ball'Image;
                     Ball_Image           : constant String :=
                       Ball_Image_Untrimmed (2 .. Ball_Image_Untrimmed'Last);
                     Length               : constant Natural :=
                       Ball_Image'Length;
                     Left, Right          : Value;
                  begin
                     if Length mod 2 = 0 then
                        Left := Value'Value (Ball_Image (2 .. Length / 2 + 1));
                        Right :=
                          Value'Value
                            (Ball_Image (Length / 2 + 2 .. Ball_Image'Last));
                        Step (Next, Left, Count);
                        Step (Next, Right, Count);
                     else
                        Step (Next, Value (2024 * Ball), Count);
                     end if;
                  end;
               end if;
            end;
         end loop;
         Curr := Next.Copy;
      end loop;
      for Count of Curr loop
         Result := @ + Value (Count);
      end loop;
      IO.Put_Line ("After 75 blinks there are" & Result'Image & " balls");
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day11;
