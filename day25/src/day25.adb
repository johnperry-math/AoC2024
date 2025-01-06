pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;

procedure Day25 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   type Lock_Or_Key_Record is array (1 .. 5) of Natural;

   package Lock_Or_Key_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Lock_Or_Key_Record);

   Keys, Locks : Lock_Or_Key_Vecs.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input, IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      while not IO.End_Of_File (Input) loop
         declare
            subtype Row_String is String (1 .. 5);
            Lines       : array (1 .. 7) of Row_String;
            Lock_Or_Key : Lock_Or_Key_Record := [others => 0];
         begin
            for Ith in 1 .. 7 loop
               Lines (Ith) := IO.Get_Line (Input);
            end loop;
            for Col in 1 .. 5 loop
               for Row in 2 .. 6 loop
                  if Lines (Row) (Col) = '#' then
                     Lock_Or_Key (Col) := @ + 1;
                  end if;
               end loop;
            end loop;
            if Lines (1) = "#####" then
               Locks.Append (Lock_Or_Key);
            else
               Keys.Append (Lock_Or_Key);
            end if;
         end;
         if not IO.End_Of_File (Input) then
            IO.Skip_Line (Input);
         end if;
      end loop;
   end Read_Input;

   procedure Part_1 is
      Result : Natural := 0;
   begin
      for Key of Keys loop
         for Lock of Locks loop
            if (for all Ith in 1 .. 5 => Key (Ith) + Lock (Ith) <= 5) then
               Result := @ + 1;
            end if;
         end loop;
      end loop;
      IO.Put_Line
        ("The number of unique key/lock pairs that fit without overlap is" &
         Result'Image);
   end Part_1;

begin
   Read_Input;
   IO.Put_Line ("there are" & Locks.Length'Image & " locks");
   IO.Put_Line ("there are" & Keys.Length'Image & " keys");
   Part_1;
end Day25;
