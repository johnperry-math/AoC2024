pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Day01 is

   package IO renames Ada.Text_IO;
   package Int_IO is new IO.Integer_IO (Num => Integer);

   package Int_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Integer);

   Left_List, Right_List : Int_Vectors.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line                    : constant String := IO.Get_Line (Input);
            Left_Value, Right_Value : Integer;
            Position                : Positive;
         begin
            Int_IO.Get (Line, Left_Value, Position);
            Int_IO.Get
              (Line (Position + 1 .. Line'Last), Right_Value, Position);
            Left_List.Append (Left_Value);
            Right_List.Append (Right_Value);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   procedure Part_1 is
      Sum_Of_Dist : Natural := 0;
      package Int_Sorter is new Int_Vectors.Generic_Sorting;
   begin
      Int_Sorter.Sort (Left_List);
      Int_Sorter.Sort (Right_List);
      for Ith in Left_List.First_Index .. Left_List.Last_Index loop
         Sum_Of_Dist := @ + abs (Left_List (Ith) - Right_List (Ith));
      end loop;
      IO.Put_Line ("The sum of distances is" & Sum_Of_Dist'Image);
   end Part_1;

   procedure Part_2 is
      package Int_Natural_Maps is new
        Ada.Containers.Ordered_Maps
          (Key_Type     => Integer,
           Element_Type => Natural);
      use all type Int_Natural_Maps.Cursor;
      Occurrences : Int_Natural_Maps.Map;
      Result      : Natural := 0;
   begin
      for Element of Left_List loop
         Occurrences.Insert (Element, 0);
      end loop;
      for Element of Right_List loop
         declare
            Cursor : Int_Natural_Maps.Cursor := Occurrences.Find (Element);
         begin
            if Cursor /= Int_Natural_Maps.No_Element then
               Occurrences.Replace_Element
                 (Cursor, Int_Natural_Maps.Element (Cursor) + 1);
            end if;
         end;
      end loop;
      for Element of Left_List loop
         Result := @ + Element * Occurrences (Element);
      end loop;
      IO.Put_Line ("The similarity score is" & Result'Image);
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day01;
