pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Day05 is

   package IO renames Ada.Text_IO;
   package Int_IO is new IO.Integer_IO (Num => Positive);

   type Rule_Record is record
      Before, After : Positive;
   end record;

   package Rule_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Rule_Record);

   Rules : Rule_Vectors.Vector;

   package Update_Vecs is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Positive);
   use all type Update_Vecs.Vector;

   package Update_Vec_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Update_Vecs.Vector);

   Updates : Update_Vec_Vecs.Vector;

   package Update_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Positive,
        Element_Type => Positive);

   use all type Update_Maps.Map;

   package Update_Map_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Update_Maps.Map);

   Pages_to_Locations : Update_Map_Vectors.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      loop
         declare
            Line          : constant String := IO.Get_Line (Input);
            Before, After : Positive;
            Position      : Positive;
         begin
            exit when Line'Length = 0;
            Int_IO.Get (Line, Before, Position);
            Position := @ + 2;
            Int_IO.Get (Line (Position .. Line'Last), After, Position);
            Rules.Append (Rule_Record'(Before, After));
         end;
      end loop;
      while not IO.End_Of_File (Input) loop
         declare
            Index    : Positive := 1;
            Value    : Positive;
            Map      : Update_Maps.Map;
            Update   : Update_Vecs.Vector;
            Line     : constant String := IO.Get_Line (Input);
            Position : Positive := Line'First;
         begin
            while Position <= Line'Last loop
               Int_IO.Get (Line (Position .. Line'Last), Value, Position);
               Position := @ + 2;
               Map.Insert (Value, Index);
               Index := @ + 1;
               Update.Append (Value);
            end loop;
            Pages_to_Locations.Append (Map);
            Updates.Append (Update);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   function Is_Valid (Update : Update_Maps.Map) return Boolean is
      Valid : Boolean := True;
   begin
      for Rule of Rules loop
         if Update.Contains (Rule.Before) and then Update.Contains (Rule.After)
         then
            Valid := Update (Rule.Before) < Update (Rule.After);
         end if;
         exit when not Valid;
      end loop;
      return Valid;
   end Is_Valid;

   procedure Part_1 is
      Result : Natural := 0;
   begin
      for Ith
        in Pages_to_Locations.First_Index .. Pages_to_Locations.Last_Index
      loop
         if Is_Valid (Pages_to_Locations (Ith)) then
            declare
               Update : constant Update_Vecs.Vector := Updates (Ith);
            begin
               Result := @ + Update ((Positive (Update.Length) + 1) / 2);
            end;
         end if;
      end loop;
      IO.Put_Line
        ("Sum of middle value of correctly-ordered udpates is" & Result'Image);
   end Part_1;

   function Order_By_Rules (First, Second : Positive) return Boolean
   is (not (for some Rule of Rules
            => Rule.Before = Second and Rule.After = First));

   package Rule_Sorter is new
     Update_Vecs.Generic_Sorting ("<" => Order_By_Rules);

   procedure Part_2 is
      Result : Natural := 0;
   begin
      for Ith
        in Pages_to_Locations.First_Index .. Pages_to_Locations.Last_Index
      loop
         if not Is_Valid (Pages_to_Locations (Ith)) then
            declare
               Update : Update_Vecs.Vector := Updates (Ith);
            begin
               Rule_Sorter.Sort (Update);
               Result := @ + Update ((Positive (Update.Length) + 1) / 2);
            end;
         end if;
      end loop;
      IO.Put_Line
        ("Sum of middle value of corrected incorrectly-ordered updates is"
         & Result'Image);
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day05;
