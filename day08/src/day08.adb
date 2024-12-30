pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

with Common;

procedure Day08 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   Dimension : constant Positive := (if Doing_Example then 12 else 50);

   subtype Index_Range is Positive range 1 .. Dimension;

   type Location_Record (Valid : Boolean := True) is record
      case Valid is
         when True =>
            Row, Col : Index_Range;

         when False =>
            null;
      end case;
   end record;

   type Locations_Array is array (1 .. 12) of Location_Record;

   function "<" (Left, Right : Location_Record) return Boolean
   is (Left.Row < Right.Row
       or else (Left.Row = Right.Row and then Left.Col < Right.Col));

   type Node_Map_Array is array (Character range <>) of Locations_Array;

   subtype Node_Range is Character range '0' .. 'z';

   Nodes : Node_Map_Array (Node_Range) :=
     [others => [others => (Valid => False)]];

   Too_Many_Nodes : exception;

   procedure Read_Input is
      Input : IO.File_Type;
      Row   : Positive := 1;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
            Node : Character;
            Ith  : Positive;
         begin
            for Col in Index_Range loop
               Node := Line (Col);
               if Node in 'A' .. 'Z'
                 or else Node in 'a' .. 'z'
                 or else Node in '0' .. '9'
               then
                  Ith := 1;
                  while Ith <= 4 and then Nodes (Node) (Ith).Valid loop
                     Ith := @ + 1;
                  end loop;
                  if Ith > 4 then
                     raise Too_Many_Nodes with Node'Image;
                  end if;
                  Nodes (Node) (Ith) :=
                    (Location_Record'(Valid => True, Row => Row, Col => Col));
               end if;
            end loop;
            Row := @ + 1;
         end;
      end loop;
   end Read_Input;

   package Node_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Location_Record);

   procedure Part_1 is
      List               : Locations_Array;
      Antinode_Locations : Node_Sets.Set;
   begin
      for Node in Node_Range loop
         List := Nodes (Node);
         for Ith in 1 .. 3 loop
            for Jth in Ith + 1 .. 4 loop
               if List (Ith).Valid and then List (Jth).Valid then
                  declare
                     Dx       : constant Integer :=
                       List (Ith).Col - List (Jth).Col;
                     Dy       : constant Integer :=
                       List (Ith).Row - List (Jth).Row;
                     Row, Col : Integer;
                     Location : Location_Record;
                  begin
                     Row := List (Ith).Row + Dy;
                     Col := List (Ith).Col + Dx;
                     if Row in Index_Range and then Col in Index_Range then
                        Location := (Valid => True, Row => Row, Col => Col);
                        Antinode_Locations.Include (Location);
                     end if;
                     Row := List (Jth).Row - Dy;
                     Col := List (Jth).Col - Dx;
                     if Row in Index_Range and then Col in Index_Range then
                        Location := (Valid => True, Row => Row, Col => Col);
                        Antinode_Locations.Include (Location);
                     end if;
                  end;
               end if;
            end loop;
         end loop;
      end loop;
      IO.Put_Line
        ("There are"
         & Antinode_Locations.Length'Image
         & " antinode locations");
   end Part_1;

   procedure Part_2 is
      package Integer_Math is new
        Common.Mathematics (Base_Type => Integer, One => 1, Zero => 0);
      List               : Locations_Array;
      Antinode_Locations : Node_Sets.Set;
   begin
      for Node in Node_Range loop
         List := Nodes (Node);
         for Ith in 1 .. 3 loop
            for Jth in Ith + 1 .. 4 loop
               if List (Ith).Valid and then List (Jth).Valid then
                  declare
                     Dx       : Integer := List (Ith).Col - List (Jth).Col;
                     Dy       : Integer := List (Ith).Row - List (Jth).Row;
                     Row, Col : Integer;
                     Location : Location_Record := List (Ith);
                     Gcd      : constant Integer :=
                       abs (Integer_Math.Gcd (Dx, Dy));
                  begin
                     if Gcd /= 1 then
                        IO.Put ("nontrivial gcd for ");
                        IO.Put (Node);
                        IO.Put_Line (Ith'Image & Jth'Image & ":" & Gcd'Image);
                     end if;
                     Dx := @ / Gcd;
                     Dy := @ / Gcd;
                     for Multiple in -1 .. 1 when Multiple /= 0 loop
                        Row := Location.Row;
                        Col := Location.Col;
                        while Row in Index_Range and then Col in Index_Range
                        loop
                           Location.Row := Row;
                           Location.Col := Col;
                           Antinode_Locations.Include (Location);
                           Row := @ + Multiple * Dy;
                           Col := @ + Multiple * Dx;
                        end loop;
                     end loop;
                  end;
               end if;
            end loop;
         end loop;
      end loop;
      IO.Put_Line
        ("No, there are"
         & Antinode_Locations.Length'Image
         & " antinode locations");
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day08;
