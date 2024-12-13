pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;

with Common;

procedure Day06 is

   package IO renames Ada.Text_IO;

   package Motion_2D renames Common.Two_Dimensional_Motion;

   type Object is (Empty, Obstacle);

   package Map_2D is new
     Common.Two_Dimensional_Map
       (Row_Length => 130,
        Col_Length => 130,
        Object     => Object);

   function Serialize (O : Object) return Character
   is (case O is
         when Empty => '.',
         when Obstacle => '#');

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Object
   is (case Symbol is
         when '.' | '^' => Empty,
         when '#' => Obstacle,
         when others => raise Invalid_Symbol);

   package Map_IO is new
     Common.Two_Dimensional_Map_IO
       (Map_Package    => Map_2D,
        Remember_Start => (Remember => True, Symbol => '^'));

   Unobstructed_Path : Map_2D.Location_Sets.Set;

   procedure Part_1 is
      use all type Motion_2D.Direction;
      use all type Motion_2D.Turn_Direction;
      Row, Col  : Integer;
      Offset    : Motion_2D.Drc;
      Dir       : Motion_2D.Direction;
      Locations : Map_2D.Location_Sets.Set;
   begin
      Row := Map_IO.Start_Location.Row;
      Col := Map_IO.Start_Location.Col;
      Dir := North;
      loop
         Offset := Motion_2D.Deltas (Dir);
         while Row in Map_2D.Row_Range
           and then Col in Map_2D.Col_Range
           and then Map_2D.Map (Row, Col) /= Obstacle
         loop
            Locations.Include (Map_2D.Location_Record'(Row, Col));
            Row := @ + Offset.DRow;
            Col := @ + Offset.DCol;
         end loop;
         exit when
           Row not in Map_2D.Row_Range or else Col not in Map_2D.Col_Range;
         --  we hit an obstacle; step back one
         Row := @ - Offset.DRow;
         Col := @ - Offset.DCol;
         Dir := Turn (@, Right);
      end loop;
      IO.Put_Line ("Visited" & Locations.Length'Image & " distinct positions");
      Unobstructed_Path := Locations.Copy;
   end Part_1;

   function This_Arrangement_Loops return Boolean is
      use all type Motion_2D.Direction;
      use all type Motion_2D.Turn_Direction;
      Row, Col : Integer;
      Offset   : Motion_2D.Drc;
      Dir      : Motion_2D.Direction;

      Looped            : Boolean := False;
      type Location_And_Direction_Record is record
         Row : Map_2D.Row_Range;
         Col : Map_2D.Col_Range;
         Dir : Motion_2D.Direction;
      end record;
      function "<" (Left, Right : Location_And_Direction_Record) return Boolean
      is (Left.Row < Right.Row
          or else (Left.Row = Right.Row and then Left.Col < Right.Col)
          or else (Left.Row = Right.Row
                   and then Left.Col = Right.Col
                   and then Left.Dir < Right.Dir));
      package Location_And_Direction_Sets is new
        Ada.Containers.Ordered_Sets
          (Element_Type => Location_And_Direction_Record);
      Locations_Visited : Location_And_Direction_Sets.Set;
   begin
      Row := Map_IO.Start_Location.Row;
      Col := Map_IO.Start_Location.Col;
      Dir := North;
      loop
         Offset := Motion_2D.Deltas (Dir);
         while (not Looped)
           and then Row in Map_2D.Row_Range
           and then Col in Map_2D.Col_Range
           and then Map_2D.Map (Row, Col) /= Obstacle
         loop
            declare
               New_Location : constant Location_And_Direction_Record :=
                 (Row, Col, Dir);
            begin
               if Locations_Visited.Contains (New_Location) then
                  Looped := True;
               else
                  Locations_Visited.Insert (New_Location);
               end if;
            end;
            Row := @ + Offset.DRow;
            Col := @ + Offset.DCol;
         end loop;
         exit when
           Looped
           or else Row not in Map_2D.Row_Range
           or else Col not in Map_2D.Col_Range;
         Row := @ - Offset.DRow;
         Col := @ - Offset.DCol;
         Dir := Turn (@, Right);
      end loop;
      return Looped;
   end This_Arrangement_Loops;

   procedure Part_2 is
      Result : Natural := 0;
   begin
      --  for Row in Map_2D.Row_Range loop
      --     for Col in Map_2D.Col_Range when Map_2D.Map (Row, Col) /= Obstacle
      --     loop
      for Location of Unobstructed_Path loop
         --  Map_2D.Map (Row, Col) := Obstacle;
         Map_2D.Map (Location.Row, Location.Col) := Obstacle;
         if This_Arrangement_Loops then
            Result := @ + 1;
         end if;
         --  Map_2D.Map (Row, Col) := Empty;
         Map_2D.Map (Location.Row, Location.Col) := Empty;
         --  end loop;
      end loop;
      IO.Put_Line
        ("You can choose from" & Result'Image & " positions to force a loop");
   end Part_2;

begin
   Map_IO.Read_Input;
   Part_1;
   Part_2;
end Day06;
