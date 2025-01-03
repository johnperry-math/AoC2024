pragma Ada_2022;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

package Common is
   --  @summary
   --  Types, packages, and subprograms useful for Advent of Code.
   --
   --  @description
   --  This package incorporates several types, packages, and subprograms
   --  that I found useful for Advent of Code.
   --  This is being generated "after the fact", so it's quite possible that
   --  some things that should be moved here haven't been moved here yet,
   --  and may never move here.
   --  At the present time, this package includes mainly
   --  the types and subprograms I found myself re-using
   --  for most of the two-dimensional map puzzles.
   --
   --  There are three subpackages:
   --  <ul>
   --  <li>The <tt>Two_Dimensional_Motion</tt> package stands on its own,
   --       and offers types (and one function) useful for moving in one of the
   --       four cardinal directions.</li>
   --  <li>The <tt>Two_Dimensional_Map</tt> package likewise stands on its own,
   --       and offers basic functionality for a two-dimensional map, including
   --       a `Location_Record` type that documents a row and a column,
   --       as well as a lexicographic ordering on that type that enables you
   --       to use it in ordered sets or maps.</li>
   --  <li>The <tt>Two_Dimensional_Map_IO</tt> package
   --       does not stand on its own, but requires the previous declaration
   --       of a <tt>Two_Dimensional_Map</tt>.
   --       It will automatically adjust the filename when doing an example.
   --       </li>
   --  </ul>

   package Two_Dimensional_Motion is
      --  @summary
      --  support for consistent motion / examination
      --  one step in a cardinal direction

      type Direction is (North, South, East, West);
      --  the four cardinal directions; used in Deltas

      type Turn_Direction is (Left, Right);

      function Opposite (First, Second : Direction) return Boolean
      is
      --  returns True iff Left and Right are opposite cardinal directions

         (case First is
            when North => Second = South,
            when South => Second = North,
            when East => Second = West,
            when West => Second = East);

      function Turn
        (From : Direction; Towards : Turn_Direction) return Direction
      is (case From is
            when North =>
              (case Towards is
                 when Left => West,
                 when Right => East),
            when South =>
              (case Towards is
                 when Left => East,
                 when Right => West),
            when East =>
              (case Towards is
                 when Left => North,
                 when Right => South),
            when West =>
              (case Towards is
                 when Left => South,
                 when Right => North));

      subtype Nudge is Integer range -1 .. 1;
      --  a "nudge" directs one from a current location
      --  to one of the cardinal directions; see Deltas

      type Drc is record
         DRow, DCol : Nudge;
         --  change in a row or column
      end record;

      Deltas : constant array (Direction) of Drc :=
        [North => (-1, 0), South => (1, 0), East => (0, 1), West => (0, -1)];

   end Two_Dimensional_Motion;

   generic

      Row_Start, Col_Start : Natural := 1;
      Row_Length, Col_Length : Positive;

      type Object is private;
      --  objects that fill the map

   package Two_Dimensional_Map
   is
      --  @summary
      --  support for a two-dimensional, non-square map

      subtype Row_Range is
        Natural range Row_Start .. Row_Length + Row_Start - 1;
      subtype Col_Range is
        Natural range Col_Start .. Col_Length + Col_Start - 1;

      type Location_Record is record
         --  a location in the map
         Row : Row_Range;
         Col : Col_Range;
      end record;

      type Map_Array is array (Row_Range, Col_Range) of Object;
      Map : Map_Array;
      --  the map itself

      package Location_Vectors is new
        Ada.Containers.Vectors
          (Index_Type   => Positive,
           Element_Type => Location_Record);

      function "=" (Left, Right : Location_Vectors.Vector) return Boolean
      is (Natural (Left.Length) = Natural (Right.Length)
          and then (for all Ith in Left.First_Index .. Left.Last_Index
                    => Left (Ith) = Right (Ith)));

      function "<" (Left, Right : Location_Record) return Boolean;
      --  lexicographic ordering useful for creating sets and maps

      package Location_Sets is new
        Ada.Containers.Ordered_Sets (Element_Type => Location_Record);

      function Can_Move
        (Location : Location_Record; Offset : Two_Dimensional_Motion.Drc)
         return Boolean
      is (Location.Row + Offset.DRow in Row_Range
          and then Location.Col + Offset.DCol in Col_Range);

      function "+"
        (Location : Location_Record; Offset : Two_Dimensional_Motion.Drc)
         return Location_Record
      is (Location.Row + Offset.DRow, Location.Col + Offset.DCol);

   end Two_Dimensional_Map;

   type Remember_Start_Record (Remember : Boolean := False) is record
      case Remember is
         when True =>
            Symbol : Character;

         when False =>
            null;
      end case;
   end record;

   No_Start : constant Remember_Start_Record := (Remember => False);

   type Remember_End_Record (Remember : Boolean := False) is record
      case Remember is
         when True =>
            Symbol : Character;

         when False =>
            null;
      end case;
   end record;

   No_End : constant Remember_End_Record := (Remember => False);

   generic

      Doing_Example : Boolean := False;

      Remember_Start : Remember_Start_Record := No_Start;

      Remember_End : Remember_End_Record := No_End;

      with package Map_Package is new Two_Dimensional_Map (<>);

      with function Serialize (O : Map_Package.Object) return Character is <>;
      --  required for output (Put_Map)
      with
        function Deserialize (Symbol : Character) return Map_Package.Object
        is <>;
      --  required for input (Read_Input)

   package Two_Dimensional_Map_IO
   is

      procedure Read_Input
        (Filename : String :=
           (if Doing_Example then "example.txt" else "input.txt"));
      --  reads from the indicated file, using Deserialize to fill
      --  Map_Package.Map with appropriate symbols

      procedure Put_Map;
      --  prints a very basic representation of the Map_Package.Map
      --  based on Serialize

      function Start_Location return Map_Package.Location_Record;
      function End_Location return Map_Package.Location_Record;

   end Two_Dimensional_Map_IO;

   generic

      type Base_Type is private;
      Zero : Base_Type;
      --  additive identity for the Base_Type;
      --  for Universal_Integer use Zero => 0
      One : Base_Type;
      --  multiplicative identify for the Base_Type;
      --  for Universal_Integer use One => 1
      with function "=" (Left, Right : Base_Type) return Boolean is <>;
      with function "+" (Left, Right : Base_Type) return Base_Type is <>;
      with function "-" (Left, Right : Base_Type) return Base_Type is <>;
      with function "*" (Left, Right : Base_Type) return Base_Type is <>;
      with function "/" (Left, Right : Base_Type) return Base_Type is <>;
      with function "mod" (Left, Right : Base_Type) return Base_Type is <>;

   package Mathematics
   is
      --  @summary
      --  support for mathematics useful for Advent of Code,
      --  but not immediately available in Ada's standard library

      function Gcd (A, B : Base_Type) return Base_Type;
      --  ye olde Euclidean
      --  (i.e., the greatest common divisor of A and B)

      type Bezout_Equation is record
         --  if Gcd is greatest common divisor of A, B,
         --  then Gcd = A * X + B * Y
         Gcd  : Base_Type;
         X, Y : Base_Type;
      end record;

      function XGcd (A, B : Base_Type) return Bezout_Equation;

      function Lcm (A, B : Base_Type) return Base_Type;
      --  the least common multiple of A and B

   end Mathematics;

end Common;
