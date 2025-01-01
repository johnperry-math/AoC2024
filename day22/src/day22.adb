pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Vectors;

procedure Day22 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   type Number is range 0 .. 2 ** 64 - 1;

   package Number_IO is new IO.Integer_IO (Num => Number);

   package Number_Vecs is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Number);

   Values : Number_Vecs.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
      Value : Number;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      while not IO.End_Of_File (Input) loop
         Number_IO.Get (Input, Value);
         Values.Append (Value);
      end loop;
   end Read_Input;

   type Modular_Number is mod 2 ** 64;

   procedure Mix (Value : Number; Secret : in out Number) is
   begin
      Secret := Number (Modular_Number (Value) xor Modular_Number (@));
   end Mix;

   procedure Prune (Secret : in out Number) is
   begin
      Secret := @ mod 16777216;
   end Prune;

   type Change_Array is array (-9 .. 9, -9 .. 9, -9 .. 9, -9 .. 9) of Number;

   procedure Evolve (Secret : in out Number; Demands : in out Change_Array) is
      Product_64, Quotient_32, Product_2048 : Number;
      Original_Secret                       : Number := Secret;

      Last_Price, New_Price : Natural := Natural (Secret mod 10);
      Changes               : array (1 .. 4) of Integer := [others => 0];

      Encountered : array (-9 .. 9, -9 .. 9, -9 .. 9, -9 .. 9) of Boolean :=
        [others => [others => [others => [others => False]]]];
   begin
      for Each in 1 .. 2_000 loop
         Product_64 := Secret * 64;
         Mix (Product_64, Secret);
         Prune (Secret);
         Quotient_32 := Secret / 32;
         Mix (Quotient_32, Secret);
         Prune (Secret);
         Product_2048 := Secret * 2048;
         Mix (Product_2048, Secret);
         Prune (Secret);

         for Ith in reverse 2 .. 4 loop
            Changes (Ith) := Changes (Ith - 1);
         end loop;
         New_Price := Natural (Secret mod 10);
         Changes (1) := New_Price - Last_Price;
         if Each > 3
           and then (not Encountered
                           (Changes (1),
                            Changes (2),
                            Changes (3),
                            Changes (4)))
         then
            Encountered (Changes (1), Changes (2), Changes (3), Changes (4)) :=
              True;
            Demands (Changes (1), Changes (2), Changes (3), Changes (4)) :=
              @ + Number (New_Price);

         end if;

         Last_Price := New_Price;
      end loop;
   end Evolve;

   procedure Both_Parts is
      Result : Number := 0;

      Sales : Change_Array :=
        [others => [others => [others => [others => 0]]]];

      Secret : Number;
   begin

      for Ith in 1 .. Values.Last_Index loop
         Secret := Values (Ith);
         Evolve (Secret, Sales);
         Result := @ + Secret;
      end loop;
      IO.Put_Line ("The sum of the 2000th secrets is" & Result'Image);

      Result := 0;
      for Monkey in 1 .. Values.Last_Index loop
         for Ith in -9 .. 9 loop
            for Jth in -9 .. 9 loop
               for Kth in -9 .. 9 loop
                  for Lth in -9 .. 9 loop
                     Result := Number'Max (Result, Sales (Ith, Jth, Kth, Lth));
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      IO.Put_Line ("You can earn at most" & Result'Image & " bananas");

   end Both_Parts;

begin
   Read_Input;
   Both_Parts;
end Day22;
