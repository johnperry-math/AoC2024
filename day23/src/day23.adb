pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

procedure Day23 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   type Node_Name is array (1 .. 2) of Character;

   procedure Put (Node : Node_Name) is
   begin
      IO.Put (Node (1));
      IO.Put (Node (2));
   end Put;

   package Connection_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Node_Name);
   subtype Connection_Set is Connection_Sets.Set;

   function "=" (Left, Right : Connection_Set) return Boolean is
     (Left.Is_Subset (Right) and then Right.Is_Subset (Left));

   package Connection_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Node_Name, Element_Type => Connection_Set);

   Connections : Connection_Maps.Map;

   procedure Read_Input is
      Input     : IO.File_Type;
      Empty_Set : Connection_Set;
   begin
      IO.Open
        (Input, IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      while not IO.End_Of_File (Input) loop
         declare
            Line   : constant String    := IO.Get_Line (Input);
            First  : constant Node_Name := Node_Name (Line (1 .. 2));
            Second : constant Node_Name := Node_Name (Line (4 .. 5));
         begin
            if not Connections.Contains (First) then
               Connections.Insert (First, Empty_Set.Copy);
            end if;
            if not Connections.Contains (Second) then
               Connections.Insert (Second, Empty_Set.Copy);
            end if;
            Connections (First).Include (Second);
            Connections (Second).Include (First);
         end;
      end loop;
   end Read_Input;

   package LAN_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Node_Name);
   subtype LAN_Vector is LAN_Vecs.Vector;
   package LAN_Sorter is new LAN_Vecs.Generic_Sorting;

   function "=" (Left, Right : LAN_Vector) return Boolean is
     (Natural (Left.Length) = Natural (Right.Length)
      and then
      (for all Ith in Left.First_Index .. Left.Last_Index =>
         Left (Ith) = Right (Ith)));

   function "<" (Left, Right : LAN_Vector) return Boolean is
   begin
      if Natural (Left.Length) < Natural (Right.Length) then
         return True;
      elsif Natural (Left.Length) > Natural (Right.Length) then
         return False;
      end if;
      for Ith in Left.First_Index .. Left.Last_Index loop
         if Left (Ith) < Right (Ith) then
            return True;
         elsif Left (Ith) > Right (Ith) then
            return False;
         end if;
      end loop;
      return False;
   end "<";

   package LAN_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Node_Name);
   subtype LAN_Set is LAN_Sets.Set;

   package Lan_Vec_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => LAN_Vector);
   subtype LAN_Vec_Set is Lan_Vec_Sets.Set;

   procedure Find_Triplets (First : Node_Name; Triplets : in out LAN_Vec_Set)
   is
      Lan : LAN_Vector;
   begin
      for Second of Connections (First) loop
         for Third of Connections (Second) loop
            if Connections (First).Contains (Third) then
               Lan.Clear;
               Lan.Append (First);
               Lan.Append (Second);
               Lan.Append (Third);
               LAN_Sorter.Sort (Lan);
               Triplets.Include (Lan);
            end if;
         end loop;
      end loop;
   end Find_Triplets;

   procedure Part_1 is
      Triplets : LAN_Vec_Set;
   begin
      for Cursor in Connections.Iterate when Connection_Maps.Key (Cursor) (1) =
      't'
      loop
         Find_Triplets (Connection_Maps.Key (Cursor), Triplets);
      end loop;
      IO.Put_Line
        ("At least" & Triplets.Length'Image &
         " triplets have a computer that starts with t");
   end Part_1;

   package Node_To_Natural_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Node_Name, Element_Type => Natural);

   procedure Part_2 is
      Result, Temp          : LAN_Set;
      Subnet                : Connection_Set;
      Common, Last_Common   : Natural := 0;
      Node_Count            : Node_To_Natural_Maps.Map;
      This_Node, Other_Node : Node_Name;
   begin
      for Cursor in Connections.Iterate loop
         --  grab the subnet connected to the current key
         This_Node := Connection_Maps.Key (Cursor);
         Subnet    := Connection_Maps.Element (Cursor);

         --  start counting connections
         --  current key isn't connected to itself, so it scores 0
         --  the others score 1
         Node_Count.Clear;
         Node_Count.Insert (This_Node, 0);
         for Each of Subnet loop
            Node_Count.Insert (Each, 1);
         end loop;

         --  increment each node's count when it appears
         for Node of Subnet loop
            for Each of Connections (Node) loop
               if Node_Count.Contains (Each) then
                  Node_Count.Replace (Each, Node_Count (Each) + 1);
               end if;
            end loop;
         end loop;

         --  determine the most common
         Common := 0;
         for Inner_Cursor in Node_Count
           .Iterate when Node_To_Natural_Maps.Key (Inner_Cursor) /= This_Node
         loop
            Common :=
              Natural'Max
                (Common, Node_To_Natural_Maps.Element (Inner_Cursor));
         end loop;

         --  build the LAN
         Temp.Clear;
         Temp.Insert (This_Node);
         for Cursor in Node_Count.Iterate loop
            if Node_To_Natural_Maps.Element (Cursor) = Common then
               Temp.Include (Node_To_Natural_Maps.Key (Cursor));
            end if;
         end loop;

         --  make sure it satisfies the predictions
         --  some connection sets have weird commonalities
         --  for instance, the example has a set containing wh where
         --  wh connects with all the elements, but
         --  none of the others connect with each other
         if Common > Last_Common and then Natural (Temp.Length) = Common + 1
         then
            Last_Common := Common;
            Result      := Temp;
         end if;
      end loop;

      IO.Put ("The largest LAN connects ");
      for Node of Result loop
         Put (Node);
         IO.Put (',');
      end loop;
      IO.New_Line;

   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day23;
