pragma Ada_2022;

with Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
use all type Ada.Containers.Count_Type;

procedure Day09 is

   package IO renames Ada.Text_IO;

   type Block_Record is record
      Number, Length : Natural;
   end record;

   package Block_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Natural,
        Element_Type => Block_Record);

   use all type Block_Maps.Cursor;

   Blocks : Block_Maps.Map;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      declare
         Line           : constant String := IO.Get_Line (Input);
         Position       : Positive := Line'First;
         Block_Number   : Natural := 0;
         Block_Position : Natural := 0;
         Block_Length   : Natural;
      begin
         while Position <= Line'Last loop
            Block_Length :=
              Character'Pos (Line (Position)) - Character'Pos ('0');
            Blocks.Insert
              (Block_Position,
               Block_Record'(Number => Block_Number, Length => Block_Length));
            Block_Position := @ + Block_Length;
            Position := @ + 1;
            if Position <= Line'Last then
               Block_Length :=
                 Character'Pos (Line (Position)) - Character'Pos ('0');
               Block_Position := @ + Block_Length;
               Position := @ + 1;
               Block_Number := @ + 1;
            end if;
         end loop;
      end;
   end Read_Input;

   First_Free_Location : Natural := 0;
   First_Free_Size     : Natural := 0;

   procedure Advance_Free_Block (Blocks : Block_Maps.Map) is
      Cursor : Block_Maps.Cursor := Blocks.First;
   begin
      while Cursor /= Block_Maps.No_Element
        and then First_Free_Location >= Block_Maps.Key (Cursor)
      loop
         First_Free_Location :=
           Block_Maps.Key (Cursor) + Block_Maps.Element (Cursor).Length;
         Block_Maps.Next (Cursor);
         if Cursor = Block_Maps.No_Element then
            First_Free_Size := Natural'Last;
         else
            First_Free_Size := Block_Maps.Key (Cursor) - First_Free_Location;
         end if;
      end loop;
   end Advance_Free_Block;

   procedure Coalesce (Blocks : in out Block_Maps.Map) is
      Curr, Next : Block_Maps.Cursor := Blocks.First;
   begin
      while Curr /= Block_Maps.No_Element loop
         Next := Block_Maps.Next (Curr);
         exit when Next = Block_Maps.No_Element;
         if Block_Maps.Element (Curr).Number = Block_Maps.Element (Next).Number
           and then Block_Maps.Key (Curr) + Block_Maps.Element (Curr).Length
                    = Block_Maps.Key (Next)
         then
            Blocks.Reference (Curr).Length :=
              @ + Block_Maps.Element (Next).Length;
            Blocks.Delete (Next);
         end if;
         Curr := Block_Maps.Next (Curr);
      end loop;
   end Coalesce;

   type Checksum_Value is range 0 .. 2 ** 64 - 1;

   procedure Part_1 is
      Delete_Last : Boolean;
      Checksum    : Checksum_Value := 0;
      Blocks_Copy : Block_Maps.Map := Blocks.Copy;
      Blocks renames Blocks_Copy;
   begin
      Advance_Free_Block (Blocks);
      while Blocks.Last_Key > First_Free_Location loop
         Delete_Last := False;
         declare
            Last_Block renames Blocks.Last_Element;
         begin
            if Last_Block.Length <= First_Free_Size then
               Delete_Last := True;
               Blocks.Insert (First_Free_Location, Last_Block);
            elsif Last_Block.Length > First_Free_Size then
               Blocks.Insert
                 (First_Free_Location,
                  Block_Record'(Last_Block.Number, First_Free_Size));
               Blocks.Replace_Element
                 (Blocks.Last,
                  Block_Record'
                    (Last_Block.Number, Last_Block.Length - First_Free_Size));
            end if;
         end;
         if Delete_Last then
            Blocks.Delete_Last;
         end if;
         --  Coalesce (Blocks);
         Advance_Free_Block (Blocks);
      end loop;
      for Cursor in Blocks.Iterate loop
         declare
            Outer : constant Checksum_Value :=
              Checksum_Value
                (Block_Maps.Key (Cursor) + Block_Maps.Element (Cursor).Length
                 - 1);
            Inner : constant Checksum_Value :=
              Checksum_Value (Integer'Max (0, Block_Maps.Key (Cursor) - 1));
         begin
            Checksum :=
              @
              + Checksum_Value (Block_Maps.Element (Cursor).Number)
                * (Outer * (Outer + 1) / 2 - Inner * (Inner + 1) / 2);
         end;
      end loop;
      IO.Put_Line ("Checksum is" & Checksum'Image);
   end Part_1;

   procedure Part_2 is
      Delete_Last      : Boolean;
      Checksum         : Checksum_Value := 0;
      Cursor           : Block_Maps.Cursor := Blocks.Last;
      Curr, Next, Temp : Block_Maps.Cursor;
   begin
      while Cursor /= Blocks.First loop
         Curr := Blocks.First;
         loop
            First_Free_Location :=
              Block_Maps.Key (Curr) + Block_Maps.Element (Curr).Length;
            Next := Block_Maps.Next (Curr);
            exit when
              Next = Block_Maps.No_Element
              or else Block_Maps.Key (Next) < First_Free_Location;
            First_Free_Size := Block_Maps.Key (Next) - First_Free_Location;
            exit when
              First_Free_Location > Block_Maps.Key (Cursor)
              or else First_Free_Size >= Block_Maps.Element (Cursor).Length;
            Block_Maps.Next (Curr);
         end loop;
         Temp := Block_Maps.Previous (Cursor);
         if First_Free_Location < Block_Maps.Key (Cursor) then
            Blocks.Insert (First_Free_Location, Block_Maps.Element (Cursor));
            Blocks.Delete (Cursor);
            --  Coalesce (Blocks);

         end if;
         Cursor := Temp;
      end loop;
      for Cursor in Blocks.Iterate loop
         declare
            Outer : constant Checksum_Value :=
              Checksum_Value
                (Block_Maps.Key (Cursor) + Block_Maps.Element (Cursor).Length
                 - 1);
            Inner : constant Checksum_Value :=
              Checksum_Value (Integer'Max (0, Block_Maps.Key (Cursor) - 1));
         begin
            Checksum :=
              @
              + Checksum_Value (Block_Maps.Element (Cursor).Number)
                * (Outer * (Outer + 1) / 2 - Inner * (Inner + 1) / 2);
         end;
      end loop;
      IO.Put_Line ("Checksum is" & Checksum'Image);
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day09;
