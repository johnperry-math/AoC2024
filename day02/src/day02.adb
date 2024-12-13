pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day02 is

   package IO renames Ada.Text_IO;
   package Int_IO is new IO.Integer_IO (Num => Integer);

   package Int_Vecs is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Integer);

   use all type Int_Vecs.Vector;

   package Int_Vec_Vecs is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Int_Vecs.Vector,
        "="          => "=");

   Reports : Int_Vec_Vecs.Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Position : Natural := Line'First;
            Value    : Integer;
            List     : Int_Vecs.Vector;
         begin
            while Position <= Line'Last loop
               Int_IO.Get (Line (Position .. Line'Last), Value, Position);
               Position := @ + 1;
               List.Append (Value);
            end loop;
            Reports.Append (List);
         end;
      end loop;
   end Read_Input;

   function Is_Safe (Report : Int_Vecs.Vector) return Boolean is
      Increasing, Maybe_Safe, First : Boolean;
   begin
      First := True;
      Maybe_Safe := True;
      for Ith in Report.First_Index + 1 .. Report.Last_Index loop
         if abs (Report (Ith) - Report (Ith - 1)) not in 1 .. 3 then
            Maybe_Safe := False;
         elsif First then
            First := False;
            Increasing := Report (Ith) > Report (Ith - 1);
         elsif Report (Ith) > Report (Ith - 1) then
            if Increasing then
               null;
            else
               Maybe_Safe := False;
            end if;
         else
            if Increasing then
               Maybe_Safe := False;
            else
               null;
            end if;
         end if;
         exit when not Maybe_Safe;
      end loop;
      return Maybe_Safe;
   end Is_Safe;

   procedure Part_1 is
      Num_Safe : Natural := 0;
   begin
      for Report of Reports loop
         if Is_Safe (Report) then
            Num_Safe := @ + 1;
         end if;
      end loop;
      IO.Put_Line ("There are" & Num_Safe'Image & " safe reports");
   end Part_1;

   procedure Part_2 is
      Num_Safe : Natural := 0;
   begin
      for Report of Reports loop
         if Is_Safe (Report) then
            Num_Safe := @ + 1;
         else
            for Ith in Report.First_Index .. Report.Last_Index loop
               declare
                  New_Report : Int_Vecs.Vector;
               begin
                  for Jth
                    in Report.First_Index .. Report.Last_Index
                    when Ith /= Jth
                  loop
                     New_Report.Append (Report (Jth));
                  end loop;
                  if Is_Safe (New_Report) then
                     Num_Safe := @ + 1;
                     exit;
                  end if;
               end;
            end loop;
         end if;
      end loop;
      IO.Put_Line
        ("With dampening, there are" & Num_Safe'Image & " safe reports");
   end Part_2;

begin
   Read_Input;
   Part_1;
   Part_2;
end Day02;
