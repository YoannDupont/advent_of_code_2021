with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;

procedure Day01 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;

    type Natural_Array is array(Positive range <>) of Natural;
    Empty_Array : constant Natural_Array(1 .. 0) := (others => <>);

    function Get(F : TIO.File_Type) return Natural_Array is
        function Get(acc : Natural_Array) return Natural_Array is
            val : Natural;
        begin
            if TIO.End_Of_File(F) then
                return acc;
            else
                ITIO.Get(F, val);
                return Get(acc & val);
            end if;
        end Get;
    begin
        return Get(Empty_Array);
    end Get;

    function Sum(arr : Natural_Array) return Natural is
        total : Natural := 0;
    begin
        for item of arr loop
            total := @ + item;
        end loop;
        return total;
    end;

    function Count_Larger(input : Natural_Array; window_size : Positive) return Natural is
        count : Natural := 0;
        previous : Natural := Sum(input(input'First .. input'First + window_size - 1));
        current : Natural;
    begin
        for I in input'First+1 .. input'Last - window_size + 1 loop
            current := Sum(input(I .. I + window_size - 1));
            if previous < current then
                count := @ + 1;
            end if;
            previous := current;
        end loop;
        return count;
    end Count_Larger;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    part_1 : Natural;
    part_2 : Natural;
begin
    TIO.Put_Line("--- Day 1: Sonar Sweep ---");

    TIO.Open(F, TIO.In_File, filepath);
    declare
        input : constant Natural_Array := Get(F);
    begin
        TIO.Close(F);
        part_1 := Count_Larger(input, 1);
        part_2 := Count_Larger(input, 3);
    end;

    TIO.Put_Line("How many measurements are larger than the previous measurement?");
    TIO.Put_Line(part_1'Img);
    TIO.New_Line;
    TIO.Put_Line("Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?");
    TIO.Put_Line(part_2'Img);
end Day01;
