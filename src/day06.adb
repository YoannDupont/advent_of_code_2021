with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

procedure Day06 is
    package TIO renames Ada.Text_IO;
    package ASM renames Ada.Strings.Maps;
    package ASF renames Ada.Strings.Fixed;

    NUMBERS : constant ASM.Character_Set := ASM.To_Set(ASM.Character_Range'('0', '9'));

    subtype Lanternfish is Natural range 0 .. 8;
    FIRST_SPAWN : constant Lanternfish := 8;
    AFTER_BIRTH : constant Lanternfish := 6;
    subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

    type Counts is array(Lanternfish) of Long_Long_Natural;

    package Lanternfish_Vectors is new Ada.Containers.Vectors(Positive, Lanternfish);
    subtype Lanternfish_Vector is Lanternfish_Vectors.Vector;

    function Get(F : in TIO.File_Type) return Lanternfish_Vector is
        vec : Lanternfish_Vector;
        line : constant String := TIO.Get_Line(F);
        first : Positive := line'First;
        last : Natural;
    begin
        loop
            ASF.Find_Token(line(first .. line'Last), NUMBERS, Ada.Strings.Inside, first, last);
            exit when last < first;
            vec.Append(Lanternfish'Value(line(first .. last)));
            first := last + 1;
        end loop;
        return vec;
    end Get;

    function Simulate(input : in Lanternfish_Vector; how_many_days : Natural) return Long_Long_Natural is
        counter : Counts := (others => 0);
        total : Long_Long_Natural := 0;
    begin
        for fish of input loop
            counter(fish) := @ + 1;
        end loop;

        for day in 1 .. how_many_days loop
            counter := counter(Lanternfish'First + 1 .. Lanternfish'Last) & counter(Lanternfish'First);
            counter(AFTER_BIRTH) := @ + counter(FIRST_SPAWN);
        end loop;

        for item of counter loop
            total := @ + item;
        end loop;

        return total;
    end Simulate;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    input : Lanternfish_Vector;
    part_1 : constant Positive := 80;
    part_2 : constant Positive := 256;
begin
    TIO.Put_Line("--- Day 6: Lanternfish ---");

    TIO.Open(F, TIO.In_File, filepath);
    input := Get(F);
    TIO.Close(F);

    TIO.Put_Line("How many lanternfish would there be after 80 days?");
    TIO.Put_Line(Simulate(input, part_1)'Img);

    TIO.New_Line;
    TIO.Put_Line("How many lanternfish would there be after 256 days?");
    TIO.Put_Line(Simulate(input, part_2)'Img);
end Day06;
