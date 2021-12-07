with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

use type Ada.Containers.Count_Type;

procedure Day04 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;
    package ASF renames Ada.Strings.Fixed;

    subtype Bingo_Range is Positive range 1 .. 5;
    type Bingo_Board is array(Bingo_Range, Bingo_Range) of Natural;
    type Bingo_Mask is array(Bingo_Range, Bingo_Range) of Natural;
    package Bingo_Board_Vectors is new Ada.Containers.Vectors(Positive, Bingo_Board);
    subtype Bingo_Board_Vector is Bingo_Board_Vectors.Vector;

    type Natural_Array is array(Positive range <>) of Natural;
    package Natural_Sets is new Ada.Containers.Ordered_Sets(Natural);
    subtype Natural_Set is Natural_Sets.Set;

    type Bingo_Subsystem(length : Natural) is record
        numbers : Natural_Array(1 .. length) := (others => 0);
        boards : Bingo_Board_Vector;
    end record;

    function Get(F : in TIO.File_Type) return Bingo_Subsystem is
        numbers_str : constant String := TIO.Get_Line(F);
        subsystem : Bingo_Subsystem(ASF.Count(numbers_str, ",") + 1);
        nth : Positive := subsystem.numbers'First;
        start : Natural := numbers_str'First;
        index : Natural;
        tmp : Bingo_Board;
    begin
        while start <= numbers_str'Last loop
            index := ASF.Index(numbers_str, ",", from => start);
            if index < start then
                index := numbers_str'Last + 1;
            end if;
            subsystem.numbers(nth) := Natural'Value(numbers_str(start .. index - 1));
            start := index + 1;
            nth := @ + 1;
        end loop;

        while not TIO.End_Of_File(F) loop
           tmp := (others => (others => 0));
           for I in Bingo_Range loop
                for J in Bingo_Range loop
                    ITIO.Get(F, tmp(J, I));
                end loop;
           end loop;
           subsystem.boards.Append(tmp);
        end loop;

        return subsystem;
    end Get;

    function Play_Bingo(input : Bingo_Subsystem; get_first : Boolean) return Natural is
        mask : array(1 .. Natural(input.boards.Length)) of Bingo_Mask := (others => (others => (others => 1)));
        rows_remaining : array(1 .. Natural(input.boards.Length)) of Natural_Array(Bingo_Range) := (others => (others => 5));
        cols_remaining : array(1 .. Natural(input.boards.Length)) of Natural_Array(Bingo_Range) := (others => (others => 5));
        board : Bingo_Board;
        winning : Boolean;
        winning_number : Natural;
        unmarked_sum : Natural := 0;
        remaining_boards : Natural_Set;
        winning_boards_step : Natural_Set;
    begin
        for I in 1 .. Natural(input.boards.Length) loop
            remaining_boards.Insert(I);
        end loop;

        mainloop: for number of input.numbers loop
            for I of remaining_boards loop
                winning := False;
                board := input.boards(I);
                for x in Bingo_Range loop
                    for y in Bingo_Range loop
                        if board(y, x) = number then
                            mask(I)(y, x) := 0;
                            rows_remaining(I)(y) := @ - 1;
                            cols_remaining(I)(x) := @ - 1;
                            winning := winning or (rows_remaining(I)(y) = 0 or cols_remaining(I)(x) = 0);
                            if winning then
                                winning_boards_step.Include(I);
                            end if;
                        end if;
                    end loop;
                end loop;
                if winning and (get_first or remaining_boards.Length = 1) then
                    winning_number := number;
                    for x in Bingo_Range loop
                        for y in Bingo_Range loop
                            unmarked_sum := @ + (mask(I)(y, x) * board(y, x));
                        end loop;
                    end loop;
                    exit mainloop;
                end if;
            end loop;
            for winning_board of winning_boards_step loop
                Natural_Sets.Exclude(remaining_boards, winning_board);
            end loop;
            winning_boards_step.Clear;
        end loop mainloop;

        return unmarked_sum * winning_number;
    end Play_Bingo;

    function Part_1(input : Bingo_Subsystem; get_first : Boolean := True) return Natural renames Play_Bingo;
    function Part_2(input : Bingo_Subsystem; get_first : Boolean := False) return Natural renames Play_Bingo;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Put_Line("--- Day 4: Giant Squid ---");

    TIO.Open(F, TIO.In_File, filepath);
    declare
        input : constant Bingo_Subsystem := Get(F);
    begin
        TIO.Close(F);

        TIO.Put_Line("To guarantee victory against the giant squid, figure out which Bingo_Board will win first. What will your final score be if you choose that Bingo_Board?");
        TIO.Put_Line(Part_1(input)'Img);

        TIO.New_Line;
        TIO.Put_Line("Figure out which board will win last. Once it wins, what would its final score be?");
        TIO.Put_Line(Part_2(input)'Img);
    end;
end Day04;
