with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Get_Array;

procedure Day02 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;

    type Direction is (Down, Up, Forward);
    package Direction_IO is new Ada.Text_IO.Enumeration_IO(Direction);

    type Instruction is record
        where : Direction;
        amount : Positive;
    end record;

    type Course is array(Positive range <>) of Instruction;
    Empty_Course : constant Course(1 .. 0) := (others => <>);

    type State is record
        horizontal, depth, aim : Integer;
    end record;

    function "+"(L, R : State) return State is ((L.horizontal + R.horizontal, L.depth + R.depth, L.aim + R.aim));

    function "*"(C : State; amount : Positive) return State is ((C.horizontal * amount, C.depth * amount, C.aim * amount));

    procedure Part_1(current : in out State; step : Instruction) is
        update : State;
    begin
        case step.where is
            when Down => update := (0, 1, 0) * step.amount;
            when Up => update := (0, -1, 0) * step.amount;
            when Forward => update := (1, 0, 0) * step.amount;
        end case;
        current := @ + update;
    end Part_1;

    procedure Part_2(current : in out State; step : Instruction) is
        update : State;
    begin
        case step.where is
            when Down => update := (0, 0, 1) * step.amount;
            when Up => update := (0, 0, -1) * step.amount;
            when Forward => update := (1, current.aim, 0) * step.amount;
        end case;
        current := @ + update;
    end Part_2;

    procedure Get(F : TIO.File_Type; S : out Instruction) is
    begin
        Direction_IO.Get(F, S.where);
        ITIO.Get(F, S.amount);
    end Get;

    function Get_Course is new Get_Array(
        Range_Type => Positive,
        Value_Type => Instruction,
        Array_Type => Course,
        Empty_Array => Empty_Course,
        Get => Get
    );

    function Follow(input : Course; Do_Step : access procedure(current : in out State; step : Instruction)) return Natural is
        current : State := (0, 0, 0);
    begin
        for step of input loop
            Do_Step(current, step);
        end loop;

        return current.horizontal * current.depth;
    end Follow;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Put_Line("--- Day 2: Dive! ---");

    TIO.Open(F, TIO.In_File, filepath);
    declare
        input : constant Course := Get_Course(F);
    begin
        TIO.Close(F);

        TIO.Put_Line("What do you get if you multiply your final horizontal position by your final depth?");
        TIO.Put_Line(Follow(input, Part_1'Access)'Img);

        TIO.New_Line;
        TIO.Put_Line("Using this new interpretation of the commands, what do you get if you multiply your final horizontal position by your final depth?");
        TIO.Put_Line(Follow(input, Part_2'Access)'Img);
    end;
end Day02;
