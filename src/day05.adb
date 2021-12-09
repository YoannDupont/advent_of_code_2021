with Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Command_Line;

with Get_Array;

procedure Day05 is
    package TIO renames Ada.Text_IO;
    package ASM renames Ada.Strings.Maps;
    package ASF renames Ada.Strings.Fixed;

    NUMBERS : constant ASM.Character_Set := ASM.To_Set(ASM.Character_Range'('0', '9'));

    type Coordinate is record
        x, y : Integer;
    end record;

    subtype Shift_Range is Integer range -1 .. 1;
    type XY_Shift is record
        x, y : Shift_Range;
    end record;

    type Line_Type is record
        c1, c2 : Coordinate;
    end record;
    type Line_Array is array(Positive range <>) of Line_Type;
    Empty_Lines : constant Line_Array(1 .. 0) := (others => <>);

    type Translation is record
        direction : XY_Shift;
        magnitude : Positive;
    end record;

    type Grid is array(Natural range <>, Natural range <>) of Natural;

    function Get_Translation(line : Line_Type) return Translation is
        t : Translation;
    begin
        if line.c1.x < line.c2.x then
            t.direction.x := 1;
            t.magnitude := abs(line.c1.x - line.c2.x);
        elsif line.c1.x = line.c2.x then
            t.direction.x := 0;
        else
            t.direction.x := -1;
            t.magnitude := abs(line.c1.x - line.c2.x);
        end if;
        if line.c1.y < line.c2.y then
            t.direction.y := 1;
            t.magnitude := abs(line.c1.y - line.c2.y);
        elsif line.c1.y = line.c2.y then
            t.direction.y := 0;
        else
            t.direction.y := -1;
            t.magnitude := abs(line.c1.y - line.c2.y);
        end if;
        return t;
    end Get_Translation;

    function Is_Diagonal(T : Translation) return Boolean is (t.direction.x /= 0 and t.direction.y /= 0);

    function To_Grid(vent_lines : Line_Array) return Grid is
        min_x : Natural := Natural'Last;
        max_x : Natural := 0;
        min_y : Natural := Natural'Last;
        max_y : Natural := 0;
    begin
        for vent_line of vent_lines loop
            min_x := Natural'Min(Natural'Min(@, vent_line.c1.x), vent_line.c2.x);
            max_x := Natural'Max(Natural'Max(@, vent_line.c1.x), vent_line.c2.x);
            min_y := Natural'Min(Natural'Min(@, vent_line.c1.y), vent_line.c2.y);
            max_y := Natural'Max(Natural'Max(@, vent_line.c1.y), vent_line.c2.y);
        end loop;

        return G : Grid(min_x .. max_x, min_y .. max_y) do
            G := (others => (others => 0));
        end return;
    end To_Grid;

    procedure Get (F : TIO.File_Type; vent_line : out Line_Type) is
        line : constant String := TIO.Get_Line(F);
        first : Positive := line'First;
        last : Natural;
    begin
        ASF.Find_Token(line(first .. line'Last), NUMBERS, Ada.Strings.Inside, first, last);
        vent_line.c1.x := Natural'Value(line(first .. last));
        first := last + 1;

        ASF.Find_Token(line(first .. line'Last), NUMBERS, Ada.Strings.Inside, first, last);
        vent_line.c1.y := Natural'Value(line(first .. last));
        first := last + 1;

        ASF.Find_Token(line(first .. line'Last), NUMBERS, Ada.Strings.Inside, first, last);
        vent_line.c2.x := Natural'Value(line(first .. last));
        first := last + 1;

        ASF.Find_Token(line(first .. line'Last), NUMBERS, Ada.Strings.Inside, first, last);
        vent_line.c2.y := Natural'Value(line(first .. last));
    end Get;

    function Get_Lines is new Get_Array(
        Range_Type => Positive,
        Value_Type => Line_Type,
        Array_Type => Line_Array,
        Empty_Array => Empty_Lines,
        Get => Get
    );

    function Read(filepath : String) return Line_Array is
        F : TIO.File_Type;
    begin
        TIO.Open(F, TIO.In_File, filepath);
        declare
            lines : constant Line_Array := Get_Lines(F);
        begin
            TIO.Close(F);
            return lines;
        end;
    end Read;

    function At_Least_Two_Overlaps(input : Line_Array; ignore_diagonals : Boolean) return Natural is
        G : Grid := To_Grid(input);
        t : Translation;
    begin
        for vent_line of input loop
            t := Get_Translation(vent_line);
            if Is_Diagonal(t) and ignore_diagonals then
                goto continue;
            end if;
            for I in 0 .. t.magnitude loop
                G(vent_line.c1.x + I*t.direction.x, vent_line.c1.y + I*t.direction.y) := @ + 1;
            end loop;
            <<continue>>
        end loop;

        return count : Natural := 0 do
            for I of G loop
                if I >= 2 then
                    count := @ + 1;
                end if;
            end loop;
        end return;
    end At_Least_Two_Overlaps;

    function Part_1(input : Line_Array) return Natural is (At_Least_Two_Overlaps(input, ignore_diagonals => True));
    function Part_2(input : Line_Array) return Natural is (At_Least_Two_Overlaps(input, ignore_diagonals => False));

    filepath : constant String := Ada.Command_Line.Argument(1);
    input : constant Line_Array := Read(filepath);
begin
    TIO.Put_Line("--- Day 5: Hydrothermal Venture ---");

    TIO.Put_Line("Consider only horizontal and vertical lines. At how many points do at least two lines overlap?");
    TIO.Put_Line(Part_1(input)'Img);

    TIO.New_Line;
    TIO.Put_Line("Consider all of the lines. At how many points do at least two lines overlap?");
    TIO.Put_Line(Part_2(input)'Img);
end Day05;
