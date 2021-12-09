with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

procedure Day07 is
    package TIO renames Ada.Text_IO;
    package ASM renames Ada.Strings.Maps;
    package ASF renames Ada.Strings.Fixed;

    NUMBERS : constant ASM.Character_Set := ASM.To_Set(ASM.Character_Range'('0', '9'));

    subtype Position is Natural;
    type Position_Array is array(Natural range <>) of Position;

    function Get(F : TIO.File_Type) return Position_Array is
        line : constant String := TIO.Get_Line(F);
        arr : Position_Array(1 .. ASF.Count(line, ",") + 1);
        nth : Positive := arr'First;
        first : Positive := line'First;
        last : Natural;
    begin
        loop
            ASF.Find_Token(line(first .. line'Last), NUMBERS, Ada.Strings.Inside, first, last);
            exit when last < first;
            arr(nth) := Position'Value(line(first .. last));
            nth := @ + 1;
            first := last + 1;
        end loop;
        return arr;
    end Get;

    function Read(filepath : String) return Position_Array is
        F : TIO.File_Type;
    begin
        TIO.Open(F, TIO.In_File, filepath);
        declare
            arr : constant Position_Array := Get(F);
        begin
            TIO.Close(F);
            return arr;
        end;
    end Read;

    function Part_1(first, last : Position) return Position_Array is (Position_Array'(for I in first .. last => I));

    function Part_2(first, last : Position) return Position_Array is (Position_Array'(for I in first .. last => (I * (I+1)) / 2));

    function Align(input : Position_Array; Fuel_Cost : access function(first, last : Position) return Position_Array) return Natural is
        total_cost : Natural := Natural'Last;
        tmp : Position;
        mini : Position := Position'Last;
        maxi : Position := Position'First;
    begin
        for item of input loop
            mini := Position'Min(@, item);
            maxi := Position'Max(@, item);
        end loop;

        declare
            cost : constant Position_Array(0 .. maxi) := Fuel_Cost(0, maxi);
        begin
            for I in mini .. maxi loop
                tmp := Position_Array'(for idx in input'Range => cost(abs(input(idx) - I)))'Reduce("+", 0);
                total_cost := Position'Min(@, tmp);
            end loop;
        end;

        return total_cost;
    end Align;

    filepath : constant String := Ada.Command_Line.Argument(1);
    input : constant Position_Array := Read(filepath);
begin
    TIO.Put_Line("--- Day 7: The Treachery of Whales ---");

    TIO.Put_Line("How much fuel must they spend to align to that position?");
    TIO.Put_Line(Align(input, Part_1'Access)'Img);

    TIO.New_Line;
    TIO.Put_Line("This is a placeholder for part 2.");
    TIO.Put_Line(Align(input, Part_2'Access)'Img);
end Day07;
