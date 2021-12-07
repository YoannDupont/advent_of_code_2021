with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;

use type Ada.Containers.Count_Type;

procedure Day03 is
    package TIO renames Ada.Text_IO;

    subtype Binary is Character range '0' .. '1';
    type Binary_String is array(Positive range <>) of Binary;
    package Binary_String_Vectors is new Ada.Containers.Indefinite_Vectors(Positive, Binary_String);
    subtype Binary_String_Vector is Binary_String_Vectors.Vector;

    type Counter is array(Binary) of Natural;
    base_counter : constant Counter := (others => 0);

    function To_String(BS : Binary_String) return String is
        output : String(BS'Range);
    begin
        for I in BS'Range loop
            output(I) := BS(I);
        end loop;
        return output;
    end To_String;

    function To_Decimal(BS : Binary_String) return Natural is
    begin
        return Natural'Value("2#" & To_String(BS) & "#");
    end To_Decimal;

    function Get(F : in TIO.File_Type) return Binary_String is
        line : constant String := TIO.Get_Line(F);
        BS : Binary_String(1 .. line'Length);
    begin
        for I in 0 .. line'Length - 1 loop
            BS(BS'First + I) := Binary(line(line'First + I));
        end loop;
        return BS;
    end Get;

    function Get(F : TIO.File_Type) return Binary_String_Vector is
        result : Binary_String_Vector;
    begin
        while not TIO.End_Of_File(F) loop
            result.Append(Binary_String'(Get(F)));
        end loop;
        return result;
    end Get;

    procedure Part_1(input : Binary_String_Vector; gamma_rate, epsilon_rate : out Natural) is
        counts : array(0 .. input.First_Element'Length - 1) of Counter := (others => base_counter);
        gamma_binary : Binary_String(1 .. counts'Last + 1);
        epsilon_binary : Binary_String(1 .. counts'Last + 1);
    begin
        for item of input loop
            for shift in 0 .. item'Length - 1 loop
                counts(shift)(item(item'First + shift)) := @ + 1;
            end loop;
        end loop;

        for shift in counts'Range loop
            if counts(shift)('0') > counts(shift)('1') then
                gamma_binary(gamma_binary'First + shift) := '0';
                epsilon_binary(epsilon_binary'First + shift) := '1';
            else
                gamma_binary(gamma_binary'First + shift) := '1';
                epsilon_binary(epsilon_binary'First + shift) := '0';
            end if;
        end loop;

        gamma_rate := To_Decimal(gamma_binary);
        epsilon_rate := To_Decimal(epsilon_binary);
    end Part_1;

    procedure Part_2(input : Binary_String_Vector; oxygen_generator_rating, co2_scrubber_rating : out Natural) is
        counts : Counter;
        oxygen_candidates : Binary_String_Vector := input;
        co2_candidates : Binary_String_Vector := input;
        oxygen_tmp, co2_tmp : Binary_String_Vector;
        least, most : Binary;
    begin
        for shift in 0 .. Natural(input.First_Element'Length - 1) loop
            if oxygen_candidates.Length > 1 then
                oxygen_tmp.Clear;
                counts := base_counter;
                for element of oxygen_candidates loop
                    counts(element(element'First + shift)) := @ + 1;
                end loop;
                if counts('0') > counts('1') then
                    most := '0';
                else
                    most := '1';
                end if;
                for element of oxygen_candidates loop
                    if element(element'First + shift) = most then
                        oxygen_tmp.Append(element);
                    end if;
                end loop;
                oxygen_candidates := oxygen_tmp.Copy;
            end if;

            if co2_candidates.Length > 1 then
                co2_tmp.Clear;
                counts := base_counter;
                for element of co2_candidates loop
                    counts(element(element'First + shift)) := @ + 1;
                end loop;
                if counts('0') > counts('1') then
                    least := '1';
                else
                    least := '0';
                end if;
                for element of co2_candidates loop
                    if element(element'First + shift) = least then
                        co2_tmp.Append(element);
                    end if;
                end loop;
                co2_candidates := co2_tmp.Copy;
            end if;

            exit when oxygen_candidates.Length = 1 and co2_candidates.Length = 1;
        end loop;

        oxygen_generator_rating := To_Decimal(oxygen_candidates.First_Element);
        co2_scrubber_rating := To_Decimal(co2_candidates.First_Element);
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    input : Binary_String_Vector;
    gamma_rate, epsilon_rate : Natural;
    oxygen_generator_rating, co2_scrubber_rating : Natural;
begin
    TIO.Put_Line("--- Day 3: Binary Diagnostic ---");

    TIO.Open(F, TIO.In_File, filepath);
    input := Get(F);
    TIO.Close(F);

    TIO.Put_Line("What is the power consumption of the submarine? (Be sure to represent your answer in decimal, not binary.)");
    Part_1(input, gamma_rate, epsilon_rate);
    TIO.Put_Line(Natural'Image(gamma_rate * epsilon_rate));

    TIO.New_Line;
    TIO.Put_Line("What is the life support rating of the submarine? (Be sure to represent your answer in decimal, not binary.)");
    Part_2(input, oxygen_generator_rating, co2_scrubber_rating);
    TIO.Put_Line(Natural'Image(oxygen_generator_rating * co2_scrubber_rating));
end Day03;
