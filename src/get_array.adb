function Get_Array(F : Ada.Text_IO.File_Type) return Array_Type is
    function Get(acc : Array_Type) return Array_Type is
        item : Value_Type;
    begin
        if Ada.Text_IO.End_Of_File(F) then
            return acc;
        else
            Get(F, item);
            return Get(acc & item);
        end if;
    end Get;
begin
    return Get(Empty_Array);
end Get_Array;