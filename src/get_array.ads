with Ada.Text_IO;
generic
   type Range_Type is (<>);
   type Value_Type is private;
   type Array_Type is array(Range_Type range <>) of Value_Type;
   Emtpy_Array : Array_Type;
   with procedure Get(F: Ada.Text_IO.File_Type; item : out Value_Type);
function Get_Array(F : Ada.Text_IO.File_Type) return Array_Type;