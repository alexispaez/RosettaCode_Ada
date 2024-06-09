pragma Ada_2022;

with Prime_Numbers;
with Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Containers.Vectors;

procedure Powerful_Numbers_Brute_Force is

   package Big_Integer_Prime_Numbers is new Prime_Numbers
     (Big_Integer, 0, 1, 2);
   use Big_Integer_Prime_Numbers;

   package Positive_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Natural,
        Element_Type => Positive);

   function Is_K_Powerful (Number : Natural; K : Natural) return Boolean is
      List : constant Number_List  := Decompose (To_Big_Integer (Number));
   begin
      for Index in List'Range loop
         if To_Big_Integer (Number) mod (List (Index)**K) /= 0 then
            return False;
         end if;
      end loop;

      return True;
   end Is_K_Powerful;

   Number_Set : Positive_Vectors.Vector;

begin
   --  Generate set of k-powerful numbers <= 10 ** K and
   --  show the 5 first and last terms along the length of the set
   for K in 2 .. 10 loop

      Number_Set.Clear;

      for Number in 1 .. 10**K loop
         declare
            Is_Powerful : Boolean;
         begin
            Is_Powerful := Is_K_Powerful (Number, K);

            if Is_Powerful then
               Number_Set.Append (Number);
            end if;
         end;
      end loop;

      Ada.Text_IO.Put (Number_Set.Length'Image);
      Ada.Text_IO.Put (" " &
                         K'Image &
                         "-powerfuls up to 10 ^" &
                         K'Image & ": ");
      --  for E of Number_Set loop
      --     Ada.Text_IO.Put (E'Image & " ");
      --  end loop;
      for Index in Number_Set.First_Index .. 4 loop
         Ada.Text_IO.Put (Number_Set.Element (Index)'Image & " ");
      end loop;
      Ada.Text_IO.Put ("... ");
      for Index in Number_Set.Last_Index - 4 .. Number_Set.Last_Index  loop
         Ada.Text_IO.Put (Number_Set.Element (Index)'Image & " ");
      end loop;

      Ada.Text_IO.New_Line;
   end loop;
end Powerful_Numbers_Brute_Force;
