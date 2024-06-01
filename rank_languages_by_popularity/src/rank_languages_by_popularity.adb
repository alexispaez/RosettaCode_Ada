pragma Ada_2022;

with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Less_Case_Insensitive;

with AWS.Client;            use AWS.Client;
with AWS.Messages;          use AWS.Messages;
with AWS.Response;

procedure Rank_Languages_By_Popularity is

   use Ada.Strings;

   function "+" (S : String) return Unbounded_String
                 renames To_Unbounded_String;

   type A_Language_Count is
      record
         Count    : Integer := 0;
         Language : Unbounded_String;
      end record;

   overriding
   function "=" (L, R : A_Language_Count) return Boolean is
   begin
      return L.Language = R.Language;
   end "=";

   function "<" (L, R : A_Language_Count) return Boolean is
   begin
      --  Sort by 'Count' and then by Language name
      return L.Count < R.Count
        or else (L.Count = R.Count
                 and then Less_Case_Insensitive
                   (Left  => To_String (L.Language),
                    Right => To_String (R.Language)));
   end "<";

   package Sets is new Ada.Containers.Ordered_Sets (A_Language_Count);
   use Sets;

   Counts : Set;
   Total_Counts : Natural := 0;

   procedure Find_Counts (S : String) is

      function Strip_Character (S : String; C : String) return String is
         Comma_At : constant Natural := Ada.Strings.Fixed.Index (S, C);
      begin
         if Comma_At = 0 then
            return S;
         else
            return Strip_Character (S (S'First .. Comma_At - 1)
                                    & S (Comma_At + 1 .. S'Last), C);
         end if;
      end Strip_Character;

      Title_Str       : constant String  := "title=""Category:";
      End_A_Tag_Str   : constant String  := "</a>";
      Space_Paren_Str : constant String := " (";

      Title_At        : constant Natural := Index (S, Title_Str);
   begin
      --  Put_Line (S (S'First .. S'First + 500));
      --  Put_Line ("Title_At: " & Title_At'Image);

      if Title_At /= 0 then
         declare
            Closing_Bracket_At : constant Natural :=
               Index (S (Title_At + Title_Str'Length .. S'Last), ">");

            End_A_Tag_At       : constant Natural :=
               Index (S (Closing_Bracket_At + 1 .. S'Last), End_A_Tag_Str);

            Language : constant String  :=
               S (Closing_Bracket_At + 1 .. End_A_Tag_At - 1);

            Space_Paren_At     : constant Natural :=
               Index (S (End_A_Tag_At + 1 .. S'Last), Space_Paren_Str);

            Space_At           : constant Natural :=
               Index (S (Space_Paren_At + Space_Paren_Str'Length + 1
                      .. S'Last),
                      " ");

            Count : constant Natural :=
               Natural'Value (Strip_Character (
                              S (Space_Paren_At + Space_Paren_Str'Length
                                .. Space_At - 1),
                              ","));
         begin
            --  Put_Line ("Closing_Bracket_At: " & Closing_Bracket_At'Image);
            --  Put_Line ("End_A_Tag_At: " & End_A_Tag_At'Image);
            --  Put_Line ("Language: " & Language);
            --  Put_Line ("Space_Paren_At: " & Space_Paren_At'Image);
            --  Put_Line ("Space_At: " & Space_At'Image);
            --  Put_Line ("Count: " & Count'Image);

            if Closing_Bracket_At /= 0
              and then End_A_Tag_At /= 0
              and then Space_Paren_At /= 0
              and then Space_At /= 0
            then
               begin
                  --  Put_Line ("Inserting: " & Count'Image & ", " & Language);

                  Counts.Insert (New_Item => (Count, +Language));

                  Total_Counts := Total_Counts + 1;
                  --  Put_Line ("Element number: " & Total_Counts'Image);
               exception
                  when Constraint_Error =>
                     Put_Line (Standard_Error, "Warning: repeated language: " &
                                 Language);
                     --  Ignore repeated results.
                     null;
               end;
            end if;
            --  Recursively parse the string for languages and counts
            Find_Counts (S (Space_At + 1 .. S'Last));
         end;
      end if;

   end Find_Counts;

   Place : Natural := 1;

   procedure Display (C : Cursor) is
   begin
      Put (Place, Width => 1);             Put (". ");
      Put (Element (C).Count, Width => 1); Put (" - ");
      Put_Line (To_String (Element (C).Language));
      Place := Place + 1;
   end Display;

   Http_Source : constant AWS.Response.Data :=
                   AWS.Client.Get ("http://rosettacode.org/w/index.php?" &
                                   "title=Special:Categories&limit=5000"
                                   , Follow_Redirection => True);
   Status      : Status_Code;
begin
   Put_Line ("Getting website data...");

   Status := AWS.Response.Status_Code (Http_Source);
   if Status not in Success then
      Put_Line ("Unable to retrieve data => Status Code :" &
                  Image (Status) &
                  " Reason :" & Reason_Phrase (Status));
      raise Connection_Error;
   end if;

   Put_Line ("Finding categories...");
   Find_Counts (AWS.Response.Message_Body (Http_Source));

   Put_Line ("Displaying categories...");
   Counts.Reverse_Iterate (Display'Access);

   Put_Line ("Process complete.");
end Rank_Languages_By_Popularity;
