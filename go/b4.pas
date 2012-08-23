program b4;
uses vm;

var
   cores : array [ 0 .. 255 ] of vm.core;
   count, i : byte;
   done : boolean;
begin
   repeat
      count := 0;
      for i := 0 to 255 do
         if cores[ i ].active then
         begin
            cores[ i ].tick;
            inc( count );
         end;
      done := count = 0;
   until done;
end.
