{
| b4 mainloop :
|
|   creates 256 vm instances and runs continuously
|   until they all shut down.
|
| -------------------------------------------------
| copyright (c) 2012 michal j. wallace
| see LICENSE.org for usage information
}
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
