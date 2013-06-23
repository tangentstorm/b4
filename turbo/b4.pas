program b4;
uses ub4, ub4asm;

begin
  open('disk.b4'); boot;
  repeat until step >= maxheap;
  close(disk);
end.