program b4;
uses ub4, ub4asm;

begin
  open('disk.b4'); boot;
  assign(input, 'bios.b4a');
  reset(input); b4as;
  ram[ram[ip]] := 40;
  repeat until step >= maxheap;
  close(disk);
end.