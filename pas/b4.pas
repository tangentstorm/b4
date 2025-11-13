{$mode delphi}
{ b4: stdio-only runner for B4 VM }
program b4;
uses sysutils, ub4;

var
  arg: string;
  i: integer;
  loadfile: string;
  f: file of byte;
  b: byte;
  addr: integer;

begin
  loadfile := '';

  { Parse command line }
  i := 1;
  while i <= ParamCount do begin
    arg := ParamStr(i);
    if arg = '-l' then begin
      inc(i);
      if i <= ParamCount then
        loadfile := ParamStr(i);
    end else if arg = '-d' then
      rg[RDB] := 1;
    inc(i);
  end;

  { Load file if specified }
  if loadfile <> '' then begin
    if not FileExists(loadfile) then begin
      writeln('Error: file not found: ', loadfile);
      halt(1);
    end;
    { Load b4x file - it's a complete memory image including registers }
    assign(f, loadfile);
    reset(f);
    addr := 0;
    while (not eof(f)) and (addr <= maxcell) do begin
      read(f, b);
      mem[addr] := b;
      inc(addr);
    end;
    close(f);
    { Set entry point: use RGO if set, otherwise default to minheap }
    if rg[RGO] <> 0 then
      rg[RIP] := rg[RGO]  { Entry point from @\ register }
    else
      rg[RIP] := minheap;  { Default entry point at 0x100 }
  end else begin
    { No file loaded - initialize empty VM }
    boot;
    rg[RIP] := minheap;
  end;

  { Main execution loop }
  while rg[RIP] <= maxheap do begin
    try
      step;
    except
      on e: Exception do begin
        writeln('Exception: ', e.Message);
        halt(1);
      end;
    end;
  end;
end.
