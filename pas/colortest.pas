program colortest;
uses uhw_vt;

var ch: char;
begin
  for ch in 'krgybmcwKRGYBMCW' do begin
    if ch = 'k' then bg('w') else bg('k');
    fg(ch); write(ch);
  end;
  writeln;
  for ch in 'krgybmcwKRGYBMCW' do begin
    if (ch = 'W') or (ch='w') then fg('k') else fg('W');
    bg(ch); write(ch);
  end;
  fg('w'); bg('k');
  writeln;
end.
