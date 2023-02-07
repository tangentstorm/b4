program helloempty;
uses empty;

  procedure hello;
  begin
    writeln('hello world');
  end;

begin
  empty.createop('hello', @hello);
  empty.mainloop;
end.
