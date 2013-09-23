// A simple console-mode file viewer like less
// with a movable highlighted line.

{$mode delphi}{$i xpc.inc}{$h+}
program lines;
uses kvm, kbd, ui, rings;

type
  TOutline = rings.GRing<string>;

procedure ShowInput;
  var
    out	 : TOutline;
    top,                     // top of screen
    usr,                     // user's cursor
    cur	 : TOutline.GCursor; // general purpose cursor
    s	 : string;
    i	 : integer;
    done : boolean = false;
  begin
    out := TOutline.Create();
    while not Eof do begin
      ReadLn(s); out.append(s);
    end;
    top := out.MakeCursor; top.ToTop;
    usr := out.MakeCursor; usr.ToTop;
    cur := out.MakeCursor; cur.ToTop;
    kvm.clrscr;
    repeat
      cur.MoveTo(top);
      for i := 0 to kvm.maxY do begin
	kvm.GotoXY(0,i); kvm.bg('k');
	if not cur.AtClasp then begin
	  if cur.index = usr.index then kvm.Bg('b');
	  write(cur.value);
	  cur.MoveNext;
	end;
	kvm.ClrEol;
      end;
      case ReadKey of
	#0 : case ReadKey of
	       kbd.PgDn : if not top.AtTop then top.MovePrev;
	       kbd.PgUp : if not top.AtEnd then top.MoveNext;
	     end;
	^N : if not usr.AtEnd then usr.MoveNext;
	^P : if not usr.AtTop then usr.MovePrev;
	^C : done := true;
      end
    until done;
    usr.Free; cur.Free; top.Free; out.Free;
  end;

var
  i : integer;
begin
  if paramcount > 0 then
    for i := 1 to paramcount do
      begin
	Assign(input, paramstr(i));
	Reset(input);
	ShowInput;
      end
  // TODO: else ShowInput; { stdinput }
  // -------------------------------------------------
  // !! How would I get the console to first read from
  // stdin, but then switch to interactive mode?
  // -------------------------------------------------
end.
