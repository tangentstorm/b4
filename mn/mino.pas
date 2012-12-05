{ mino : minimal notation }
program mino;

{ recurse is just a delegate for the main routine }
type handler = procedure ( sentinel : char );
var recurse : handler;

var ch : char;
function getch : char;
begin
  read( ch );
  getch := ch;
end; { getch }


{ The abc codes are the first three ascii characters after NUL.
  They allow you to define nested structures, much like XML or
  lisp's s-expressions.

^A#1STHstart of header
^B#2STXstart of text
^C#3ETXend of text
 
  In minno, no special meaning is given to the header, eunless
  the very first character is a control character.
}  
procedure abc_code;
  var special : boolean = false;
  procedure alert_user;
  begin
    special := true;
    repeat getch until ch = ^C;
  end;
begin
  case getch of
    ^G : alert_user;
    ^^ : {  todo : table }
    else write( ch )
  end;
  if not special then begin
    write( ^A );   recurse( ^B );
    write( ^B );   recurse( ^C );
    write( ^C )
  end
end;


{ termcodes provide a simple mnemonic alternative to ansi escape codes.
  instead of using ascii ^[ ( escape ) they use ^T ( device control 4) }
procedure termcode;
begin
  case getch of
    '!'	: write( ^[, '[1m' );
    '.' : write( ^[, '[0m' );
    else write( ^T, ch ) { unrecognized, so just pass it through }
  end
end;


{ main routine for mino }

procedure main( sentinel : char );
begin
  while not ( eof or ( ch = sentinel )) do begin
    case getch of
      ^A : abc_code;
      ^T : termcode;
      else write( ch );
    end
  end
end;

begin
  recurse := @main;
  recurse( ^D );
end.
