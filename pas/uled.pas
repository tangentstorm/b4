// ulned: line editor
// adapted from https://github.com/tangentstorm/linenoise
// (which in turn inspired by of antirez/linenoise)
{$mode objfpc}
unit uled;
interface uses classes, sysutils, crt;

type
  TCompleteFn = procedure( const buf: string; var comps : TStringList );

  TLineEditor = class
    history : TStringList;
    on_complete : TCompleteFn;
    constructor create;
    function flush: string;
    function input( var res : string ) : boolean;
    procedure refresh;
    procedure backspace;
    procedure delete_char;
    procedure transpose;
    procedure kill_prev_word;
    procedure complete( var buf : string );
    procedure browse_history( new_index : integer );
    procedure reset;
    procedure step;
  private
    done, keep : boolean;
    hist_index : integer;
    xpos, len, cur : integer;
  public
    buf : string;
  end;

var ed : TLineEditor;
function input( var buf : string ) : boolean;
procedure loadHist( path : string );
procedure saveHist( path : string );


implementation

constructor TLineEditor.Create;
begin
  history := TStringList.create;
  hist_index := 0;
  self.reset;
end;

procedure TLineEditor.refresh;
  var ch: char; ofs : byte = 0; i : integer = 0;
begin
  crt.gotoxy( xpos, crt.wherey ); // left edge
  for ch in buf do begin
    inc( i );
    if ( ch < ' ' ) and not ( ch = ^J ) then begin
      crt.textcolor( 2 );
      write( '^', chr( ord( '@' ) + ord( ch )));
      if i <= cur then inc( ofs );
      crt.textcolor( 7 );
    end
    else write( ch )
  end;
  crt.clreol;
  crt.gotoxy( xpos + cur + ofs - 1, crt.wherey );
end;


procedure TLineEditor.backspace;
begin
  if ( cur > 1 ) and ( len > 0 ) then begin
    dec( cur ); dec( len );
    delete( buf, cur, 1 );
  end
end;

procedure TLineEditor.delete_char; inline;
begin
  if cur <= len then begin
    dec( len );
    delete( buf, cur, 1 );
  end
end;

procedure TLineEditor.transpose;
  var ch : char;
begin
  if cur > 1 then begin
    if cur >= len then cur := len;
    ch := buf[ cur - 1 ];
    buf[ cur - 1 ] := buf[ cur ];
    buf[ cur ] := ch;
    inc( cur );
    if cur >= len then cur := len + 1
  end
end;

procedure TLineEditor.kill_prev_word;
  var old, dif : integer;
begin
  old := cur;
  while ( cur > 1 ) and ( buf[ cur - 1 ] <= ' ' ) do dec( cur );
  while ( cur > 1 ) and ( buf[ cur - 1 ]  > ' ' ) do dec( cur );
  dif := old - cur + 1;
  delete( buf, cur, dif );
  len := length( buf );
end;



procedure TLineEditor.complete( var buf : string );// TODO
begin
end;


procedure TLineEditor.browse_history( new_index : integer );
begin
  // clamp:
  hist_index := new_index;
  if hist_index < 0 then hist_index := 0;
  if hist_index > history.count then hist_index := history.count;

  // special case for new input at end of list:
  if hist_index = history.count then buf := ''
  else buf := history[ hist_index ];
  len := length( buf );

  // cursor tracking:
  // !! maybe remember column for hopping past short lines?
  if cur > len then cur := len + 1;
end;


procedure TLineEditor.step;
  var ch : char;
begin
  refresh; ch := crt.readkey;
  case ch of
    ^A : cur := 1;
    ^B : begin dec( cur ); if cur = 0 then cur := 1 end;
    ^C : begin keep := false; done := true end;
    ^D : if (len > 0) or (cur > 1) then delete_char
         else begin keep := false; done := true end;
    ^E : cur := len + 1;
    ^F : begin inc( cur ); if cur > len then cur := len + 1 end;
    ^G : ;
    ^H : backspace;
    ^I : complete( buf );
    ^J : done := true;
    ^K : begin len := cur - 1; setlength( buf, len ) end;
    ^L : crt.clrscr;
    ^M : done := true;
    ^N : browse_history( hist_index + 1 );
    ^O : ;
    ^P : browse_history( hist_index - 1 );
    ^Q : ;
    ^R : ;
    ^S : ;
    ^T : transpose;
    ^U : begin delete( buf, 1, cur - 1); len := length( buf ); cur := 1
         end;
    ^V : ;
    ^W : kill_prev_word;
    ^X : ;
    ^Y : ;
    ^Z : ;
    ^? : backspace;
    else if (ch>=' ') and (ord(ch)<$7F) then begin
      write( ch );
      insert( ch, buf, cur );
      inc( cur ); inc( len );
    end
  end
end; { step }


procedure TLineEditor.reset;
begin
  len := 0; cur := 1;
  done := false;
  browse_history( history.count );
end;

function TLineEditor.input( var res : string ) : boolean;
begin
  xpos := crt.WhereX;
  self.buf := res;
  reset; refresh;
  done := false; keep := true; // optimism!
  repeat step until done;
  if keep then res := flush else res := '';
  result := keep;
end;


function TLineEditor.flush : string;
begin
  result := self.buf;
  if result <> '' then history.add( result );
  self.buf := '';
  reset;
end;



function input( var buf : string ) : boolean;
begin
  result := ed.input( buf )
end;

procedure loadHist( path : string );
begin
  if FileExists(path) then ed.history.LoadFromFile(path)
end;

procedure saveHist( path : string );
begin
  ed.history.SaveToFile(path)
end;


initialization
  ed := TLineEditor.create;
end.
