{$mode delphi}{$i xpc}
unit ub4asm;
interface uses xpc, ub4ops, ub4;

  type chargen = function( var ch : char ) : char;

  var nextchar : chargen;
  function b4op(code : opstring; out op:byte) : boolean;
  function b4opc(code:opstring) : byte;
  function readnext( var ch : char ) : char;
  procedure b4as;

implementation

function b4op(code : opstring; out op:byte) : boolean;
  var found : boolean;
  begin op := low(optbl); found := false;
    while (op < high(optbl)) and not found do begin
      found := code = optbl[op];
      if not found then inc(op) end;
    b4op := found end;

function b4opc(code:opstring) : byte;
  begin
    if b4op(code, result) then ok
    else begin writeln('invalid op: ', code); halt end end;

type
  tokentag = ( wsp, cmt, raw, lit, chr, def, ref, adr, _wh, _do, _od, _if, _fi );
  token = record tag : tokentag; str : string; end;
  entry = record key : string[32]; val : value; end;

function readnext( var ch : char ) : char;
  begin read(ch); readnext := ch; end;

function next( var tok : token; var ch : char ) : boolean;
  procedure keep; begin tok.str := tok.str + ch end;
  begin tok.str := ch; next := true;
    case ch of
      #0..#32: begin tok.tag := wsp; repeat until eof or (nextchar(ch) >= #32) end;
      '#' : begin tok.tag := cmt; readln(tok.str); ch := nextchar(ch) end;
      '0'..'9': begin { TODO : this should be hex! }
        tok.tag := raw;
        while nextchar(ch) in ['0'..'9'] do keep end;
      '''' : begin tok.tag := chr; tok.str := ''; while nextchar(ch) > #32 do keep end;
      ':' : begin tok.tag := def; tok.str := ''; while nextchar(ch) > #32 do keep end;
      '$' : begin tok.tag := adr; tok.str := ''; while nextchar(ch) > #32 do keep end;
      'a'..'z' : begin tok.tag := ref; while nextchar(ch) > #32 do keep end;
      { curly = (while | body) }
      '{' : begin tok.tag := _wh; ch:=nextchar(ch) end;
      '|' : begin tok.tag := _do; ch:=nextchar(ch) end;
      '}' : begin tok.tag := _od; ch:=nextchar(ch) end;
      '[' : begin tok.tag := _if; ch:=nextchar(ch) end;
      ']' : begin tok.tag := _fi; ch:=nextchar(ch) end;
      else  begin tok.tag := ref; ch:=nextchar(ch) end;
    end
  end;


procedure b4as;
  var here: value; err: integer; tok: token; ch: char;
      dict: array[0..31] of entry; ents : byte;
  procedure emit(v:value); begin ram[here] := v; inc(here); end;
  procedure emit_call(v:value); begin emit(b4opc('cl')); emit(v) end;
  procedure emit_lit(v:value); begin emit(b4opc('li')); emit(v); end;
  procedure unknown(s:string); begin writeln('unknown word:', s); halt end;
  function find_addr(s:string): value; { return address of label }
    var op:byte = 0; found: boolean=false;
    begin while (op < ents) and not found do begin
      if dict[op].key = tok.str then begin find_addr := dict[op].val; found:=true end;
      inc(op) end;
      if not found then unknown(tok.str) end;
  procedure compile;
    var op : byte; v : value;
    begin
      case tok.tag of
        wsp, cmt : ok; { do nothing }
        {=123 is a raw number, 123 is 'lit 123'}
        raw : begin val(tok.str, v, err);
                if err=0 then emit(v) else unknown(tok.str) end;
        lit : begin val(tok.str, v, err); { todo: introduce new syntax for lit #? }
                if err=0 then emit_lit(v) else unknown(tok.str) end;
        chr : if length(tok.str)>1 then begin writeln('bad char: ', tok.str); halt end
              else emit(ord(tok.str[1]));
        def : if ents > 31 then err := -12345
              else begin
                dict[ents].key := tok.str; dict[ents].val := here; inc(ents) end;
        ref : if b4op(tok.str, op) then emit(op) else emit_call(find_addr(tok.str));
        adr : emit(find_addr(tok.str));
        _wh : dput(here); {(- wh)}
        _do : begin {(- do)} emit(b4opc('j0')); emit(0); dput(here-1) end;
        _od : begin { compile time: (wh do -)}
                { first, an unconditional jump back to the _do }
                emit(b4opc('jm')); swap; emit(dpop);
                { now go back to the guard and compile the forward jump }
                ram[dpop] := here; end;
        _if : ; { 'if' does nothing. just syntactic sugar. }
        _fi : {(do-)} ram[dpop] := here; { jump to 'end' when 'if' test fails }
      end end;
  begin
    err := 0; ents := 0; here := ram[hp]; read(ch);
    while (err = 0) and not eof do if next(tok, ch) then compile;
    if err <> 0 then dput(err) else ram[hp] := here;
  end;

begin
  nextchar := readnext;
end.
