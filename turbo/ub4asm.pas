{$mode delphi}{$i xpc}
unit ub4asm;
interface uses xpc, ub4ops, ub4;

  type ident = string[16];
  type entry = record id: ident; adr: ub4.address end;
  var dict: array[0..128] of entry; ents : byte;

  function b4opc(code:opstring) : byte;
  function find_ident(adr:ub4.address; out id:ident): boolean;
  procedure b4as;


implementation

  type chargen = function( var ch : char ) : char;
  var nextchar : chargen;

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
  tokentag = ( wsp, cmt, raw, chr, def, ref, adr,
              _if, _th, _el, _fi, _wh, _do, _od, _fr, _nx );
  token = record tag : tokentag; str : string; end;

function readnext( var ch : char ) : char;
  begin read(ch); readnext := ch; end;


function find_ident(adr:ub4.address; out id:ident): boolean;
  var i : integer;
  begin result := false;
    for i := high(dict) downto 0 do
      if dict[i].adr = adr then
        begin id := dict[i].id; result := true; break end;
  end;

procedure clear_dict;
  var i : byte;
  begin
    for i := 0 to high(dict) do begin dict[i].id := ''; dict[i].adr := 0 end
  end;



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
      '.' :
        begin
          case nextchar(ch) of
            'i': tok.tag := _if;
            't': tok.tag := _th;
            'e': tok.tag := _el;
            'z': tok.tag := _fi;
            'w': tok.tag := _wh;
            'd': tok.tag := _do;
            'o': tok.tag := _od;
            'f': tok.tag := _fr;
            'n': tok.tag := _nx;
            otherwise begin writeln('unknown macro: !',ch); halt end;
          end;
          ch:=nextchar(ch);
        end
    end;
  end;


procedure b4as;
  var here: value; err: integer; tok: token; ch: char;
  procedure emit(v:value); begin ram[here] := v; inc(here); end;
  procedure emitv(v:value); begin wrval(here, v); inc(here,4) end;
  procedure emit_call(v:value); begin emit(b4opc('cl')); emitv(v) end;
  procedure unknown(s:string); begin writeln('unknown word:', s); halt end;
  function find_addr(s:string): value; { return address of label }
    var op:byte = 0; found: boolean=false;
    begin while (op < ents) and not found do begin
      if dict[op].id = tok.str then begin find_addr := dict[op].adr; found:=true end;
      inc(op) end;
      if not found then unknown(tok.str) end;
  procedure emit_slot(op:string); begin emit(b4opc(op)); dput(here); emitv(0); end;
  procedure compile;
    var op : byte; v : value;
    begin
      case tok.tag of
        wsp, cmt : ok; { do nothing }
        raw : begin val(tok.str, v, err);
                if err=0 then emit(v) else unknown(tok.str) end;
        chr : if length(tok.str)>1 then begin writeln('bad char: ', tok.str); halt end
              else emit(ord(tok.str[1]));
        def : if ents = high(dict) then begin writeln('too many definitions'); halt end
              else begin
                dict[ents].id := tok.str; dict[ents].adr := here; inc(ents) end;
        ref : if b4op(tok.str, op) then emit(op) else emit_call(find_addr(tok.str));
        adr : emitv(find_addr(tok.str));
        _wh : dput(here); {(- wh)}
        _th ,
        _do : emit_slot('j0');
        _od : begin { compile time: (wh do -)}
                { first, an unconditional jump back to the _do }
                emit(b4opc('jm')); dswp; emitv(dpop);
                { now go back to the guard and compile the forward jump }
                wrval(dpop, here); end;
        _if : ; { 'if' does nothing. just syntactic sugar. }
        _el : begin emit_slot('jm'); dswp; wrval(dpop, here) end;
        _fi : {(do-)} wrval(dpop, here); { jump to 'end' when 'if' test fails }
        _fr : dput(here);
        _nx : begin emit(b4opc('nx')); emitv(dpop) end;
      end end;
  begin
    clear_dict; err := 0; ents := 0; here := reg_hp^; read(ch);
    while (err = 0) and not eof do if next(tok, ch) then compile;
    if err <> 0 then dput(err) else reg_hp^ := here;
  end;

begin
  nextchar := readnext;
end.
