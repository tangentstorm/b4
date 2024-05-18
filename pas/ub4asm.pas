{$mode delphi}{$i xpc}
unit ub4asm;
interface uses ub4ops, ub4;

  type ident = string[16];
  type entry = record id: ident; adr: ub4.address end;
  var dict: array[0..128] of entry; ents : byte;

  function b4op(code : opstring; out op:byte) : boolean;
  function b4opc(code:opstring) : byte;
  function find_ident(adr:ub4.address; out id:ident): boolean;
  procedure b4as;


implementation

  type chargen = function( var ch : char ) : char;
  var nextchar : chargen;

procedure ok; begin end;

function isRegChar(c:char; out r:byte) : boolean;
begin
  result := (c>='@') and (c<='_');
  if result then r := ord(c)-ord('@')
end;

function b4op(code : opstring; out op:byte) : boolean;
  var o,r : byte;
  begin
    result := false;
    for o := low(optbl) to high(optbl) do
      if code = optbl[o] then begin
        op := o; result := true; break
      end;
    if code = '..' then begin op := 0; result := true end
    else if (length(code)=2) and isRegChar(code[2],r) then begin
      result := true;
      case code[1] of
        '^': op := r;
        '@': op := r+$20;
        '!': op := r+$40;
        '+': op := r+$60;
        else result := false
      end
    end
  end;

function b4opc(code:opstring) : byte;
  begin
    if b4op(code, result) then ok
    else begin writeln('invalid op: ', code); halt end end;

type
  tokentag = ( wsp, cmt, raw, chr, def, ref, adr, get, put, fwd,
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
  procedure rest(t:tokentag);
    begin tok.tag := t; tok.str := ''; while nextchar(ch) > #32 do keep end;
  begin tok.str := ch; next := true;
    case ch of
      #0..#32: begin tok.tag := wsp; repeat until eof or (nextchar(ch) >= #32) end;
      '#' : begin tok.tag := cmt; readln(tok.str); ch := nextchar(ch) end;
      '0'..'9': begin { TODO : this should be hex! }
        tok.tag := raw;
        while nextchar(ch) in ['0'..'9'] do keep end;
      '''' : begin tok.tag := chr; tok.str := nextchar(ch); nextchar(ch); end;
      ':' : rest(def);
      '$' : rest(adr);
      '@' : rest(get);
      '!' : rest(put);
      '>' : rest(fwd);
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
      if dict[op].id = s then begin find_addr := dict[op].adr; found:=true end;
      inc(op) end;
      if not found then unknown(s) end;
  procedure hop_slot(op:string); begin emit(b4opc(op)); dput(here); emit(0); end;
  procedure hop_here(); var slot,dist:value;
    begin slot := dpop; dist := here-slot;
      if dist < 0 then begin writeln('invalid hop_here at ', here); halt end;
      if dist > 126 then begin writeln('hop too big: ', here, ' -> ',slot); halt end;
      ram[slot] := dist+1
    end;
  procedure hop_back(); var dest,dist:value;
    begin dest := dpop; dist := dest-here;
      if dist > 0 then begin writeln('invalid hop_back at ', here); halt end;
      if dist < -128 then begin writeln('hop too big: ',here, ' -> ',dest); halt end;
      emit(byte(dist+1))
    end;
  type TFwd = record key: string; at: value end;
  var fwds : array of TFwd; fw:TFwd;
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
        fwd : begin
                fw.key := tok.str; fw.at := here;
                emitv(0); insert(fw, fwds, maxint);
              end;
        adr : emitv(find_addr(tok.str));
        get : begin emit(b4opc('li')); emitv(find_addr(tok.str)); emit(b4opc('rv')) end;
        put : begin emit(b4opc('li')); emitv(find_addr(tok.str)); emit(b4opc('wv')) end;
        _wh : dput(here); {(- wh)}
        _th ,
        _do : hop_slot('h0');
        _od : begin { compile time: (wh do -)}
                { first, an unconditional hop back to the _do }
                emit(b4opc('hp')); dswp; hop_back;
                { now go back to the guard and compile the forward jump }
                hop_here end;
        _if : ; { 'if' does nothing. just syntactic sugar. }
        _el : begin hop_slot('hp'); dswp; hop_here end;
        _fi : {(do-)} hop_here; { jump to 'end' when 'if' test fails }
        _fr : begin emit(b4opc('dc')); dput(here) end;
        _nx : begin emit(b4opc('nx')); hop_back end;
      end end;
  begin
    SetLength(fwds, 0);
    clear_dict; err := 0; ents := 0; here := reg_hp^; read(ch);
    while (err = 0) and not eof do if next(tok, ch) then compile;
    if err <> 0 then dput(err) else reg_hp^ := here;
    for fw in fwds do wrval(fw.at, find_addr(fw.key));
  end;

begin
  nextchar := readnext;
end.
