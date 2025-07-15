{$mode delphi}
unit ub4asm;
interface uses ub4ops, ub4, classes, sysutils, strutils, character;

  type ident = string[16];
  type entry = record id: ident; adr: ub4.address end;
  var dict: array[0..128] of entry; ents : byte;
  type TFwd = record key: string; at: value end;
  var fwds : array of TFwd; fw:TFwd;

  function b4op(code : opstring; out op:byte) : boolean;
  function b4opc(code:opstring) : byte;
  function find_ident(adr:ub4.address; out id:ident): boolean;
  function isRegChar(c:char; out r:byte) : boolean;
  function find(s:string; out a:ub4.address):boolean;
  procedure b4as;
  procedure b4a_strs(strs:TStringList);
  procedure b4a_file(path:string);
  procedure b4a(s:string);
  function unhex(tok:string): integer;
  procedure clear_dict;
  function tryHex(tok:string; out i:integer):boolean;


implementation

  type chargen = function( var ch : char ) : char;
  var
    nextchar : chargen;
    atEnd: boolean;
    g_strs: TStringList;
    g_line: integer;
    g_col: integer;


function unhex(tok:string): integer;
begin
  if length(tok) = 0 then raise EConvertError.Create('unhex of empty string?');
  if tok[1]='-' then result := -Hex2Dec(copy(tok,2,length(tok)-1))
  else result := Hex2Dec(tok);
end;

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
  tokentag = ( wsp, cmt, hex, chr, def, ref, ivk, adr, get, put, ink, fwd,
              _if, _th, _el, _wh, _do, _od, _fr, _nx, _lp, _rs, _cs, _ob, _cb );
  token = record tag : tokentag; str : string; end;

function readnext( var ch : char ) : char;
  begin
    atEnd := eof;
    if not atEnd then read(ch);
    readnext := ch;
  end;

function read_from_strs(var ch: char): char;
begin
  if g_line >= g_strs.Count then atEnd := true
  else begin
    if g_col > length(g_strs[g_line]) then begin
      g_col := 1;
      inc(g_line);
      if g_line >= g_strs.Count then atEnd := true else ch := #10;
    end else begin
      ch := g_strs[g_line][g_col];
      inc(g_col);
    end;
  end;
  if atEnd then ch := #0;
  result := ch;
end;


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
    for i := 0 to high(dict) do begin
      dict[i].id := ''; dict[i].adr := 0
    end;
    ents := 0;
  end;



function next( var tok : token; var ch : char ) : boolean;
  procedure keep; begin tok.str := tok.str + ch end;
  procedure rdstr(var tok: token; var ch: char);
  begin
    tok.str := '';
    while (not atEnd) and (nextchar(ch) <> '"') do keep;
    if atEnd then begin writeln('unterminated string literal'); halt end;
    ch := nextchar(ch); // consume closing quote
  end;
  procedure rest(t:tokentag);
    begin tok.tag := t; tok.str := ''; while (not atEnd) and (nextchar(ch) > #32) do keep end;
  begin tok.str := ch; next := true;
    case ch of
      #0: next := false;
      #1..#32: begin tok.tag := wsp; repeat until atEnd or (nextchar(ch) >= #32) end;
      '#' : begin tok.tag := cmt; while (not atEnd) and (ch <> #10) do ch := nextchar(ch); end;
      '-': begin
        tok.tag := hex;
        tok.str := '-';
        while (not atEnd) and (nextchar(ch) in ['0'..'9','A'..'F']) do keep;
      end;
      '0'..'9','A'..'F':
        begin tok.tag := hex;
        while (not atEnd) and (nextchar(ch) in ['0'..'9','A'..'F']) do keep end;
      '''' : begin tok.tag := chr; tok.str := nextchar(ch); nextchar(ch); end;
      ':' : rest(def);
      '^' : rest(ivk);
      '`' : rest(adr);
      '@' : rest(get);
      '!' : rest(put);
      '>' : rest(fwd);
      '+' : rest(ink);
      '"' : begin tok.tag := _rs; rdstr(tok, ch) end;
      '.' : begin
              case nextchar(ch) of
                '.': begin tok.tag := hex; tok.str := '0'; ch := nextchar(ch); end;
                '^' : tok.tag := _lp;
                'i' : tok.tag := _if;
                'e' : tok.tag := _el;
                't' : tok.tag := _th;
                'w' : tok.tag := _wh;
                'd' : tok.tag := _do;
                'o' : tok.tag := _od;
                'f' : tok.tag := _fr;
                'n' : tok.tag := _nx;
                '"' : begin tok.tag := _cs; rdstr(tok, ch) end;
                '[': tok.tag := _ob;
                ']': tok.tag := _cb;
                otherwise begin writeln('unknown macro: .',ch); halt end
              end;
              ch:=nextchar(ch);
            end
      otherwise begin
        tok.tag := ref; while (not atEnd) and (nextchar(ch) > #32) do keep end;
    end;
  end;


function find(s:string; out a:ub4.address):boolean;
var op:byte = 0;
begin
  result := false;
  while (op < ents) and not result do
  begin
    if dict[op].id = s then
    begin
      a := dict[op].adr;
      result := true;
    end;
    inc(op);
  end;
end;

function tryHex(tok:string; out i:integer):boolean;
  var c:char;
begin
  for c in tok do if not (isdigit(c) or isUpper(c)) then exit(false);
  try i := unhex(tok); result := true;
  except on EConvertError do result := false end
end;


procedure b4as_core;
  var here: value; err: integer; tok: token; ch: char;
  procedure emit(v:value); begin mem[here] := v; inc(here); end;
  procedure emitv(v:value); begin wrval(here, v); inc(here,4) end;
  procedure emit_call(v:value); begin emit(b4opc('cl')); emitv(v) end;
  procedure unknown(s:string); begin writeln('unknown word:', s); halt end;
  function isreg(s:string):boolean; begin
    result:=(length(s)=1) and (s[1]>='@') and (s[1]<='_') end;
  function find_addr(s:string): value; { return address of label }
    var a: ub4.address;
    begin
      if find(s, a) then
        result := a
      else if isreg(s) then
        result := rega(s[1])
      else
        unknown(s);
    end;
  procedure hop_slot(op:string); begin emit(b4opc(op)); dput(here); emit(0); end;
  procedure hop_here(); var slot,dist:value;
    begin slot := dpop; dist := here-slot;
      if dist < 0 then begin writeln('invalid hop_here at ', here); halt end;
      if dist > 126 then begin writeln('hop too big: ', here, ' -> ',slot); halt end;
      mem[slot] := dist+1
    end;
  procedure hop_back(); var dest,dist:value;
    begin dest := dpop; dist := dest-here;
      if dist > 0 then begin writeln('invalid hop_back at ', here); halt end;
      if dist < -128 then begin writeln('hop too big: ',here, ' -> ',dest); halt end;
      emit(byte(dist+1))
    end;
  procedure compile;
    var op,i : byte; a:integer;
    begin
      case tok.tag of
        wsp, cmt : ok; { do nothing }
        hex : emit(unhex(tok.str));
        chr : if length(tok.str)>1 then begin writeln('bad char: ', tok.str); halt end
              else emit(ord(tok.str[1]));
        def : if isreg(tok.str) then rg[regn(tok.str[1])]:=here
              else if tryHex(tok.str,a) then here:=a
              else if ents=high(dict) then begin writeln('too many :defs'); halt end
              else begin
                dict[ents].id:=tok.str; dict[ents].adr:=here; inc(ents); i:=0;
                if length(fwds)>0 then while i < length(fwds) do begin
                  fw := fwds[i]; if fw.key=tok.str then
                  begin wrval(fw.at, here);  delete(fwds,i,1) end
                  else inc(i)
                end
              end;
        ref : if b4op(tok.str, op) then emit(op) else emit_call(find_addr(tok.str));
        ivk : if isreg(tok.str) then emit(b4opc('^'+tok.str[1]))
              else begin writeln('bad word: ^',tok.str); halt end;
        fwd : begin
                fw.key := tok.str; fw.at := here;
                emitv(0); insert(fw, fwds, maxint);
              end;
        adr : emitv(find_addr(tok.str));
        get : if isreg(tok.str) then emit(b4opc('@'+tok.str[1]))
              else begin
                emit(b4opc('li')); emitv(find_addr(tok.str)); emit(b4opc('ri')) end;
        put : if isreg(tok.str) then emit(b4opc('!'+tok.str[1]))
              else begin
                emit(b4opc('li')); emitv(find_addr(tok.str)); emit(b4opc('wi')) end;
        ink : if isreg(tok.str) then emit(b4opc('+'+tok.str[1])) // inKrement :)
              else begin writeln('no such word: +',tok.str); halt end;
        _wh : dput(here); {(- wh)}
        _if ,
        _do : hop_slot('h0');
        _od : begin { compile time: (wh do -)}
                { first, an unconditional hop back to the _do }
                emit(b4opc('hp')); dswp; hop_back;
                { now go back to the guard and compile the forward jump }
                hop_here end;
        _el : begin hop_slot('hp'); dswp; hop_here end;
        _th : {(do-)} hop_here; { jump to 'end' when 'if' test fails }
        _fr : begin emit(b4opc('dc')); dput(here) end;
        _nx : begin emit(b4opc('nx')); hop_back end;
        _lp : begin emitv(rg[RLP]); rg[RLP]:=here-4 end;
        _rs : for op := 1 to length(tok.str) do emit(ord(tok.str[op]));
        _cs : begin
                emit(length(tok.str));
                for op := 1 to length(tok.str) do emit(ord(tok.str[op]));
              end;
        _ob : hop_slot('hp');
        _cb : begin dput(tos); hop_here; emit(b4opc('li')); emitv(dpop+1); end;
      end end;
  begin
    err := 0; here := rg[RHP];
    ch := nextchar(ch);
    while (err = 0) and not atEnd do if next(tok, ch) then compile;
    if err <> 0 then dput(err) else rg[RHP] := here;
  end;

procedure b4as;
begin
  nextchar := @readnext;
  atEnd := eof;
  b4as_core;
end;

procedure b4a_strs(strs:TStringList);
begin
  g_strs := strs;
  g_line := 0;
  g_col := 1;
  atEnd := g_strs.Count = 0;
  nextchar := @read_from_strs;
  b4as_core;
end;

procedure b4a_file(path:string);
var strs: TStringList;
begin
  if not FileExists(path) then begin
    writeln('file not found: ', path);
    exit;
  end;
  strs := TStringList.Create;
  try
    strs.LoadFromFile(path);
    b4a_strs(strs);
  finally
    strs.Free;
  end;
end;

procedure b4a(s:string);
var strs: TStringList;
begin
  strs := TStringList.Create;
  try
    strs.Text := s;
    b4a_strs(strs);
  finally
    strs.Free;
  end;
end;

begin
  nextchar := readnext;
  clear_dict;
end.
