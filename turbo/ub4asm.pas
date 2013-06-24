unit ub4asm;
interface uses ub4ops, ub4;

  type chargen = function( var ch : char ) : char;

  var nextchar : chargen;
  function b4op(code : opstring; var op:byte) : boolean;
  function readnext( var ch : char ) : char;
  procedure b4as;

implementation

function b4op(code : opstring; var op:byte) : boolean;
  var found : boolean;
  begin
    op := 0; found := false;
    while (op < 64) and not found do
      begin
        found := code = optbl[op];
        if not found then inc(op);
      end;
    b4op := found;
  end;

function b4opc(code:opstring) : byte;
  var result : byte;
  begin
    if b4op(code, result) then b4opc := result
    else begin writeln('invalid op: ', code); halt end;
  end;

type
  tokentag = ( wsp, cmt, raw, lit, def, ref, _wh, _do, _od, _if, _fi );
  token = record
    tag : tokentag;
    str : string;
  end;
  entry = record
    key : string[32];
    val : value;
  end;


function readnext( var ch : char ) : char;
  begin
    read(ch); readnext := ch;
  end;


function next( var tok : token; var ch : char ) : boolean;
  procedure keep;
    begin
      tok.str := tok.str + ch
    end;
  begin
    tok.str := ch;
    next := true;
    case ch of
      #0..#32: begin
                 tok.tag := wsp;
                 repeat until eof or (nextchar(ch) >= #32)
               end;
      '#' : begin
              tok.tag := cmt;
              readln(tok.str); ch := nextchar(ch)
            end;
      '-', '=',
      '0'..'9': begin
                  if ch='=' then tok.tag := raw else tok.tag := lit;
                  while nextchar(ch) in ['0'..'9'] do keep
                end;
      ':' : begin
              tok.tag := def;
              tok.str := '';
              while nextchar(ch) > #32 do keep
            end;
      'a'..'z' : begin
                   tok.tag := ref;
                   while nextchar(ch) > #32 do keep
                 end;

      { curly = (while | body) }
      '{' : begin tok.tag := _wh; ch:=nextchar(ch) end;
      '|' : begin tok.tag := _do; ch:=nextchar(ch) end;
      '}' : begin tok.tag := _od; ch:=nextchar(ch) end;
      '[' : begin tok.tag := _if; ch:=nextchar(ch) end;
      ']' : begin tok.tag := _fi; ch:=nextchar(ch) end;
      else next := false
    end
  end;

procedure b4as;
  var
    here : value;
    err  : integer;
    tok  : token;
    ch   : char;
    dict : array[0..31] of entry;
    ents : byte;

    procedure emit(v:value);
      begin
        ram[here] := v; inc(here);
      end;

    procedure compile;
      var op : byte; v : value;
      begin
        case tok.tag of
          wsp : ;
          cmt : ;
          raw : begin
                  val(tok.str, v, err);
                  if err=0 then emit(v);
                end;
          lit : begin
                  val(tok.str, v, err);
                  if err=0 then begin
                    emit(1);
                    emit(v)
                  end
                end;
          def : if ents > 31 then err := -12345
                  else begin
                    dict[ents].key := tok.str;
                    dict[ents].val := here;
                    inc(ents)
                  end;
          ref : if b4op(tok.str, op) then emit(op)
                else
                  begin
                    op := 0;
                    while op < ents do
                      begin
                        if dict[op].key = tok.str then
                          emit(dict[op].val);
                        inc(op);
                      end

                end;
          _wh : dput(here-1);
          _do : begin
                  emit(b4opc('jwz'));
                  emit(0);
                  dput(here-1);
                  emit(b4opc('drop'));
                end;
          _od : begin
                  { first, an unconditional jump back to the _do }
                  { the if is just to discard the boolean }
                  emit(b4opc('jmp'));
                  swap; emit(dpop);
                  { now go back to the guard and compile the forward jump }
                  ram[dpop] := here-1;
                 end;
          _if : ; { if does nothing. it's just syntactic sugar. }
          _fi : ram[dpop] := here-1; { compile the forward jump at '|' }
        end
      end;

  begin
    err := 0; ents := 0; here := ram[hp];
    read(ch);
    while (err = 0) and not eof do
      if next(tok, ch) then
        compile;

    if err <> 0
      then dput(err)
      else begin ram[hp] := here end
  end;

begin
  nextchar := readnext;
end.