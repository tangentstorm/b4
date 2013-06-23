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

type
  tokentag = ( wsp, cmt, raw, lit, def, ref );
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
                  end
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
      else begin ram[hp] := here; dput(0) end
  end;

begin
  nextchar := readnext;
end.