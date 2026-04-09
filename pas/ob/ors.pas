{$mode objfpc}{$H+}
unit ors;
{ ORS - Oberon Scanner, ported from N. Wirth's ORS.Mod (2014)
  Adapted for Free Pascal. Reads from a file stream instead of Texts.Reader. }
interface

const
  IdLen = 32;
  NKW = 34;
  maxExp = 38;
  stringBufSize = 256;

  { lexical symbols }
  sNULL = 0; sTIMES = 1; sRDIV = 2; sDIV = 3; sMOD = 4;
  sAND = 5; sPLUS = 6; sMINUS = 7; sOR = 8; sEQL = 9;
  sNEQ = 10; sLSS = 11; sLEQ = 12; sGTR = 13; sGEQ = 14;
  sIN = 15; sIS = 16; sARROW = 17; sPERIOD = 18;
  sCHAR = 20; sINT = 21; sREAL = 22; sFALSE = 23; sTRUE = 24;
  sNIL = 25; sSTRING = 26; sNOT = 27; sLPAREN = 28; sLBRAK = 29;
  sLBRACE = 30; sIDENT = 31;
  sIF = 32; sWHILE = 34; sREPEAT = 35; sCASE = 36; sFOR = 37;
  sCOMMA = 40; sCOLON = 41; sBECOMES = 42; sUPTO = 43; sRPAREN = 44;
  sRBRAK = 45; sRBRACE = 46; sTHEN = 47; sOF = 48; sDO = 49;
  sTO = 50; sBY = 51; sSEMICOLON = 52; sEND = 53; sBAR = 54;
  sELSE = 55; sELSIF = 56; sUNTIL = 57; sRETURN = 58;
  sARRAY = 60; sRECORD = 61; sPOINTER = 62; sCONST = 63; sTYPE = 64;
  sVAR = 65; sPROCEDURE = 66; sBEGIN = 67; sIMPORT = 68; sMODULE = 69;

type
  Ident = string[31];

var
  ival: longint;
  slen: longint;
  rval: single;
  id: Ident;
  str: array[0..stringBufSize-1] of char;
  errcnt: integer;

procedure Init(const filename: string);
procedure Get(var sym: integer);
procedure Mark(const msg: string);
procedure CopyId(var ident: Ident);
function Pos: longint;

implementation

uses sysutils;

type
  KWRec = record
    sym: integer;
    id: string[12];
  end;

var
  ch: char;
  eof_reached: boolean;
  errpos: longint;
  position: longint;
  srcbuf: ansistring;
  srclen: longint;
  k: integer;
  KWX: array[0..9] of integer;
  keyTab: array[0..NKW-1] of KWRec;

procedure ReadCh;
begin
  if position < srclen then begin
    ch := srcbuf[position + 1]; { 1-based string }
    inc(position);
  end else begin
    ch := #0;
    eof_reached := true;
  end;
end;

procedure CopyId(var ident: Ident);
begin
  ident := id;
end;

function Pos: longint;
begin
  result := position - 1;
end;

procedure Mark(const msg: string);
var p: longint;
begin
  p := Pos;
  if (p > errpos) and (errcnt < 25) then
    writeln('  pos ', p, ' ', msg);
  inc(errcnt);
  errpos := p + 4;
end;

procedure Identifier(var sym: integer);
var i, kk: integer;
begin
  i := 0;
  id := '';
  repeat
    if i < IdLen - 1 then begin
      id := id + ch;
      inc(i);
    end;
    ReadCh;
  until eof_reached or (ch < '0') or ((ch > '9') and (ch < 'A'))
        or ((ch > 'Z') and (ch < 'a')) or (ch > 'z');
  if i < 10 then begin
    kk := KWX[i - 1];
    while (id <> keyTab[kk].id) and (kk < KWX[i]) do inc(kk);
    if kk < KWX[i] then sym := keyTab[kk].sym
    else sym := sIDENT;
  end else
    sym := sIDENT;
end;

procedure ScanString;
var i: integer;
begin
  i := 0;
  ReadCh;
  while (not eof_reached) and (ch <> '"') do begin
    if ch >= ' ' then begin
      if i < stringBufSize - 1 then begin str[i] := ch; inc(i) end
      else Mark('string too long');
    end;
    ReadCh;
  end;
  str[i] := #0; inc(i);
  ReadCh;
  slen := i;
end;

procedure HexString;
var i, m, n: integer;
begin
  i := 0;
  ReadCh;
  while (not eof_reached) and (ch <> '$') do begin
    while (ch = ' ') or (ch = #9) or (ch = #13) or (ch = #10) do ReadCh;
    if (ch >= '0') and (ch <= '9') then m := ord(ch) - ord('0')
    else if (ch >= 'A') and (ch <= 'F') then m := ord(ch) - ord('A') + 10
    else begin m := 0; Mark('hexdig expected') end;
    ReadCh;
    if (ch >= '0') and (ch <= '9') then n := ord(ch) - ord('0')
    else if (ch >= 'A') and (ch <= 'F') then n := ord(ch) - ord('A') + 10
    else begin n := 0; Mark('hexdig expected') end;
    if i < stringBufSize then begin str[i] := chr(m * 16 + n); inc(i) end
    else Mark('string too long');
    ReadCh;
  end;
  ReadCh;
  slen := i;
end;

function Ten(e: longint): single;
var x, t: single;
begin
  x := 1.0; t := 10.0;
  while e > 0 do begin
    if odd(e) then x := t * x;
    t := t * t;
    e := e div 2;
  end;
  result := x;
end;

procedure Number(var sym: integer);
const max = 2147483647;
var i, kk, e, n, s: longint;
    h: longint;
    x: single;
    d: array[0..15] of integer;
    negE: boolean;
begin
  ival := 0; i := 0; n := 0; kk := 0;
  repeat
    if n < 16 then begin d[n] := ord(ch) - ord('0'); inc(n) end
    else begin Mark('too many digits'); n := 0 end;
    ReadCh;
  until eof_reached or (ch < '0') or ((ch > '9') and (ch < 'A')) or (ch > 'F');
  if (ch = 'H') or (ch = 'R') or (ch = 'X') then begin
    { hex }
    repeat
      h := d[i];
      if h >= 10 then h := h - 7;
      kk := kk * 16 + h;
      inc(i);
    until i = n;
    if ch = 'X' then begin
      sym := sCHAR;
      if kk < 256 then ival := kk
      else begin Mark('illegal value'); ival := 0 end;
    end else if ch = 'R' then begin
      sym := sREAL;
      move(kk, rval, 4);
    end else begin
      sym := sINT; ival := kk;
    end;
    ReadCh;
  end else if ch = '.' then begin
    ReadCh;
    if ch = '.' then begin
      { double dot => upto; put sentinel }
      ch := #127;
      repeat
        if d[i] < 10 then begin
          if kk <= (max - d[i]) div 10 then kk := kk * 10 + d[i]
          else begin Mark('too large'); kk := 0 end;
        end else Mark('bad integer');
        inc(i);
      until i = n;
      sym := sINT; ival := kk;
    end else begin
      { real number }
      x := 0.0; e := 0;
      repeat x := x * 10.0 + d[i]; inc(i) until i = n;
      while (ch >= '0') and (ch <= '9') do begin
        x := x * 10.0 + (ord(ch) - ord('0'));
        dec(e);
        ReadCh;
      end;
      if (ch = 'E') or (ch = 'D') then begin
        ReadCh; s := 0;
        if ch = '-' then begin negE := true; ReadCh end
        else begin
          negE := false;
          if ch = '+' then ReadCh;
        end;
        if (ch >= '0') and (ch <= '9') then begin
          repeat
            s := s * 10 + ord(ch) - ord('0');
            ReadCh;
          until (ch < '0') or (ch > '9');
          if negE then e := e - s else e := e + s;
        end else Mark('digit?');
      end;
      if e < 0 then begin
        if e >= -maxExp then x := x / Ten(-e) else x := 0.0;
      end else if e > 0 then begin
        if e <= maxExp then x := Ten(e) * x
        else begin x := 0.0; Mark('too large') end;
      end;
      sym := sREAL; rval := x;
    end;
  end else begin
    { decimal integer }
    repeat
      if d[i] < 10 then begin
        if kk <= (max - d[i]) div 10 then kk := kk * 10 + d[i]
        else begin Mark('too large'); kk := 0 end;
      end else Mark('bad integer');
      inc(i);
    until i = n;
    sym := sINT; ival := kk;
  end;
end;

procedure comment;
begin
  ReadCh;
  repeat
    while (not eof_reached) and (ch <> '*') do begin
      if ch = '(' then begin
        ReadCh;
        if ch = '*' then comment;
      end else
        ReadCh;
    end;
    while ch = '*' do ReadCh;
  until (ch = ')') or eof_reached;
  if not eof_reached then ReadCh
  else Mark('unterminated comment');
end;

procedure Get(var sym: integer);
begin
  repeat
    while (not eof_reached) and (ch <= ' ') do ReadCh;
    if eof_reached then begin sym := sNULL; exit end;

    if ch < 'A' then begin
      if ch < '0' then begin
        if ch = '"' then begin ScanString; sym := sSTRING end
        else if ch = '#' then begin ReadCh; sym := sNEQ end
        else if ch = '$' then begin HexString; sym := sSTRING end
        else if ch = '&' then begin ReadCh; sym := sAND end
        else if ch = '(' then begin
          ReadCh;
          if ch = '*' then begin sym := sNULL; comment end
          else sym := sLPAREN;
        end
        else if ch = ')' then begin ReadCh; sym := sRPAREN end
        else if ch = '*' then begin ReadCh; sym := sTIMES end
        else if ch = '+' then begin ReadCh; sym := sPLUS end
        else if ch = ',' then begin ReadCh; sym := sCOMMA end
        else if ch = '-' then begin ReadCh; sym := sMINUS end
        else if ch = '.' then begin
          ReadCh;
          if ch = '.' then begin ReadCh; sym := sUPTO end
          else sym := sPERIOD;
        end
        else if ch = '/' then begin ReadCh; sym := sRDIV end
        else begin ReadCh; sym := sNULL end;
      end
      else if ch < ':' then Number(sym)
      else if ch = ':' then begin
        ReadCh;
        if ch = '=' then begin ReadCh; sym := sBECOMES end
        else sym := sCOLON;
      end
      else if ch = ';' then begin ReadCh; sym := sSEMICOLON end
      else if ch = '<' then begin
        ReadCh;
        if ch = '=' then begin ReadCh; sym := sLEQ end
        else sym := sLSS;
      end
      else if ch = '=' then begin ReadCh; sym := sEQL end
      else if ch = '>' then begin
        ReadCh;
        if ch = '=' then begin ReadCh; sym := sGEQ end
        else sym := sGTR;
      end
      else begin ReadCh; sym := sNULL end;
    end
    else if ch < '[' then Identifier(sym)
    else if ch < 'a' then begin
      if ch = '[' then sym := sLBRAK
      else if ch = ']' then sym := sRBRAK
      else if ch = '^' then sym := sARROW
      else sym := sNULL;
      ReadCh;
    end
    else if ch < '{' then Identifier(sym)
    else begin
      if ch = '{' then sym := sLBRACE
      else if ch = '}' then sym := sRBRACE
      else if ch = '|' then sym := sBAR
      else if ch = '~' then sym := sNOT
      else if ch = #127 then sym := sUPTO
      else sym := sNULL;
      ReadCh;
    end;
  until sym <> sNULL;
end;

procedure Init(const filename: string);
var f: file;
    sz: longint;
begin
  errpos := 0; errcnt := 0;
  position := 0;
  eof_reached := false;
  assignfile(f, filename);
  {$I-}
  reset(f, 1);
  {$I+}
  if ioresult <> 0 then begin
    writeln('Error: cannot open ', filename);
    halt(1);
  end;
  sz := filesize(f);
  setlength(srcbuf, sz);
  if sz > 0 then blockread(f, srcbuf[1], sz);
  closefile(f);
  srclen := sz;
  ReadCh;
end;

procedure EnterKW(sym: integer; const name: string);
begin
  keyTab[k].id := name;
  keyTab[k].sym := sym;
  inc(k);
end;

initialization
  k := 0;
  KWX[0] := 0; KWX[1] := 0;
  EnterKW(sIF, 'IF');
  EnterKW(sDO, 'DO');
  EnterKW(sOF, 'OF');
  EnterKW(sOR, 'OR');
  EnterKW(sTO, 'TO');
  EnterKW(sIN, 'IN');
  EnterKW(sIS, 'IS');
  EnterKW(sBY, 'BY');
  KWX[2] := k;
  EnterKW(sEND, 'END');
  EnterKW(sNIL, 'NIL');
  EnterKW(sVAR, 'VAR');
  EnterKW(sDIV, 'DIV');
  EnterKW(sMOD, 'MOD');
  EnterKW(sFOR, 'FOR');
  KWX[3] := k;
  EnterKW(sELSE, 'ELSE');
  EnterKW(sTHEN, 'THEN');
  EnterKW(sTRUE, 'TRUE');
  EnterKW(sTYPE, 'TYPE');
  EnterKW(sCASE, 'CASE');
  KWX[4] := k;
  EnterKW(sELSIF, 'ELSIF');
  EnterKW(sFALSE, 'FALSE');
  EnterKW(sARRAY, 'ARRAY');
  EnterKW(sBEGIN, 'BEGIN');
  EnterKW(sCONST, 'CONST');
  EnterKW(sUNTIL, 'UNTIL');
  EnterKW(sWHILE, 'WHILE');
  KWX[5] := k;
  EnterKW(sRECORD, 'RECORD');
  EnterKW(sREPEAT, 'REPEAT');
  EnterKW(sRETURN, 'RETURN');
  EnterKW(sIMPORT, 'IMPORT');
  EnterKW(sMODULE, 'MODULE');
  KWX[6] := k;
  EnterKW(sPOINTER, 'POINTER');
  KWX[7] := k; KWX[8] := k;
  EnterKW(sPROCEDURE, 'PROCEDURE');
  KWX[9] := k;
end.
