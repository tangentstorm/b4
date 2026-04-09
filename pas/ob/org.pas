{$mode objfpc}{$H+}
unit org;
{ ORG - Code Generator for b4vm. Emits b4a assembly text. }
interface
uses ors, orb;

const
  WordSize = 4;
  maxStrx = 2400;

  { internal item modes (beyond orb class values) }
  mReg = 10;   { value is on the data stack (TOS) }
  mRegI = 11;  { address on DS, needs dereference }
  mCond = 12;  { conditional }

type
  Item = record
    mode: integer;
    typ: orb.PType;
    a, b: longint;
    r: longint;
    rdo: boolean;
  end;

var
  pc: longint;       { label counter for generating unique labels }
  varsize: longint;
  entry: longint;
  strx: longint;
  tdx: longint;
  modname: ors.Ident; { current module name, used as label prefix }
  out_: text;         { output file }

procedure Open(v: integer);
procedure SetDataSize(dc: longint);
procedure CheckRegs;
procedure MakeConstItem(var x: Item; tp: orb.PType; val: longint);
procedure MakeRealItem(var x: Item; val: single);
procedure MakeStringItem(var x: Item; len: longint);
procedure MakeItem(var x: Item; y: orb.PObj; curlev: longint);

{ emit helpers }
procedure EmitRuntime;
procedure Wr(const s: string);    { write assembly text }
procedure WrLn(const s: string);   { write line }
procedure WrLbl(const s: string); { write label definition }
procedure WrComment(const s: string);
procedure WrPushInt(v: longint);
function NewLabel: string;        { generate unique label }

procedure load(var x: Item);
procedure loadAdr(var x: Item);
procedure loadCond(var x: Item);
procedure Store(var x, y: Item);
procedure StoreStruct(var x, y: Item);
procedure CopyString(var x, y: Item);
procedure StrToChar(var x: Item);
procedure Field(var x: Item; y: orb.PObj);
procedure Index(var x, y: Item);
procedure DeRef(var x: Item);
procedure ValueParam(var x: Item);
procedure VarParam(var x: Item; ftype: orb.PType);
procedure OpenArrayParam(var x: Item);
procedure StringParam(var x: Item);
procedure Not_(var x: Item);
procedure And1(var x: Item);
procedure And2(var x, y: Item);
procedure Or1(var x: Item);
procedure Or2(var x, y: Item);
procedure Neg(var x: Item);
procedure AddOp(op: longint; var x, y: Item);
procedure MulOp(var x, y: Item);
procedure DivOp(op: longint; var x, y: Item);
procedure RealOp(op: integer; var x, y: Item);
procedure Singleton(var x: Item);
procedure Set_(var x, y: Item);
procedure In_(var x, y: Item);
procedure SetOp(op: longint; var x, y: Item);
procedure IntRelation(op: integer; var x, y: Item);
procedure SetRelation(op: integer; var x, y: Item);
procedure RealRelation(op: integer; var x, y: Item);
procedure StringRelation(op: integer; var x, y: Item);
procedure For0(var x, y: Item);
procedure For1(var x, y, z, w: Item; var L: longint);
procedure For2(var x, y, w: Item);
function Here: longint;
procedure FJump(var L: longint);
procedure CFJump(var x: Item);
procedure BJump(L: longint);
procedure CBJump(var x: Item; L: longint);
procedure Fixup(var x: Item);
procedure FixLink(L: longint);
procedure PrepCall(var x: Item; var r: longint);
procedure Call(var x: Item; r: longint);
procedure Enter(parblksize, locblksize: longint; int_: boolean);
procedure Return(form: integer; var x: Item; size: longint; int_: boolean);
procedure Increment(upordown: longint; var x, y: Item);
procedure Include(inorex: longint; var x, y: Item);
procedure Assert_(var x: Item);
procedure New_(var x: Item);
procedure Pack(var x, y: Item);
procedure Unpk(var x, y: Item);
procedure Led(var x: Item);
procedure Get_(var x, y: Item);
procedure Put_(var x, y: Item);
procedure Copy_(var x, y, z: Item);
procedure LDPSR(var x: Item);
procedure LDREG(var x, y: Item);
procedure Abs_(var x: Item);
procedure Odd_(var x: Item);
procedure Floor_(var x: Item);
procedure Float_(var x: Item);
procedure Ord_(var x: Item);
procedure Len_(var x: Item);
procedure Shift(fct: longint; var x, y: Item);
procedure ADC_(var x, y: Item);
procedure SBC_(var x, y: Item);
procedure UML_(var x, y: Item);
procedure Bit_(var x, y: Item);
procedure Register_(var x: Item);
procedure H_(var x: Item);
procedure Adr_(var x: Item);
procedure Condition_(var x: Item);
procedure Header;
procedure Close(var modid: ors.Ident; key, nofent: longint);
procedure BuildTD(T: orb.PType; var dc: longint);
procedure TypeTest(var x: Item; T: orb.PType; varpar, isguard: boolean);

implementation
uses sysutils;

const
  GlobalBase = $0100;

var
  strbuf: array[0..maxStrx-1] of char;
  version: integer;
  labelCount: longint;

{ ---- Assembly output helpers ---- }

procedure Wr(const s: string);
begin write(out_, s) end;

procedure WrLn(const s: string);
begin writeln(out_, s) end;

procedure WrLbl(const s: string);
begin write(out_, ':', s, ' ') end;

procedure WrComment(const s: string);
begin writeln(out_, ' # ', s) end;

function NewLabel: string;
begin
  result := '_L' + IntToStr(labelCount);
  inc(labelCount);
end;

procedure WrPushInt(v: longint);
begin
  if v = 0 then Wr('c0 ')
  else if v = 1 then Wr('c1 ')
  else if v = 2 then Wr('c2 ')
  else if v = 4 then Wr('c4 ')
  else if v = -1 then Wr('n1 ')
  else if (v >= 0) and (v <= 255) then Wr('lb ' + IntToHex(v, 2) + ' ')
  else Wr('li ' + IntToHex(byte(v), 2) + ' ' + IntToHex(byte(v shr 8), 2) + ' '
           + IntToHex(byte(v shr 16), 2) + ' ' + IntToHex(byte(v shr 24), 2) + ' ');
end;

procedure WrRegRd(r: byte);
begin Wr('@' + chr(ord('@') + r) + ' ') end;

procedure WrRegWr(r: byte);
begin Wr('!' + chr(ord('@') + r) + ' ') end;

procedure WrLoadVar(level: integer; offset: longint; size: longint);
begin
  if level > 0 then begin
    WrRegRd(6); { @F }
    WrPushInt(offset); Wr('ad ');
  end else
    WrPushInt(GlobalBase + offset);
  if size = 1 then Wr('rb ') else Wr('ri ');
end;

procedure WrStoreVar(level: integer; offset: longint; size: longint);
begin
  { value is already on DS; emit address then wi/wb }
  if level > 0 then begin
    WrRegRd(6); { @F }
    WrPushInt(offset); Wr('ad ');
  end else
    WrPushInt(GlobalBase + offset);
  if size = 1 then Wr('wb ') else Wr('wi ');
end;

procedure WrLoadAdr(level: integer; offset: longint);
begin
  if level > 0 then begin
    WrRegRd(6); WrPushInt(offset); Wr('ad ');
  end else
    WrPushInt(GlobalBase + offset);
end;

{ ---- Item constructors ---- }

procedure MakeConstItem(var x: Item; tp: orb.PType; val: longint);
begin x.mode := orb.clsConst; x.typ := tp; x.a := val; x.b := 0; x.r := 0; x.rdo := false end;

procedure MakeRealItem(var x: Item; val: single);
begin x.mode := orb.clsConst; x.typ := orb.realType; move(val, x.a, 4) end;

procedure MakeStringItem(var x: Item; len: longint);
var i: longint;
begin
  x.mode := orb.clsConst; x.typ := orb.strType; x.a := strx; x.b := len; x.r := 0;
  if strx + len + 4 < maxStrx then begin
    i := 0;
    while len > 0 do begin strbuf[strx] := ors.str[i]; inc(strx); inc(i); dec(len) end;
    while strx mod 4 <> 0 do begin strbuf[strx] := #0; inc(strx) end;
  end else ors.Mark('too many strings');
end;

procedure MakeItem(var x: Item; y: orb.PObj; curlev: longint);
begin
  if y = nil then begin x.mode := orb.clsConst; x.typ := orb.intType; x.a := 0; exit end;
  x.mode := y^.cls; x.typ := y^.typ; x.a := y^.val; x.rdo := y^.rdo; x.r := 0; x.b := 0;
  if y^.cls = orb.clsPar then x.b := 0
  else if y^.cls = orb.clsTyp then begin x.a := y^.typ^.len; x.r := -y^.lev end
  else if (y^.cls = orb.clsConst) and (y^.typ^.form = orb.tString) then x.b := y^.lev
  else x.r := y^.lev;
  if (y^.lev > 0) and (y^.lev <> curlev) and (y^.cls <> orb.clsConst) then
    ors.Mark('level error, not accessible');
end;

procedure Open(v: integer);
begin
  pc := 0; tdx := 0; strx := 0; version := v; labelCount := 0;
end;

procedure SetDataSize(dc: longint);
begin varsize := dc end;

procedure CheckRegs;
begin end;

{$I org2.inc}
{$I org3.inc}
{$I org4.inc}

end.
