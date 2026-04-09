{$mode objfpc}{$H+}
unit org;
{ ORG - Code Generator for b4vm, replacing Wirth's RISC backend.
  Emits b4 bytecode into a memory buffer, writes .b4x image files. }
interface
uses ors, orb;

const
  WordSize = 4;
  maxCode = 15000; { max bytes of generated code }
  maxStrx = 2400;
  maxTD = 120;

  { b4 opcodes }
  oAD = $80; oSB = $81; oML = $82; oDV = $83; oMD = $84;
  oSH = $85; oAN = $86; oOR = $87; oXR = $88; oNT = $89;
  oEQ = $8A; oLT = $8B;
  oDU = $8C; oSW = $8D; oOV = $8E; oZP = $8F;
  oDC = $90; oCD = $91;
  bRB = $92; bRI = $93; bWB = $94; bWI = $95;
  oLB = $96; oLI = $97; bRS = $98; oLS = $99;
  oJM = $9A; oHP = $9B; oH0 = $9C; oCL = $9D; oRT = $9E; oNX = $9F;
  oC0 = $C0; oC1 = $C1; oC2 = $F6; oN1 = $F7; oC4 = $F8;
  oIO = $FD; oHL = $FF;

  { b4 register opcodes: base + register_number }
  oINV = $00; { invoke: $01..$1F }
  oRDR = $20; { read:   $21..$3F }
  oWRR = $40; { write:  $41..$5F }
  oINC = $60; { stream: $61..$7F }

  { named registers (index into register array) }
  rF = ord('F') - ord('@'); { frame pointer = 6 }
  rG = ord('G') - ord('@'); { global base = 7 }

  { internal item modes (beyond orb class values) }
  mReg = 10;  { value is on the data stack (TOS) }
  mCond = 12; { conditional: a=false-chain, b=true-chain, r=condition }

type
  Item = record
    mode: integer;
    typ: orb.PType;
    a, b: longint;  { meaning depends on mode }
    r: longint;      { register/condition }
    rdo: boolean;
  end;

var
  pc: longint;      { current code position (byte offset from code start) }
  varsize: longint;  { total global variable size }
  entry: longint;    { entry point address }
  codebase: longint; { byte address where code starts in b4 memory }
  strx: longint;     { string buffer index }
  tdx: longint;

procedure Open(v: integer);
procedure SetDataSize(dc: longint);
procedure CheckRegs;
procedure MakeConstItem(var x: Item; tp: orb.PType; val: longint);
procedure MakeRealItem(var x: Item; val: single);
procedure MakeStringItem(var x: Item; len: longint);
procedure MakeItem(var x: Item; y: orb.PObj; curlev: longint);

{ emit helpers - used internally and by other org routines }
procedure Emit(b: byte);
procedure Emit4(v: longint);
procedure EmitLI(v: longint);
procedure EmitLB(v: byte);
procedure EmitRegRd(r: byte);
procedure EmitRegWr(r: byte);

{ forward declarations for procedures defined in later parts }
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
  GlobalBase = $0100; { where global variables start in b4 memory }
  B4MemSize = 65536;  { total b4 memory image size }

var
  code: array[0..maxCode-1] of byte;
  strbuf: array[0..maxStrx-1] of char;
  version: integer;

{ ---- Emit helpers ---- }

procedure Emit(b: byte);
begin
  if pc < maxCode then begin code[pc] := b; inc(pc) end
  else ors.Mark('program too long');
end;

procedure Emit4(v: longint);
begin
  { little-endian 32-bit }
  Emit(byte(v));
  Emit(byte(v shr 8));
  Emit(byte(v shr 16));
  Emit(byte(v shr 24));
end;

procedure EmitLI(v: longint);
begin
  { li <int32> — push 32-bit literal }
  Emit(oLI);
  Emit4(v);
end;

procedure EmitLB(v: byte);
begin
  { lb <byte> — push unsigned byte }
  Emit(oLB);
  Emit(v);
end;

procedure EmitPushInt(v: longint);
begin
  { push an integer constant using the shortest encoding }
  if v = 0 then Emit(oC0)
  else if v = 1 then Emit(oC1)
  else if v = 2 then Emit(oC2)
  else if v = 4 then Emit(oC4)
  else if v = -1 then Emit(oN1)
  else if (v >= 0) and (v <= 255) then EmitLB(byte(v))
  else EmitLI(v);
end;

procedure EmitRegRd(r: byte);
begin
  Emit(oRDR + r); { @R — read register to data stack }
end;

procedure EmitRegWr(r: byte);
begin
  Emit(oWRR + r); { !R — write data stack to register }
end;

{ ---- Item constructors ---- }

procedure MakeConstItem(var x: Item; tp: orb.PType; val: longint);
begin
  x.mode := orb.clsConst;
  x.typ := tp;
  x.a := val;
  x.b := 0;
  x.r := 0;
  x.rdo := false;
end;

procedure MakeRealItem(var x: Item; val: single);
begin
  { b4vm has no float support - store as int bits }
  x.mode := orb.clsConst;
  x.typ := orb.realType;
  move(val, x.a, 4);
end;

procedure MakeStringItem(var x: Item; len: longint);
var i: longint;
begin
  x.mode := orb.clsConst;
  x.typ := orb.strType;
  x.a := strx;  { offset into string buffer }
  x.b := len;   { length }
  x.r := 0;
  if strx + len + 4 < maxStrx then begin
    i := 0;
    while len > 0 do begin
      strbuf[strx] := ors.str[i];
      inc(strx); inc(i); dec(len);
    end;
    while strx mod 4 <> 0 do begin strbuf[strx] := #0; inc(strx) end;
  end else
    ors.Mark('too many strings');
end;

procedure MakeItem(var x: Item; y: orb.PObj; curlev: longint);
begin
  if y = nil then begin
    x.mode := orb.clsConst; x.typ := orb.intType; x.a := 0;
    exit;
  end;
  x.mode := y^.cls;
  x.typ := y^.typ;
  x.a := y^.val;
  x.rdo := y^.rdo;
  x.r := 0;
  x.b := 0;
  if y^.cls = orb.clsPar then
    x.b := 0
  else if y^.cls = orb.clsTyp then begin
    x.a := y^.typ^.len;
    x.r := -y^.lev;
  end else if (y^.cls = orb.clsConst) and (y^.typ^.form = orb.tString) then
    x.b := y^.lev
  else
    x.r := y^.lev;
  if (y^.lev > 0) and (y^.lev <> curlev) and (y^.cls <> orb.clsConst) then
    ors.Mark('level error, not accessible');
end;

{ ---- Open / SetDataSize / CheckRegs ---- }

procedure Open(v: integer);
begin
  pc := 0;
  tdx := 0;
  strx := 0;
  version := v;
  codebase := 0; { set later by Header }
end;

procedure SetDataSize(dc: longint);
begin
  varsize := dc;
  codebase := GlobalBase + varsize;
end;

procedure CheckRegs;
begin
  if pc >= maxCode - 40 then ors.Mark('program too long');
end;

{$I org2.inc}
{$I org3.inc}
{$I org4.inc}

end.
