unit empty;
interface

const
  tokLen = 15;
type
  tokstr = string[toklen];
  address  = 0..65535;
  cardinal = address;
  integer  = int32;
  thunk    = procedure;

  {-- main public inteface --}
  procedure createop( const iden : tokstr; code : thunk );
  procedure mainloop;
  
  {-- these might wind up hidden... --}
  procedure pushdat( x:integer );
  procedure pushret( x:integer );
  procedure pass;
  procedure gosub;
  procedure step;
  procedure execute;
  procedure interpret;
  function lookup : boolean;
  procedure notfound;
  function default_number : boolean;
  procedure default_getnext;
  procedure default_welcome;
  procedure default_refill;
  

const
  prim	 = 0;
  quit	 : integer = -1;

type
  TWord	 = record
    // prev : address;
    word : tokstr;
    code : address;
    data : cardinal
  end;

var
  brand	 : string  = 'EmptyForth';
  verMaj : byte  = 0;
  verMin : byte = 1;



{-- the virtual machine ---------------------------------------}

var
  ram : array[ address  ] of integer;
  ops : array[ 0 .. 255 ] of thunk;

  sp : integer  = 0;
  rp : integer  = 1024;
  ip : cardinal = 0;

  src, msg : string[ 255 ]; // input and output buffers
  inp   : byte;             // pointer into src
  token : string[ tokLen ]; // temp buffer for token
  done  : boolean = false;  // controls the outer interpreter
  which : cardinal;         // address of last looked-up word

  // for now, we're just using a simple array for the dictionary
  words  : array[ 0 .. 1023 ] of TWord;
  numwds : cardinal = 0;
  numops : cardinal = 0;

{-- delegates -------------------------------------------------}

var
  refill,       { repopulates 'src' }
  getnext,      { copy next token from 'src' to 'token' }
  respond,
  welcome
  : thunk;

  number : function : boolean;

implementation


{-- direct stack access from pascal ---------------------------}

procedure pushdat( x:integer );
begin
  dec(sp); ram[sp] := x;
end;

procedure pushret( x:integer );
begin
  dec(rp); ram[rp] := x;
end;


{-- primitive control flow  -----------------------------------}

procedure pass;
begin
end;

procedure gosub;
begin
  pushret( ip ); ip := ram[ip];
end;


{-- inner interpreter -----------------------------------------}

procedure step;
var op : integer;
begin
  op := ram[ip];
  case abs(op) of
    -1 : ops[abs(op)];
     0 : { do nothing } ;
    +1 : gosub;
  end;
  inc(ip);
end;

procedure execute;
begin
  repeat step until ram[ip] = quit
end;

procedure interpret;
begin
  with words[which] do
    if code = prim then ops[data] { run primitive directly }
    else begin                    { run from dictionary }
      pushdat(data); ip := code;
      gosub; execute;
    end;
end;

{-- outer interpreter -----------------------------------------}

function lookup : boolean;
  var found: boolean = false;
begin
  which := numwds; found := false;
  while (which > 0) and not found do begin
    dec(which);
    found := words[which].word = token;
  end;
  lookup := found;
end;

procedure notfound;
begin
  msg := token + '?';
end;

procedure mainloop;
begin
  welcome;
  repeat
    getnext;
    if lookup then interpret
    else if number() then notfound;
    respond
  until done;
end;


{-- default tokenizer -----------------------------------------}

function default_number : boolean;
begin
  {  todo }
  default_number := false;
end;

procedure default_getnext;
  function ignorable : boolean;
  begin
    ignorable := (inp > length(src)) or (src[inp] <= ' ')
  end;
begin
  while ignorable do
    if inp <= length(src) then inc(inp)
    else begin refill; inp := 0; end;
  token := '';
  repeat token := token + src[inp]; inc(inp)
  until ignorable or (inp > length(src));
end;

{-- user interface --------------------------------------------}

procedure default_refill;
begin
  repeat write('> '); readln(src)
  until length(src) > 0;
end;

procedure default_respond;
begin
  if msg <> '' then writeln(msg);
  msg := '';
end;

procedure default_welcome;
begin
  writeln(brand, ' ', verMaj, '.', verMin);
end;


{-- dictionary-handlers ---------------------------------------}

procedure createop( const iden : tokstr; code : thunk );
begin
  ops[numops] := code;
  with words[numwds] do begin
    // prev := ... ;
    word := iden;
    code := prim;
    data := numops;
  end;
  inc(numwds); inc(numops);
end;

var i : integer;
begin
  refill  := @default_refill;
  getnext := @default_getnext;
  respond := @default_respond;
  welcome := @default_welcome;
  number  := @default_number;
  for i := high(ops) downto low(ops) do ops[i] := @pass;
  for i := high(ram) downto low(ram) do ram[i] := 0;
end.
