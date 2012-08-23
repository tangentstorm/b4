{
| the b4 virtual machine and instruction set
|
|   This is a tiny, 16-instruction virtual machine.
|   It has only 256 bytes of ram, some of which act as
|   registers, stacks, and i/o ports.
|
|   While this machine is so simple as to be almost
|   useless by itself, it is possible to run up to
|   256 of them simultaneously.
|
| -------------------------------------------------
| copyright (c) 2012 michal j. wallace
| see LICENSE.org for usage information
}
unit vm;
interface

const
   
   { opcodes }
  
   oNOP	= $0; oCMP = $1; oELS = $2; oJMP = $3;
   oGET	= $4; oPUT = $5; oINC = $6; oDEC = $7;
   oPSH	= $8; oPOP = $9; oSWP = $a; oDUP = $b;
   oXOR	= $c; oAND = $d; oNOT = $e; oSHR = $f;
   
   { registers }
   
   IP = $00;
   P1 = $01; P2 = $02; P3 = $03; P4 = $04;
   IO = $09;
   A  = $0A; B = $0B; C = $0C; D = $0D; E = $0E; F = $0F;
      
   { offset of stacks and special memory locations }
   
   ADDR = $10; R = $17;
   DATA = $18; T = $18; N = $19;
   
   IBUF = $20;
   OBUF = $F0;
   BUFSIZE = 16; { including a 1 byte count / status }
   
   CODE = $30;
   MISC = $80;
   
type
   
   memory = array [ 0 .. 255 ] of byte;
       
   { vm.core - as in "cpu core" } 
   core = object
     
     ram : memory;
           
     constructor init;
     
     { runtime interface for b4 mainloop }
     procedure boot;
     procedure tick;
     procedure send ( x : byte );
     
     function active : boolean;
     function ready  : boolean; { ready for input? }
     
     { serialization }
     procedure load ( var state : memory );
     procedure dump ( var state : memory );
     
     { right and left shift of blocks of ram for stacks, io }
     function rshift ( off, num, amt : byte; wrap : boolean ) : byte;
     function lshift ( off, num, amt : byte; wrap : boolean ) : byte;
      
   end;
   
{ ------------------------------------------------- }
   
implementation

constructor core.init;
  var i : byte;
begin
  for i := 0 to 255 do self.ram [ i ] := 0;
end;

{ -- runtime interface for b4 mainloop -- }

procedure core.boot;
begin
  self.ram[ IP ] := CODE;
end;

procedure core.tick;
begin
end;

procedure core.send ( x : byte );
begin
end;

{ -- queries -- }

function core.active : boolean;
  { To deactivate a core, simply set its IP to 0 }
begin   
  result := self.ram[ IP ] <> 0;
end;

function core.ready : boolean;
  { Returns true unless the input buffer is full. }
begin   
  result := self.ram[ IBUF ] <= BUFSIZE;
end;

{ -- serialization -- }

procedure copy_memory ( var src, dst : memory );
  var i : byte;
begin
  for i := 0 to 255 do dst [ i ] := src [ i ];
end;

procedure core.load ( var state : memory );
begin
  copy_memory ( state, self.ram );
end;

procedure core.dump ( var state : memory );
begin
  copy_memory ( self.ram, state );
end;

{ -- semi-private -- }

function core.rshift ( off, num, amt : byte; wrap : boolean ) : byte;
  var tmp : byte;
begin
  result := tmp;
end;

function core.lshift ( off, num, amt : byte; wrap : boolean ) : byte;
  var tmp : byte;
begin	 
  result := tmp;
end;

{ ------------------------------------------------- }

begin
end.
