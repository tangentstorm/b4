{
| the b4 virtual machine and instruction set
|
|   This is a tiny, 16-instruction virtual machine.
|   It has only 256 bytes of ram, 8 of which act as
|   registers, and 8 of which act as i/o ports.
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
   oNOP	= $0; oCMP = $1; oELS = $2; oJMP = $3;
   oGET	= $4; oPUT = $5; oINC = $6; oDEC = $7;
   oPSH	= $8; oPOP = $9; oSWP = $a; oDUP = $b;
   oXOR	= $c; oAND = $d; oNOT = $e; oSHR = $f;
   
type
   core	= object
      { data, addr : stack; }
      function active : boolean;
      procedure tick;
   end;
   
implementation

function core.active : boolean;
begin
   result := false;
end;

procedure core.tick;
begin
   { do nothing }
end;

begin
end.
