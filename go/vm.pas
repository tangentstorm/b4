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
