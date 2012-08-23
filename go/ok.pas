program b4;

const
   oNOP = $0; oCMP = $1; oELS = $2; oJMP = $3;
   oGET = $4; oPUT = $5; oINC = $6; oDEC = $7;
   oPSH = $8; oPOP = $9; oSWP = $a; oDUP = $b;
   oXOR = $c; oAND = $d; oNOT = $e; oSHR = $f;
   
type
   core = object
   end;
   
var
   cloud : array[ 0..255 ] of core;
   msg   : byte;
   i     : byte;
begin
   
end.
