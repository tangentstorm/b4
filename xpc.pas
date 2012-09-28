{$i xpc.inc }
unit xpc; { cross-platform compilation help }
interface

  type set32 = set of 0 .. 31;
  type int32 = longint;
  function toint( s : set32 ) : int32;
    

implementation

  function toint( s : set32 ) : int32;
    var i, p : byte;
  begin
    result := 0;
    p := 1;
    for i := 0 to 31 do begin
      if i in s then result := result + p;
      p := p * 2;
    end;
  end; { toint }
  
end.		    
