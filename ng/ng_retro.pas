{ retro-specific conventions for ngaro }

{ -- read string from ram --------------------------------------- }

function TNgaroVM.rx_getstring( start : int32 ) : string;
var at, ch : int32;
begin
  at := start;
  result := '';
  {  Do i really need/want to trim at 255? Maybe use AnsiSrings? }
  while ( ram[ at ] <> 0 )
    and ( at < length( ram ))
    and ( length( result ) < 255 )
    and ( ram[ at ] < 128 ) do
  begin
    ch := ram[ at ];
    result += chr( ch );
    inc( at );
  end;
end;
