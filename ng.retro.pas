{ retro-specific conventions for ngaro }

{ -- read string from ram --------------------------------------- }

function vm.rx_getstring( start : int32 ) : string;
var at, ch : int32;
begin
  at := start;
  result := '';
  {  Do i really need/want to trim at 255? Maybe use AnsiSrings? }
  while ( ram[ at ] <> 0 )
    and ( at < length( ram ))
    and ( length( result ) < 255 ) do
  begin
    ch := ram[ at ];
    if ch > 127 then begin
      writeln;
      for at := start to start + 254 do begin
	if at < length( ram ) then
	  if ( ram[ at ] < 32 ) or ( ram[ at ] > 127 ) then
	    write( '[#' + inttostr( ram[ at ]) + ']' )
	  else write( chr( ram[ at ]))
	end;
      hexdump( result);
      halt;
    end else begin
      result += chr( ch );
      inc( at );
    end;
  end;
end;
