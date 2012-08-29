unit log;
interface

type tLevel = ( lvl_normal, lvl_debug );
var level : tLevel = lvl_normal;

procedure debug(msg :  string );

implementation

procedure debug( msg : string );
begin
  if ord( level ) >= ord( lvl_debug ) then writeln( msg );
end;

end.