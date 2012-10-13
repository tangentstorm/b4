{
  posix :provides general posix routines
  copyright (c) 2012 michal j. wallace. all rights reserved.
  available to the public under the ISC or MIT/X11 license.
}
{$i xpc.inc }
unit posix;
interface uses xpc, sysutils, dateutils;

  function time : int64;

implementation

  function time : int64;
  begin
    result := dateutils.DateTimeToUnix( sysutils.Time );
  end;
  
begin
end.
