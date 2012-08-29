program test_sd;
uses sd, chk;

var drive : sd.tDrive;
begin
  drive.init( 'test_sd.b4sd' );
  chk.equal( drive.size, 0, 'drive should be empty' );
end.
