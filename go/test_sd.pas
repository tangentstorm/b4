program test_sd;
uses sd, chk;

var
  drive	: sd.tDrive;
  block	: sd.tBlock;
begin

  { constructor } 
  drive.init( 'test_sd.b4sd' );
  chk.equal( drive.size, 0,
	    'drive should be empty' );

  { load }
  drive.grow( 1 );
  chk.equal( drive.size, 1024,
	    'file should be 1K after grow' );

  report;

end.
