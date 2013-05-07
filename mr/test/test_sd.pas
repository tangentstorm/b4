{$i test_sd.def}
implementation
uses sd;

var
  drive	: sd.tDrive;
  block	: sd.tBlock;

  procedure test_sd;
  begin
    { constructor } 
    drive.init( 'test_sd.b4sd' );
    drive.wipe;
    chk.equal( drive.byte_count, 0,
	    'drive should be empty' );

    { load }
    drive.grow( 2 );
    chk.equal( drive.block_count, 2, 
	      'file should be 2 blocks after grow(2)' );
    chk.equal( drive.byte_count, 2048, 
	      'file should be 2K after grow(2)' );
    
    drive.done;
  end;
  
begin
end.
