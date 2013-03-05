{-- gnu pascal version of sd.TDrive --}

constructor tDrive.init( path : string );
begin
  log.debug( 'init' );
  assign( mFile, path );
end;

procedure tDrive.wipe;
begin
  log.debug( 'wipe' );
  rewrite( mFile ); // truncates to 0 
  close( mFile );
end;


procedure tDrive.grow ( n : byte );
var i : cardinal;
begin
  log.debug( 'grow' );
  append( mFile );
  for i := 1 to n do write( mFile, empty_block );
  close( mFile );
end;

procedure tDrive.load ( i : integer; var b : tBlock );
begin
  log.debug( 'load' );
  reset( mFile );
  seek( mFile, i );
  read( mFile, b );
  close( mFile );
end;

procedure tDrive.save ( i : integer; var b : tBlock );
begin
  log.debug( 'save' );
  rewrite( mFile );
  reset( mFile );
  seek( mFile, i );
  write( mFile, b );
  close( mFile );
end;

function tDrive.block_count ( ) : cardinal;
begin
  append( mFile );
  block_count := filesize( mFile );
  close( mFile );
end;

function tDrive.byte_count ( ) : cardinal;
begin
  byte_count := self.block_count * sizeof( empty_block );
end;

destructor tDrive.done ( );
begin
end;
