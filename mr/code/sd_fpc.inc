
constructor TDrive.init ( path : string );
  begin
    // change to fs.open when it's ready
    fs.update( mFile, path );
  end;

procedure TDrive.wipe;
  begin
    seek( mFile, 0 );
    truncate( mFile );
  end;

procedure TDrive.grow ( n : byte );
  begin
    seek( mFile, filesize( mfile ));
    for n := n downto 1 do write( mFile, empty_block );
  end;

procedure TDrive.load ( i : integer; var b : TBlock );
  begin
    seek( mFile, i );
    read( mFile, b );
  end;

procedure TDrive.save ( i : integer; var b : TBlock );
  begin
    seek( mFile, i );
    write( mFile, b );
  end;

function TDrive.block_count : cardinal;
  begin
    result := filesize( mFile ) div sizeOf( TBlock );
  end;

function TDrive.byte_count : cardinal;
  begin
    result := self.block_count * sizeOf( TBlock );
  end;

destructor TDrive.done;
  begin
    close( mfile );
  end;
