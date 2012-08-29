{
| storage device
|
| -------------------------------------------------
| copyright (c) 2012 michal j. wallace
| see LICENSE.org for usage information
}
unit sd;

interface

type
  tBlock = array [ 0..1023 ] of byte;
  tDrive = object
    constructor init ( path : string );
    procedure grow ( n : byte );
    procedure load ( i : integer; var b : tBlock );
    procedure save ( i : integer; var b : tBlock );
    function size ( ) : cardinal;
    destructor done ( );
  private
    mPath : string[ 96 ];
    mFile : file of tBlock;
  end;
  
var main : tDrive;

implementation

const disk = 'tBlocks.b4sd';
const kBlockSize = 1024;
var empty_block : tblock;

constructor tDrive.init( path : string );
begin
  mPath := path;
  assign( mFile, path );
  rewrite( mFile );
end;

procedure tDrive.grow ( n : byte );
var i : cardinal;
begin
  seek( mFile, self.size );
  for i := 1 to n do write( mFile, empty_block );
  flush( mFile );
end;

procedure tDrive.load ( i : integer; var b : tBlock );
begin
end;

procedure tDrive.save ( i : integer; var b : tBlock );
begin
end;

function tDrive.size ( ) : cardinal;
begin
  size := filesize( mFile ) * sizeof( empty_block );
end;

destructor tDrive.done ( );
begin
  close( mFile );
end;

var i : integer;
begin
  for i := 0 to kBlockSize - 1 do
    empty_block[ i ] := 0;
end.
