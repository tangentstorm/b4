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
  tLine = string[ 64 ];
  tBlock = record
    head : tLine;
    body : array [ 0..16 ] of tLine;
  end;
  
  tDrive = object
    constructor init ( path : string );
    procedure   load ( n : integer; var b : tBlock );
    procedure   save ( n : integer; var b : tBlock );
    procedure   grow ( var b : tBlock );
    function  size ( ) : cardinal;
    destructor  done ( );
  private
    mPath : string[ 96 ];
    mFile : file of tBlock;
  end;
  
var main : tDrive;

implementation

const disk = 'tBlocks.b4sd';

constructor tDrive.init( path : string );
begin
  mPath := path;
  assign( mFile, path );
  rewrite( mFile );
end;
  
procedure tDrive.load ( n : integer; var b : tBlock );
begin
end;

procedure tDrive.save ( n : integer; var b : tBlock );
begin
end;

procedure tDrive.grow ( var b : tBlock );
begin
end;

function tDrive.size ( ) : cardinal;
begin
  size := filesize( mFile );
end;

destructor tDrive.done ( );
begin
  close( mFile );
end;

begin
end.
