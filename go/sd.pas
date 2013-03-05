{
| storage device
|
| -------------------------------------------------
| copyright (c) 2012 michal j. wallace
| see LICENSE.org for usage information
}
unit sd;

interface uses log, fs, sysutils;

type
  TBlock = array [ 0..1023 ] of byte;
  TDrive = object {  object because of gpc... probably should drop }
    constructor init ( path : string );
    procedure wipe;
    procedure grow ( n : byte );
    procedure load ( i : integer; var b : TBlock );
    procedure save ( i : integer; var b : TBlock );
    function block_count : cardinal;
    function byte_count : cardinal;
    destructor done ( );
  private
    mFile : file of TBlock;
  end;

var
  main : TDrive;

implementation
  var
    empty_block	: TBlock;
  {$IFDEF GPC}
    {$i sd_gpc.inc}
  {$ELSE}
    {$i sd_fpc.inc}
  {$ENDIF}

begin
  FillDWord( empty_block, 256, 0 );
end.
