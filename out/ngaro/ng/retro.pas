//
// This is the main entry point for the pascal implementation
// of the ngaro virtual machine. It's called retro because that's
// the "operating system" that runs on it.
//
// see: http://retroforth.org/
//
{$i xpc.inc}{$mode objfpc}
program retro;
uses
  xpc, sysutils, classes, lined, kvm,
  {$IFDEF GL}
    {$IFDEF SDL}
       rxgl in 'ng\rxgl_sdl.pas',
    {$ELSE}
       rxgl in 'ng\rxgl_zen.pas',
    {$ENDIF}
  {$ENDIF}
  ng;

{ configuration / startup  helpers }
type
  TRetroConfig = record
    minSize   : int32;
    imgPath   : TStr;
    debug     : boolean;
    dumpAfter : boolean;
    lineEdit  : boolean;
    inputs    : array of TStr;
  end;

procedure fail( msg : TStr );
  begin
    writeln; writeln( msg ); halt;
  end; { fail }
  
{ if file exists, add to array of inputs to include }
  procedure include( var cfg : TRetroConfig;  const path : TStr );
  begin
    if sysutils.fileexists( path ) then begin
      // we can't just call include until we initialize the vm, and
      // we can'd do that until we're done with args, so we use the
      // inputs array as a buffer.
      setlength( cfg.inputs, length( cfg.inputs ) + 1 );
      cfg.inputs[ length( cfg.inputs ) - 1 ] := path;
    end else fail( Format('"%s" not found', [path]));
  end; { include }

{ load configuration (from command line parameters) }

function configure( var cfg : TRetroConfig ) : boolean;
  var p : TStr; i : integer = 1;
  function haveparam:boolean; begin haveparam := i <= paramcount end;
  function nextparam:TStr; begin result := paramstr(i); inc(i) end;
  begin
    result := true; { false will indicate failure }
    cfg.imgPath := ''; cfg.minSize := -1;
    cfg.dumpAfter := false; cfg.lineEdit := false; cfg.debug := false;
    { parse the command line parameters }
    while result and haveparam do begin
      p := nextparam;
      if (p = '--debug') or (p = '-d') then cfg.debug := true
      else if p = '--dump' then cfg.dumpAfter := true
      else if (p = '--editline') or (p = '-e') then cfg.lineEdit := true
      else if p = '--with' then
        if haveparam then include( cfg, nextparam )
        else fail( 'no filename given for --with' )
      else if p = '--pad' then
        if haveparam then cfg.minsize := strtoint( nextparam )
        else fail( 'expected a number after --pad' )
      else if p = '--image' then cfg.imgPath := nextparam
      else begin { no prefix, so expect image name }
        if cfg.imgpath = '' then cfg.imgpath := p
        else fail( 'error: more than one image path given.' )
      end;
    end;
    if cfg.imgPath = '' then cfg.imgPath := 'retroImage';
    if ( cfg.minSize = -1 ) then cfg.minSize := 100000;
  end; { configure }

{ optional line editor (replaces ng.vm.handle_keyboard) }
type
  TEdlnDevice = class (TComponent)
    public
      edln : lined.LineEditor;
      buff : TStr;
      constructor Create(aOwner :TComponent); override;
      function handle ( msg : int32 ) : int32;
      procedure step;
    end;

constructor TEdlnDevice.Create(aOwner :TComponent);
  begin
    inherited Create(aOwner);
    edln := lined.ed;
    edln.prompt := '|Gok|g>|w '
  end;

procedure TEdlnDevice.step;
  begin
    edln.step;
  end;

function TEdlnDevice.handle ( msg : int32 ) : int32;
  begin
    while length( buff ) = 0 do begin
      while not edln.done do edln.step;
      buff := edln.flush + #10;
      writeln;
    end;
    result := ord( buff[ 1 ]);
    delete( buff, 1, 1 );
  end; { handle_keyboard_buffer }

{ main code }
var
  cfg  : TRetroConfig;
  path : TStr;
  ed   : TEdlnDevice;
  vm   : ng.TNgaroVM;
begin
  if configure( cfg ) then begin
    {$i-} vm := TNgaroVM.New( cfg.imgpath ); {$i+}
    if IOResult <> 0 then
      die(Format('couldn''t open image file: %s', [cfg.imgpath]))
    else begin
      vm.debugMode := cfg.debug;
      vm.minsize := cfg.minsize;
      for path in cfg.inputs do vm.include( path );
      if cfg.lineEdit then begin // replace ngaro's keyboard driver
        ed := TEdlnDevice.Create(vm);
        vm.devices[1] := @ed.handle;
      end;
      {$IFDEF GL} rxgl.Main(vm); {$ELSE} vm.Loop; {$ENDIF}
      if cfg.dumpAfter then vm.dump;
      vm.free;
    end
  end
end.
