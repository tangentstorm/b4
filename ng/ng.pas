{$mode objfpc}{$i xpc.inc}{$M+}
unit ng;
interface uses xpc, stacks, kvm, kbd, posix, sysutils, classes;

type
  token  = string[ 6 ];
  thunk  = procedure of object;
  device = function( msg : int32 ) : int32 of object;
  oprec  = record
    go  : thunk;
    tok, sig    : token;
    hasarg      : boolean;
  end;
  TInt32Stack = specialize GStack<Int32>;

{ interface > types }

  TNgaroVM = class (TComponent)
    private
      data, addr : TInt32Stack;
      ram, ports : array of int32;
      ip         : integer;           { instruction pointer }
      done       : boolean;
      inputs     : array of textfile; { input files - see ng.input.pas }
      input      : ^textfile;
      optbl      : array of oprec;
      _debug     : boolean;
      _imgpath   : string;
      _imgsize   : cardinal;
      _minsize   : cardinal;
    public
      devices    : array of device;
      retroTerm  : kvm.ITerm;
      debugTerm  : kvm.ITerm;

      constructor New(imagepath : string);
      destructor Destroy;
      { single-step instructions }
      procedure Step;
      procedure RunOp( op:int32 );

      { input/output }
      procedure RunIO;
      function Waiting : boolean;

      { main loop(s) }
      procedure Loop;

      { input file loader }
      procedure Include( path : string );

      { image routines }
      procedure Load;
      procedure Save;
      procedure SetImgSize( newsize : cardinal );
      procedure SetMinSize( newsize : cardinal );

      { debug / inspect routines }
      procedure Trace;
      procedure Dump;
      procedure Show_Debugger( msg : string );
    published
      property debugmode : boolean read _debug write _debug;
      property imgpath : string read _imgpath write _imgpath;
      property imgsize : cardinal read _imgsize;
      property minsize : cardinal read _minsize write SetMinSize;

{ continued... }

{ interface > type vm = object ... }
    public
  { opcodes, defined in ng.ops.pas }
      procedure oNOP;  procedure oLIT;  procedure oDUP;  procedure oDROP;
      procedure oSWAP; procedure OPUSH; procedure oPOP;  procedure oLOOP;
      procedure oJMP;  procedure oRET;  procedure oJLT;  procedure oJGT;
      procedure oJNE;  procedure oJEQ;  procedure oLOD;  procedure oSTO;
      procedure oADD;  procedure oSUB;  procedure oMUL;  procedure oDIVM;
      procedure oAND;  procedure oOR;   procedure oXOR;  procedure oSHL;
      procedure oSHR;  procedure oZEX;  procedure oINC;  procedure oDEC;
      procedure oIN;   procedure oOUT;  procedure oWAIT; procedure oIVK;
      procedure oDEBUG; { custom opcode }
      procedure init_optable;

  { port handlers, defined in ng.ports.pas }
      procedure clear;
      function handle_syncport( msg : int32 ) : int32;
      function handle_keyboard( msg : int32 ) : int32;
      function handle_input( msg : int32 ) : int32;
      procedure next_input;
      function handle_write( msg : int32 ) : int32;
      function handle_refresh( msg : int32 ) : int32;
      function handle_files( msg : int32 ) : int32;
      function handle_vmquery( msg : int32 ) : int32;
      function handle_canvas( msg : int32 ) : int32;
      function handle_mouse( msg : int32 ) : int32;
      function handle_eterm( msg : int32 ) : int32;
      procedure init_porthandlers;

   { retro image layout conventions }
      function rx_getstring( start : int32 ) : string;
    end;
  ENotFinished = class (Exception);

const
  rxWAITING = 0;
  rxACTIVE  = 1;


implementation

  {$i ng_ops.pas }
  {$i ng_retro.pas }
  {$i ng_input.pas }
  {$i ng_files.pas }
  {$i ng_ports.pas }
  {$i ng_debug.pas }

constructor TNgaroVM.New( imagepath : string );
  begin
    _imgsize := 0;
    _minsize := 65536;
    self.init_optable;
    assert( length( self.optbl ) >= 31 );
    self.init_porthandlers;
    self.data := TInt32Stack.Create( 128 );
    self.addr := TInt32Stack.Create( 128 );
    self.ip := 0;
    self.imgpath := imagepath;
    self.load;
    self.debugmode := false;
    self.ports[0] := rxACTIVE;
    self.retroTerm := kvm.work;
    self.debugTerm := kvm.work;
  end; { vm.init }

destructor TNgaroVM.Destroy;
  begin
    inherited destroy;
    self.data.free;
    self.addr.free;
  end;


{ load and save the vm }

procedure TNgaroVM.Load;
  var i : int32; f : file of int32;
  begin
    {$i-}
    system.assign( f, imgpath );
    reset( f );
    {$i+}
    if ioresult = 0 then begin
      SetImgSize(FileSize(f));
      //  TODO: ReadBlock
      for i := 0 to FileSize(f) - 1 do begin
        read( f, self.ram[ i ]);
      end;
      close( f );
    end else begin
      writeln( 'error: unable to open ', self.imgpath );
      halt;
    end;
  end;

procedure TNgaroVM.SetImgSize( newsize : cardinal );
  begin
    _imgsize := max(_minsize, newsize);
    setlength( self.ram, _imgsize );
  end;

procedure TNgaroVM.SetMinSize( newsize : cardinal );
  begin
    _minsize := newsize; setlength( self.ram, min(_imgsize, _minsize));
  end;

procedure TNgaroVM.Save;
  var size, i : int32; f: file of int32;
  begin
    size := length( self.ram );
    system.assign( f, imgpath );
    rewrite( f, 1 );
    for i := 0 to size - 1 do begin
      write( f, self.ram[ i ])
    end;
    close( f );
  end;


{ step by step run of the vm }

procedure TNgaroVM.Step;
  begin
    if self.debugmode then show_debugger( '' );
    if ( ip >= low( ram )) and ( ip <= high( ram )) then
    begin
      runop( ram[ ip ] );
      inc( ip );
    end else done := true
  end;

procedure TNgaroVM.Trace;
  var log : text;
  begin
    system.assign( log, 'pascal.log' );
    rewrite( log );
    repeat
      writeln( log, ip, ^_, ram[ ip ], ^_, data.dumps, ^_, addr.dumps );
      step
    until ip >= length( ram );
  end;

procedure TNgaroVM.RunOp( op: int32 );
  begin
    { TODO : real breakpoints }
    if false { or ( op = 129 ) } and not debugmode then begin
      debugmode := true; show_debugger( ' break ' );
    end;
    if op >= length( optbl ) then oIVK { invoke a procedure }
    else if op < 0 then begin
      show_debugger( 'bad opcode: ' + inttostr( op ));
      readln; debugmode := true;
    end
    else optbl[ op ].go;
  end;

{ IO System
  -------------------------------------------------------------

  The VM communicates with the outside world through a group of
  virtual ports. These are simply slots in an array, associated with a
  particular callback function of type:

  function callback( msg : Int32 ) : Int32;

  - from the VM, use the OUT instruction to send data to a port.
  - [ NOTE: this part is going away ]
       * set ports[ 0 ] to 0, again via the OUT opcode
       * invoke the 'wait' opcode, which triggers =runio=, below.

  A device will only be triggered when you write a non-zero values
  to its port. Only one device will trigger on each WAIT, and they
  will always be executed in order of ascending port number.

  NOTE:Because only one is port is called per step, the WAIT
  opcode isn't necessary, and should be removed.

  <I am only keeping it for the time being because I haven't fully
  tested retro without it.>
  ------------------------------------------------------------- }
procedure TNgaroVM.RunIO; { triggered by the oWAIT op }
  var p: int32 = 1;
  begin
    { find the first message }
    while (p < length( ports )) and (ports[p] = 0) do inc(p);
    if p < length( ports ) then
      try
        ports[ p ] := devices[ p ]( ports[ p ]);
        ports[ 0 ] := rxACTIVE;
      except
        on ENotFinished do ports[ 0 ] := rxWAITING;
      else
        writeln('protocol error on port: ', 1);
        raise
      end;
    { waiting could be set directly or triggered by the try..except }
    if waiting then dec(ip);
    {TODO: use something besides exceptions for asynchronous
      requests: probably port 0. }
    {TODO: design a mechanism for sending non-stack data. }
  end;

{ the top level routine }

procedure TNgaroVM.Loop;
  begin
    repeat step until done
  end;


initialization
end.
