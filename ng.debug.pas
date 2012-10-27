{$IFDEF NESTUNITS}
unit ng.debug; implementation
{$ENDIF}

  procedure vm.dump;
    var i : integer;
  begin
    write( ^A, 'dump', ^B );
    write( self.data.dumps, ^_ );
    write( self.addr.dumps, ^_ );
    if length( self.ram ) > 1 then
      for i := 0 to length( ram ) - 2 do
	write( ram[ i ], ' ' );
    write( ram[ length( ram ) - 1 ], ^C );
  end;


  procedure vm.show_debugger;
    var
      s	      : string[ 4 ];
      r	      : oprec;
      b, e, i : int32;  { begin, end, index }


    function reverse_lookup( addr : int32 ) : string;
    begin
      result := '(invoke)';
    end;
    
    procedure show_addr;
    begin
      if i = ip then write( ' -> ' ) else write( '    ' );
      write( i:4, ' ' );
    end;
    
    procedure show_opcode;
    begin
      r := optbl[ ram[ i ]];
      write( r.tok );
      if r.hasarg then begin
	inc( i );
	{ show the argument }
	kvm.fg( 'g' ); str( ram[ i ], s ); writeln( ' ', s );
	{ on next line, show where it was }
	kvm.fg( 'K' ); show_addr; write( '...' );
	kvm.fg( 'w' );
      end;
      writeln;
    end; { show_opcode }
    
  begin

    kvm.clrscr;
    kvm.gotoxy( 0, 0 );

    { mini-debugger }
    write( 'ip   : ', ip, ' / ', length( ram ) - 1 );
    writeln;
    write( 'data : ' ); data.dump;
    write( 'addr : ' ); addr.dump;
    write( 'port : ' );
    for i:= 0 to length( ports ) - 1 do
    begin
      write( i , ':' );
      str( ports[ i ], s );
      write( s, ' ');
    end;
    writeln;

    { let's try and keep the pointer somewhere around middle of the 15-line screen }
    b := max( 0, ip - 7 );
    e := min( b + 15, high( self.ram ));

    { mini-debugger }
    i := b;
    repeat
      show_addr;
      if ( i >= low( ram )) and ( i <= high( ram )) then
        if( ram[i] >= low( optbl )) and ( ram[i] <= high( optbl ))
        then show_opcode
        else begin
            write( ram[ i ] );
            write( reverse_lookup( ram[ i ]));
            writeln;
          end
        else writeln( '???' );
      inc( i );
    until i >= e; { can be greater if we read an argument in last cell }
    
    if kvm.readkey <> #13 then ip := high( ram ) + 1;  { halt machine. todo: call it ascii.esc }
  end;

{$IFDEF NESTUNITS}
end.
{$ENDIF}
