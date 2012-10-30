{$IFDEF NESTUNITS}
unit ng.debug; implementation
{$ENDIF}

  procedure vm.dump;
    var i : integer;
  begin
    write( ^\ );
    write( self.data.dumps, ^] );
    write( self.addr.dumps, ^] );
    if length( self.ram ) > 1 then
      for i := 0 to length( ram ) - 2 do
	write( ram[ i ], ' ' );
    write( ram[ length( ram ) - 1 ] );
  end;

  procedure vm.show_debugger( msg : string );
    var				  
      s         : string[ 4 ];
      r	        : oprec;
      b, e, i   : int32;  { begin, end, index }
 
    function reverse_lookup( xt : int32 ) : string;
      var last : int32 = 2; jumps : int32 = 0; found : boolean = false;
      const xtofs = 2; helpofs = 3; tokenofs = 4;
    begin
      // : skim ( a-a ) ( from kernel.rx )
      // last repeat @ over over d->xt @ == [ nip 0 ] ifTrue 0; again ;
      repeat
	last := ram[ last ]; inc( jumps );
	found := ram[ last + xtofs ] = xt;
      until found or ( last = 0 ) or ( last > length( ram )) or ( jumps > 1000 );
      if found then
	result := ' ( ' + getstring( last + tokenofs ) +
		  ' : ' + getstring( last + helpofs ) + ' ) '
      else result := '( ??? )'
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

  begin  {  show_debugger }

    if msg <> '' then begin
      writeln( msg );
      writeln( 'press enter to debug' );
      readln;
    end;
    
    kvm.clrscr;
    kvm.gotoxy( 0, 0 );
    writeln( msg );

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
  end; { show_debugger }

{$IFDEF NESTUNITS}
end.
{$ENDIF}
