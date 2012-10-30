{ ngaro opcodes }

{$IFDEF NESTUNITS}
unit ng.ops; implementation
{$ENDIF}

  { TN : move data.tos and data.nos into t and n }
  var t, n : integer;
  {$MACRO ON}
  {$DEFINE TN := t :=data.pop; n:=data.pop; }

  procedure vm.oNOP; begin end;

  { -- memory ops --------------------------------------------- }

  procedure vm.oLIT;
  begin
    inc( ip );
    data.push( ram[ ip ])
  end;

  procedure vm.oLOD;
  begin
    t := data.pop;
    if ( t < length( ram )) then data.push( ram[ t ])
    else show_debugger( 'failed on [ ' + inttostr( t ) + ' @ ]: address out of bounds.' )
  end;
  
  { STORE : (na-) - put nos into ram at tos }
  procedure vm.oSTO;
  begin TN;
    if ( t >= 0 ) and ( t < length( ram )) then ram[ t ] := n
    else show_debugger( 'failed on [ ' + inttostr( t ) + ' ! ]: address out of bounds.' )
  end;

  { -- stack ops ---------------------------------------------- }

  procedure vm.oDUP ; begin data.dup end;
  procedure vm.oDROP; begin data.drop end;
  procedure vm.oSWAP; begin data.swap end;
  procedure vm.oPUSH; begin addr.push( data.pop ) end;
  procedure vm.oPOP ; begin data.push( addr.pop ) end;

  { -- port ops ----------------------------------------------- }

  procedure vm.oIN; { p-n }
  begin t := data.pop;
    data.push( ports[ t ] );
    ports[ t ] := 0;
  end; 
  procedure vm.oOUT ; { np- } begin TN; ports[ t ] := n; end;
  procedure vm.oWAIT; { - } begin runio; end;

  { -- arithmetic --------------------------------------------- }

  procedure vm.oADD ; begin data.push(  data.pop + data.pop ) end;
  procedure vm.oSUB ; begin data.push( -data.pop + data.pop ) end;
  procedure vm.oMUL ; begin data.push(  data.pop * data.pop ) end;
  procedure vm.oDIVM;
  begin
    TN
    data.push( n mod t ); { yep. mod comes first }
    data.push( n div t );
  end;
  procedure vm.oINC ; begin inc( data.cell[ data.sp ] ) end;
  procedure vm.oDEC ; begin dec( data.cell[ data.sp ] ) end;

  { -- logic -------------------------------------------------- }

  procedure vm.oAND ; begin data.push( data.pop AND data.pop ) end;
  procedure vm.oOR  ; begin data.push( data.pop OR data.pop ) end;
  procedure vm.oXOR ; begin data.push( data.pop XOR data.pop ) end;
  procedure vm.oSHL ; begin TN; data.push( n shl t ); end;
  { shr in retro preserves the sign }
  procedure vm.oSHR ; begin TN; data.push( sarlongint( n, t )); end;


  { -- jump and conditional jumps ----------------------------- }

  procedure jump_and_roll( var v : vm; dest : int32 );
  begin
    v.ip := dest - 1; { compensate for ip++ }
    while ( v.ip < length( v.ram ) - 1 )
      and ( v.ram[ v.ip + 1 ] = 0 ) do inc( v.ip ); { skip over no-ops }
  end;
  
  procedure vm.oJMP; begin jump_and_roll( self, ram[ ip + 1 ])  end;
  procedure vm.oJLT; begin TN if t <  n then oJMP else inc( ip ) end;
  procedure vm.oJGT; begin TN if t >  n then oJMP else inc( ip ) end;
  procedure vm.oJNE; begin TN if t <> n then oJMP else inc( ip ) end;
  procedure vm.oJEQ; begin TN if t =  n then oJMP else inc( ip ) end;
  
  { invoke / return }
  procedure vm.oIVK; begin addr.push( ip ); jump_and_roll( self, ram[ ip ]); end;
  procedure vm.oRET; begin ip := addr.pop end;
  procedure vm.oLOOP;
  begin
    dec( data.cell[ data.sp ] );
    if data.cell[ data.sp ] > 0 then
      jump_and_roll( self, ram[ ip + 1 ])
    else begin
      inc( ip );
      data.pop;
    end
  end;

  { zex : exit (return) if TOS = 0 ( sort of like ~assert~ ) }
  procedure vm.oZEX;
  begin
    if data.cell[ data.sp ] = 0 then begin
      data.pop;
      ip := addr.pop;
    end
  end;

  { -- jump and conditional jumps ----------------------------- }

  procedure vm.oDEBUG;
  begin
    writeln( '<< press enter >>' );
    readln;
  end;

  procedure vm.init_optable;
    
    procedure addop(	 
		id	 : int32;
		go	 : thunk;
		tok, sig : token;
		hasarg	 : boolean );
      var rec : oprec;
    begin
      rec.go  := go;
      rec.tok := tok;
      rec.sig := sig;
      rec.hasarg := hasarg;
      self.optbl[ id ] := rec;
    end; { addop }

    
    const _ = false; X = true;
  begin
    setlength( self.optbl, 31 );
    addop( 00, @oNOP , 'nop' , '  -  ', _ );
    addop( 01, @oLIT , 'lit' , '  -  ', X );
    addop( 02, @oDUP , 'dup' , ' n-nn', _ );
    addop( 03, @oDROP, 'drop', ' n-  ', _ );
    addop( 04, @oSWAP, 'swap', 'xy-yx', _ );
    addop( 05, @oPUSH, 'push', ' n-^ ', _ );
    addop( 06, @oPOP , 'pop' , ' ^-n ', _ );
    addop( 07, @oLOOP, 'loop', '  -  ', _ );
    addop( 08, @oJMP , 'jmp' , '  -  ', X );
    addop( 09, @oRET , 'ret' , '  -  ', _ );
    addop( 10, @oJLT , 'jlt' , '  -  ', X );
    addop( 11, @oJGT , 'jgt' , '  -  ', X );
    addop( 12, @oJNE , 'jne' , '  -  ', X );
    addop( 13, @oJEQ , 'jeq' , '  -  ', X );
    addop( 14, @oLOD , '@'   , ' a-n ', _ );
    addop( 15, @oSTO , '!'   , 'na-  ', _ );
    addop( 16, @oADD , '+'   , ' y-n ', _ );
    addop( 17, @oSUB , '-'   , 'xy-n ', _ );
    addop( 18, @oMUL , '*'   , 'xy-n ', _ );
    addop( 19, @oDIVM, '/mod', 'xy-rq', _ );
    addop( 20, @oAND , 'and' , 'xy-n ', _ );
    addop( 21, @oOR  , 'or'  , 'xy-n ', _ );
    addop( 22, @oXOR , 'xor' , 'xy-n ', _ );
    addop( 23, @oSHL , '<<'  , 'xy-n ', _ );
    addop( 24, @oSHR , '>>'  , 'xy-n ', _ );
    addop( 25, @oZEX , 'zex' , '  -  ', _ );
    addop( 26, @oINC , '1+'  , ' n-n ', _ );
    addop( 27, @oDEC , '1-'  , ' n-n ', _ );
    addop( 28, @oIN  , 'in'  , ' p-n ', _ );
    addop( 29, @oOUT , 'out' , 'np-  ', _ );
    addop( 30, @oWAIT, 'wait', '  -  ', _ );
    setlength( self.optbl, 32 );
    addop( 31, @oDEBUG, '~?~', '  -  ', _ );
  end; { init_optable }

{$IFDEF NESTUNITS}
end.				   
{$ENDIF}
