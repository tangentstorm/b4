{ ngaro opcodes }

{$IFDEF NESTUNITS}
unit ng.ops; implementation
{$ENDIF}

{ TN : move data.tos and data.nos into t and n }
var t, n : integer;

{$MACRO ON}
{$DEFINE TN := t :=data.pop; n:=data.pop; }

procedure TNgaroVM.oNOP;
  begin
  end;

{ -- memory ops --------------------------------------------- }

procedure TNgaroVM.oLIT;
  begin
    inc( ip );
    data.push( ram[ ip ])
  end;

procedure TNgaroVM.oLOD;
  begin
    t := data.pop;
    if ( t < length( ram )) then data.push( ram[ t ])
    else show_debugger(
           'failed on [ ' + IntToStr( t ) +
           ' @ ]: address out of bounds.' )
  end;

{ STORE : (na-) - put nos into ram at tos }
procedure TNgaroVM.oSTO;
  begin TN;
    if ( t >= 0 ) and ( t < length( ram )) then ram[ t ] := n
    else show_debugger( 'failed on [ ' + IntTosTr( t ) +
                       ' ! ]: address out of bounds.' )
  end;

{ -- stack ops ---------------------------------------------- }

procedure TNgaroVM.oDUP;
  begin
    data.dup
  end;

procedure TNgaroVM.oDROP;
  begin
    data.drop
  end;

procedure TNgaroVM.oSWAP;
  begin
    data.swap
  end;

procedure TNgaroVM.oPUSH;
  begin
    addr.push( data.pop )
  end;

procedure TNgaroVM.oPOP;
  begin
    data.push( addr.pop )
  end;

{ -- port ops ----------------------------------------------- }

procedure TNgaroVM.oIN; { p-n }
  begin
    t := data.pop;
    data.push( ports[ t ] );
    ports[ t ] := 0;
  end;

procedure TNgaroVM.oOUT ; { np- }
  begin
    TN; ports[ t ] := n;
  end;

procedure TNgaroVM.oWAIT; { - }
  begin
    RunIO;
  end;


{ -- arithmetic --------------------------------------------- }

procedure TNgaroVM.oADD;
  begin
    data.push(  data.pop + data.pop )
  end;

procedure TNgaroVM.oSUB;
  begin
    data.push( -data.pop + data.pop )
  end;

{$PUSH}{$RANGECHECKS OFF}
procedure TNgaroVM.oMUL;
  begin
    data.push(data.pop * data.pop)
  end;
{$POP}

procedure TNgaroVM.oDIVM;
  begin
    TN
    data.push( n mod t ); { yep. mod comes first }
    data.push( n div t );
  end;

procedure TNgaroVM.oINC;
  begin
    inc( data.cells[ data.count ] )
  end;

procedure TNgaroVM.oDEC;
  begin
    dec( data.cells[ data.count ] )
  end;

{ -- logic -------------------------------------------------- }

procedure TNgaroVM.oAND;
  begin
    data.push( data.pop AND data.pop )
  end;

procedure TNgaroVM.oOR;
  begin
    data.push( data.pop OR data.pop )
  end;

procedure TNgaroVM.oXOR;
  begin
    data.push( data.pop XOR data.pop )
  end;

procedure TNgaroVM.oSHL;
  begin
    TN; data.push( n shl t )
  end;

{ shr in retro preserves the sign }
procedure TNgaroVM.oSHR;
  begin
    TN; data.push( sarlongint( n, t ))
  end;

{ -- jump and conditional jumps ----------------------------- }

procedure jump_and_roll( var v : TNgaroVM; dest : int32 );
  begin
    v.ip := dest - 1; { compensate for ip++ }
    while ( v.ip < length( v.ram ) - 1 )
      and ( v.ram[ v.ip + 1 ] = 0 ) do inc( v.ip ); { skip over no-ops }
  end;

procedure TNgaroVM.oJMP;
  begin
    jump_and_roll( self, ram[ ip + 1 ])
  end;

procedure TNgaroVM.oJLT;
  begin
    TN if t <  n then oJMP else inc( ip )
  end;

procedure TNgaroVM.oJGT;
  begin
    TN if t >  n then oJMP else inc( ip )
  end;

procedure TNgaroVM.oJNE;
  begin
    TN if t <> n then oJMP else inc( ip )
  end;

procedure TNgaroVM.oJEQ;
  begin
    TN if t =  n then oJMP else inc( ip )
  end;

{ invoke / return }
procedure TNgaroVM.oIVK;
  begin
    addr.push( ip );
    jump_and_roll( self, ram[ ip ]);
  end;

procedure TNgaroVM.oRET;
  begin
    ip := addr.pop
  end;

procedure TNgaroVM.oLOOP;
  begin
    dec( data.cells[ data.count ] );
    if data.cells[ data.count ] > 0 then
      jump_and_roll( self, ram[ ip + 1 ])
    else
      begin
        inc( ip );
        data.pop;
      end
  end;

{ zex : exit (return) if TOS = 0 ( sort of like ~assert~ ) }
procedure TNgaroVM.oZEX;
  begin
    if data.cells[ data.count ] = 0 then
      begin
        data.pop;
        ip := addr.pop;
      end
  end;

  { -- jump and conditional jumps ----------------------------- }

procedure TNgaroVM.oDEBUG;
  var ok : boolean = false;
  begin
    writeln;
    writeln( '<< press enter to continue, or type "q" to quit >>' );
    repeat
      case kbd.readkey of
        'q' : halt;
        #13 : ok := true;
      end
    until ok
  end;


procedure TNgaroVM.init_optable;

    procedure addop(
                id       : int32;
                go       : thunk;
                tok, sig : token;
                hasarg   : boolean );
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
    SetLength( self.optbl, 31 );
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
