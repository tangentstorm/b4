{ This draws a little byte-oriented hex editor.
  The colors represent the various types of
  characters in utf-8 }

{$i xpc.inc }
program hx;
  uses xpc, kvm;

  procedure colorize( b	:  byte );
  begin
    case b of
      $00        : kvm.fg( 8 );
      $01 .. $1F ,
             $7F : kvm.fg( 4 );
      $20 .. $7E : kvm.fg( 7 );
      $80 .. $BF : kvm.fg( 2 );
      $C0 .. $C1 : kvm.fg( 1 );
      $C2 .. $DF : kvm.fg( 3 );
      $E0 .. $EF : kvm.fg( 6 );
      $F0 .. $F5 : kvm.fg( 5 );
      $F6 .. $FF : kvm.fg( 1 );
    end
  end; { colorize }
  
  var x, y, i : byte;
  const hexdig = '0123456789ABCDEF';
begin
  kvm.clrscr;
  for y := 0 to 15 do
  begin
    for x := 0 to 15 do
    begin
      colorize( byte( y * 16 + x ));
      write( hexdig[ y  + 1 ],
	     hexdig[ x  + 1 ]);
      if x = 15 then writeln else write( ' ' );
    end
  end
end.