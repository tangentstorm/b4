// Mockup of UTF-8 encoding/decoding system.
// My main interest is exploring the UTF-8 encoding system
// as compression for instructions in retropascal.
unit utf8;
implementation

  procedure control; begin end;
  procedure ascii;   begin end;
  procedure invalid; begin end;
  procedure offset;  begin end;
  procedure read2;   begin end;
  procedure read3;   begin end;
  procedure read4;   begin end;

  procedure decode;
  begin
    case code of
      $00 .. $1F : control;
      $20 .. $7F : ascii;
      $80 .. $BF : offset;
      $C0 .. $C1 : invalid;
      $C2 .. $DF : read2;
      $E0 .. $EF : read3;
      $F0 .. $F7 : read4;
      else invalid;
    end
  end; { decode }

  procedure encode( value : int32 );
  begin
    case value of
      $00 .. $1F : emit2( $c0, value );
      $20 .. $7F : emit1( value );
      $80 .. $BF : emit2( $c2, value );
    end;
  end; { encode }

begin
end.
