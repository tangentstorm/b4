
program urldb1b;
uses strutils;

  const
    kRecSize = 68;
    kLenPos  = 4;  // position of url length byte (numbering from 0)
    kSep     = #9; // tab character
  var
    f : file;                      // untyped file
    i : byte;                      // loop counters
    a : array of byte;

  type
    TKeyType = ( ktPadding, ktCardinal, ktString );
    TKeyDef  = record
		 keyName : string[31];
		 keyType : TKeyType;
		 keySize : cardinal;
	       end;
  const
    keys : array [0..3] of TKeyDef =
	   (( keyName: 'id';    keyType : ktCardinal; keySize : 4 ),
	    ( keyName: 'url';   keyType : ktString;   keySize: 37 ),
	    ( keyName: 'title'; keyType : ktString;   keySize: 25 ),
	    ( keyName: '';      keyType : ktPadding;  keySize:  2 ));

  procedure WriteTuple( a : array of byte );
    var
      offs : byte = 0;                      // current offset into the record
      key  : TKeyDef;
    begin
      for key in keys do with key do begin
	case keyType of
	  ktCardinal : write(cardinal( a[offs]) : 5);
	  ktString   : begin
			 for i := 1 to a[offs] do write(chr(a[offs + i]));
			 for i := i to keySize do write(' ');
		       end;
	  // otherwise do nothing
	end;
	write( kSep );
	inc(offs, keySize);
      end;
      writeln;
    end;

  procedure WriteHeader;
    var key : TKeyDef;
    begin
      { write the the field names }
      for key in keys do with key do begin
	case keyType of
	  ktCardinal : write( PadLeft( keyname, 5 ));
	  ktString   : write( PadRight( keyname, keySize ));
	end;
	write( kSep );
      end;
      writeln;
      { show the separator line }
      for key in keys do with key do begin
	case keyType of
	  ktCardinal : write( '-----');
	  ktString   : write( AddChar( '-', '', keySize ));
	end;
	write( kSep );
      end;
      writeln;
    end;

begin
  SetLength( a, kRecSize );        // allocate 68 bytes for the array
  Assign( f, 'urldb0.db' );        // same file from last lesson
  Reset( f, kRecSize );            // untyped files need to know the size

  WriteHeader;
  while not eof(f) do
    begin
      BlockRead( f, a[0], 1 );     // load 1*kRecSize bytes from f into a
      WriteTuple(a);
    end;

  Close(f);
  a := Nil;                        // free allocated ram.
end.
