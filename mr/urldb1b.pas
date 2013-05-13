
program urldb1b;
uses strutils;
  
  const
    kRecSize = 68; 
    kLenPos  = 4; // position of url length byte (numbering from 0)
    kTab     = #9;
  var
    f : file;                      // untyped file
    i : byte;                      // loop counters
    a : array of byte;

  type
    TKeyType = ( ktString, ktCardinal, ktPadding );
    TKeyDef  = record
		 keyName : string[31];
		 keyType : TKeyType;
		 keySize : cardinal;
	       end;
  var
    offs : byte = 0;                      // current offset into the record
    key	 : TKeyDef;
    keys : array [0..3] of TKeyDef =
	   (( keyName: 'id';    keyType : ktCardinal; keySize : 4 ),
	    ( keyName: 'url';   keyType : ktString;   keySize: 37 ),
	    ( keyName: 'title'; keyType : ktString;   keySize: 25 ),
	    ( keyName: '';      keyType : ktPadding;  keySize:  2 ));

begin
  SetLength( a, kRecSize );        // allocate 68 bytes for the array
  Assign( f, 'urldb0.db' );        // same file from last lesson
  Reset( f, kRecSize );            // untyped files need to know the size

  { show the headlines }
  for key in keys do with key do begin
    case keyType of
      ktCardinal : write( PadLeft( keyname, 5 ));
      ktString	 : write( PadRight( keyname, keySize ));
      otherwise  
    end;
    write( kTab );
  end;
  writeln;

  { show the separator line }
  for key in keys do with key do begin
    case keyType of
      ktCardinal : write( '-----');
      ktString	 : write( AddChar( '-', '', keySize ));
      otherwise	 
    end;
    write( kTab );
  end;
  writeln;

  { for each tuple in the relation ... }
  while not eof(f) do
    begin
      BlockRead( f, a[0], 1 );     // load 1*kRecSize bytes from f into a

      { show the tuple on a line }
      offs := 0;
      for key in keys do with key do begin
	case keyType of
	  ktCardinal : begin
			 write(cardinal( a[offs]) : 5)
		       end;
	  ktString   : begin
			 for i := 1 to a[offs] do write(chr(a[offs + i]));
			 for i := i to keySize do write(' ');
		       end;
	  else // do nothing
	end;
	write( kTab );
	inc(offs, keySize);
      end;
      writeln;
    end

end.
