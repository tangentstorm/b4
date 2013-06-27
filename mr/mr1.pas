
type

  TField   = class
    function ToString( row : trowdata ;  len : cardinal ) : AnsiString; override;
  end;

  OStream  = class
    procedure emit (byte); //inc( wp )
  end;

  TFormat  = ( tfInteger, tfNatural, tfText, tfData, tfEnum );
  TColDef  = class
  end;

  {
    Database rows are a lot like classes, but instead of one instance
    at a time, you have 0..n instances all in the same place.

    If you have 1,000 objects, then you will have 1,000 VMT pointers.

    With a relation, you only have one such pointer.

    
  }

  TVal	   = class
    def	 : TColDef;
    row	 : TRow;
    size : cardinal;
    property bytes[ i : cardinal ] : byte; default;
  end;

  TVals	   = array of TVal;
  TRelDef  = array of TColDef;
  TRowData = array of bytes;
  TRelData = array of TRowData;
  TRel	   = class
    defaults : TRow;
    cols     : TColDefs;
    rows     : TRelData;
  end;

function rel.NewRow : TRow;
  var col : TVal; b : byte = 0;
  begin
    setlength( result, numcols );
    for col in defaults do
      for b in cols[ col ].bytes do
        begin
	  emit( b ); // inc( wp )
	end
  end;
