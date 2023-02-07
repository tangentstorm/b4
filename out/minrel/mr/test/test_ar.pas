
{$mode delphi}
{$i test_ar.def}
implementation
uses ar, fs, sysutils;


type
  ICardinalArray = IArray<cardinal>;

procedure check_array( a : ICardinalArray );
  var i : byte;
  begin

    { test that we can grow the array with append: }
    chk.equal( 0, a.length );
    for i := 0 to 127 do a.append( i * i );

    chk.equal( 128, a.length );

    { now resize and make sure we can both read the
      old values and write the new slots }
    a.resize( 256 );
    for i := 255 downto 128 do
      begin
        { writeln( 'a[', i, '] := a[', 256 - i - 1, '] -> ',
          a[ 256 - i - 1 ], ' -> ', i, ' ',
          round(sqrt( a[ 256 - i - 1 ]))); }
        a[ i ] := round(sqrt( a[ 256 - i - 1 ]));
      end;

    { do a few spot tests to make sure it worked right. }
    chk.equal( a[   0 ],     0 );
    chk.equal( a[  64 ],  4096 );
    chk.equal( a[ 126 ], 15876 );
    chk.equal( a[ 127 ], 16129 );
    chk.equal( a[ 128 ],   127 );
    chk.equal( a[ 129 ],   126 );
    chk.equal( a[ 192 ],    63 );
    chk.equal( a[ 255 ],     0 );
  end;

type
  TDynArray      =  class (GDynArray<cardinal>, ICardinalArray);
  TFileArray     =  GFileArray<cardinal>;
  TBPlusArray    =  GBPlusArray<cardinal>;
  TEmbeddedArray =  GEmbeddedArray<cardinal>;

procedure test_dynarray;
  begin
    check_array( TDynArray.Create( 0 ));
  end;

procedure test_filearray;
  var f : file of cardinal;
  begin
    fs.update( f, 'test_ar.b4sd' );
    check_array( TFileArray.Create( f ));
  end;

procedure test_bplusarray;
  begin
    check_array( TBPlusArray.Create( 0 ));
  end;

procedure test_embeddedarray;
  begin
    check_array( TEmbeddedArray.Create( TDynArray.Create( 1024 ), 32 ));
  end;

begin
end.
