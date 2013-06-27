
{$i xpc.inc}
program bpdemo;
uses bp, strutils, sysutils;

  const kMax = 255; // 65535;  { change if you want to test speed }
  type
    TTriple = class
      sub, rel, obj : integer;
      constructor Create( SubId, RelId, ObjId : integer );
      procedure Print;
      function tostring: string; override;
      // function reversed : IEnumerator;
    end;

  constructor TTriple.Create( SubId, RelId, ObjId : integer );
  begin
    sub := subid;
    rel := relid;
    obj := objid;
  end;


  var building : boolean = true;
  function TTriple.ToString : string;
  begin
    result := '('  + PadLeft(IntToStr( sub ), 4) +
              ', ' + PadLeft(IntToStr( rel ), 4) +
              ', ' + PadLeft(IntToStr( obj ), 4) +
              ')';
  end;

  procedure TTriple.print;
  begin
    writeln( self.tostring )
  end;

    var
      subs, rels, objs : bp.TTree;
      trip             : TObject;
      i, j, k, tmp     : cardinal;
      nums             : array [0..2, 0..kMax] of word;
  begin
  
  randomize;
  
  { create three indices for a triplestore }
  subs := bp.TTree.create(16); // just to make the trace interesting
  rels := bp.TTree.create;
  objs := bp.TTree.create;
  
  { generate the numbers 0..kMax in three columns }
  for j := 0 to 2 do for i := 0 to kMax do nums[j][i] := i;
  
  { shuffle the columns independently }
  for j := 0 to 2 do for i := 0 to kMax * 4 do
  begin
    k := random(kMax);
    tmp := nums[j][k];
    nums[j][k] := nums[j][k+1];
    nums[j][k+1] := tmp;
  end;
  
  { initial index: }
  writeln('initial index:');
  writeln(subs.tostring);
  
  { generate and index the random triples }
  for i := 0 to kMax do begin
    trip := TTriple.create(nums[0][i], nums[1][i], nums[2][i]);
    with TTriple(trip) do begin
      { for debugging, show one of the indices being built step by step }
      writeln;
      writeln('adding key:', sub:2 ); //, '-> ', rel:2, ', ',  obj:2 );
  
      subs.put( sub, trip );
      rels.put( rel, trip );
      objs.put( obj, trip );
  
      writeln(subs.tostring);
    end;
  end;
   building := false;
  { print them in order by each index }
  writeln('--subs--');
  for trip in subs do TTriple(trip).print;
  writeln('--rels--');
  for trip in rels do TTriple(trip).print;
  writeln('--objs--');
  for trip in objs do TTriple(trip).print;
end.
