
{ NOTE : this file is generated from ../web/bp.pas.org , so...
  --->> DON'T EDIT THIS FILE! <<--- }
{ B+ Trees for pascal }
{$mode delphi} {$i xpc.inc} {$H+}
unit bp;
interface uses xpc, sysutils;

  const empty = maxint;

  type
    TKey   = integer;
    TVal   = TObject;
    IBPlus = interface
      procedure put( key : TKey; val : TVal );
      //procedure del( key : TKey );
      //function get( key : TKey ): TVal;
      //function has( key : Tkey ): boolean;
    end;
    TTree  = class; // forward reference for TIter
    
    TNode = class ( TInterfacedObject, IBPlus )
     public
      constructor CreateLeaf( size : integer );
      constructor CreateInner( size : integer );
      procedure put( key : TKey; val : TVal );
      //procedure del( key : TKey );
      //function get( key : TKey ): TVal;
      //function has( key : Tkey ): boolean;
      function tostring: string; override;
     private
      isleaf      : boolean;
      _parent   : TNode;
      _next     : TNode;
      keys : array of TKey;
      vals : array of TObject;
      function locate( key : TKey ): TNode;
      function inskey( key : TKey ): cardinal;
      function full : boolean;
      procedure split;
      procedure rebalance;
      function firstkey : TKey;
      function isroot : boolean;
      procedure addchild( key : TKey; kid : TNode );
      function next : TNode;
      function findslot( key : TKey ): word;
      function findroot : TNode;
      function isinner : boolean;
    end;
    
    TIter = class
    private
      tree : TTree; // only useful to support Reset
      node : TNode;
      step : integer;  // index/offset within the node
    public
      constructor Create( aTree : TTree );
      function GetCurrent : TVal;
      function MoveNext : Boolean;
      procedure Reset;
      property Current:TVal read GetCurrent;
    end;
    TTree = class
     public
      constructor Create( branchfactor : integer = 64 );
      procedure put( key : TKey; val : TVal );
      //procedure del( key : TKey );
      //function get( key : TKey ): TVal;
      //function has( key : Tkey ): boolean;
      function GetEnumerator: TIter;
      function tostring: string; override;
     private
      root : TNode;
    end;

implementation
  constructor TNode.CreateLeaf( size : integer );
    var i : integer;
  begin
    isleaf := true;
    _next  := nil;
    _parent := nil;
    SetLength( keys, size + 1 );
    SetLength( vals, size + 1 ); // extra link
    for i := 0 to size do
    begin
      keys[i] := empty;
      vals[i] := nil;
    end;
  end;
  
  constructor TNode.CreateInner( size : integer );
  begin
    self.CreateLeaf(size);
    self.isleaf := false;
  end;
  
  
  { Find first child key less the new key. It is important that
    both =locate= and =inskey= use the same algorithm for locating
    the key, or the linked list at the bottom of the tree will break.
  
    Further, we will make sure that we walk left to right (lowest
    key to highest) because in inner nodes, the links always point to
    the first child.
  
    Example:
  
        tree: [ 5:[ 5 _ _ _ ] 8:[ 8 12 _ _ ] 50:[ 50 _ _ _ ] _ ]
        goal: find slot for 24
  
    Here, 24 is both "after 8" or "before 50". It's important to check
    both numbers, and then choose the /leftmost/ side of the range.
    (If we had chosen to use the highest value for the child keys instead
    of the lowest, then of course we would choose the rightmost child.) }
  
  function TNode.FindSlot( key : TKey ) : word;
  begin
    result := 0;
    if key >= keys[ 0 ] then
      repeat
        inc(result)
      until (result = high(keys)) or (key < keys[ result ])
  end;
  
  { This search routine always succeeds, since it finds the location
    where the key /should/ be in the tree, whether it's /actually/
    there or not. }
  function TNode.locate( key : TKey ) : TNode;
  var slot : integer;
  begin
    if isleaf
      then result := self
      else begin
        slot := findslot(key);
        if slot > 0 then dec( slot ); // keys[slot] = empty then dec( slot );
        result := (vals[slot] as TNode).locate( key );
      end
  end;
  
  procedure TNode.put( key : TKey; val : TVal );
  begin
    if isleaf then
      begin
        vals[inskey( key )] := val;
        rebalance;
      end
    else locate( key ).put( key, val )
  end;
  { insert key }
  function TNode.inskey( key : TKey ) : cardinal;
   var i, slot : integer;
  begin
    slot := findslot( key );
  
    // if first value is about to change, update the parent key:
    if (slot = 0) and not isroot then
    begin
      i := 0;
      while _parent.keys[i] <> keys[0] do inc(i);
      _parent.keys[i] := key;
    end;
  
    for i := high(keys) downto slot+1 do
    begin
      vals[i] := vals[i-1];
      keys[i] := keys[i-1];
    end;
  
    { finally, fill the hole we just made, and return its location }
    keys[ slot ] := key;
    vals[ slot ] := nil;
    result := slot
  end;
  procedure TNode.rebalance;
  begin
    if full then split
  end;
  function TNode.full : boolean;
  begin
    result := keys[high(keys)] <> empty;
  end;
  procedure TNode.split;
    var newnode: TNode;
    procedure copyhalf;
      var i, half : integer;
    begin
      half := length(keys) div 2;
      for i := half to high(keys) do
      begin
        newnode.keys[i-half] := keys[i];
        newnode.vals[i-half] := vals[i];
        if (newnode.isinner) and (vals[i] <> nil) then
          (vals[i] as TNode)._parent := newnode;
        keys[i] := empty;
        vals[i] := nil;
      end;
    end;
  begin
    if isleaf then
      begin
        newnode := TNode.CreateLeaf( length(keys ));
        newnode._next := _next;
        _next := newnode;
      end
    else newnode := TNode.CreateInner( length(keys ));
    copyhalf;
    if isroot then begin
      _parent := TNode.CreateInner( length( keys ));
      _parent.addchild( self.firstkey, self )
    end;
    _parent.addchild( newnode.firstkey, newnode )
  end;
  function TNode.isroot : boolean;
  begin
    result := not assigned(_parent);
  end;
  function TNode.isinner : boolean;
  begin
    result := not isleaf
  end;
  function TNode.firstkey : TKey;
  begin
    result := keys[0]
  end;
  procedure TNode.addchild( key : TKey; kid : TNode );
  begin
    assert( not isleaf );
    vals[inskey( key )] := kid;
    kid._parent := self;
    rebalance;
  end;
  function TNode.next : TNode;
  begin
    result := _next;
  end;
  function TNode.findroot : TNode;
  begin
    if isroot
      then result := self
      else result := _parent.findroot
  end;
  function TTree.tostring: string;
  begin
    result := self.root.tostring;
  end;
  
  var gIndent : string = '';
  function TNode.ToString : string;
    var s : string; i : integer;
  begin
  
    s := '';
  
    // draw the keys
    if isleaf then s += '['  else s += '{';
  
    for i := low(keys) to high(keys) do
      if (i = 0) and (keys[i] = empty) then s := s + ' < '
      else if (self.keys[i] = empty) then s := s + ' - '
      else s := s + ' ' + IntToStr(self.keys[ i ]) + ' ';
  
    if isleaf and assigned(_next) then
      s := s + ' -> ' + IntToStr(_next.firstkey);
  
    if isleaf then s += ' ]' else s += ' }';
  
    // draw the values
    gIndent += '  ';
    for i := low(keys) to high(keys) do
      if assigned(self.vals[ i ]) then
         s := s + lineending + gIndent + self.vals[ i ].ToString;
    setlength(gIndent, length(gIndent)-2);
    result := s;
  end;
  
  constructor TTree.Create( branchfactor : integer = 64 );
  begin
    root := TNode.CreateLeaf( branchfactor );
  end;
  
  procedure TTree.put( key : TKey; val : TVal );
  begin
    root.put( key, val );
    // find the new root
    root := root.findroot; // "for the home team"
  end;
  
  function TTree.GetEnumerator : TIter;
  begin
    result := TIter.Create( self )
  end;
  
  constructor TIter.Create( aTree : TTree );
  begin
    self.tree := aTree;
    self.Reset;
  end;
  procedure TIter.Reset;
  begin
    // this should walk down to the first (leftmost) leaf node
    node := self.tree.root.locate(-maxint);
    step := -1;
  end;
  
  function TIter.GetCurrent : TObject;
  begin
    result := node.vals[step];
  end;
  function TIter.MoveNext : Boolean;
  begin
    result := false;
    if assigned(node) then begin
      inc(step);
      if (step > high(node.keys))
      or (node.keys[step] = empty)
      then begin
        node := node.next;
        step := 0;
      end;
      result := assigned(node);
    end
  end;
end.
