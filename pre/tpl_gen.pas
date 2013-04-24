{ templang: a lightweight template language using xpath }
{$i xpc.inc }
unit tpl_gen;
interface uses dom, xpath;

type
  TXPathExpr = dom.DOMString;
  TOutContext = dom.TDOMNode;

{-- abstract interface / parent classes --}

  IOutput = interface
    procedure Render( ctx : TOutContext; var f : text );
  end;
  
  IBlock = interface( IOutput )
    function Append( child : IOutput ) : IBlock;
  end;

  TOutput = class( TInterfacedObject, IOutput )
    expr : TXPathExpr;
    constructor Create;
    constructor Create( ex : TXPathExpr );
    procedure Render( ctx : TOutContext; var f : text ); virtual; abstract;
  end;

{-- concrete classes --}

  TString = class( TOutput ) { for literal strings in the template }
    procedure Render( ctx : TOutContext; var f : text ); override;
  end;

  TExpr = class( TOutput ) { for rendering xpath scalars }
    procedure Render( ctx : TOutContext; var f : text ); override;
  end;

  TBlock = class( TOutput, IBlock )
    children : array of IOutput;
    function Append( child : IOutput ) : IBlock;
    procedure Render( ctx : TOutContext; var f : text ); override;
  end;

  TLoop = class( TBlock ) { for looping through nodesets }
    procedure Render( ctx : TOutContext; var f : text ); override;
  end;

  TWhen = class( TBlock ) { for if/then/case constructs }
    procedure Render( ctx : TOutContext; var f : text ); override;
  end;

implementation

  {-- scalars --}

  constructor TOutput.Create( ex : TXPathExpr );
  begin
    self.expr := ex;
  end;

  constructor TOutput.Create;
  begin
    self.expr := '';
  end;

  procedure TString.Render( ctx : TOutContext; var f : text );
  begin
    write( f, self.expr )
  end;

  procedure TExpr.Render( ctx : TOutContext; var f : text );
  begin
    write( f, EvaluateXPathExpression( self.expr, ctx ).AsText );
  end;

  {-- blocks --}

  function TBlock.Append( child : IOutput ) : IBlock;
    var i : integer;
  begin
    i := length( children );
    setlength( children, i + 1 );
    children[ i ] := child;
    result := self;
  end;

  procedure TBlock.Render( ctx : TOutContext; var f : text );
    var child : IOutput;
  begin
    for child in self.children do child.Render( ctx, f )
  end;

  procedure TLoop.Render( ctx : TOutContext; var f : text );
    var item : pointer; child : IOutput;
  begin
    for item in EvaluateXPathExpression( self.expr, ctx ).AsNodeSet do
      inherited Render(TDOMNode( item ), f );
  end;

  procedure TWhen.Render( ctx : TOutContext; var f : text );
    var child : IOutput;
  begin
    if EvaluateXPathExpression( self.expr, ctx ).AsBoolean then
      inherited Render( ctx, f )
  end;

begin
end.
