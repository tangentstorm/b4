{ templang: a lightweight template language using xpath }
{$i xpc.inc }
program templang;
uses dom, xpath;

type
  TXPathExpr = dom.DOMString;
  TOutContext = dom.TDOMNode;

{-- abstract interface / parent classes --}

  IOutput = interface
    procedure Render( ctx : TOutContext );
  end;

  TOutput = class( TInterfacedObject, IOutput )
    expr : TXPathExpr;
    constructor Create( ex : TXPathExpr );
    procedure Render( ctx : TOutContext ); virtual; abstract;
  end;

{-- concrete classes --}

  TString = class( TOutput ) { for literal strings in the template }
    procedure Render( ctx : TOutContext ); override;
  end;

  TExpr = class( TOutput ) { for rendering xpath scalars }
    procedure Render( ctx : TOutContext ); override;
  end;

  TOutBlock = class( TOutput )
    children : array of IOutput;
    procedure Append( child : IOutput );
    procedure Render( ctx : TOutContext ); override;
  end;

  TLoop = class( TOutBlock ) { for looping through nodesets }
    procedure Render( ctx : TOutContext ); override;
  end;

  TWhen = class( TOutBlock ) { for if/then/case constructs }
    procedure Render( ctx : TOutContext ); override;
  end;

{-- implementation --}

  {-- scalars --}

  constructor TOutput.Create( ex : TXPathExpr );
  begin
    self.expr := ex;
  end;

  procedure TString.Render( ctx : TOutContext );
  begin
    write( self.expr )
  end;

  procedure TExpr.Render( ctx : TOutContext );
  begin
    write( EvaluateXPathExpression( self.expr, ctx ).AsText );
  end;

  {-- blocks --}

  procedure TOutBlock.Append( child : IOutput );
    var i : integer;
  begin
    i := length( children );
    setlength( children, i + 1 );
    children[ i ] := child;
  end;

  procedure TOutBlock.Render( ctx : TOutContext );
    var child : IOutput;
  begin
    for child in self.children do child.Render( ctx )
  end;

  procedure TLoop.Render( ctx : TOutContext );
    var item : pointer; child : IOutput;
  begin
    for item in EvaluateXPathExpression( self.expr, ctx ).AsNodeSet do
      inherited Render(TDOMNode( item ));
  end;

  procedure TWhen.Render( ctx : TOutContext );
    var child : IOutput;
  begin
    if EvaluateXPathExpression( self.expr, ctx ).AsBoolean then
      inherited Render( ctx )
  end;

begin
end.
