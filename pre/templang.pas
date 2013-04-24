{ templang: a lightweight template language using xpath }
{$i xpc.inc }
program templang;
uses dom;

type

  TExpr = dom.DOMString;
  TOutContext = dom.TDOMNode;

  IOutput = interface
    procedure Render( ctx : TOutContext );
  end;
  
  TOutput = class( TInterfacedObject, IOutput )
    data : dom.DOMString;
    constructor Create( s : TExpr );
    procedure Render( ctx : TOutContext ); virtual; abstract;
  end;

  TString = class( TOutput )
    procedure Render( ctx : TOutContext ); override;
  end;

  TLoop = class( TOutput )
    procedure Render( ctx : TOutContext ); override;
  end;

  TWhen = class( TOutput )
    procedure Render( ctx : TOutContext ); override;
  end;

  constructor TOutput.Create( s : TExpr );
  begin
    self.data := s;
  end;

  procedure TString.Render( ctx : TOutContext );
  begin
    write( self.data )
  end;

  procedure TLoop.Render( ctx : TOutContext );
  begin
    write( self.data ) { TODO : loop through nodeset }
  end;

  procedure TWhen.Render( ctx : TOutContext );
  begin
    write( self.data ) { TODO : xpath booleans }
  end;

begin
end.
