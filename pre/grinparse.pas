unit grinparse;
interface uses grinlex;

type
  
  TGrammar = class
  end;
  
{  
  TNode	= class
    _children : array of TNode;
    _token    : TToken;
    constructor Create( token : TToken );
    procedure AddChild( node : TNode );
    function GetText : String;
    destructor Destroy; override;
    property text : String read GetText;
  end;
}
  
  TParser = class
    _scanner : IScanner;
    _root    : TNode;
    _last    : TNode;
    constructor Create;
  public
    function Next : TNode;
    property last : TNode read _last;
    property root : TNode read _root;
  end;
  
implementation

end.
