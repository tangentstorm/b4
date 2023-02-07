unit grinlex;
interface

type

  TGrinKind = ( tkTokenSigil, tkRuleSigil, tkComment, tkNeck, tkEnd
	        tkChar, tkEnd, tkTo, tkOr, TkString, tkIden,
	        tkLParen, tkRparen, tkPlus, tkStar,
	        tkMaybe, tkSplice, tkType, tkLBrack, tkRBrack,
                tkLBrace, tkRBrace, tHoist, tkDrop, tkAction, 
	        { whitespace doesn't matter for parsing, but 
		  provides layout examples for code gen }
	        tkSpace, tkIndent, tkDedent, tkLine, tkMerge )

  TGrinToken = class
    line, col : cardinal;
    kind      : TGrinKind;
    data      : string;
  end;
  
  TGrinLexer = class
    ( tok : TGrinToken ) : TGrinToken;
    procedure Next( out tok : TGrinToken ) : TGrinToken;
  private
    function Next( _ch :  char ) : char;
    tx : text;
    ch : char;
  end;			 
  
implementation

procedure TGrinLexer.Next( out tok : TGrinToken );
  begin
    if NextChar( ch ) = ';' then on_comment
    else if state = main_state then
      case ch of
         '%' : emit( tkToken );
         ':' : ;
      end
    else if in_token or in_rule then
      case ch of
         '%' : emit( tkToken );
         ^J  : inc(lineNum);
         ^L  : inc(pageNum);
         '.' : ; // range if '.' else end rule
         '|' : ; // alt
         '(' : ; // group
         '{' : ; // narrative
         '[' : ; // forth code
         '?' : ; // 0..1
         '*' : ; // 0..*
         '+' : ; // 1..*
      else if in_rule then
        case ch of
           '^' : ; // hoist
           '!' : ; // [ drop ]
           '"', '''' : // string/char
        else
          { rule name}
        end
      end
  end;

end.
