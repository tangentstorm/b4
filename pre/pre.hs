-- primitive string matching functions:
type Matcher a = Text -> IO ( Maybe a )
     -- Matcher is the actual imperative program type.
     -- i.e., in pascal:
     --    type matcher = function ( txt : Text  ) : boolean;
     -- There is one corresponding to each of the rules below:


-- regular expression constructors for nonrecursive patterns
data Exp a            -- a = underlying alphabet ( chars, tokens, etc )
     -- Technically, these are fragments. You need to sequence
     -- these to actually match regular grammars. That feature
     -- is part of the BNF notation below.
     = Nul            -- the empty string
     | Sym a          -- exact match on one symbol/char/token
     | Any [a]        -- any of a set. eg: "vowels", '0'..'9'
     | Lit [a]        -- a sequence / string literal
     | Mul a          -- 0..N copies of one repeating symbol


-- recursive structures match context free grammars
data Rec a
     | Reg (Exp a)  -- build on the plain regexp stuff
     | Seq [Rec a]  -- sequence / regexp "(...)"
     | Alt [Rec a]  -- alternate / regexp "|" / bnf ";"
     | Rep (Rec a)  -- repetition / regexp "*" / ebnf "{}"
     | Sub Str a    -- Reference a named rule


-- BNF provides a structure for such a thing
type BNF a = [( Str , [ Rec a ])]
     -- "or"-ing can be done by defining multiple
     -- rules with the same name in BNF syntax, hence
     -- the nested list. Semantically, it's the same
     -- as Alt, but this lets us more closely represent
     -- actual BNF syntax if we want.


-- EBNF style grammar ( adds syntactic sugar )
data Gram a
     -- These are Wirth's extensions, except for plus, which
     -- he doesn't use. But, the names I use are from ANTLR,
     -- because I find its syntax easier to understand, and
     -- more in line with common regexp syntax.
     = Rec_ (Rec a)
     | Star (Gram a) -- antlr: g* after kleene star. ebnf: { a }
     | Plus (Gram a) -- antlr: g+ after kleene plus. ebnf: a { a }
     | Grup (Gram a) -- antlr: ( g ) like everywhere else.
     | Ques (Gram a) -- antlr: g? 0..1 like perl. ebnf: [ a ] bnf: a | nul



-- ANTLR-style transformation rules ( for abstract syntax trees )
data Rule a
     -- Abstract syntax trees discard irrelevant details
     -- like punctuation and secordary keywords. for example:
     --
     --    WHILE cond DO task; ->  (WHILE cond task)
     --
     -- ANTLR's syntax makes it very convenient to describe
     -- these transformations directly in the grammar.
     --
     = Rule Gram (Rule a)
            -- TODO : I'm not 100% sure haskell allows the above line.
            -- ( it's not installed on this machine so will test later )
            -- The idea is that these annotations can appear at various
            -- places in the meta-syntax, and so they need to be able
	    -- to wrap any of the previous structures.
	    --
	    -- Actually, this is far more liberal than the subset of
	    -- Antlr's syntax that it models, and allows creating
	    -- valid structures in haskell that make no sense in antlr,
	    -- but it does allow us to keep these transformation rules
	    -- completely separate up until this point.
