# unfinished picolisp implementation of pre/gramco ( TODO )
# pre : pattern recognition engine

(load "/usr/home/michal/i/impi.l") # TODO : fix this!! 


(class +Matcher)

(var ch . "^D")
(var fx . 'STREAM) # current effects

(dm ch> () # return the current character
   (var: ch) )

(dm ch+> () # 
   (nextchar>)
   (var: ch) )

# -- 0 char patterns: --

# null pattern. always true
(dm nul> () T )

# -- 1 char patterns: --

# match a specific symbol/character
(dm sym> (Ch) 
   (= (var: ch) Ch) )

# match any of the characters in set Cs
(dm any> (Cs)
   (member (ch>) Cs) )

# -- n char patterns: --

# match a string of literal characters
(dm lit> (S) 
   )

# match a sequence of patterns
(dm seq> (Ps)
   )

# match one of a list of patterns
(dm alt> (Ps)
   )

# optionally match a pattern
(dm opt> (P)
   )

# match a repeating pattern
(dm rep> (P)
   )

# -- context free grammars --

(dm def> (Name Effect P)
   )

# match a named rule
(dm sub> (Name)
   )
