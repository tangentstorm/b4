(* grammar combinators *)
datatype 'a gram
  = gram of string * 'a gram list
  | nul
  | any
  | lit of string
  | chs of string
  | seq of 'a gram list
  | alt of 'a gram list
  | rep of 'a gram list
  | inv of 'a gram list
  | sub of string
  | def of string * 'a gram list
  | rul of string * 'a gram list
  | tok of string * 'a gram list
  | act of 'a gram -> unit
  | gen of 'a gram -> 'a
  ;

(* ebnf meta-grammar *)
val ebnf = gram("ebnf", [
  rul("start", [rep [sub "rule"]]),
  rul("rule",  [sub "iden", lit "=", sub "expr", lit "."]),
  rul("expr",  [sub "term", rep [lit "|", sub "term"]]),
  rul("term",  [rep [sub "factor"]]),
  rul("rep",   [lit "{", sub "expr", lit "}"]),
  rul("opt",   [lit "[", sub "expr", lit "]"]),
  rul("grp",   [lit "(", sub "expr", lit ")"]),
  def("factor",[alt [sub "iden", sub "str", sub "rep", sub "opt", sub "grp"]]),
  tok("iden",  [sub "alpha", rep [alt [sub "alpha", sub "digit"]]]),
  def("alpha", [chs "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]),
  def("digit", [chs "0123456789"]),
  tok("str",   [lit "\"", sub "esc", lit "\""]),
  def("esc",   [alt [seq[lit "\^[", any],
                     inv[chs "\^[\"" ]]])
]);

(* a simple xml-like type for our (annotated) abstract syntax trees *)
datatype 'a ast
  = elem of string * 'a ast list
  | attr of string * 'a
  | text of string
  ;
