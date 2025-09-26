grammar B4a

entry Model:
    (codes+=(Def|Seg|Ref|Fwd|Op|Hex|Chr|Str|Mac|Adr|Get|Put|RDef) )*;

Op: code=(OP0 | OP1 | OP2 | OP3
    | 'ad' | 'sb' | 'ml' | 'dv' | 'md' | 'sh' | 'an' | 'or'
    | 'xr' | 'nt' | 'eq' | 'lt' | 'du' | 'sw' | 'ov' | 'zp'
    | 'dc' | 'cd' | 'rb' | 'ri' | 'wb' | 'wi' | 'lb' | 'li'
    | 'rs' | 'ls' | 'jm' | 'hp' | 'h0' | 'cl' | 'rt' | 'nx'
    | 'c0' | 'c1' | 'c2' | 'n1' | 'c4' | 'wl' | 'ds' | 'cs'
    | 'hx' | 'io' | 'db' | 'hl' | 'tm' );

Def: ':' name=ID;
RDef: rdef= // set register to current address
    ( ':@' | ':A' | ':B' | ':C' | ':D' | ':E' | ':F' | ':G'
    | ':H' | ':I' | ':J' | ':K' | ':L' | ':M' | ':N' | ':O'
    | ':P' | ':Q' | ':R' | ':S' | ':T' | ':U' | ':V' | ':W'
    | ':X' | ':Y' | ':Z' | ':[' | ':\\'| ':]' | ':^' | ':_' );
Seg: ':' addr=HEX;
Fwd: '>' ref=[Def:ID];
Ref: to=[Def:ID];
Adr: '`' ref=[Def:ID];
Get: '@' ref=[Def:ID];
Put: '!' ref=[Def:ID];
Mac: v=MAC;
Hex: v=HEX;
Chr: v=CHR;
Str: v=STR;

hidden terminal WS: /\s+/;
hidden terminal COMMENT: /#[^\n]*/;

terminal CHR: /[']./;
terminal STR: /"[^""]*"/;
terminal MAC: /[.][a-z]/;
terminal HEX: /-?[0-9A-F]+/;
terminal fragment REG: '@' | REG0;
terminal fragment REG0: /[A-Z\[\\\]^_]/;
terminal OP0: '^' REG0 | '..';
terminal OP1: '@' REG ;
terminal OP2: '!' REG ;
terminal OP3: '+' REG ;
terminal ID: /[\\_a-z][\^a-z0-9_-]*/;
