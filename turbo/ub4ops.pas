{-- do not edit! regenerate with mkoptbl.pas --}
unit ub4ops;
interface
  type opstring = string[4];
  var optbl : array[ $80 .. $BF ] of opstring;
implementation
begin
  optbl[128] := 'lb';
  optbl[129] := 'li';
  optbl[130] := 'sw';
  optbl[131] := 'du';
  optbl[132] := 'ov';
  optbl[133] := 'zp';
  optbl[134] := 'dr';
  optbl[135] := 'rd';
  optbl[136] := 'ad';
  optbl[137] := 'sb';
  optbl[138] := 'ml';
  optbl[139] := 'dv';
  optbl[140] := 'md';
  optbl[141] := 'ng';
  optbl[142] := 'sl';
  optbl[143] := 'sr';
  optbl[144] := 'an';
  optbl[145] := 'or';
  optbl[146] := 'xr';
  optbl[147] := 'nt';
  optbl[148] := 'eq';
  optbl[149] := 'ne';
  optbl[150] := 'gt';
  optbl[151] := 'lt';
  optbl[152] := 'ge';
  optbl[153] := 'le';
  optbl[154] := 'dx';
  optbl[155] := 'xd';
  optbl[156] := 'dy';
  optbl[157] := 'yd';
  optbl[158] := 'dz';
  optbl[159] := 'zd';
  optbl[160] := 'dc';
  optbl[161] := 'cd';
  optbl[162] := 'hl';
  optbl[163] := 'jm';
  optbl[164] := 'hp';
  optbl[165] := 'j0';
  optbl[166] := 'h0';
  optbl[167] := 'cl';
  optbl[168] := 'rt';
  optbl[169] := 'r0';
  optbl[170] := 'nx';
  optbl[171] := 'ev';
  optbl[172] := 'rb';
  optbl[173] := 'wb';
  optbl[174] := 'ri';
  optbl[175] := 'wi';
  optbl[176] := 'tg';
  optbl[177] := 'ta';
  optbl[178] := 'tw';
  optbl[179] := 'tr';
  optbl[180] := 'tk';
  optbl[181] := 'ts';
  optbl[182] := 'tl';
  optbl[183] := 'tc';
  optbl[184] := 'db';
  optbl[185] := '';
  optbl[186] := '';
  optbl[187] := '';
  optbl[188] := '';
  optbl[189] := '';
  optbl[190] := '';
  optbl[191] := '';
end.
