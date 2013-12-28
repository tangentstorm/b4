NB. pascal compiler/generator for j


hello =: noun : 0
program hello;
uses kvm,cw;

procedure say(msg:string);
  var cx, cy, i : byte;
  begin
    cx - kvm.xmax div 2;
    cy - kvm.ymax div 2;
    cwxy(cx - length(msg div 2)-4, cy,
      '|b-|B-|K[ |w' + msg + ' |K]|B-|b-');
  end;

begin
  clrscr;
  say('hello world');
end.
)