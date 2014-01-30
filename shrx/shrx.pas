{$mode delphi}
program shrx;
uses uapp, ushrx, cw, kbd, kvm, ng, ui, ukm;

type
  TRxShell = class (uapp.TCustomApp)
    public
      ed : ui.ZInput;
      procedure init; override;
      procedure keys(km : ukm.TKeyMap); override;
      procedure step; override;
      procedure prompt;
      procedure DelegateKey( ext : boolean; ch : char );
      procedure CmdAccept( cmd :  string );
    end;

procedure TRxShell.init;
  begin
    ed := ui.ZInput.Create(self);
    ed.OnAccept := self.CmdAccept;
    ed.y := kvm.yMax; ed.is_dirty:=true;
    kvm.clrscr; gotoxy(0,kvm.yMax-3);
    writeln('shrx 0.0000000000001 pre-alpha deluxe.');
    writeln('type "bye" to quit.');
    prompt;
  end;

procedure TRxShell.keys(km : ukm.TKeyMap);
  var ch : char;
  begin
    for ch := #0 to #225 do km.crt[ ch ] := DelegateKey;
    km.cmd[ ^C ] := quit;
  end;

procedure TRxShell.CmdAccept( cmd : string );
  begin
    if cmd = 'bye' then quit
    else writeln('unknown command. try: "bye"');
    prompt;
  end;


//  !! refactor: copied directly from TEditor.DelegateKey :/
procedure TRxShell.DelegateKey( ext : boolean; ch : char );
  begin
    if ext then ed.handlestripped(ch)
    else ed.handle(ch);
  end;

procedure TRxShell.prompt;
  begin
    ed.work := ''; writeln; cwriteln('|cok|w')
  end;

procedure TRxShell.step;
  begin
    if ed.is_dirty then ed.show; //!! encapsulate this in ui.zinput itself
  end;

var app : TRxShell;
begin
  uapp.Run(TRxShell);
end.
