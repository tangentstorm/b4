program b4fv; { b4 ui with free vision ui library from free pascal }

uses
  ub4i, Objects, Drivers, Views, Menus, Dialogs, App, MsgBox,
  Editors, StdDlg;

type
  PB4iWin = ^TB4iWin;
  TB4iWin = object(TWindow)
    ipl : PInputLine;
    constructor Init(var r: TRect; aTitle: string);

  end;
  TB4Ui = object(App.TApplication)
    b4iWin : PB4iWin;
    constructor Init;
    procedure InitMenuBar; virtual;
    procedure HandleEvent(var e:TEvent); virtual;
    procedure NewEditWindow;
    procedure About;
  end;

const
  cmAbout = 200;
  cmRunB4i = 210;

{ -- TB4iWin ------------------------------------------------ }

constructor TB4iWin.Init(var R:TRect; aTitle: string);
begin
  inherited Init(R, aTitle, wnNoNumber);
  R.Assign(1,R.B.Y-2,64,R.B.Y-1);
  ipl := New(PInputLine, Init(R, 64));
  Insert(ipl);
  R.A.X := 66;
  R.B.X := 70;
  Insert(New(PButton, Init(R, 'go', cmRunB4i, bfNormal)));
end;


{ -- TB4Ui -------------------------------------------------- }

constructor TB4Ui.Init;
  var R:TRect;
begin
  EditorDialog := @StdEditorDialog;
  inherited Init;
  GetExtent(R);
  R.B.Y := R.A.Y + 10;
  b4iWin := New(PB4iWin, Init(R, 'b4i'));
  InsertWindow(b4iWin);
end;


procedure TB4Ui.InitMenuBar;
  var R:TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', hcNoContext, NewMenu(
      NewItem('~N~ew', 'Alt-N', kbAltN, cmNew, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext,
      nil))),
    NewSubMenu('~H~elp', hcNoContext, NewMenu(
      NewItem('~A~bout', 'F1', kbF1, cmAbout, hcNoContext,
      nil)),
    nil)))));
end;

procedure TB4Ui.HandleEvent(var e: TEvent);
begin
  inherited HandleEvent(e);
  if e.What = evCommand then begin
    case e.Command of
      cmNew: NewEditWindow;
      cmAbout: About;
    else Exit end;
    ClearEvent(e);
  end
end;

procedure TB4Ui.About;
begin
  MessageBox('A TUI for B4, made with Free Vision',
             nil, mfInformation or mfOkButton);
end;


procedure TB4Ui.NewEditWindow;
  var r : TRect;
begin
  GetExtent(R);
  InsertWindow(New(PEditWindow, Init(R, '', wnNoNumber)));
end;


var
  B4App : TB4Ui;

begin
  B4App.Init;
  B4App.Run;
  B4App.Done;
end.

