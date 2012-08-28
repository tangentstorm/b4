unit Console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, SynEdit;

type

  { TConsoleForm }

  TConsoleForm = class(TForm)
    dbc: TSQLite3Connection;
    term: TSynEdit;
    procedure CharsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure termChange(Sender: TObject);
    procedure rhsChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ConsoleForm: TConsoleForm;

implementation

{$R *.lfm}

{ TConsoleForm }

procedure TConsoleForm.FormCreate(Sender: TObject);
begin

end;

procedure TConsoleForm.CharsChange(Sender: TObject);
begin

end;

procedure TConsoleForm.FormResize(Sender: TObject);
begin
  self.term.Width := self.Width;
  self.term.Height := self.Height;
  self.term.ScrollBars:= ssNone;
end;

procedure TConsoleForm.termChange(Sender: TObject);
begin

end;

procedure TConsoleForm.rhsChange(Sender: TObject);
begin

end;

end.

