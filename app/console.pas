unit Console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus;

type

  { TConsoleForm }

  TConsoleForm = class(TForm)
    dbc: TSQLite3Connection;
    Chars: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
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

procedure TConsoleForm.FormResize(Sender: TObject);
begin
  self.Chars.Width := self.Width;
  self.Chars.Height := self.Height;
end;

end.

