unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    HelpMemo: TMemo;
    PageControl1: TPageControl;
    Help: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

{ TOptionsForm }
uses Unit1;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  Help.color := clSilver;
  PageControl1.color := clSilver;
  try
  	HelpMemo.Lines.LoadFromFile('Help.txt');
  except
  	HelpMemo.Lines.Add('Help.txt was not found in this directory.');
    HelpMemo.Lines.Add('Please ensure the file resides with GSES in the installation folder.');
  end;
end;

procedure TOptionsForm.FormHide(Sender: TObject);
begin
  Form1.IdleTimer.Enabled := true;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  Form1.IdleTimer.Enabled := false;
end;

end.

