unit unit2;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, fileutil, forms, controls, graphics, dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TSessionForm }

  TSessionForm = class(tform)
    bitbtn1: tbitbtn;
    PWEdit: TEdit;
    memo1: tmemo;
    procedure bitbtn1click(sender: tobject);
    procedure formclick(sender: tobject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure formshow(sender: tobject);
    procedure memo1enter(sender: tobject);
    procedure PWEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    procedure ValidationLines(article: STRING; dumm,first: BOOLEAN);
    { public declarations }
  end;

var
  SessionForm: TSessionForm;

implementation

{$R *.lfm}

{ TSessionForm }
uses Unit1, gsesUtils, MD5;

var PWHash: STRING = '';
    DUHash: STRING = '';
    salt: STRING = '';
    FirstShow: BOOLEAN = TRUE;
    PWTries	 : CARDINAL = 0;
    PWDone   : BOOLEAN  = FALSE;

procedure TSessionForm.ValidationLines(article: STRING; dumm,first: BOOLEAN);
begin
    Memo1.Lines.Delete(1);
  	Memo1.Lines.Insert(1,'Please enter '+article+' session password');
	if not FirstShow then begin
      Memo1.Lines.Delete(2);
      Memo1.Lines.Insert(2,'(3 wrong attempts will abort the program)');
      Memo1.Lines.Delete(0);
      Memo1.Lines.Insert(0,'GSES Session Validation');
    end;
end;


procedure TsessionForm.BitBtn1Click(Sender: TObject);
var EHash : STRING;
begin
  Case FirstShow of
    TRUE:
        if PWEdit.Text <> '' then begin
            // MD5 password hash
            PWHash := Uppercase(MD5Print(MD5String(PWEdit.Text+salt)));
            PWEdit.Clear;
            FirstShow:= false;
            ValidationLines('the',false,false);
            Form1.Enabled := true;
            Form1.Show;
            Form1.SetFocus;
            Form1.IdleTimer.Enabled := true;
            SessionForm.Hide;
        end else ActiveControl := PWEdit;
    FALSE:
        begin
           if PWEdit.Text <> '' then begin
              EHash := Uppercase(MD5Print(MD5String(PWEdit.Text+salt)));
              inc(PWTries);
              if PWTries<=3 then begin
              	if EHash=PWHash then begin
                    PWTries := 0;
                    PWDone := True;
                   	PWEdit.Clear;
                 	Form1.Enabled := true;
                 	Form1.Show;
                 	Form1.ActiveControl := Form1.Ciphertext;
                 	Form1.IdleTimer.Enabled := true;
                 	SessionForm.Hide;
              	end else
					ActiveControl := PWEdit;
              end;
              if (not PWDone) and (PWTries>=3) then begin
                 Form1.Plaintext.Clear;
                 Form1.Ciphertext.Clear;
                 Form1.KeyPhrase.Clear;
                 CtxSave.Clear;
                 PtxSave.Clear;
                 Form1.Close;
              end;
           end else ActiveControl := PWEdit;
        end;
  end; // case
end;

procedure tsessionform.formclick(sender: tobject);
begin
  ActiveControl := PWEdit;
end;

procedure TSessionForm.FormCreate(Sender: TObject);
begin
  salt := FormatDateTime('YYYY-MM-DD-hh:nn:ss:zzz',Now);
end;

procedure TSessionForm.FormMouseEnter(Sender: TObject);
begin
	Enabled := true;
    Visible := true;
    SetFocus;
end;

procedure tsessionform.formshow(sender: tobject);
begin
   Form1.Enabled := false;
   Enabled := true;
   Visible := true;
   SetFocus;
   ActiveControl := PWEdit;
   PWDone := FALSE;
   PWTries:= 0;
end;

procedure tsessionform.memo1enter(sender: tobject);
begin
  ActiveControl := PWEdit;
end;

procedure TSessionForm.PWEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=13 then BitBtn1Click(Self);
end;


end.

