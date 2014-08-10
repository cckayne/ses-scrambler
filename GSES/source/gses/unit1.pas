{$define test}
{$define calling} // Are we calling the CL SES?
{$define caesar} // Are we using Caesar-shift to obfuscate key-phrase?
{$define ptxsave}
unit Unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, Buttons;

VAR		MaxTimeOutValue: CARDINAL = 120; {120 seconds}
		TimeOut: CARDINAL = 0;
        PtxSave: TStringList;
		CtxSave: TSTringList;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    KeyPhrase: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    KeyLabel: TLabel;
    CLengthLabel: TLabel;
    RatingLabel: TLabel;
    StrengthLabel: TLabel;
    PLengthLabel: TLabel;
    OptionsLabel: TLabel;
    MainMenu1: TMainMenu;
    Plaintext: TMemo;
    Ciphertext: TMemo;
    IdleTimer: TTimer;
    TopPanel: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure BitBtn1MouseEnter(Sender: TObject);
    procedure BitBtn1MouseLeave(Sender: TObject);
    procedure CiphertextChange(Sender: TObject);
    procedure CiphertextDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CiphertextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CiphertextMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CiphertextMouseEnter(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: Array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure KeyPhraseChange(Sender: TObject);
    procedure KeyPhraseExit(Sender: TObject);
    procedure KeyPhraseKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OptionsLabelClick(Sender: TObject);
    procedure OptionsLabelMouseEnter(Sender: TObject);
    procedure OptionsLabelMouseLeave(Sender: TObject);
    procedure PlaintextChange(Sender: TObject);
    procedure PlaintextDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PlaintextEnter(Sender: TObject);
    procedure PlaintextExit(Sender: TObject);
    procedure PlaintextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PlaintextMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlaintextMouseEnter(Sender: TObject);

  private
    { private declarations }
    procedure ResetTimeOut;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
USES MyStrUtils, gsesUtils, Unit2, Unit3, MyIsaac;


procedure TForm1.BitBtn1Click(Sender: TObject);
var ctx,ptx,k: STRING;
    n : CARDINAL;
	tmp: TStringList;
begin
  try
    ResetTimeOut;
    mode := mNone;
    ctx := ''; ptx := '';
    if Ciphertext.Lines.Count > 0 then
       for n := 0 to Ciphertext.Lines.Count-1 do
        ctx += Ciphertext.Lines[n];
    if Plaintext.Lines.Count > 0 then
       for n := 0 to Plaintext.Lines.Count-1 do
        // escape line-feeds for SES
        if n<PlainText.Lines.Count-1 then
        	ptx += Plaintext.Lines[n]+'\n'
        else
          	ptx += Plaintext.Lines[n];
    {$ifdef caesar}
    k := rCaesarStr(mDecipher,KeyPhrase.Text,95,' ');
    {$else}
    k := KeyPhrase.Text;
    {$endif}
    // 1) Decide which mode we're in
    if k <> '' then begin
       if ptx <> '' then
          mode := mEncipher
       else if ctx <> '' then
          mode := mDecipher;
    end else begin
          mode := mNone;
          ShowMessage('  Please enter your Key-phrase.');
    end;
    // belt and braces - strip non-printables
    ctx := UTF8ToAnsi(ctx); ptx := UTF8ToAnsi(ptx);
    ctx := CleanStr(ctx); ptx := CleanStr(ptx);

    case mode of
         mEncipher: begin
           Ciphertext.Clear;
           Ciphertext.Lines[0] := SESCall(k,ptx,mode);
           //Plaintext.Clear;
         end;
         mDecipher: begin
           ActiveControl := Plaintext;
           tmp := TStringList.Create;
           Plaintext.Clear;
           ptx := SESCall(k,ctx,mode);
           PostProcess(ptx,tmp);
           Plaintext.Lines.Assign(tmp);
           tmp.Free;
         end;
         mNone: ShowMessage('  Please enter a Plaintext or Ciphertext.');
    end;
  except
    ShowMessage('Cipher error: Ensure you have a Key-phrase and a valid Ciphertext or Plaintext entered.'+ #13#10 +'       It''s possible you may have exceeded the command-line length constraints.');
  end;
end;

procedure TForm1.BitBtn1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ResetTimeOut;
  // emergency hide
  if Key=27 then Hide;
end;

procedure TForm1.BitBtn1MouseEnter(Sender: TObject);
begin
  BitBtn1.Font.Color := clTeal;
end;

procedure TForm1.BitBtn1MouseLeave(Sender: TObject);
begin
  BitBtn1.Font.Color := clBlack;
end;

procedure TForm1.CiphertextChange(Sender: TObject);
begin
    CLengthLabel.Caption := IntToStr(Length(Ciphertext.Text));
end;

procedure TForm1.CiphertextDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  //Ciphertext.Lines.Assign(CtxSave);
  CtxSave.Assign(Ciphertext.Lines);
  Ciphertext.Clear;
  ActiveControl := Ciphertext;
end;

procedure TForm1.CiphertextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ResetTimeOut;
  // emergency hide
  if Key=27 then Hide;
end;

procedure TForm1.CiphertextMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ResetTimeOut;
end;

procedure TForm1.CiphertextMouseEnter(Sender: TObject);
begin
  if KeyPhrase.Text<>'' then ActiveControl := Ciphertext;
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  ResetTimeOut;
end;

procedure TForm1.FormCreate(Sender: TObject);
var noncekey: STRING;
begin
     noncekey := FormatDateTime('YYYY-MM-DD-hh:nn:ss:zzz',Now);
     iSeed(noncekey,true);
     PtxSave := TStringList.Create;
     CtxSave := TStringList.Create;
     PLengthLabel.Caption := IntToStr(Length(Plaintext.Text));
     CLengthLabel.Caption := IntToStr(Length(Ciphertext.Text));
     StrengthLabel.Caption := '';
     RatingLabel.Caption := '';
     ActiveControl := KeyPhrase;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var z: TStringList;
    n: CARDINAL;
begin
    try
		z := TStringList.Create;
		for n := 0 to 6000 do z.Add(StringStr(255,'Z'));
		// security clean-up
		KeyPhrase.Assign(z) ; KeyPhrase.Clear;
		Plaintext.Assign(z) ; Plaintext.Clear;
		Ciphertext.Assign(z); Ciphertext.Clear;
		PtxSave.Assign(z); PtxSave.Clear; PtxSave.Free;
		CtxSave.Assign(z); CtxSave.Clear; CtxSave.Free;
		z.Free;
		iReset;
	except
	end;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: Array of String);
var i,a: CARDINAL;
    k  : STRING;
begin
    try
        // ISC-Encipher or -Decipher dropped files
      	{$ifdef caesar}
    		k := rCaesarStr(mDecipher,KeyPhrase.Text,95,' ');
    	{$else}
        	k := KeyPhrase.Text;
    	{$endif}
		a:=0;
  		if ActiveControl=Plaintext then  a:=1;
    	if ActiveControl=Ciphertext then a:=2;
  		case a of
			1: begin
            	Plaintext.Clear;
            	Plaintext.Font.Color := clRed;
          		for i := Low(FileNames) to High(FileNames) do begin
            		Plaintext.Lines.Add(SEScallF(k,Filenames[i],mEncipher));
                	Plaintext.Lines.Add('');
                end;
            	Plaintext.Font.Color := clBlack;
           	end;
    		2: begin
            	Ciphertext.Clear;
            	Ciphertext.Font.Color := clRed;
            	for i := Low(FileNames) to High(FileNames) do begin
        			Ciphertext.Lines.Add(SEScallF(k,Filenames[i],mDecipher));
                	Ciphertext.Lines.Add('');
                end;
           	Ciphertext.Font.Color := clWhite;
   		   	end;
   		end;
	except
      ShowMessage('SES file cipher error. Files for decryption end in ".ses"');
    end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
    ResetTimeOut;
    // emergency hide
    if Key=27 then Hide;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    ResetTimeOut;
end;

procedure TForm1.FormShow(sender: tobject);
begin
    WindowState := wsNormal;
    SessionForm.Visible := true;
    SessionForm.Show;
end;


procedure TForm1.IdleTimerTimer(Sender: TObject);
begin
  Inc(TimeOut);
  if TimeOut >= MaxTimeOutValue then begin
     if WindowSTate<>wsMinimized then begin
        SessionForm.Show;
        Hide;
        IdleTimer.Enabled := FALSE;
        ResetTimeout;
     end;
  end;

end;

procedure TForm1.KeyPhraseChange(Sender: TObject);
var entropy : dword;
    score   : byte;
begin
    {$ifdef test}
  		evaluate_password(Keyphrase.Text, entropy, score);
    	StrengthLabel.Caption := IntToStr(entropy div 2);
    	RatingLabel.Caption := IntToStr(score);
    {$endif}
end;


procedure TForm1.KeyPhraseExit(Sender: TObject);
var n: CARDINAL;
    k: STRING;
begin
  	{$ifdef caesar}
     k := KeyPhrase.Text;
     if k<>'' then
        if not caesard then begin
            KeyPhrase.Text := rCaesarStr(mEncipher,k,95,' ');
            caesard := true;
            KeyPhrase.Enabled := false;
            KeyPhrase.ShowHint := true;
            KeyPhrase.Hint := 'Only one key per session allowed. Please restart if you wish to use a different key.';
        	KeyLabel.Caption := 'Enciphered Key:';
        end;
     {$endif}
     StrengthLabel.Caption:='';
     RatingLabel.Caption:='';
end;

procedure TForm1.KeyPhraseKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ResetTimeOut;
  // emergency hide
  if Key=27 then Hide;
  if Key=13 then ActiveControl:=Plaintext;
end;


procedure TForm1.PlaintextDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var tmp: TStringList;
begin
  {$ifdef ptxsave}
  try
  	tmp := TStringList.Create;
  	tmp.Assign(Plaintext.Lines);
  	// save enciphered copy of Plaintext
  	PtxSave.Assign(rCaesarStrL(mEncipher,tmp,95,' '));
  	tmp.Free;
  	Plaintext.Clear;
  	ActiveControl := Plaintext;
  except
  	Plaintext.Clear;
  	ActiveControl := Plaintext;
  end;
  {$endif}
end;

procedure TForm1.PlaintextEnter(Sender: TObject);
begin
  try
  	{$ifdef ptxsave}
	// return deciphered copy of Plaintext
  	Plaintext.Lines.Assign(rCaesarStrL(mDecipher,PtxSave,95,' '));
  	{$endif}
	Plaintext.Font.Color := clBlack;
  except
  end;
end;

procedure TForm1.PlaintextExit(Sender: TObject);
var tmp: TStringList;
begin
  try
  	{$ifdef ptxsave}
		tmp := TStringList.Create;
		tmp.Assign(Plaintext.Lines);
		// save enciphered copy of Plaintext
		PtxSave.Assign(rCaesarStrL(mEncipher,tmp,95,' '));
		tmp.Free;
  	{$endif}
	Plaintext.Font.Color := clWhite;
  except
  end;
end;

procedure TForm1.PlaintextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ResetTimeOut;
  // emergency hide
  if Key=27 then Hide;
end;

procedure TForm1.PlaintextMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ResetTimeOut;
end;

procedure TForm1.PlaintextMouseEnter(Sender: TObject);
begin
  if KeyPhrase.Text<>'' then ActiveControl := Plaintext;
end;

procedure TForm1.ResetTimeOut;
begin
  TimeOut := 0;
end;

procedure TForm1.OptionsLabelMouseEnter(Sender: TObject);
begin
    OptionsLabel.Font.Style := [fsUnderline];
    OptionsLabel.Cursor := crHandpoint;
end;

procedure TForm1.OptionsLabelMouseLeave(Sender: TObject);
begin
  OptionsLabel.Font.Style := [];
  OptionsLabel.Cursor := crDefault;
end;

procedure TForm1.PlaintextChange(Sender: TObject);
begin
  PLengthLabel.Caption := IntToStr(Length(Plaintext.Text));
end;


procedure TForm1.OptionsLabelClick(Sender: TObject);
begin
	OptionsForm.Show;
end;


end.

