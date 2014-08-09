unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    KeyPhrase: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    Plaintext: TMemo;
    Ciphertext: TMemo;
    TopPanel: TPanel;
    procedure BitBtn1Click(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

TYPE TCipherMode = (mEncipher,mDecipher,mNone);
     TCipherModes= ARRAY [mEncipher..mNone] OF STRING;
CONST CipherModes: TCipherModes = ('Encipher','Decipher','None');
VAR   mode : TCipherMode = mEncipher;
      key  : STRING = '';

// Call SES in appropriate mode
Function SESCall(k,txt: STRING; mode: TCipherMode): STRING;
var AProcess: TProcess;
    AStringList: TStringList;
    cmd: STRING;
begin
  cmd := '';
  case mode of
       mEncipher:
            cmd += 'ses -k "'+k+'" -e "'+txt+'"';
       mDecipher:
            cmd += 'ses -k "'+k+'" -d "'+txt+'"';
       mNone: Exit;
  end;
  // Now we will create the TProcess object, and
  // assign it to the var AProcess.
  AProcess := TProcess.Create(NIL);
  // Create the TStringList object.
  AStringList := TStringList.Create;
  // Tell the new AProcess what the command to execute is.
  AProcess.CommandLine := cmd;
  // We will define an option for when the program
  // is run. This option will make sure that our program
  // does not continue until the program we will launch
  // has stopped running. Also now we will tell it that
  // we want to read the output of the file.
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  // Now that AProcess knows what the commandline is
  // we will run it.
  AProcess.Execute;
  // Now read the output of the program we just ran
  // into the TStringList.
  AStringList.LoadFromStream(AProcess.Output);
  // Print the output from SES
  SESCall := AStringList[0];
  // Clean up
  AStringList.Free;
  AProcess.Free;
end;


procedure TForm1.BitBtn1Click(Sender: TObject);
var output,ctx,ptx: STRING;
    n : CARDINAL;
begin
  try
    ctx := ''; ptx := '';
    if Ciphertext.Lines.Count > 0 then
       for n := 0 to Ciphertext.Lines.Count-1 do
        ctx += Ciphertext.Lines[n];
    if Plaintext.Lines.Count > 0 then
       for n:= 0 to Plaintext.Lines.Count-1 do
        ptx += Plaintext.Lines[n];
    key := KeyPhrase.Text;

    // 1) Decide which mode we're in
    if key <> '' then begin
       if ptx <> '' then
          mode := mEncipher
       else if ctx <> '' then
          mode := mDecipher;
    end else begin
          mode := mNone;
          ShowMessage('Please enter your Key-phrase.');
    end;
    case mode of
         mEncipher: begin
           Ciphertext.Clear;
           Ciphertext.Lines[0] := SESCall(key,ptx,mode);
         end;
         mDecipher: begin
           Plaintext.Clear;
           Plaintext.Lines[0] := SESCall(key,ctx,mode);
         end;
         mNone: ShowMessage('Please enter a Key, Plaintext, or Ciphertext.');
    end;
  except
    ShowMessage('Cipher error: Ensure you have a Key-phrase and either Ciphertext or Plaintext entered.');
  end;
end;



end.

