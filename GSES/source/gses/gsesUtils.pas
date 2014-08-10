{$mode delphi}
{ $define test}
UNIT gsesUtils;

INTERFACE

USES    Classes;

TYPE 	TCipherMode = (mEncipher,mDecipher,mNone);
		TCipherModes= ARRAY [mEncipher..mNone] OF STRING;
		TTextType   = (tPlaintext,tCiphertext,tNone);
CONST	CipherModes: TCipherModes = ('Encipher','Decipher','None');
VAR 	vernamd: BOOLEAN = FALSE;
		caesard: BOOLEAN = FALSE;
		mode : TCipherMode = mEncipher;

// Call SES in appropriate mode
FUNCTION SESCall(k,txt: STRING; mode: TCipherMode): STRING;
// Call SES for basic file ciphering in appropriate mode
Function SESCallF(k,txt: STRING; mode: TCipherMode): STRING;
{ Caesar-shift a character <shift> places: Generalized Vigenere }
FUNCTION Caesar(m: TCipherMode; ch: CHAR; shift, modulo: INTEGER; start: CHAR): CHAR;
// Randomly Caesar-shift a string letter by letter
// < equivalent to Vigenere-ing on a random letter key >
FUNCTION rCaesarStr(m: TCiphermode; str: ANSISTRING; modulo: INTEGER; start: CHAR): ANSISTRING;
// Randomly Caesar-shift a StringList
FUNCTION rCaesarStrL(m: TCipherMode; str: TStringList; modulo: INTEGER; start: CHAR): TStringList;
{ Vernam XOR cipher a string }
FUNCTION Vernam(ky: STRING): STRING;
// strip string of non-printing characters
FUNCTION CleanStr(s: STRING): STRING;
{ Replace escaped quotes and tabs; explode str into a StringList by lines }
PROCEDURE PostProcess(str: STRING; VAR list: TStringList);
{ Simple password evaluation according to NIST Special Publication 800-63 - not reliable }
PROCEDURE evaluate_password(pw: STRING; VAR pw_strength:dword; VAR pw_rating:byte);                              //1..8 rating


IMPLEMENTATION

USES SysUtils, StrUtils, MyStrUtils, Process, MyIsaac{$ifdef test},Dialogs{$endif};

TYPE TStrArray = ARRAY of STRING;


// Call SES in appropriate mode
Function SESCall(k,txt: STRING; mode: TCipherMode): STRING;
var AProcess: TProcess;
    AStringList: TStringList;
    exe: STRING;
begin
  exe := 'ses';
  // Now we will create the TProcess object, and
  // assign it to the var AProcess.
  AProcess := TProcess.Create(NIL);
  // Create the TStringList object.
  AStringList := TStringList.Create;
  // Tell the new AProcess what the command to execute is.
  AProcess.Executable := exe;
  // List parameters
  case mode of
       mEncipher:
          With AProcess.Parameters do begin
              add('-k');
              add('"'+k+'"');
              add('-e');
              add('"'+txt+'"');
          end;
       mDecipher:
          With AProcess.Parameters do begin
              add('-k');
              add('"'+k+'"');
              add('-d');
              add(txt);
          end;
       mNone: Exit;
  end;
  // define execution options
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  // hiding the process' window
  AProcess.ShowWindow := swoHIDE;
  // generously allocate parameter storage
  AProcess.PipeBufferSize := Length(txt)*4+256;
  // Now that AProcess knows what the commandline is
  // we will run it.
  AProcess.Execute;
  // Now read the output of the program we just ran
  // into the TStringList.
  AStringList.LoadFromStream(AProcess.Output);
  // return the output from SES
  SESCall := AStringList[0];
  // Clean up
  AStringList.Free;
  AProcess.Free;
end;


// Call SES for file ciphering in appropriate mode
Function SESCallF(k,txt: STRING; mode: TCipherMode): STRING;
var AProcess: TProcess;
    AStringList: TStringList;
    f,p,exe: STRING;
begin
  // filename and file-path
  f := ExtractFilename(txt);
  p := ExtractFilepath(txt);
  exe := 'ses';
  {$ifdef test}
	ShowMessage(p);
	ShowMessage(f);
  {$endif}
  // Now we will create the TProcess object, and
  // assign it to the var AProcess.
  AProcess := TProcess.Create(NIL);
  // Create the TStringList object.
  AStringList := TStringList.Create;
  // Tell the new AProcess what the command to execute is.
  AProcess.Executable := exe;
  // List parameters
  case mode of
       mEncipher:
          With AProcess.Parameters do begin
              add('-k');
              add('"'+k+'"');
              add('-e');
              // file
			  add('-f');
			  add('"'+f+'"');
			  // path
			  add('-p');
			  add(p);
          end;
       mDecipher:
          With AProcess.Parameters do begin
              add('-k');
              add('"'+k+'"');
              add('-d');
              // file
			  add('-f');
			  add('"'+f+'"');
			  // path
			  add('-p');
			  add(p);
          end;
       mNone: Exit;
  end;
  // define execution options
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  // hiding the process' window
  AProcess.ShowWindow := swoHIDE;
  // generously allocate parameter storage
  AProcess.PipeBufferSize := Length(txt)*4+256;
  // Now that AProcess knows what the commandline is
  // we will run it.
  AProcess.Execute;
  // Now read the output of the program we just ran
  // into the TStringList.
  AStringList.LoadFromStream(AProcess.Output);
  // return the output from SES
  SESCallF := AStringList[0];
  // Clean up
  AStringList.Free;
  AProcess.Free;
end;


// Get position of the letter in chosen alphabet
FUNCTION letternum(letter, start: CHAR): byte;
	BEGIN
		result := (ord(letter)-ord(start));
	END;

	
{ Caesar-shift a character <shift> places: Generalized Vigenere }
FUNCTION Caesar(m: TCipherMode; ch: CHAR; shift, modulo: INTEGER; start: CHAR): CHAR;
	VAR n: INTEGER;
	BEGIN
		IF m = mDecipher THEN shift := -shift;
		n := letternum(ch,start) + shift;
		n := n MOD modulo;
		IF n<0 THEN n += modulo;
		Caesar := chr(ord(start)+n);
	END;

// Randomly Caesar-shift a string letter by letter
// < equivalent to Vigenere-ing on a random letter key >
FUNCTION rCaesarStr(m: TCiphermode; str: ANSISTRING; modulo: INTEGER; start: CHAR): ANSISTRING;
	VAR n: CARDINAL;
	BEGIN
		Result := '';
		FOR n := 1 TO Length(str) DO
			Result += Caesar(m,str[n], (iPeek(n) mod (modulo-1)), modulo, start);
	END;

// Randomly Caesar-shift a StringList
FUNCTION rCaesarStrL(m: TCipherMode; str: TStringList; modulo: INTEGER; start: CHAR): TStringList;
	VAR n: CARDINAL;
	BEGIN
		TRY
			rCaesarStrL := TStringList.Create;
			FOR n := 0 TO str.Count-1 DO
				rCaesarStrL.Add(rCaesarStr(m,str[n],95,' '));
		EXCEPT
		END;
	END;
	
	
FUNCTION Vernam(ky: STRING): STRING;
VAR n: CARDINAL;
    s: STRING;
BEGIN
     SetLength(s,Length(ky));
     // Vernam decipher the key string
     FOR n := 1 TO Length(ky) DO
         s[n] := chr(byte((iPeek(n) mod 95 + 32) xor ord(ky[n])));
     Vernam := s;
     vernamd := NOT vernamd;
END;


FUNCTION Explode(str,sep: STRING): TStrArray;
	VAR h,t: STRING;
		n:	 CARDINAL;
	BEGIN
		
	END;

	
{ Replace escaped quotes and tabs; explode str into a StringList by lines }
PROCEDURE PostProcess(str: STRING; VAR list: TStringList);
	VAR n: CARDINAL;
		t,h: STRING;
	BEGIN
		// Replace quotes, tabs, etc
		str := AnsiReplaceStr(str,'\q','"');
		str := AnsiReplaceStr(str,'\t',chr(9));
		// Explode the string on '\n'
		t := str;
		REPEAT
			h := HeadAnsi(t,'\n');
			t := TailAnsi(t,'\n');
			list.Add(h);
		UNTIL t='';
	END;


// strip string of non-printing characters
FUNCTION CleanStr(s: STRING): STRING;
VAR str: STRING;
BEGIN
	str := s;
	str := AnsiReplaceStr(str,chr(0),'');
	str := AnsiReplaceStr(str,chr(1),'');
	str := AnsiReplaceStr(str,chr(2),'');
	str := AnsiReplaceStr(str,chr(3),'');
	str := AnsiReplaceStr(str,chr(4),'');
	str := AnsiReplaceStr(str,chr(5),'');
	str := AnsiReplaceStr(str,chr(6),'');
	str := AnsiReplaceStr(str,chr(7),'');
	str := AnsiReplaceStr(str,chr(8),'');
	str := AnsiReplaceStr(str,chr(9),'\t');
	str := AnsiReplaceStr(str,chr(10),'\n');
	str := AnsiReplaceStr(str,chr(11),'');
	str := AnsiReplaceStr(str,chr(12),'');
	str := AnsiReplaceStr(str,chr(13),'');
	str := AnsiReplaceStr(str,chr(14),'');
	str := AnsiReplaceStr(str,chr(15),'');
	str := AnsiReplaceStr(str,chr(16),'');
	str := AnsiReplaceStr(str,chr(17),'');
	str := AnsiReplaceStr(str,chr(18),'');
	str := AnsiReplaceStr(str,chr(19),'');
	str := AnsiReplaceStr(str,chr(20),'');
	str := AnsiReplaceStr(str,chr(21),'');
	str := AnsiReplaceStr(str,chr(22),'');
	str := AnsiReplaceStr(str,chr(23),'');
	str := AnsiReplaceStr(str,chr(24),'');
	str := AnsiReplaceStr(str,chr(25),'');
	str := AnsiReplaceStr(str,chr(26),'');
	str := AnsiReplaceStr(str,chr(27),'');
	str := AnsiReplaceStr(str,chr(28),'');
	str := AnsiReplaceStr(str,chr(29),'');
	str := AnsiReplaceStr(str,chr(30),'');
	str := AnsiReplaceStr(str,chr(31),'');
	str := AnsiReplaceStr(str,chr(127),'');
	str := AnsiReplaceStr(str,'"','\q');
	CleanStr := str;
END;


{ Simple password evaluation according to NIST Special Publication 800-63 - not reliable }
procedure evaluate_password ( pw: STRING;                                       //password
                              var pw_strength:dword;                            //estimated entropy*2
                              var pw_rating:byte);                              //1..8 rating
{
Password strength is expressed as raw estimated entropy entered at the keyboard,
2 points for each entropy bit (to allow easy fractional increments).
Please note that the unit doesn't perform a quality check on the password neither
using dictionary nor using composition rules
}
var
   i,w_len,pw_len:integer;
   bonus_upcase,bonus_nonalph:boolean;
begin
pw_len:=length(pw);
w_len:=0;
pw_strength:=0;
bonus_nonalph:=false;
bonus_upcase:=false;
for i:=1 to pw_len do
   begin
   {
   //apply the evaluation criteria to the passphrase as a whole object
   if i=1 then pw_strength:=pw_strength+8; //4 bit of entropy for first character
   if (i>1) and (i<9) then pw_strength:=pw_strength+4; //2 bit for 2..8
   if (i>=9) and (i<21) then pw_strength:=pw_strength+3; //1.5 bit for 9..20
   if i>21 then pw_strength:=pw_strength+2; //1 bit for utter characters
   }
   //apply the evaluation criteria to each word separately
   w_len:=w_len+1;
   if w_len=1 then pw_strength:=pw_strength+8; //4 bit of entropy for first character
   if (w_len>1) and (w_len<9) then pw_strength:=pw_strength+4; //2 bit for 2..8
   if (w_len>=9) and (w_len<21) then pw_strength:=pw_strength+3; //1,5 bit for 9..20
   if w_len>21 then pw_strength:=pw_strength+2; //1 bit for utter characters
   if (ord(pw[i])=32) and (pw[i]<>pw[i-1]) then w_len:=0; //if char is a space, start a new word (ignore repeated spaces)
   {
   give 6 enthropy bit bonus for use of both non alphabetic character and upcase
   characters;
   each case here gives a 3 bit bonus, but not if the special char is in first
   or last position of the password, since it's a quite common situation
   }
   if bonus_nonalph=false then
      if (ord(pw[i])<>32) and ((ord(pw[i])<97) or (ord(pw[i])>122)) and ((ord(pw[i])<65) or (ord(pw[i])>90)) then
         if (i<>1) and (i<>length(pw)) then
            begin
            pw_strength:=pw_strength+6;
            bonus_nonalph:=true;
            end;
   if bonus_upcase=false then
      if ((ord(pw[i])>=65) and (ord(pw[i])<=90)) then
         if (i<>1) and (i<>length(pw)) then
            begin
            pw_strength:=pw_strength+6;
            bonus_upcase:=true;
            end;
   end;
if pw_strength<32*2 then pw_rating:=1 //trivial to bruteforce
else
   if pw_strength<64*2 then pw_rating:=2 //successful bruteforce attacks in this range are known to literature
   else
      if pw_strength<72*2 then pw_rating:=3
      else
         if pw_strength<80*2 then pw_rating:=4  //two 8 bit steps cover an area that give progressively wider security margin against bruteforce
         else
            if pw_strength<96*2 then pw_rating:=5
            else
               if pw_strength<112*2 then pw_rating:=6
               else
                  if pw_strength<128*2 then pw_rating:=7 //tree 16 bit steps cover an area that give progressively wider security margin for non-short timed secrets
                  else pw_rating:=8; //area that give a good security margin for long time secrets
end;


FINALIZATION
	iReset;
END.
