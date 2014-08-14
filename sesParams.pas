{$mode delphi}
// Command line switch handling for SES
unit sesParams;

interface

uses sesTypes;

Var CipherMode: TMCipher = mEncipher;

//See if a given command switch is present
function ExistParam(p:string): boolean;
//Find position of given command switch
function LocateParam(p:string): integer;
//Return the value of a given command switch
function GetParam(p:string): string;
//Get key-phrase from the command line, if present
function GetKeyphrase: string;
//Get ciphertext from the command line, if present
function GetCiphertext: string;
//Get plaintext from the command line, if present
function GetPlaintext: string;
//Get input filename from the command line, if present
function GetFile: string;
//Get input filename from the command line, if present
function GetFileT: string;
// Get file path
function GetPath: string;
//Get real OTP filename from the command line, if present
function GetOTP: string;
//Get grouping for letter groups
function GetGrouping: Cardinal;
//Get ciphertext authentication code
function GetAuth: string;
//Get mode (encipher or decipher) from the command line, if present
function GetMode: TMCipher;
//Get modulo (1=26, 2=95, 3=128 etc) from the command line, if present
function GetModulo: integer;
// Get number of passes for file delete
function GetPasses: Cardinal;
// Delete original after file-encryption?
// <optionally followed by number of passes>
function doDelete: boolean;
//Are we enciphering or deciphering a file of bytes?
function doFile: boolean;
// Are we super-enciphering or deciphering a text file?
function doFileT: boolean;
//Are we displaying help screen?
function doHelp: boolean;
//Will input and output be hexadecimal?
// <essential at modulo 128 and higher>
function doHex: boolean;
//Are we timing operations?
function doTiming: boolean;
//Are we displaying info screen?
function doInfo: boolean;
//Are we displaying options screen?
function doOptions: boolean;
//Are we doing a real OTP operation?
function doOTP: boolean;
//Are we checking out the SES Challenge?
function doChallenge: boolean;
//Are we checking GPL Warranty?
function doWarranty: boolean;
//Are we checking GPL Terms?
function doConditions: boolean;
//Are we displaying acknowledgments?
function doThanks: boolean;
//Are we displaying log abbreviations legend?
function doLegend: boolean;
//Are we running in verbose mode?
function doVerbose: boolean;
// emit the current SES version and build
function doVersion: boolean;
//Are we outputting letter groups?
function doGroups: boolean;
//Are we including ciphertext authentication?
function doAuth: boolean;
//Are there no parameters specified?
function noParams: boolean;
//The Key to SES, life, the Universe, and everything
function doMagic: boolean;


implementation

uses StrUtils, SysUtils, sesCiphers;

//See if a given command switch is present
function ExistParam(p:string): boolean;
var c: integer;
var found: boolean;
begin
	c := 0; found := false;
	repeat
		inc(c);
		found := paramstr(c)=p;
	until found or (paramstr(c)='');
	Result := found;
end; {ExistParam}

//Find position of given command switch
function LocateParam(p:string): integer;
var c: integer;
var found: boolean;
begin
	c := 0; found := false;
	repeat
		inc(c);
		found := paramstr(c)=p;
	until found or (paramstr(c)='');
	if found then result := c else result := 0;
end;{LocateParam}

//Return the value of a given command switch
function GetParam(p:string): string;
begin
	if ExistParam(p) then begin
		Result := paramstr(LocateParam(p)+1);
		if result[1] = '-' then result := '';
	end else
		Result := '';
end; {GetParam}

//Get rng seed from the command line, if present
function GetSeed: longint;
begin
	if ExistParam('-s') then
		Result := StrToInt(GetParam('-s'))
	else
		Result := 0;
end;



//Get key-phrase from the command line, if present
function GetKeyphrase: string;
begin
	try
		Result := GetParam('-k');
	except
		Result := '';
	end;
end;


//Get ciphertext from the command line, if present
function GetCiphertext: string;
begin
	try
		Result := GetParam('-d');
	except
		Result := '';
	end;
end;

//Get plaintext from the command line, if present
function GetPlaintext: string;
begin
	try
		Result := GetParam('-e');
	except
		Result := '';
	end;
end;

//Get input filename from the command line, if present
function GetFile: string;
begin
	Result := GetParam('-f');
end;


//Get input filename from the command line, if present
function GetFileT: string;
begin
	Result := GetParam('-F');
end;

// Get file path
function GetPath: string;
begin
	Result := GetParam('-p');
end;

//Get real OTP filename from the command line, if present
function GetOTP: string;
begin
	Result := GetParam('-o');
end;

//Get ciphertext authentication code
function GetAuth: string;
begin
	Result := GetParam('-a');
end;

//Get grouping for letter groups
function GetGrouping: Cardinal;
var r : string;
	g : byte;
begin
	result := 4;
	r := GetParam('-g');
	if r <> '' then
		try
			g := StrToInt(r);
			if g in [3..9] then
				result := g
			else result := 4;
		except
			result := 4;
		end;
end;


//Get modulo (1=26, 2=95, 3=128 etc) from the command line, if present
function GetModulo: integer;
var r: integer;
begin
	try
		r := StrToInt(GetParam('-m'));
		case r of
			1 : begin modulo := 26; start := 'A'; end;
			2 : begin modulo := 95; start := ' '; end;
			3 : begin modulo := 128;start := chr(0); hexadecimal:=true;end;
			4 : begin modulo := 256;start := chr(0); hexadecimal:=true;end;
		else
			begin modulo := 26; start := 'A'; end;
		end;
	except
		begin modulo := 26; start := 'A'; end;
	end;
	result := modulo;
end;


//Get mode (encipher or decipher) from the command line, if present
function GetMode: TMCipher;
begin
	if ExistParam('-e') then Result := mEncipher else
	if ExistParam('-d') then Result := mDecipher else
	Result := mNone;
end;


// Get number of passes for file delete
function GetPasses: CARDINAL;
begin
	result := StrToInt(GetParam('-D'));
end;


// Delete original after file-encryption?
// <optionally followed by number of passes>
function doDelete: boolean;
begin
	result := ExistParam('-D');
end;


// emit the current SES version and build
function doVersion: boolean;
begin
	result := ExistParam('-V');
end;


//Will input and output be hexadecimal?
// <essential at modulo 128 and higher>
function doHex: boolean;
begin
	Result := ExistParam('-H');
end;

//Are we timing operations?
function doTiming: boolean;
begin
	Result := ExistParam('-t');
end;

//Are we displaying options screen?
function doOptions: boolean;
begin
	Result := ExistParam('-h');
end;

//Are we displaying help screen?
function doHelp: boolean;
begin
	Result := ExistParam('-help');
end;

//Are we displaying info screen?
function doInfo: boolean;
begin
	Result := ExistParam('-i');
end;

//Are we running in verbose mode?
function doVerbose: boolean;
begin
	Result := ExistParam('-v');
end;

//Are we doing a real OTP cipher?
function doOTP: boolean;
begin
	Result := ExistParam('-o');
end;

//Are we checking out the SES Challenge?
function doChallenge: boolean;
begin
	Result := ExistParam('-c');
end;

//Are we checking GPL Warranty?
function doWarranty: boolean;
begin
	Result := ExistParam('-w');
end;

//Are we checking GPL Terms?
function doConditions: boolean;
begin
	Result := ExistParam('-W');
end;

// Are we displaying acknowledgments?
function doThanks: boolean;
begin
	Result := ExistParam('-A');
end;

//Are we displaying log abbreviations legend?
function doLegend: boolean;
begin
	Result := ExistParam('-L');
end;

// Are we enciphering or deciphering a file of bytes?
function doFile: boolean;
begin
	Result := ExistParam('-f');
end;

// Are we super-enciphering or deciphering a text file?
function doFileT: boolean;
begin
	Result := ExistParam('-F');
end;

// Are we outputting letter groups?
function doGroups: boolean;
begin
	Result := ExistParam('-g');
end;

//Are we including ciphertext authentication?
function doAuth: boolean;
begin
	Result := ExistParam('-a');
end;

//Are there no parameters specified?
function noParams: boolean;
begin
	Result := paramstr(1)='';
end;

//The Key to SES, life, the Universe, and everything
function doMagic: boolean;
begin
	Result := ExistParam('-K');
end;


initialization

finalization

end.
