{$MODE DELPHI}
{$H-}

unit MyStrUtils;

{ A GENERAL-PURPOSE UNIT COMPRISING OPERATIONS ON PASCAL-STYLE STRINGS }
{ & operations which convert from one system to another, especially    }
{ those having equivalent in BASIC and PHP }

INTERFACE

// Array holds letter frequencies A..Z = 0..25
Type TFreqArray=array[0..25] of cardinal;

// convert a hexadecimal string to an ASCII string	
FUNCTION hex2ascii(s: ANSISTRING): ANSISTRING;
// convert an ASCII string to a hexadecimal string
FUNCTION ascii2hex(s: ANSISTRING): ANSISTRING;
// More... a paged display of any text file
function More(f: STRING; LinesUsed: INTEGER): BOOLEAN;
// Check if a string contains any uppercase characters
function hasUppercase(s: string): boolean;
// Check if a string contains any lowercase characters
function hasLowercase(s: string): boolean;
// Check if a string is all uppercase
function isUppercase(s: string): boolean;
// Check if a string is all lowercase
function isLowercase(s: string): boolean;
// Check if a string is mixed-case
function isMixedCase(s: string): boolean;
// Boolean to string ('true'/'false')
function BoolStr(bool:boolean): string;
// Get the numeric product of letters in string
function TextProd(s:string): QWord;
// Count occurrences of given char in string
function CharCount(str: AnsiString; ch: char): longint;
// Return an array of letter-frequencies for given string
function CharFreqs(str: AnsiString): TFreqArray;
function nearest(figure,number:longint): longint;
function bmax( int1,int2 : byte   ) : byte;
function imax( int1,int2 : integer) : integer;
function rmax( float1, float2 : real ): real;
function iMin( int1, int2 : longint ): longint;
function rmin( float1, float2 : real ): real;
function IntToStr( I:longint ) : string;
function FloatToStr( F:extended ; const digits,dplaces : byte ) : string;
function PtoLong( ptr: pointer) : longint;{ convert pointer to 32 bit Long }
function LongToP( lng : longint): pointer;{ convert longint to pointer }
function IntVal(s:string) : longint;      { Like BASIC's VAL() for integers }
function CardVal(s:string) : cardinal;
function RealVal(s:string) : Extended;    { Like BASIC's VAL() for reals }
function Len(x: string): byte;            { Identical to BASIC's LEN() }
function Midstr(source: string; position, sublen: word): string;
function RightStr(str:string; pos: byte): string;
function LeftStr(str:string; pos: byte): string;
function RightAnsi(str:Ansistring; pos: cardinal): Ansistring;
function LeftAnsi(str:AnsiString; pos: cardinal): AnsiString;
function UpperStr(var s:string): string;
function Headstr(source,separator:string): string;
FUNCTION HeadAnsi(source,separator:Ansistring): Ansistring;
function Tailstr(source,separator:string): string;
FUNCTION TailAnsi(source,separator:Ansistring): Ansistring;
{ Get Nth element in a [sep]-delimited string }
function Element(num: integer; source, sep: AnsiString): AnsiString;
procedure HeadfromTail(var H,T: string; Sep: string);
procedure HeadfromTailAnsi(var H,T: AnsiString; Sep: AnsiString);
function ContainsChar(source: AnsiString; ch: char): boolean;
{ Excise toExtract from Source, returning source sans toExtract }
function  ExtractStr(source,toExtract: string): string;
function Args(expression,Obracket,Cbracket: string): string;
function InStr(source,substr: string; from: byte) : byte;

function SpaceStr(num: byte): string;
function StringStr(numchars: byte; char: char): string; { Bas STRING$ }

function ReverseAnsi(inputStr: AnsiString):AnsiString;

function FormatStr(str: string; const spaces: word; padchar: char): string;
function MouseToTextline(mseY : integer; const textHeight: byte): byte;

function Capitalise(str : string): string;  { capitalise initial }
function th(number: longint): string;  { -th, -nd, -rd, -st etc }
function Plural(num: extended; str: string): string; { add ~s if applicable }
function Times(num: longint): string; { 'once','twice','3 times' etc }

procedure iSwap( var value1, value2 : word );   { swap integers }
procedure bSwap( var value1, value2 : byte );   { swap bytes }
procedure lSwap( var value1, value2 : longint );{ swap longs }
procedure sSwap( var value1, value2 : string ); { swap strings }
procedure cswap( var value1, value2 : char );	{ swap chars}

function  Exist(FileName: String): Boolean;
procedure Open(IOstr: string; var handle: text; filename: string);

function  TextSum(txt: string): longint;
{ Output text with trailing spaces to given length }
function  TextTab(Txt: string; tab: word): string;

{ Conversion routines }

function  mm2inches(mm:extended): extended;
function inches2mm(inches:extended): extended;

function DaysInMonth(month,year:integer):integer;



IMPLEMENTATION

uses SysUtils, StrUtils, Classes, Print;

CONST PUNCTUATION 	= [' ',',','.',';',':','-','+','*','/','\','%','&','(',')','{','}','[',']','<','>'];
CONST NUMERIC		= ['0'..'9'];
CONST UCASE 		= ['A'..'Z'] + NUMERIC + PUNCTUATION;
CONST LCASE 		= ['a'..'z'] + NUMERIC + PUNCTUATION;
CONST MIXEDCASE 	= LCASE + UCASE+ NUMERIC+ PUNCTUATION;


// convert a hexadecimal string to an ASCII string	
FUNCTION hex2ascii(s: ANSISTRING): ANSISTRING;
	VAR h,t: ANSISTRING;
		l: CARDINAL;
	BEGIN
		t := s;
		TRY
			REPEAT
				l := Length(t);
				h := LeftAnsi(t,2);
				t := RightAnsi(t,l-2);
				result += chr(Hex2Dec(h));
			UNTIL l<=2;
		EXCEPT
			result := result;
		END;
	END;

// convert an ASCII string to a hexadecimal string
FUNCTION ascii2hex(s: ANSISTRING): ANSISTRING;
	VAR i,l: CARDINAL;
	BEGIN
		result := '';
		TRY
			l := Length(s);
			FOR i := 1 TO l DO
				result += Dec2Numb(ord(s[i]),2,16);
		EXCEPT
			result := result;
		END;
	END;

	
// Check if a string contains any uppercase characters
function hasUppercase(s: string): boolean;
	var i,l: integer;
		yes: boolean = false;
begin
	l := length(s);
	for i := 1 to l do begin
		if (not (s[i] in NUMERIC)) and (not (s[i] in PUNCTUATION)) then
			yes := (s[i] in UCASE);
		if yes then BREAK;
	end;
	result := yes;
end;


// Check if a string contains any lowercase characters
function hasLowercase(s: string): boolean;
	var i,l: integer;
		yes: boolean = false;
begin
	l := length(s);
	for i := 1 to l do begin
		if (not (s[i] in NUMERIC)) and (not (s[i] in PUNCTUATION)) then
			yes := (s[i] in LCASE);
		if yes then BREAK;
	end;
	result := yes;
end;


// Check if a string is mixed-case
function isMixedCase(s: string): boolean;
	begin
		result := hasUppercase(s) and hasLowercase(s);
	end;
	
	
// Check if a string is all uppercase
function isUppercase(s: string): boolean;
	var i,l: integer;
begin
	result := true;
	l := length(s);
	for i := 1 to l do
		if (not (s[i] in NUMERIC)) and (not (s[i] in PUNCTUATION)) then
			result := result AND (s[i] in UCASE);
end;


// Check if a string is all lowercase
function isLowercase(s: string): boolean;
	var i,l: integer;
begin
	result := true;
	l := length(s);
	for i := 1 to l do
		if (not (s[i] in NUMERIC)) and (not (s[i] in PUNCTUATION)) then
			result := result AND (s[i] in LCASE);
end;


// More... a paged display of any text file
FUNCTION More(f: STRING; LinesUsed: INTEGER): BOOLEAN;
	VAR i,l,t	: INTEGER;
		M		: TStringList;
		TH		: INTEGER = 18;
	BEGIN
		TRY
			// the file must reside
			// in the src or installation directory
			M := TStringlist.Create;
			M.LoadFromFile(f);
			l := TH-LinesUsed; t := 0;
			FOR i:=0 TO M.Count-1 DO BEGIN
				// determine how many lines will be written
				t := PrintS(M[i]);
				// record number of lines silently written
				l += t;
				IF (l > TH) THEN BEGIN 
					Write('[ MORE... ENTER ]'); Readln;
					Writeln;
					// reset number of lines
					l := 0; t := 0;
				END;
				t := PrintLn(M[i]);
			END;
			M.Free;
			Write('[ END... ENTER ]'); Readln;
			Writeln;
		EXCEPT
			M.Free;
			result := false;
			EXIT;
		END;
		result := true
	END;
	
	
function boolstr(bool:boolean): string;
begin
	if bool then result:='true' else result:='false';
end; // BoolStr


function DaysInMonth(month,year:integer):integer;
var days: integer;
begin
    Case month of
     1,3,5,7,8,10,12: Days := 31;
     2,4,6,9,11: Days := 30;
    end;

    If month = 2 then Days:=days-2;

    If ((Year mod 100)=0) or
        ((Year mod 1000)=0) or
        ((Year mod 4)=0) then
         if month = 2 then Inc(Days);

    Result := Days;
end;


// Get the numeric product of letters in string
function TextProd(s:string): QWord;
var c,l: cardinal;
	tpq: Qword;
	str: string;
begin
	// Get the product of letters in s
	str := s;
	l := Len(str);
	tpq:=1;
	for c := 1 to l do
		tpq *= Qword(ord(str[c]));
	result := tpq;
end; {TextProd}	


// count occurrences of given char in string
function CharCount(str: AnsiString; ch: char): longint;
var n,l,count: longint;
	temp: string;
begin
	temp:= uppercase(str);
	l := len(temp);
	count := 0;
	for n:=1 to l do
		if temp[n]=ch then inc(count);
	Result:=count;
end; {CharCount()}


// Returns an array of letter-frequencies for given string
// Assumes uppercase (string converted in CharCount()
function CharFreqs(str: AnsiString): TFreqArray;
var n: cardinal;
	f: TFreqArray;
	s: AnsiString;
begin
	s := str;
	for n := 0 to 25 do
		f[n] := CharCount(s,chr(ord('A')+n));
	Result := f;
end; {CharFreqs}



function ContainsChar(source: AnsiString; ch: char): boolean;
var n,l: longint;
    s: AnsiString;
    found: boolean;
begin
    found := false;
    s:= source;
    if s<>'' then begin
     l:= Length(s);
     n:=-1;
     repeat
      inc(n);
      found := s[n]=ch;
     until  ((found) or (n=l));
    end; 
    Result := found;
end;


function  mm2inches(mm:extended): extended;
begin
     Result := mm*0.03937008;
end;

function inches2mm(inches:extended): extended;
begin
    Result := inches*25.4;
end;



function  TextTab(Txt: string; tab: word ): string;
{ Output text padded with chars to given tab length }
var length,tot: word;
    text   : string;
begin
   length := len(Txt);
   if length>tab then text := leftStr(Txt,tab) else text := Txt;
   length := len(text);
   tot := tab-length;
   TextTab := Text+SpaceStr(tot);
end; { TextTab() }


function TextSum;
{ sum ascii chars in string }
var str: string;
    count,n:   integer;
    sum: longint;
begin
    str := txt;
    n   := len(str);
    sum := 0;
    for count := 1 to n do
        sum := sum + ord(str[count]);
    TextSum := sum;
end; { TextSum }



function Plural;
begin
     If (abs(num)>1) or (num=0) then
       Plural := str + 's'
     else
       Plural := str;

end; { Plural }

function Times;
begin
     Case num of
      1: Times := 'once';
      2: Times := 'twice';
     else
         Times := IntToStr(num)+' times';
     end; {Case}
end; { Times }


function UpperStr(var s:string): string;
{**************************************}
var str   : string;
    len,i : byte;

begin

len    := Length(s);
str[0] := chr(len);

for i := 1 to Len do

  str[i] := UpCase(s[i]);


  UpperStr := str;

end; { UpperStr() }




function SpaceStr(num: byte): string;
{***********************************}
var count : byte;
    str   : string;

begin

     str[0] :=  chr(num);

     for count := 1 to num do
                  str[count] := ' ' ;

     SpaceStr := str;
end;



procedure cswap( var value1, value2 : char );
{Swap two ASCII chars}
var interim1 : char;
begin
     interim1 := value1 ;
     value1 := value2 ;
     value2 := interim1 ;
end; { cSwap() }


procedure iSwap( var value1, value2 : word );   { swap integers }
{******************************************}
var interim1 : word;

begin
     interim1 := value1 ;
     value1 := value2 ;
     value2 := interim1 ;

end; { iSwap() }


procedure bSwap( var value1, value2 : byte );   { swap bytes }
{******************************************}
var interim1 : byte;

begin
     interim1 := value1 ;
     value1 := value2 ;
     value2 := interim1 ;

end; { bSwap() }


procedure lSwap( var value1, value2 : longint );   { swap integers }
{**********************************************}
var interim1 : longint;

begin
     interim1 := value1 ;
     value1 := value2 ;
     value2 := interim1 ;

end; { lSwap() }


procedure sSwap( var value1, value2 : string );   { swap strings }
{********************************************}
var interim1 : string;

begin
     interim1 := value1 ;
     value1 := value2 ;
     value2 := interim1 ;

end; { sSwap() }




function Exist(FileName: String): Boolean;
{****************************************}
{ Returns True if file exists, False if not }
var
 F: file;

begin
 {$I-}
 Assign(F, FileName);
 FileMode := 0;  { Set file access to read only }
 Reset(F);
 Close(F);
 {$I+}
 Exist := (IOResult = 0) and (FileName <> '');

end;  { Exist }





procedure Open(IOstr: string; var handle: text; filename: string);
{****************************************************************}
{ implement BASIC style file ops }

var fname : string;

begin

     if ioSTR = 'o' then begin

        Filemode := 1 ;

        fname := filename;
        Assign(handle,fname);


        If Exist(fname) then begin

           Rewrite(handle);

           end
           else begin

           Rewrite(handle);

        end; { if Exist()...else..}


     end
     else if ioSTR = 'i' then begin

          Filemode := 0 ;

          fname := filename;
          Assign(handle,fname);


          If Exist(fname) then begin

             Reset(handle);

          end
          else begin

               Rewrite(handle);

     end; { if Exist()...else..}


    end
    else begin

     Filemode := 2;
     fname := filename;
     Assign(handle,fname);


     If Exist(fname) then begin

        Rewrite(handle);

     end
     else begin

        Rewrite(handle);

     end; { if Exist()...else..}


    end; { if ioStr..}


end; { Open() }







function th{(number: integer): string};  { -th, -nd, -rd, -st etc }
{***********************************}
var numStr   : string[5];
    num      : longint;
    digit    : char;
    digiStr  : string[1];
    strip    : string[2];
    stripnum : integer;

begin
    num    := number;
    numStr := IntToStr(num);

    if num > 9 then begin
      strip   := rightStr(numStr,2);
      stripnum:= Intval(strip);
    end;

    digiStr := rightStr(numStr,1);
    digit   := digiStr[1];

    Case digit of

         '1'   : th := 'st' ;
         '2'   : th := 'nd' ;
         '3'   : th := 'rd' ;
         else    th := 'th' ;
    end;


    If stripnum in [11..13] then           { exceptions to above rule }

     Case (stripnum mod 10) of

         1    : th := 'th' ;
         2    : th := 'th' ;
         3    : th := 'th' ;

     end; { case..}

end; { th() }





function nearest(figure,number:longint): longint;
{***********************************************}
{ return number to the nearest figure(th) }
var n : longint;

begin

    IF figure<= 0 then figure := 1;

     n :=  number - (number mod figure);

     if n=0 then nearest := number else nearest := n;

end;





function bmax( int1,int2 : byte) : byte;
(********************************************)
 begin
     if int1>int2 then bmax:=int1 else bmax:=int2;
 end;



function imax( int1,int2 : integer) : integer;
(********************************************)
 begin
     if int1>int2 then imax:=int1 else imax:=int2;
 end;



function rmax( float1, float2 : real ): real;
(*******************************************)
begin
    if float1>float2 then rmax:=float1 else rmax:=float2;
end;


function rmin( float1, float2 : real ): real;
(*******************************************)
begin
    if float1<float2 then rmin:=float1 else rmin:=float2;
end;


function iMin( int1, int2 : longint ): longint;
(*******************************************)
begin
    if int1<int2 then iMin:=int1 else iMin:=int2;
end;



function IntToStr( I:longint ) : string;
(**************************************)
var S : string[11];

begin
     Str(I,S);
     IntToStr := S ;
end;



function FloatToStr( F:extended ; const digits,dplaces : byte ) : string;
(***************************************************************)
var S : string;

begin

     Str(F :digits :dplaces ,S);

     FloatToStr := S ;
end;



function PtoLong(ptr : pointer): longint;
{***************************************}
var long      : longint;
    point     : pointer;

begin
      point := ptr;
    asm
       mov dx,word ptr [point]
       mov es,word ptr [point+2]
       mov word ptr [long],dx
       mov word ptr [long+2],es
    end;

    PtoLong := long;
end;





function LongToP(lng : longint): pointer;
{***************************************}
var long      : longint;
    point     : pointer;

begin
      long := lng;
    asm
       mov dx,word ptr [long]
       mov es,word ptr [long+2]
       mov word ptr [point],dx
       mov word ptr [point+2],es
    end;

    LongToP := point;
end;


function CardVal(s:string) : cardinal;      { Like BASIC's VAL() for integers }
{**********************************}
var temp : cardinal;
    code : integer;

begin
     val(s,temp,code);

     if (code<>0) then CardVal:=0 else CardVal:=temp;


end; {CardVal() }



function IntVal(s:string) : longint;      { Like BASIC's VAL() for integers }
{**********************************}
var temp : longint;
    code : integer;

begin
     val(s,temp,code);

     if (code<>0) then IntVal:=0 else intVal:=temp;


end; {intval() }





function RealVal(s:string) : Extended;      { Like BASIC's VAL() for reals }
{************************************}
var temp : extended;
    code : integer;

begin
     val(s,temp,code);

     if (code<>0) then RealVal:=0.0 else RealVal:=temp;


end; {intval() }






function Len(x: string): byte;      { Identical to BASIC's LEN() }
{*****************************}

begin
      Len:=ord(x[0]);

end; { Len() }



function ReverseAnsi;
var backw,forw: Ansistring;
    length, place: integer;
begin
    forw := '';
	Backw := InputStr;
    length  := Len(backw);
    for place := length downto 1 do
     forw:=forw+backw[place];

   Result := forw;
end;


function Midstr(source: string; position, sublen: word): string;
{**************************************************************}

begin
  MidStr :=  Copy(Source,position,sublen);
end; {midstr}




function RightStr(str:string; pos: byte): string;
{************************************************}

var length,start     : integer;
    count            : byte;
    NewStr           : string;

begin

      length:=ord(str[0]);
      NewStr:='';
      start:=(length-pos)+1;

      for count := start to length do
                   NewStr:=NewStr+str[count];

      RightStr:=NewStr;

end; {function RightSTR()}



function RightAnsi(str:Ansistring; pos: cardinal): Ansistring;
{************************************************}

var len,start     : integer;
    count            : integer;
    NewStr           : Ansistring;

begin

      len:=length(str);
      NewStr:='';
      start:=(len-pos)+1;

      for count := start to len do
                   NewStr:=NewStr+str[count];

      Result:=NewStr;

end; {function RightAnsi()}




function LeftStr(str:string; pos: byte): string;
{************************************************}

var
    count,len        : byte;
    NewStr           : string;

begin

      len:=ord(str[0]);

      if pos>len then pos:=len;

      NewStr:='';

      for count := 1 to pos do
                   NewStr:=NewStr+str[count];

      LeftStr:=NewStr;

end; {function LeftSTR()}





function LeftAnsi(str:Ansistring; pos: cardinal): Ansistring;
{************************************************}

var
    count,len        : integer;
    NewStr           : Ansistring;

begin

      len:=length(str);

      if pos>len then pos:=len;

      NewStr:='';

      for count := 1 to pos do
                   NewStr:=NewStr+str[count];

      Result:=NewStr;

end; {function LeftAnsi()}



procedure HeadfromTail(var H,T: string; Sep: string);
{***************************************************}
begin
   H := HeadStr(T,Sep);
   T := TailStr(T,Sep);
end; { HeadfromTail }


procedure HeadfromTailAnsi(var H,T: AnsiString; Sep: AnsiString);
{*************************************************************}
begin
   H := HeadAnsi(T,Sep);
   T := TailAnsi(T,Sep);
end; { HeadfromTail }


{ Nth element in [sep]-delimited AnsiString }
function Element(num: integer; source, sep: AnsiString): AnsiString;
{******************************************************************}
var el,H,T: AnsiString;
    x: integer;
begin
     T := source;
     for x := 1 to num do begin
       HeadFromTailAnsi(H,T,sep);
       el := H;
     end; {for..}
     Result := el;
end; { Element() }



FUNCTION HeadStr(source,separator:string): string;
{**************************************************}
var hStr     : string;
    position : byte;

begin

  position := pos(separator,source);

  IF position > 0 then begin
    DEC( position );
    hStr := LEFTStr(source,position);
  end ELSE
    hStr := source;
  { if position }

  HeadStr := hStr;

end; {HeadStr}



FUNCTION HeadAnsi(source,separator:Ansistring): Ansistring;
{**************************************************}
var hStr     : Ansistring;
    position : integer;

begin

  position := pos(separator,source);

  IF position > 0 then begin
    DEC( position );
    hStr := LEFTAnsi(source,position);
  end ELSE
    hStr := source;
  { if position }

  Result := hStr;

end; {HeadAnsi}




FUNCTION TailStr(source,separator:string): string;
{*************************************************}
 var length, position, offset: byte;
     tStr                    : string;

 begin

  tStr := ''; { initialise }
  length := LEN(source) ;
  offset := LEN(separator) ;
  position := POS(separator,source);

  IF position > 0 then begin
    DEC( position );
    position := position + offset;
    length   := length - position;
    tStr := RIGHTStr(source,length);
  end;

  TailStr := tStr;

end; {TailStr}


FUNCTION TailAnsi(source,separator:Ansistring): Ansistring;
{*************************************************}
 var len, position, offset: integer;
     tStr                    : Ansistring;

 begin

  tStr := ''; { initialise }
  len := LENGTH(source) ;
  offset := LENGTH(separator) ;
  position := POS(separator,source);

  IF position > 0 then begin
    DEC( position );
    position := position + offset;
    len   := len - position;
    tStr := RIGHTAnsi(source,len);
  end;

  Result := tStr;

end; {TailAnsi}



function ExtractStr;
var h,t: string;
begin
    t := Source;
    HeadFromTail(h,Source,toExtract);
    ExtractStr := h+t;
end;


function Args;
{ extract argument-string from bracketed expression }
var Argstr: string;

begin
     ArgStr := TailStr(Expression,Obracket);
     ArgStr := HeadStr(ArgStr,CBracket);

     Args := ArgStr;

end; { Args() }




function InStr(source,substr: string; from: byte) : byte;
{*******************************************************}
{ Identical to BASIC's INSTR() function }

var len{,start}             : byte;
    possible                : byte;
    impossible,found        : boolean;

begin
      impossible:=true;
      len := ord(substr[0]);
      // start := from;

    repeat
          possible:=Pos(source,substr[1]); {,start); }

          impossible:=(possible=0);

          if possible>0 then begin
             found := midstr(source,possible,len)=substr;
             // start := possible;
          end; {if..}

    until impossible or found;

    instr:=possible;

end; { InStr() }






function FormatStr(str: string; const spaces: word; padchar: char): string;
{*************************************************************************}
var length,numspaces   : word;
    padstr,tmpstr      : string;

label  endofSformat;

begin
    tmpstr := str;
    padstr := '' ;
    length := len(tmpstr);

    if length >= spaces then goto endofSformat;

    numspaces := spaces - length;

    if numspaces <= 0 then goto endofSformat;

    for length := 1 to numspaces do
               padstr := padstr+padchar;


endofSformat:

             FormatStr := padstr+tmpstr;

end; { SFormatStr() }




function MouseToTextline(mseY : integer; const textHeight :byte): byte;
{***************************************************************}

begin

     MouseToTextline :=  mseY  div textHeight ;


end; { MouseToTextline() }





function Capitalise{(str : string): string};  { capitalise initial char}
{********************************************}
var ch   : char;
    tStr : string;

begin
    tStr := str;
    ch   := UpCase(tStr[1]);
    tStr[1] := ch;

    Capitalise := tStr;

end;  { Capitalise() }




function StringStr(numchars: byte; char: char): string;
{**********************************************}
var count : word;
    str   : string;

begin
     str := '';
     for count := 1 to numchars do
         str := str+char;

     StringStr := str;
end; { StringStr() }







end.
