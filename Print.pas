{$mode delphi}
// PrintLn(): Formatted line output using the greedy algorithm
// Explode(): PHP-like Explode function - string to array, on delimiter
UNIT Print;

INTERFACE

TYPE  	TStringArray = ARRAY OF STRING;
		TCharSet	 = SET OF CHAR;
CONST	Space		 : TCharSet = [' '];
		Punctuation	 : TCharSet = ['.',',',';',':'];
// Standard terminal width
CONST	TW			 = 80;		


// Text output formated to standard terminal width - with line-feed
// <returns number of lines written>
FUNCTION PrintLn(s: STRING): INTEGER;
// Text output formated to standard terminal width - no line-feed
// <returns amount of space left on line>
FUNCTION PrintL(s: STRING): INTEGER;
// Text output formatted to specified line-width
// <returns number of lines written>
FUNCTION PrintW(s: STRING; LineWidth: INTEGER): INTEGER;
// Text output formated to standard terminal width
// <for continuing a partial line - space left passed in sp>
FUNCTION PrintLp(s: STRING; sp: INTEGER): INTEGER;
// "Silent" PrintLn - with line-feed - no output
// for determining how many lines _would_ be written
// <returns number of lines silently written>
FUNCTION PrintS(s: STRING): INTEGER;
// Break a string up into an array on a delimiter - like PHP's Explode()
FUNCTION  Explode(delimiter: CHAR; str: STRING): TStringArray;


IMPLEMENTATION

USES SysUtils, StrUtils;

// Text output formated to standard terminal width
// for continuing a partial line - space left passed in sp
FUNCTION PrintLp(s: STRING; sp: INTEGER): INTEGER;
	VAR SpaceLeft : INTEGER;
		Wrd		  : STRING;
		SpaceWidth: INTEGER = 1;
		i		  : INTEGER;
		arr		  : TStringArray;
	BEGIN
		arr := Explode(' ', s);
		SpaceLeft := sp;
		// we usually need a space before the continuation
		Write(' ');
		FOR i := 1 TO Length(arr)-1 DO BEGIN
			Wrd := arr[i];
			IF (Length(Wrd) + SpaceWidth) > SpaceLeft THEN BEGIN
				//insert line break before Word in Text
				Writeln;
				SpaceLeft := TW - Length(Wrd)
			END	ELSE BEGIN
				// insert spaces, except before first word
				IF i>1 THEN Write(' ');
				SpaceLeft := SpaceLeft - (Length(Wrd) + SpaceWidth);
			END;
			Write(Wrd);
		END;
		Writeln;
		result := SpaceLeft;
	END;

	
// Text output formated to standard terminal width
// <returns number of lines written>
FUNCTION PrintLn(s: STRING): INTEGER;
	VAR SpaceLeft : INTEGER;
		Wrd		  : STRING;
		SpaceWidth: INTEGER = 1;
		i,l		  : INTEGER;
		arr		  : TStringArray;
	BEGIN
		arr := Explode(' ', s);
		SpaceLeft := TW;
		// reset number of lines written
		l := 0;
		FOR i := 1 TO Length(arr)-1 DO BEGIN
			Wrd := arr[i];
			IF (Length(Wrd) + SpaceWidth) > SpaceLeft THEN BEGIN
				//insert line break before Word in Text
				Writeln;
				// increment number of lines written
				inc(l);
				SpaceLeft := TW - Length(Wrd)
			END	ELSE BEGIN
				// insert spaces, except before first word
				IF i>1 THEN Write(' ');
				SpaceLeft := SpaceLeft - (Length(Wrd) + SpaceWidth);
			END;
			Write(Wrd);
		END;
		Writeln;
		inc(l);
		result := l;
	END;

	
// "Silent" PrintLn - with line-feed - no output
// for determining how many lines _would_ be written
// <returns number of lines silently written>
FUNCTION PrintS(s: STRING): INTEGER;
	VAR SpaceLeft : INTEGER;
		Wrd		  : STRING;
		SpaceWidth: INTEGER = 1;
		i,l		  : INTEGER;
		arr		  : TStringArray;
	BEGIN
		arr := Explode(' ', s);
		SpaceLeft := TW;
		// reset number of lines written
		l := 0;
		FOR i := 1 TO Length(arr)-1 DO BEGIN
			Wrd := arr[i];
			IF (Length(Wrd) + SpaceWidth) > SpaceLeft THEN BEGIN
				//insert line break before Word in Text
				// Writeln;
				// increment number of lines written
				inc(l);
				SpaceLeft := TW - Length(Wrd)
			END	ELSE BEGIN
				// insert spaces, except before first word
				// IF i>1 THEN Write(' ');
				SpaceLeft := SpaceLeft - (Length(Wrd) + SpaceWidth);
			END;
			// Write(Wrd);
		END;
		// Writeln;
		inc(l);
		result := l;
	END;


// Text output formated to standard terminal width - no line-feed
FUNCTION PrintL(s: STRING): INTEGER;
	VAR SpaceLeft : INTEGER;
		Wrd		  : STRING;
		SpaceWidth: INTEGER = 1;
		i		  : INTEGER;
		arr		  : TStringArray;
	BEGIN
		arr := Explode(' ', s);
		SpaceLeft := TW;
		FOR i := 1 TO Length(arr)-1 DO BEGIN
			Wrd := arr[i];
			IF (Length(Wrd) + SpaceWidth) > SpaceLeft THEN BEGIN
				//insert line break before Word in Text
				Writeln;
				SpaceLeft := TW - Length(Wrd)
			END	ELSE BEGIN
				// insert spaces, except before first word
				IF i>1 THEN Write(' ');
				SpaceLeft := SpaceLeft - (Length(Wrd) + SpaceWidth);
			END;
			Write(Wrd);
		END;
		// no line-feed
		result := SpaceLeft;
	END;

	
// Text output formatted to specified line-width
FUNCTION PrintW(s: STRING; LineWidth: INTEGER): INTEGER;
	VAR SpaceLeft : INTEGER;
		Wrd		  : STRING;
		SpaceWidth: INTEGER = 1;
		i,l		  : INTEGER;
		arr		  : TStringArray;
	BEGIN
		arr := Explode(' ', s);
		SpaceLeft := LineWidth;
		l := 0;
		FOR i := 1 TO Length(arr)-1 DO BEGIN
			Wrd := arr[i];
			IF (Length(Wrd) + SpaceWidth) > SpaceLeft THEN BEGIN
				//insert line break before Word in Text
				Writeln;
				inc(l);
				SpaceLeft := LineWidth - Length(Wrd)
			END	ELSE BEGIN
				// insert spaces, except before first word
				IF i>1 THEN Write(' ');
				SpaceLeft := SpaceLeft - (Length(Wrd) + SpaceWidth);
			END;
			Write(Wrd);
		END;
		Writeln;
		result := l;
	END;

	
// Break a string up into an array on delimiter - like PHP's Explode()
// <first word is in array[1] and array[length] is empty>
FUNCTION Explode(delimiter: CHAR; str: STRING): TStringArray;
	var k  : TStringArray;
		l,c: INTEGER;
		d  : TCharSet;
	BEGIN
		d := [delimiter];
		l := WordCount(str,d);
		SetLength(k,l+1);
		//Extract words and place them in array
		FOR c := 1 TO l DO
			k[c] := ExtractWord(c,str,d);
		Result:= k;
	END;


// Break a string up into an array on delimiter - like PHP's Explode()
// with variable-length delimiter string
// <needs work - sometimes causes AccessViolation>
FUNCTION Explode0(delimiter: STRING; str: STRING; limit: INTEGER): TStringArray;
	VAR  p,cc,dsize:integer;
	BEGIN
		cc := 0;
		dsize := length(delimiter);
		IF dsize = 0 THEN BEGIN
			SetLength(result,1);
			result[0] := str;
			EXIT;
		END;
		WHILE cc+1 < limit DO BEGIN
			p := Pos(delimiter,str);
			IF p > 0 THEN BEGIN
				INC(cc);
				SetLength(result,cc);
				result[cc-1] := copy(str,1,p-1);
				Delete(str,1,p+dsize-1);
			END ELSE BREAK;
		END;
		INC(cc);
		SetLength(result,cc);
		result[cc-1] := str;
	END;       


END.
