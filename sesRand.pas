{$mode delphi}
{ $define I64} // are we using Isaac64?

// re-usable CSPRNG pool objects for SES
// plus a _true_ one-time pad object 
UNIT sesRand;

INTERFACE

USES sesTypes, Classes;

TYPE
	// a re-usable pool of pseudo-random numbers
	// (cryptographically secure ones)
	TRandPool	= OBJECT
		PUBLIC
			PROCEDURE 	Fill;
			PROCEDURE	Reset;
			// Zeroise the Random pool
			PROCEDURE	Clear;
			FUNCTION	Next: CARDINAL;
			FUNCTION	Get(i: CARDINAL): CARDINAL;
		PRIVATE
			Pool : TRandPoolA;
			// index into pool
			PIdx : CARDINAL;
	END;
	
	
	// a re-usable pool of pseudo-"one-time pad" characters
	// (cryptographically secure ones)
	TOTPPool	= OBJECT
		PUBLIC
			PROCEDURE 	Fill;
			// Zeroise the Random pool
			PROCEDURE	Clear;
			FUNCTION	Next:  CHAR;
			FUNCTION	Get(i: CARDINAL): CHAR;
		PRIVATE
			Pool : TOTPPoolA;
			// index into pool
			PIdx : CARDINAL;
	END;

	
	// a usable pool of REAL one-time pad characters
	// collected in a file from somewhere like Random.org
	// <format: one number in {0..25} per line>
	// <preferably at least 10,000 numbers per file>
	TRealOTPPool	= OBJECT
		PUBLIC
			PROCEDURE 	Fill(fname: STRING);
			// Zeroise the Random pool
			PROCEDURE	Clear;
			FUNCTION	Next:  CHAR;
			FUNCTION	Get(i: CARDINAL): CHAR;
		PRIVATE
			Pool 	: ARRAY OF CHAR;
			// holds the numbers before
			// conversion to CHAR
			Pad  	: TStringList;
			// Pad filename, usually otp.txt
			// but it might be encrypted, 
			// in which case - otp.ses
			PadF 	: STRING;
			// is the pad-file encrypted?
			PadE 	: BOOLEAN;
			// was the file successfully loaded?
			Loaded	: BOOLEAN;
			// is the OTP pool active?
			Active	: BOOLEAN;
			// total characters collected
			Chars	: CARDINAL;
			// index into pool
			PIdx 	: CARDINAL;
	END;



IMPLEMENTATION

USES SysUtils, MyStrUtils, Print, sesCiphers, rng;



// get a pseudo-random letter in [A..Z]
FUNCTION RandLetter: CHAR;
	BEGIN
		result := chr(ord('A') + rRand0(baseRNG,26));
	END;


// TRandPool methods

// Fill pool with random values
PROCEDURE TRandPool.Fill;
	var i: 	CARDINAL;
	BEGIN
		PIdx := 1;
		FOR i := 1 TO MAXDEPTH DO
			Pool[i] := rRandom(baseRNG);
	END;
	

// pull the next value from pool	
FUNCTION TRandPool.Next;
	BEGIN
		IF PIdx < MAXDEPTH THEN
			INC(PIdx)
		ELSE
			Fill;
		result := Pool[PIdx];
	END;


// pull from pool by external index	
FUNCTION TRandPool.Get;
	BEGIN
		result := Pool[i];
	END;

	
// Reset the index into Pool	
PROCEDURE TRandPool.Reset;
	BEGIN
		PIdx := 1;
	END;
	
	
// Zeroise the Random pool
PROCEDURE TRandPool.Clear;
	VAR i: CARDINAL;
	BEGIN
		for i := 1 to MAXDEPTH do
			Pool[i] := 0;
	END;
	
	
// TOTPPool methods

// fill pool with random characters
PROCEDURE TOTPPool.Fill;
	var i: 	CARDINAL;
	BEGIN
		PIdx := 1;
		FOR i := 1 TO MAXDEPTH DO
			Pool[i] := RandLetter;
	END;
	

// pull the next value from pool	
FUNCTION TOTPPool.Next;
	BEGIN
		IF PIdx < MAXDEPTH THEN
			INC(PIdx)
		ELSE
			Fill;
		result := Pool[PIdx];
	END;

	
// pull from pool by external index	
FUNCTION TOTPPool.Get;
	BEGIN
		result := Pool[i];
	END;


// Zeroise the Random pool
PROCEDURE TOTPPool.Clear;
	VAR i: CARDINAL;
	BEGIN
		for i := 1 to MAXDEPTH do
			Pool[i] := chr(0);
	END;
	
	

// TRealOTPPool methods


// Zeroise the Random pool
PROCEDURE TRealOTPPool.Clear;
	VAR i: CARDINAL;
	BEGIN
		TRY
			FOR i := 0 TO Length(Pool)-1 DO
				Pool[i] := chr(0);
		EXCEPT END;
	END;

	
// pull the next value from pool	
FUNCTION TRealOTPPool.Next;
	BEGIN
		IF PIdx < Chars THEN
			INC(PIdx)
		ELSE
			PIdx := 0;
		result := Pool[PIdx];
	END;

	
// pull from pool by external index	
FUNCTION TRealOTPPool.Get;
	BEGIN
		IF i< Chars THEN
			result := Pool[i]
		ELSE
			result := Pool[i mod Chars];
	END;

	
// fill real OTP pool with random characters
PROCEDURE TRealOTPPool.Fill;
	VAR i: INTEGER;
	BEGIN
		PadF := fname;
		// with unspecified filename, assume it is otp.txt
		IF (PadF = '') THEN PadF := 'otp.txt';
		Pad  := TStringList.Create;
		// assume file is encrypted
		// if extension is 'ses'
		PadE := TailAnsi(PadF,'.') = 'ses';
		// try to load the pad file
		TRY
			Pad.LoadFromFile(PadF);
			Loaded := true;
			// set length of Pool array based on Pad.Count
			SetLength(Pool,Pad.Count+1);
			// convert the Pad's numbers to [A..Z] and pool them
			FOR i := 0 TO Pad.Count-1 DO
				Pool[i] := chr(ord('A') + StrToInt(Pad[i]));					
			// our real OTP pool is active now
			Active 	:= true;	
			Chars	:= Pad.Count;
		EXCEPT
			ON E : EConvertError DO
				Println ('Invalid number encountered in <'+PadF+'>. Please check the file''s integrity and retry. Perhaps there is a rogue line-feed.');
			ELSE BEGIN
				PrintLn ('Unable to load <'+PadF+'>. Does the file exist? Is it in the program''s directory? Please verify and retry. If the error persists, consider contacting the developer.');
			END;
			Loaded := false;
			Active := false;
			Pad.Free;
			HALT;
		END;
		Pad.Free;
		PIdx := 0;
	END;


INITIALIZATION
	// always use a CSPRNG as our base!
	rSetBaseRNG(ISAAC);
	
FINALIZATION

	
END.
