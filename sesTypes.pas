{$mode delphi}
// Types and constants for SES
UNIT sesTypes;

INTERFACE

// maximum permitted scrambler depth
CONST MAXDEPTH 	= 1000000;
	  
TYPE SESTRING = AnsiString;
	 UTYPE	  = CARDINAL;
	 
TYPE  TPair 	= RECORD
	element1, element2: CARDINAL;
END;

// Cipher mode
TYPE TMCipher = (mEncipher,mDecipher,mNone);
	 TMCiphers= ARRAY [mEncipher..mNone] OF STRING;
CONST MCiphers: TMCiphers = ('Encipher','Decipher','None');

// to hold the words in the keyphrase
TYPE  kArray 	= ARRAY[1..500] OF SESTRING;

TYPE
	TRandPairA 	= ARRAY[1..MAXDEPTH] OF TPair;
	TRandPoolA	= ARRAY[1..MAXDEPTH] OF CARDINAL;
	TOTPPoolA	= ARRAY[1..MAXDEPTH] OF CHAR;

// terminal width	  
VAR	  SW: INTEGER		= 80;
	
IMPLEMENTATION

END.
