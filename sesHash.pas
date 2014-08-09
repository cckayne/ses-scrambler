// NB: Keccak doesn't work in Delphi mode
{ $define test}
{$define KECCAK}
// iterative cryptographic hashes for SES
UNIT sesHash;

INTERFACE

USES sesTypes, oKeccak;

{$ifdef KECCAK}
	VAR Keccak : 	TKeccak;
		aKeccak:	TKeccak;
{$endif}

// get a 32-byte hash digest for any input string
FUNCTION doHash(ms: SESTRING; len: Cardinal): SESTRING;
// iterative hashing of any string
FUNCTION cckHash(msg: SESTRING; len: Cardinal): SESTRING;
// produce a numeric seed from any string by truncating its hex digest
// to a [tr]-bit value
FUNCTION HashSeed(k: SESTRING; len: Cardinal; yes: BOOLEAN): SESTRING;


IMPLEMENTATION

USES SysUtils, MyStrUtils, sesCiphers,
	{$ifdef KECCAK} BTypes, Mem_Util, keccak_n {$else} md5{$endif};
	

{Get a 32-byte cryptographic hash digest for any input string}
FUNCTION doHash(ms: SESTRING; len: Cardinal): SESTRING;
	BEGIN
		{$ifdef KECCAK}
		// Use OOP Keccak
				// provisionally, truncate digest to len bytes (56 max)
				doHash := LeftAnsi(Keccak.Go(ms),len);
		{$else}
			// otherwise there is MD5 as a fallback
			doHash := Uppercase(MD5Print(MD5String(msg)));
		{$endif}
	END;

	
// iterative hashing of a message string
FUNCTION cckHash(msg: SESTRING; len: Cardinal): SESTRING;
	VAR i,j: Cardinal;
		s: SESTRING;
	BEGIN
		s := doHash(msg,len);
		j := depth div 100;
		for i := 1 to j do
			s := doHash(s,len);
		cckHash := s;
	END;

	
// produce a numeric seed from the keyphrase by truncating its hex digest
// to a [tr]-bit value
FUNCTION HashSeed(k: SESTRING; len: Cardinal; yes: boolean): SESTRING;
	VAR kh: SESTRING;
	BEGIN
		IF yes THEN BEGIN
			kh := cckHash(k,len);
		END ELSE BEGIN
			kh := doHash(k,len);
		END;
		HashSeed := kh;
	END;

	

INITIALIZATION

{$ifdef KECCAK}
	//Keccak.Start;
{$endif}

FINALIZATION

END.
