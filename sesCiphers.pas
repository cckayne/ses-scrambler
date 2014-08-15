{$mode delphi}
{$define CSPRNG} { CSPRNG (ISAAC) }
{$define SES}
{$define distrib}
{ $define test}

{ Scrambler, OTP, Caesar and Vigenere ciphers for SES.
  ----------------------------------------------------
  NOTE on MOD operation: Monte Carlo trials have shown that the use of MOD 
  on 32-bit pseudo-random values does NOT skew the distribution enough to 
  warrant using an alternative scheme for limiting the range of the RNGs. 
  
  Please run ModTrial.exe to confirm the fallacy of the proposition that it does.
  
  An alternative scheme was tested and it resulted in a somewhat less
  uniform distribution than did the MOD operator alone.}
  

UNIT sesCiphers;

INTERFACE

USES rng, sesTypes, sesRand;

{$ifdef distrib}
	// the GPL license must always be present
	{$I 'gpl.pas'}
{$endif}

// SES global variables
VAR seed		: SESTRING 	= 'SES: A Super-Encipherment Scrambler. Copyright (C) C.Kayne 2014';
	depth		: CARDINAL 	= 10000;
	modulo		: INTEGER	=  26;
	start		: CHAR		= 'A';
	hexadecimal	: BOOLEAN	= FALSE;
	msg			: SESTRING 	=  '';
	plaintext	: SESTRING 	=  '';
	ciphertext	: SESTRING 	=  '';
	scrtext1	: SESTRING 	=  '';
	checktext	: SESTRING 	=  '';
	authcode	: SESTRING 	=  '';
	
	keywords	: kArray;
	keyphrase	: SESTRING = '';
	okeyphrase	: SESTRING = '';
	numkeywords	: CARDINAL = 0;
	aHash, bHash, eHash, kHash, mHash, nHash, sHash: SESTRING;

	RandPair	: TRandPairA;
	RandPool	: TRandPool;
	OTPPool		: TOTPPool;
	RealOTPPool	: TRealOTPPool;
	
	prepared	: BOOLEAN = FALSE;
	preparedonce: BOOLEAN = FALSE;
	keysprepared: BOOLEAN = FALSE;
	keychanged	: BOOLEAN = FALSE;
	
// M A C H I N E   P R E P A R A T I O N
PROCEDURE PrepareMachine(k,m: SESTRING);
// befuddle a hex character  
FUNCTION hB(s: CHAR): CHAR;
// unbefuddle a hex character
FUNCTION hUB(s: CHAR): CHAR;
// befuddle a hex SESTRING	
FUNCTION hexBefuddle(h: SESTRING): SESTRING;
// unbefuddle a hex SESTRING  
FUNCTION hexUnbefuddle(h: SESTRING): SESTRING;
// convert a key-phrase to an array of strings
FUNCTION KeyArray(m:TMCipher; keyphrase:SESTRING; VAR l:Cardinal): kArray;
// Seed and stir the RNG
PROCEDURE doSeed;
// convert an input string to CAPITALs
FUNCTION CapsEncode(txt: SESTRING): SESTRING;
// decode a CAPS-encoded input string
FUNCTION CapsDecode(txt: SESTRING): SESTRING;
// prepare an input string for encryption
FUNCTION PreProcessText(txt:SESTRING): SESTRING;
// render a raw decrypted string into original form
FUNCTION PostProcessText(txt:SESTRING): SESTRING;
// record pseudo-random pair-values for swapping
PROCEDURE InitRandPairs(dep,length: Cardinal);
// swap two character values
PROCEDURE cswap(VAR value1, value2 : CHAR );
// scramble by repeatedly swapping elements in SESTRING
// according to pattern preconfigured in InitRandPairs
FUNCTION Scrambled(t:SESTRING): SESTRING;
// unscramble by repeatedly swapping elements in SESTRING
// according to pattern preconfigured in InitRandPairs
FUNCTION Unscrambled(t:SESTRING): SESTRING;


// V I G E N E R E   S E C T I O N
//--------------------------------

// get a letter's position in chosen ASCII range
FUNCTION letternum(letter, start: CHAR): BYTE;
// Vigenere encipherment, generalized to <modulo> and <start>-character
FUNCTION enVig(pt,key:SESTRING; modulo: INTEGER; start: CHAR): SESTRING;
// Vigenere decipherment, generalized to <modulo> and <start>-character
FUNCTION deVig(ct,key:SESTRING; modulo: INTEGER; start: CHAR): SESTRING;
// Vigenere mod <modulo> encryption & decryption. Output: ASCII string
FUNCTION Vigenere(m: TMCipher; msg: STRING; modulo: BYTE; start: CHAR): STRING;
// Caesar-shift a character <shift> places: Generalized Vigenere 
FUNCTION Caesar(m:TMCipher; ch:CHAR; shift, modulo:INTEGER; start:CHAR): CHAR;
// Caesar-shift a string <shift> places, generalized to <modulo> and <start>	
FUNCTION CaesarStr(m:TMCipher; str:SESTRING; shift, modulo:INTEGER; start:CHAR): SESTRING;
// Randomly Caesar-shift a string letter by letter
// < equivalent to Vigenere-ing on a random letter key >
FUNCTION rCaesarStr(m:TMCipher; str:SESTRING; ng:TRNG; modulo:INTEGER; start:CHAR): SESTRING;
// Caesar shift or unshift a character, randomly selecting rectus or reversus mode 
FUNCTION mCaesar(m: TMCipher; ch: CHAR; ng: TRNG; modulo: INTEGER; start: CHAR): CHAR;
// Randomly mix Caesar-shift rectus and reversus on a string letter by letter
FUNCTION mCaesarStr(m:TMCipher; str:SESTRING; ng:TRNG; modulo:INTEGER; start:CHAR): SESTRING;
// extend one randomized keyword to length of message	
FUNCTION rKeyToMsg(m:TMCipher; pt, key:SESTRING; ng:TRNG; modulo:INTEGER; start:CHAR): SESTRING;


// "O T P"  S E C T I O N
// OTP any SESTRING
FUNCTION enOTP(msg: SESTRING): SESTRING;
// un-OTP any SESTRING
FUNCTION deOTP(msg: SESTRING): SESTRING;
// REAL OTP any SESTRING
FUNCTION enRealOTP(msg: SESTRING): SESTRING;
// REAL un-OTP any SESTRING
FUNCTION deRealOTP(ctx: SESTRING): SESTRING;


// S E S   S E C T I O N
// Main SES encipherment
FUNCTION sesEncipher(key, msg : SESTRING): SESTRING;
// Main SES decipherment
FUNCTION sesDecipher(key, ctxt: SESTRING): SESTRING;
PROCEDURE ShowLog;
// set a random depth scaled to message length
FUNCTION setDepth: Cardinal;
// get a unique nonce IV and hash it
FUNCTION getNonceHash: SESTRING;
// split a ciphertext into x-letter groups
FUNCTION Groups(ctx: SESTRING; x: Cardinal): SESTRING;
// authenticate a ciphertext
FUNCTION isAuthentic(ctx,hash: SESTRING): BOOLEAN;
// Perform some key-stretching on the seed
FUNCTION StretchKey(s,k: SESTRING; dep,len: Cardinal): SESTRING;
// Cipher a file of bytes on ISAAC stream 
FUNCTION iscCipherF(fn,p,k: STRING; mode: TMCipher; del: BOOLEAN): STRING;
// ses-encipher a text file - return number of lines written	
FUNCTION FileEncipher(k,f,p: SESTRING; del: BOOLEAN): Cardinal;
// ses-decipher a text file	- return number of lines written
FUNCTION FileDecipher(k,f,p: SESTRING): Cardinal;


IMPLEMENTATION

USES Classes, Print, SysUtils, StrUtils, MyStrUtils, sesHash, sesVer, sesParams, HRTimer, uFiles;

VAR Log		: TStringList;
	t		: THRTimer;

	
// "O T P"  S E C T I O N

// OTP any SESTRING
FUNCTION enOTP(msg: SESTRING): SESTRING;
	VAR s: SESTRING = '';
		i: Cardinal;
	BEGIN
		FOR i := 1 TO Length(msg) DO
			s += OTPPool.Get(i);
		result := enVig(msg,s,modulo,start);
	END;
	
	
// un-OTP any SESTRING
FUNCTION deOTP(msg: SESTRING): SESTRING;
	VAR s: SESTRING = '';
		i: Cardinal;
	BEGIN
		FOR i := 1 TO Length(msg) DO
			s += OTPPool.Get(i);
		result := deVig(msg,s,modulo,start);
	END;
	
	
// REAL OTP any SESTRING
FUNCTION enRealOTP(msg: SESTRING): SESTRING;
	VAR s: SESTRING = '';
		m: SESTRING = '';
		i  : Cardinal;
	BEGIN
		m := PreProcessText(msg);
		FOR i := 1 TO Length(m) DO
			s += RealOTPPool.Next;
		result := enVig(m,s,modulo,start);
	END;
	
	
// REAL un-OTP any SESTRING
FUNCTION deRealOTP(ctx: SESTRING): SESTRING;
	VAR s: SESTRING = '';
		c: SESTRING = '';
		i: Cardinal;
	BEGIN
		FOR i := 1 TO Length(ctx) DO
			s += RealOTPPool.Next;
		c := PostProcessText(deVig(ctx,s,modulo,start));
		result := c;
	END;
	
	
// S E S   S E C T I O N


// get a unique nonce IV and hash it
FUNCTION getNonceHash: SESTRING;
	BEGIN
		// nonce hashed on microsecond resolution system time string
		result := cckHash(FormatDateTime('YYYY-MM-DD-hh:nn:ss:zzz',Now),32);
	END;
	

// authenticate a ciphertext
FUNCTION isAuthentic(ctx,hash: SESTRING): BOOLEAN;
	VAR ahash: SESTRING;
	BEGIN
		ahash 	:= aKeccak.Go(ctx);
		result	:= ahash=hash;
	END;
	
	
// split a ciphertext into x-letter groups
// padding last group with dummies if necessary
FUNCTION Groups(ctx: SESTRING; x: Cardinal): SESTRING;
	VAR i: Cardinal = 0;
		l: Cardinal;
	BEGIN
		result := '';
		REPEAT
			inc(i);
			result += ctx[i];
			IF i mod x = 0 THEN
				result += ' ';
		UNTIL i >= Length(ctx);
		l := i mod x;
		IF l > 0 THEN
			result += StringStr(x-l,'#');
	END;
	
	
// set the mean random scramble-depth
FUNCTION SetDepth: Cardinal;
	VAR i,l: Cardinal;
		tot: Cardinal = 0;
	BEGIN
		l := Length(keyphrase);
		FOR i := 1 TO 1000 DO
			tot += rRandom(ISAAC) shr 21 * l;
		result := tot DIV i;
	END;

	
// randomly pad each keyword to message-length
PROCEDURE PadKeys(txt: SESTRING);
	VAR c,n: integer;
	BEGIN
		FOR c := 1 TO numkeywords DO BEGIN
			IF length(keywords[c]) < length(txt) THEN
				FOR n := length(keywords[c])+1 TO length(txt) DO
					keywords[c] += OTPPool.Next;
			// CHECK
			Log.Add('PK : '+keywords[c]);
		END;
	END;

	
// extend one randomized keyword to length of message	
FUNCTION rKeyToMsg(m: TMCipher; pt, key: ANSISTRING; ng: TRNG; modulo: integer; start: char): ANSISTRING;
	VAR kt: ANSISTRING;
		c,n: integer;
	BEGIN
		// construct the key-string from the key
		kt := '';
		FOR c := 1 TO Length(pt) DO BEGIN 
			n := c mod (Length(key));
			IF n>0 THEN kt += key[n] ELSE kt += key[Length(key)];
		END;
		// randomize the entire extended keyword
		Result := mCaesarStr(m, kt, ng, modulo, start);
	END;
	

// extend each randomized keyword in keyword-array to length of message
PROCEDURE rKeysToMsg(m: TMCipher; pt: ANSISTRING; ng: TRNG; modulo: INTEGER; start: CHAR); 	
	VAR c: INTEGER;
	BEGIN
		FOR c := 1 TO numkeywords DO BEGIN
			keywords[c] := rKeyToMsg(m, pt, keywords[c], ng, modulo, start);
			// CHECK
			Log.Add('EK'+IntToStr(c)+': '+keywords[c]);
		END;
	END;

	
// Perform some key-stretching on the seed
FUNCTION StretchKey(s,k: SESTRING; dep,len: Cardinal): SESTRING;
	VAR n,sh: Cardinal;
		salt: SESTRING;
	BEGIN
		// let the salt be the current seed
		salt := s;
		sh := (dep shr 5); // was 5
		{$ifdef test}
			// TEST
			Writeln('Performing ',sh,' key stretches...');
		{$endif}
		result := '';
		for n := 1 to sh do
			result := doHash(result+k+salt,len);
	END;
	

	
// M A C H I N E   P R E P A R A T I O N
PROCEDURE PrepareMachine(k,m: SESTRING);
	BEGIN
		// in case of re-entry during the same run
		okeyphrase := keyphrase;
		keyphrase  := k;
		keychanged := okeyphrase<>keyphrase;
		IF keychanged THEN BEGIN
			preparedonce := FALSE;
			keysprepared := FALSE;
		END;
		Log.Clear;
		// display default banner
		Log.Add(seed);
		modulo := GetModulo;
		// is hexadecimal output specified or set?
		IF NOT hexadecimal THEN hexadecimal := doHex;
		Log.Add('Using modulo '+IntToStr(modulo)+' with start "'+start+'".');
		IF hexadecimal THEN Log.Add('Hexadecimal.');
		// are we timing this operation?
		IF doTiming THEN StartTimer(t);
		// 1. preliminary seeding of the RNGs
		IF NOT preparedonce THEN BEGIN 
			seed := HashSeed(keyphrase,128,false); 
			doSeed;
			// extend seed's range to all printable ASCII
			seed := mCaesarStr(mEncipher,seed,ISAAC,95,' ');
			// seed the CSPRNG
			doSeed;
			// 2. split and C-encipher the keyphrase & keywords 
			keywords:= KeyArray(mEncipher,keyphrase,numkeywords);
			// 3. Set up the new seed and lookup tables
			// seed RNGs with encrypted keyphrase's hash
			seed := HashSeed(keywords[numkeywords],128,false);
			// randomly set the scramble depth
			doSeed;
			depth := SetDepth;	
			doSeed;
			// perform a little key-stretching
			seed := StretchKey(seed,keywords[numkeywords],depth,128);
			doSeed;
			// extend new seed's range to all printable ASCII
			seed := mCaesarStr(mEncipher,seed,ISAAC,95,' ');
			// seed the CSPRNG with the final seed
			doSeed;
		END;
		// fill up the 'one-time pad' pool
		OTPPool.Fill;
		// fill up the random number pool
		RandPool.Fill;
		// E N D  M A C H I N E   P R E P A R A T I O N
		prepared := TRUE;
		preparedonce := TRUE;
	END;
	

// Main SES super-encipherment cascade
FUNCTION sesEncipher(key, msg: SESTRING): SESTRING;
	VAR c: Cardinal;
	BEGIN
		// prepare the machine if it has not been done
		IF NOT prepared THEN PrepareMachine(key,msg);
		Log.Add('Encipherment:');
		Log.Add('SE2: '+LeftAnsi(seed,60)+' ... ('+Keccak.GetNibbles+' B)');
		Log.Add('DE : '+SysUtils.IntToStr(depth));
		// pre-process plaintext only in modulo 26 (uppercase ASCII mode)
		IF (modulo<=26) AND (start='A') THEN
			plaintext := PreProcessText(msg)
		ELSE plaintext := msg;
		Log.Add('PP : '+plaintext);
		// randomly extend each keyword to message-length.
		IF NOT keysprepared THEN BEGIN
			rKeysToMsg(mEncipher,plaintext, rAnyRNG, modulo, start);
			keysprepared := TRUE;
		END;
		// initiate the core encipher sequence
		InitRandPairs(depth,Length(plaintext));
		scrtext1 := plaintext;
		// B E G I N   C O R E   E N C I P H E R M E N T
		FOR c := 1 TO numkeywords DO BEGIN
			// 1. Vigenere encipher
			scrtext1 := enVig(scrtext1,keywords[c],modulo,start);
			Log.Add('VE'+IntToStr(c)+': '+scrtext1);
			// 2. Scramble
			scrtext1 := Scrambled(scrtext1);
			Log.Add('SC'+IntToStr(c)+': '+scrtext1);
		END; 
		// E N D   C O R E  E N C I P H E R M E N T
		
		// C I P H E R T E X T - H A S H  S E Q U E N C E
		// get a 32-byte hash digest from the ciphertext
		mHash := cckHash(scrtext1,32);
		// get a unique nonce IV and hash it
		nHash 	:= getNonceHash;
		// get the hash of nonce+ciphertext hashes
		nHash 	:= cckHash(mHash+nHash,32);
		// prepare to scramble the hash digest
		InitRandPairs(depth,Length(nHash));
		// befuddle the hash digest
		bHash	:= hexBefuddle(nHash);
		// encrypt the hash digest
		eHash   := mCaesarStr(mEncipher,bHash, BaseRNG, modulo, start);
		// 3. scramble the hash digest
		sHash	:= Scrambled(eHash);
		// 4. encipher the ciphertext on the scrambled hash
		scrtext1 := enVig(scrtext1,sHash,modulo,start);
		Log.Add('CH : '+mHash); 	// Ciphertext Hash
		Log.Add('NH : '+nHash); 	// Nonce Hash
		Log.Add('BH : '+bHash); 	// Befuddled Hash
		Log.Add('EH : '+eHash); 	// Encrypted Hash
		Log.Add('SH : '+sHash); 	// Scrambled Hash
		Log.Add('SC : '+scrtext1); 	// Scrambled Ciphertext
		// prefix the ciphertext with the scrambled hash
		scrtext1 := sHash+scrtext1;
		// 5. OTP-encipher this combination
		scrtext1 := enOTP(scrtext1);
		// output Final Ciphertext
		// converting to hexadecimal if modulo > 26
		IF hexadecimal THEN scrtext1 := ascii2hex(scrtext1);
		Log.Add('FC : '+scrtext1);
		// E N D  C I P H E R T E X T - H A S H  S E Q U E N C E
		IF doTiming THEN 
			Log.Add('TI : '+FloatToStr(ReadSeconds(t),2,5)+' sec');
		RandPool.Reset;
		result := scrtext1;
	END;


// Main SES super-decipherment cascade
FUNCTION sesDecipher(key, ctxt: SESTRING): SESTRING;
	VAR c: Cardinal;
	BEGIN
		// prepare the machine if it has not been done
		IF NOT prepared THEN PrepareMachine(key,ctxt);
		// 0. Sanity check for text case in modulo 26
		IF (modulo <= 26) AND (start='A') THEN
			IF isLowercase(ctxt) or isMixedCase(ctxt) THEN BEGIN 
				Log.Add('PT : '+ctxt);
				result := ctxt; 
				EXIT;
			END;
		// convert hexadecimal to ASCII if modulo > 26
		IF hexadecimal THEN ctxt := hex2ascii(ctxt);
		Log.Add('Decipherment:');
		Log.Add('SE2: '+LeftAnsi(seed,64)+' ... ('+Keccak.GetNibbles+' B)');
		Log.Add('DE : '+SysUtils.IntToStr(depth));
		scrtext1 := ctxt;

		// C I P H E R T E X T - H A S H  S E Q U E N C E
		// 1. decipher the OTP'd hash-ciphertext combo
		scrtext1 := deOTP(scrtext1);
		// 2. detach the scrambled hash digest from the ciphertext
		sHash 	:= LeftAnsi(scrtext1,32);
		scrtext1:= RightAnsi(scrtext1,Length(scrtext1)-32);
		// randomly extend each keyword to message-length.
		IF NOT keysprepared THEN BEGIN
			rKeysToMsg(mEncipher, scrtext1, rAnyRNG, modulo, start);
			keysprepared := TRUE;
		END;
		// 3. decipher the hash-enciphered ciphertext using the scrambled hash digest
		scrtext1 := deVig(scrtext1,sHash,modulo,start);
		// E N D  C I P H E R T E X T - H A S H  S E Q U E N C E
		Log.Add('SH : '+sHash);   // Scrambled Hash
		Log.Add('SC : '+scrtext1);// Scrambled Ciphertext
					
		// initiate the core decipher sequence
		InitRandPairs(depth,Length(scrtext1));
		// B E G I N   C O R E   D E C I P H E R M E N T
		FOR c := numkeywords DOWNTO 1 DO BEGIN
			// 4. Unscramble
			scrtext1 := Unscrambled(scrtext1);
			Log.Add('UC'+IntToStr(c)+': '+scrtext1);
			// 5. Vigenere decipher
			scrtext1 := deVig(scrtext1,keywords[c],modulo,start);	
			Log.Add('VD'+IntToStr(c)+': '+scrtext1);
		END; 
		IF (modulo <= 26) AND (start='A') THEN
			checktext := PostProcessText(scrtext1)
		ELSE
			checktext := scrtext1;
		// E N D   C O R E   D E C I P H E R M E N T
		RandPool.Reset;
		Log.Add('PT : '+checktext);
		IF doTiming THEN 
			Log.Add('TI : '+FloatToStr(ReadSeconds(t),2,5)+' sec');
		result := checktext;
	END;


	
{ Key derivation function - compute and stretch a derived key }
FUNCTION KDF(k: STRING): STRING;
	VAR salt: STRING;
		i,r: CARDINAL;
	BEGIN
		result := '';
		// let the salt be a 256-byte random plus a fixed string
		salt := rRandStr(ISAAC,$FF-Length(BANNER1),$FF) + BANNER1;
		r := rRandom(ISAAC) mod (10*Length(k));
		// stretch key r times
		FOR i := 1 TO r DO
			result:=doHash(result+k+salt,128);
		// let KDF be a 256-byte random string
		rSeed(ISAAC,result);
		result := rRandStr(ISAAC,$FF,$FF);
	END;
	

{ Pre-seed and Key derivation for ISC file ciphering}	
FUNCTION PreProcess(txt,ky: STRING; mode: TMCipher): STRING;
	VAR k: STRING;
	BEGIN
		k := ky;
		depth := SetDepth;
		// 1a) seed ISAAC with the key
		rSeed(ISAAC,k);
		//  b) key derivation
		k := KDF(k);
		k := Vigenere(mEncipher,k,255,chr(0));
		// c) re-seed with derived key
		rSeed(ISAAC,k);
		// d) CSPRNG warm-up
		rStir(ISAAC,depth);
		PreProcess := k;
	END;
	
	
{ Cipher a file of bytes on ISAAC stream }
FUNCTION iscCipherF(fn,p,k: STRING; mode: TMCipher; del: BOOLEAN): STRING;
	VAR fSz,n,total,nr,nw: CARDINAL;
		bIn, bOut: POINTER;
		fIn, fOut: FILE;
		fB : FILE OF BYTE;
		fname: STRING;
	BEGIN
		TRY
			StartTimer(t);
			// path + filename
			iscCipherF:=''; fname:=p+fn;
			{ 1) Get the file's size }
			Assign(fB,fname); Reset(fB);
			fSz := FileSize(fB);
			Close(fB);
			{ 2) Set storage size and reserve it }
			bIn:=GetMem(fSz); bOut:=GetMem(fSz);
			{ 3) Read the file into storage }
			Assign(fIn,fname); Reset(fIn,1);
			total:=0;
			REPEAT
				BlockRead(fIn,bIn^,fSz,nr);
				INC(total,nr);
			UNTIL (nr=0) OR (total>=fSz);
			Close(fIn);
			iscCipherF+=IntToStr(total)+' bytes read from <'+fname+'>. ';
			{ 4) Cipher the in-buffer and store results in out-buffer }
			PreProcess('',k,mode);
			FOR n := 0 TO total-1 DO
			// byte((bOut+n)^) := byte(Caesar(mode,char((bIn+n)^),rRandB(ISAAC),256,chr(0)));
				byte((bOut+n)^) := byte(mCaesar(mode,char((bIn+n)^),ISAAC,256,chr(0)));			{ 5) Write out the ciphered file }
			IF mode = mEncipher THEN fname+='.ses'
				ELSE fname := p+'ses.'+HeadStr(ExtractFilename(fname),'.ses'); 
			Assign(fOut,fname); Rewrite(fOut,1);
			total:=0;
			REPEAT
				BlockWrite(fOut,bOut^,fSz,nw);
				INC(total,nw);
			UNTIL (nw=0) OR (total>=fSz);
			Close(fOut);
			iscCipherF+=IntToStr(total)+' ciphered bytes written to <'+fname+'>. ';
		FINALLY
			{ 6) Overwrite the buffers to erase the plaintext and ciphertext }
			FillChar(bIn^,fSz,chr(0)); FillChar(bOut^,fSz,chr(0));
			{ 7) Free the Heap memory previously allocated }
			FreeMem(bIn,fSz); FreeMem(bOut,fSz);
			{ 8) Optionally, securely delete the original file }
			IF mode = mEncipher THEN
				IF del THEN
					IF FSDel(fn, p, 3) THEN
						iscCipherF += '<' +fn + '> securely deleted. ';
			iscCipherF += 'Timing: '+FloatToStr(ReadSeconds(t),2,4)+' s.';
		END;
	END;
	
	
// ses-encipher a text file	
FUNCTION FileEncipher(k,f,p: SESTRING; del: BOOLEAN): Cardinal;
	VAR fi,fo: SESTRING;
		seIn, seOut: TStringList;
		i: Cardinal;
	BEGIN
		// path + filename
		fi := p+f;
		// output file will be 'infile-name.ses'
		fo   := HeadAnsi(fi,'.')+'.'+'ses';
		seIn := TStringList.Create;
		// try to load the input file
		TRY
			seIn.LoadFromFile(fi);
		EXCEPT
			PrintLn('Unable to load <' + fi + '> for encipherment. Please verify that this file exists and try again.');
			seIn.Free;
			HALT;
		END;
		// prepare for file encipherment
		seOut := TStringList.Create;
		// try to encipher line by line
		TRY
			FOR i := 0 TO seIn.Count-1 DO
				IF seIn[i] <> '' THEN
					seOut.Add(sesEncipher(k,seIn[i]))
				ELSE
					seOut.Add('');
		EXCEPT
			PrintLn('Unable to encipher after line '+IntToStr(i)+' in file <' + fi + '>. Is it a text file? If this problem persists with other files, please consider contacting the developer.');
			seOut.Free;	seIn.Free;
			HALT;
		END;
		// try to save the ecrypted output file
		TRY
			seOut.SaveToFile(fo);
		EXCEPT
			PrintLn('Unable to write to enciphered file <' + fo + '>. Please verify that this file does not already exist or has the required permissions, and try again.');
			seOut.Free;	seIn.Free;
			HALT;
		END;
		seIn.Free; seOut.Free;
		{ Optionally, securely delete the original file }
			IF del THEN FSDel(f, p, 3);
		result := i;
	END;
	
	
// ses-decipher a text file	
FUNCTION FileDecipher(k,f,p: SESTRING): Cardinal;
	VAR fi,fo: SESTRING;
		seIn, seOut: TStringList;
		i: Cardinal;
	BEGIN
		// path + filename
		fi := p+f;
		// output file will be path+'infile-name.txt'
		fo   := p+HeadAnsi(fi,'.')+'.'+'txt';
		seIn := TStringList.Create;
		// try to load the input file
		TRY
			seIn.LoadFromFile(fi);
		EXCEPT
			PrintLn('Unable to load <' + fi + '> for decipherment. Please verify that this file exists and try again.');
			seIn.Free;
			HALT;
		END;
		// prepare for file decipherment
		seOut := TStringList.Create;
		// try to decipher line by line
		TRY
			FOR i := 0 TO seIn.Count-1 DO
				IF seIn[i] <> '' THEN
					seOut.Add(sesDecipher(k,seIn[i]))
				ELSE
					seOut.Add('');
		EXCEPT
			PrintLn('Unable to decipher after line '+IntToStr(i)+' in file <' + fi + '>. Is it a text file? If this problem persists with other files, please consider contacting the developer.');
			seOut.Free;	seIn.Free;
			HALT;
		END;
		// print the decrypted file to the console - don't save it!
		// <it is up to the user to redirect it somewhere if he/she must>
		TRY
			FOR i := 0 TO seOut.Count-1 DO
				Writeln(seOut[i]);
		EXCEPT
			PrintLn('Unable to write out the enciphered file <' + fo + '>. Please verify that this file does not already exist or has the required permissions, and try again.');
			seOut.Free;	seIn.Free;
			HALT;
		END;
		seIn.Free; seOut.Free;
		result := i;
	END;

	
// play back logged cipher sequence
PROCEDURE ShowLog;
	VAR i: Cardinal;
	BEGIN
		FOR i := 0 TO Log.Count-1 DO
			Writeln(Log[i]);
	END;

	
// befuddle a hex character  
FUNCTION hB(s: CHAR): CHAR;
	BEGIN
		IF Random(10) > 4 THEN
			result := chr(ord('G') + (ord(s) - ord('0')))
		ELSE
			result := chr(ord('Q') + (ord(s) - ord('0')))
	END;

// unbefuddle a hex character
FUNCTION hUB(s: CHAR): CHAR;
	BEGIN
		IF Random(10) > 4 THEN
			result := chr(ord('0') + (ord(s) - ord('G')))
		ELSE
			result := chr(ord('0') + (ord(s) - ord('Q')))
	END;
	

// befuddle a hex string	
FUNCTION hexBefuddle(h: SESTRING): SESTRING;
	VAR i,l: cardinal;
		s: SESTRING;
	BEGIN
		s := Uppercase(h);
		l := Length(s);
		RandSeed := l;
		FOR i := 1 TO l DO 
			IF s[i] IN ['0'..'9'] THEN s[i] := hB(s[i]);
		result := s;
	END;

	
// unbefuddle a hex string  
FUNCTION hexUnBefuddle(h: SESTRING): SESTRING;
	VAR i,l: cardinal;
		s: SESTRING;
	BEGIN
		s := Uppercase(h);
		l := Length(s);
		RandSeed := l;
		FOR i := 1 TO l DO 
			IF s[i] IN ['G'..'Z'] THEN s[i] := hUB(s[i]);
		result := s;
	END;



{ Caesar-shift or -unshift a character <shift> places: Generalized Vigenere }
FUNCTION Caesar(m: TMCipher; ch: CHAR; shift, modulo: INTEGER; start: CHAR): CHAR;
	VAR n: INTEGER;
	BEGIN
		IF m = mDecipher THEN shift := -shift;
		n := letternum(ch,start) + shift;
		// MOD: The standard, classical, accepted and time-honoured way
		//   of limiting the range of an RNG. Does not skew 32-bit results.
		n := n MOD modulo;
		IF n<0 THEN n += modulo;
		Caesar := chr(ord(start)+n);
	END;

	
// Caesar-shift or -unshift a string <shift> places	
FUNCTION CaesarStr(m:TMCipher; str:SESTRING; shift, modulo:INTEGER; start:CHAR): SESTRING;
	VAR n: CARDINAL;
	BEGIN
		Result := '';
		FOR n := 1 TO Length(str) DO
			Result += Caesar(m, str[n], shift, modulo, start);
	END;

	
// Randomly Caesar-shift or unshift a string letter by letter
// < equivalent to Vigenere-ing on a random letter key >
FUNCTION rCaesarStr(m: TMCipher; str: SESTRING; ng: TRNG; modulo: INTEGER; start: CHAR): ANSISTRING;
	VAR n: CARDINAL;
	BEGIN
		Result := '';
		FOR n := 1 TO Length(str) DO
			Result += Caesar(m, str[n], rRand(ng, modulo-1), modulo, start);
	END;

	
{ Caesar shift or unshift a character, randomly selecting rectus or reversus mode }
FUNCTION mCaesar(m: TMCipher; ch: CHAR; ng: TRNG; modulo: INTEGER; start: CHAR): CHAR;
	BEGIN
		IF rRand(ng,100) > 50 THEN BEGIN
			// rectus
			IF m = mEncipher THEN
				// shift
				Result := Caesar(mEncipher, ch, rRand(ng, modulo-1), modulo, start)
			ELSE
				// unshift
				Result := Caesar(mDecipher, ch, rRand(ng, modulo-1), modulo, start);
		END ELSE
			// reversus
			IF m = mEncipher THEN
				// unshift
				Result := Caesar(mDecipher, ch, rRand(ng, modulo-1), modulo, start)
			ELSE
				// shift
				Result := Caesar(mEncipher, ch, rRand(ng, modulo-1), modulo, start)
	END;


	
// Randomly mix/unmix Caesar-shift rectus and reversus letter by letter
FUNCTION mCaesarStr(m:TMCipher; str:SESTRING; ng:TRNG; modulo:INTEGER; start:CHAR): SESTRING;
	VAR n: CARDINAL;
	BEGIN
		Result := '';
		FOR n := 1 TO Length(str) DO
			Result += mCaesar(m, str[n], ng, modulo, start);
	END;
	

	
// Return array of (processed) keywords, passing array length in (l)
FUNCTION KeyArray(m: TMCipher; keyphrase: SESTRING; VAR l: CARDINAL): kArray;
	VAR c: INTEGER;
		kp: SESTRING;
		k: kArray;
	BEGIN
		//Pre-process keyphrase if modulo is 26 (uppercase)
		//IF (modulo <= 26) AND (start='A') THEN BEGIN
			kp:= PreProcessText(keyphrase);
			kp:= AnsiReplaceStr(kp,'ZX',' ');
			kp:= UpperCase(kp);
		//END;
		l := WordCount(kp,StdWordDelims);
		//Extract words, randomly C-shift, and place them in array
		FOR c := 1 TO l DO 
			k[c] := mCaesarStr(m,ExtractWord(c,kp,StdWordDelims), rAnyRNG, modulo, start);
		//Place entire keyphrase last, reversed and randomly C-shifted again
		INC(l);
		k[l] := '';
		FOR c := 1 TO l-1 DO
			k[l] += mCaesarStr(m,ReverseAnsi(k[c]), rAnyRNG, modulo, start);
		Result:= k;
	END;

	
// seed the RNGs
PROCEDURE doSeed;
	BEGIN
		{$ifndef CSPRNG}
			// use PRNG (MT)
			RandSeed := seed;
		{$else}
			// use CSPRNG (ISAAC)
			rResetAll;
			rSeedAll(seed);
			rStirAll(rRandom(baseRNG) shr 16);
		{$endif}
	END;

	
{Record pairs for swapping}
PROCEDURE InitRandPairs(dep, length: Cardinal);
	VAR c: Cardinal;
	BEGIN
		FOR c := 1 TO dep DO BEGIN
			WITH RandPair[c] DO BEGIN
			// increment index into RandPool
				element1 := RandPool.Next mod length + 1;
				element2 := RandPool.Next mod length + 1;
			END;
		END;
	END;


PROCEDURE cswap( VAR value1, value2 : CHAR );
{Swap two ASCII chars}
	VAR interim1 : CHAR;
	BEGIN
		interim1 := value1 ;
		value1 := value2 ;
		value2 := interim1 ;
	END;


{Scramble by repeatedly swapping elements in SESTRING}
{according to pattern preconfigured in InitRandPairs}
FUNCTION Scrambled(t:SESTRING): SESTRING;
	VAR c: Cardinal;
		temp: SESTRING;
	BEGIN
		temp := t;
		FOR c := 1 TO depth DO
			WITH RandPair[c] DO
				cswap(temp[element1], temp[element2]);
		Result := temp;
	END; {Scrambled}


{Unscramble by repeatedly swapping elements in SESTRING}
{according to pattern preconfigured in InitRandPairs}
FUNCTION Unscrambled(t:SESTRING): SESTRING;
	VAR c: Cardinal;
		temp: SESTRING;
	BEGIN
		temp := t;
		FOR c := depth DOWNTO 1 DO
			WITH RandPair[c] DO
				cswap(temp[element1], temp[element2]);
		Result := temp;
	END; {Unscrambled}



// V I G E N E R E   S E C T I O N
//--------------------------------

// Get position of the letter in chosen alphabet
FUNCTION letternum(letter, start: CHAR): byte;
	BEGIN
		result := (ord(letter)-ord(start));
	END;

	
// Classical Vigenere: encipherment
FUNCTION enVig(pt,key:SESTRING; modulo: INTEGER; start: CHAR): SESTRING;
	VAR kt, ct: SESTRING;
		c,n: Cardinal;
	BEGIN
	//construct the key-string from the key
		kt := '';
		FOR c := 1 TO Length(pt) DO BEGIN 
			n := c mod (Length(key));
			IF n>0 THEN kt += key[n] ELSE kt += key[Length(key)];
		END;
		//And encipher the plaintext using the keytext
		//(after initializing ciphertext string to length)
		ct := pt;
		FOR c := 1 TO Length(pt) DO BEGIN
			n := letternum(pt[c],start)+letternum(kt[c],start);
			// MOD: The standard, classical, accepted and time-honoured way
			//   of limiting the range of an RNG. Does not skew 32-bit results.
			n := n mod modulo;
			ct[c]:=chr(ord(start)+n);
		END;
		Result := ct;
	END;	


// Classical Vigenere: decipherment
FUNCTION deVig(ct,key:SESTRING; modulo: INTEGER; start: CHAR): SESTRING;
	VAR kt, pt: SESTRING;
		c,n: INTEGER;
	BEGIN
		//construct the key-string from the key
		kt := '';
		FOR c := 1 TO Length(ct) DO BEGIN 
			n := c mod (Length(key));
			IF n>0 THEN kt += key[n] ELSE kt += key[Length(key)];
		END;
		//And decipher the ciphertext using the keytext
		//(after initializing ciphertext string to length)
		pt := ct;
		FOR c := 1 TO Length(ct) DO BEGIN
			n := letternum(ct[c],start)-letternum(kt[c],start);
			IF n<0 THEN n:=modulo+n;
			pt[c]:=chr(ord(start)+n);
		END;
		Result := pt;
	END;	


{ Vigenere mod <modulo> encryption & decryption. Output: ASCII string }
FUNCTION Vigenere(m: TMCipher; msg: STRING; modulo: BYTE; start: CHAR): STRING;
	VAR i: CARDINAL;
	BEGIN
		Vigenere := '';
		FOR i := 1 to length(msg) DO
			Vigenere += Caesar(m,msg[i],rRandA(ISAAC),modulo,start);
	END;


	
FUNCTION CapsEncode(txt: SESTRING): SESTRING;
VAR t: SESTRING;
BEGIN
	t := '';
	t := txt;
	//Signal CAPS
	t := AnsiReplaceStr(t,'A','KQA');
	t := AnsiReplaceStr(t,'B','KQB');
	t := AnsiReplaceStr(t,'C','KQC');
	t := AnsiReplaceStr(t,'D','KQD');
	t := AnsiReplaceStr(t,'E','KQE');
	t := AnsiReplaceStr(t,'F','KQF');
	t := AnsiReplaceStr(t,'G','KQG');
	t := AnsiReplaceStr(t,'H','KQH');
	t := AnsiReplaceStr(t,'I','KQI');
	t := AnsiReplaceStr(t,'J','KQJ');
	t := AnsiReplaceStr(t,'K','KQK');
	t := AnsiReplaceStr(t,'L','KQL');
	t := AnsiReplaceStr(t,'M','KQM');
	t := AnsiReplaceStr(t,'N','KQN');
	t := AnsiReplaceStr(t,'Ñ','KQNT');
	t := AnsiReplaceStr(t,'O','KQO');
	t := AnsiReplaceStr(t,'P','KQP');
	t := AnsiReplaceStr(t,'Q','KQQ');
	t := AnsiReplaceStr(t,'R','KQR');
	t := AnsiReplaceStr(t,'S','KQS');
	t := AnsiReplaceStr(t,'T','KQT');
	t := AnsiReplaceStr(t,'U','KQU');
	t := AnsiReplaceStr(t,'V','KQV');
	t := AnsiReplaceStr(t,'W','KQW');
	t := AnsiReplaceStr(t,'X','KQX');
	t := AnsiReplaceStr(t,'Y','KQY');
	t := AnsiReplaceStr(t,'Z','KQZ');
	Result := t;
END; {CapsEncode}


FUNCTION CapsDecode(txt: SESTRING): SESTRING;
VAR t: SESTRING;
BEGIN
	t := '';
	t := txt;
	//Signal CAPS
	t := AnsiReplaceStr(t,'kqa','A');
	t := AnsiReplaceStr(t,'kqb','B');
	t := AnsiReplaceStr(t,'kqc','C');
	t := AnsiReplaceStr(t,'kqd','D');
	t := AnsiReplaceStr(t,'kqe','E');
	t := AnsiReplaceStr(t,'kqf','F');
	t := AnsiReplaceStr(t,'kqg','G');
	t := AnsiReplaceStr(t,'kqh','H');
	t := AnsiReplaceStr(t,'kqi','I');
	t := AnsiReplaceStr(t,'kqj','J');
	t := AnsiReplaceStr(t,'kqk','K');
	t := AnsiReplaceStr(t,'kql','L');
	t := AnsiReplaceStr(t,'kqm','M');
	t := AnsiReplaceStr(t,'kqn','N');
	t := AnsiReplaceStr(t,'kqnt','Ñ');
	t := AnsiReplaceStr(t,'kqo','O');
	t := AnsiReplaceStr(t,'kqp','P');
	t := AnsiReplaceStr(t,'kqq','Q');
	t := AnsiReplaceStr(t,'kqr','R');
	t := AnsiReplaceStr(t,'kqs','S');
	t := AnsiReplaceStr(t,'kqt','T');
	t := AnsiReplaceStr(t,'kqu','U');
	t := AnsiReplaceStr(t,'kqv','V');
	t := AnsiReplaceStr(t,'kqw','W');
	t := AnsiReplaceStr(t,'kqx','X');
	t := AnsiReplaceStr(t,'kqy','Y');
	t := AnsiReplaceStr(t,'kqz','Z');
	Result := t;
END; {CapsDecode}

	
FUNCTION PreProcessText(txt:SESTRING): SESTRING;
VAR t,pt: SESTRING;
	c: Cardinal;
BEGIN
	t := txt;
	
	//Signal CAPS
	t:=CapsEncode(t);
	//Kludge - sometimes AnsiReplaceStr doubles letters
	t := AnsiReplaceStr(t,'KKQQ','KQ');

	//P U N C T U A T I O N
	//Replace '.' with 'STST'
	t := AnsiReplaceStr(t,'.','ZSTX');
	//Replace ',' with 'comma'
	t := AnsiReplaceStr(t,',','ZCOX');
	//Replace '''' with 'quote'
	t := AnsiReplaceStr(t,'''','ZQTX');
	//Replace '"' with 'double quote'
	t := AnsiReplaceStr(t,'"','ZDQX');
	//Replace '-' with 'DXDX'
	t := AnsiReplaceStr(t,'-','DXDX');
	//Replace ':' with 'CXCX'
	t := AnsiReplaceStr(t,':','ZCLX');
	//Replace ';' with 'SCSC'
	t := AnsiReplaceStr(t,';','ZSCX');
	//Replace '/' with 'SLASH'
	t := AnsiReplaceStr(t,'/','ZSLX');
	//Replace '?' with 'QQ'
	t := AnsiReplaceStr(t,'?','ZQQX');
	//Replace '!' with 'EXCLM'
	t := AnsiReplaceStr(t,'!','ZLCX');
	//Replace '(' with 'OBRACKET'
	t := AnsiReplaceStr(t,'(','OBRACKET');
	//Replace ')' with 'CBRACKET'
	t := AnsiReplaceStr(t,')','CBRACKET');
	//Replace '{' with 'OBRACE'
	t := AnsiReplaceStr(t,'{','OBRACE');
	//Replace '-' with 'dash'
	t := AnsiReplaceStr(t,'}','CBRACE');
	//Replace '_' with 'ULULU'
	t := AnsiReplaceStr(t,'_','ZULX');
	//Replace '*' with 'ASTER'
	t := AnsiReplaceStr(t,'*','ZASX');
	//Replace '#' with 'HASH'
	t := AnsiReplaceStr(t,'#','ZHSX');
	//Replace '&' with 'AMPMP'
	t := AnsiReplaceStr(t,'&','ZAMX');
	//Replace '@' with 'AMPMP'
	t := AnsiReplaceStr(t,'@','ZMPX');
	//Replace '$' with 'DOLLAR'
	t := AnsiReplaceStr(t,'$','ZDLX');
	//Replace '%' with 'ZPCX'
	t := AnsiReplaceStr(t,'%','ZPCX');
	//Replace '\' with 'BACKSLASH'
	t := AnsiReplaceStr(t,'\','ZBSX');
	//Replace '=' with 'ZEQX'
	t := AnsiReplaceStr(t,'=','ZEQX');
	//Replace '+' with 'ZPLX'
	t := AnsiReplaceStr(t,'+','ZPLX');
	//Replace '[' with 'OSQBRACE'
	t := AnsiReplaceStr(t,'[','OSQBRACE');
	//Replace ']' with 'CSQUBRACE'
	t := AnsiReplaceStr(t,']','CSQUBRACE');
	//Replace '<' with 'OABRACKET'
	t := AnsiReplaceStr(t,'<','OABRACKET');
	//Replace '>' with 'CABRACKET'
	t := AnsiReplaceStr(t,'>','CABRACKET');
	
	//N U M B E R S
	// Morse-encoded - B = dit, H = dah
	//Replace '0' with 'zero'
	t := AnsiReplaceStr(t,'0','JHHHHHJ');
	//Replace '1' with 'ONE'
	t := AnsiReplaceStr(t,'1','JBHHHHJ');
	//Replace '2' with 'TWO'
	t := AnsiReplaceStr(t,'2','JBBHHHJ');
	//Replace '3' with 'THREE'
	t := AnsiReplaceStr(t,'3','JBBBHHJ');
	//Replace '4' with 'FOUR'
	t := AnsiReplaceStr(t,'4','JBBBBHJ');
	//Replace '5' with 'FIVE'
	t := AnsiReplaceStr(t,'5','JBBBBBJ');
	//Replace '6' with 'SIX'
	t := AnsiReplaceStr(t,'6','JHBBBBJ');
	//Replace '7' with 'SEVEN'
	t := AnsiReplaceStr(t,'7','JHHBBBJ');
	//Replace '8' with 'EIGHT'
	t := AnsiReplaceStr(t,'8','JHHHBBJ');
	//Replace '9' with 'NINE'
	t := AnsiReplaceStr(t,'9','JHHHHBJ');
	
	// ACCENTED CHARACTERS & DIACRITICS
	
	//Replace spaces with either 'ZZ' or 'XX'
	t := AnsiReplaceStr(t,' ','ZX');
		
	//Convert entire SESTRING to uppercase	
	pt := UpperCase(t);
	
	//Finally, replace any other extraneous chars with 'X'
	for c := 1 to Length(pt) do
		if not (pt[c] in ['A'..'Z']) then pt[c]:='X';
	
	Result := pt;
	
END; {PreProcessText}



FUNCTION PostProcessText(txt:SESTRING): SESTRING;
VAR t,pt: SESTRING;

BEGIN
	t := txt;
		
	//P U N C T U A T I O N
	//Replace '.' with 'STST'
	t := AnsiReplaceStr(t,'ZSTX','.');
	//Replace ',' with 'comma'
	t := AnsiReplaceStr(t,'ZCOX',',');
	//Replace '''' with 'quote'
	t := AnsiReplaceStr(t,'ZQTX','''');
	//Replace '"' with 'double quote'
	t := AnsiReplaceStr(t,'ZDQX','"');
	//Replace '-' with 'DXDX'
	t := AnsiReplaceStr(t,'DXDX','-');
	//Replace ':' with 'CXCX'
	t := AnsiReplaceStr(t,'ZCLX',':');
	//Replace ';' with 'SCSC'
	t := AnsiReplaceStr(t,'ZSCX',';');
	//Replace '/' with 'SLASH'
	t := AnsiReplaceStr(t,'ZSLX','/');
	//Replace '?' with 'QQ'
	t := AnsiReplaceStr(t,'ZQQX','?');
	//Replace '!' with 'EXCLM'
	t := AnsiReplaceStr(t,'ZLCX','!');
	//Replace '(' with 'OBRACKET'
	t := AnsiReplaceStr(t,'OBRACKET','(');
	//Replace ')' with 'CBRACKET'
	t := AnsiReplaceStr(t,'CBRACKET',')');
	//Replace '{' with 'OBRACE'
	t := AnsiReplaceStr(t,'OBRACE','{');
	//Replace '-' with 'dash'
	t := AnsiReplaceStr(t,'CBRACE','}');
	//Replace '_' with 'ULULU'
	t := AnsiReplaceStr(t,'ZULX','_');
	//Replace '*' with 'ZASX'
	t := AnsiReplaceStr(t,'ZASX','*');
	//Replace '#' with 'HASH'
	t := AnsiReplaceStr(t,'ZHSX','#');
	//Replace '&' with 'AMPMP'
	t := AnsiReplaceStr(t,'ZAMX','&');
	//Replace '@' with 'AMPMP'
	t := AnsiReplaceStr(t,'ZMPX','@');
	//Replace '$' with 'DOLLAR'
	t := AnsiReplaceStr(t,'ZDLX','$');
	//Replace '%' with 'ZPCX'
	t := AnsiReplaceStr(t,'ZPCX','%');
	//Replace '\' with 'BACKSLASH'
	t := AnsiReplaceStr(t,'ZBSX','\');
	//Replace '=' with 'ZEQX'
	t := AnsiReplaceStr(t,'ZEQX','=');
	//Replace '+' with 'ZPLX'
	t := AnsiReplaceStr(t,'ZPLX','+');
	//Replace '[' with 'OSQBRACE'
	t := AnsiReplaceStr(t,'OSQBRACE','[');
	//Replace ']' with 'CSQUBRACE'
	t := AnsiReplaceStr(t,'CSQUBRACE',']');
	//Replace '<' with 'OABRACKET'
	t := AnsiReplaceStr(t,'OABRACKET','<');
	//Replace '>' with 'CABRACKET'
	t := AnsiReplaceStr(t,'CABRACKET','>');
	
	//N U M B E R S
	// Morse-encoded - B = dit, H = dah
	//Replace '0' with 'zero'
	t := AnsiReplaceStr(t,'JHHHHHJ','0');
	//Replace '1' with 'ONE'
	t := AnsiReplaceStr(t,'JBHHHHJ','1');
	//Replace '2' with 'TWO'
	t := AnsiReplaceStr(t,'JBBHHHJ','2');
	//Replace '3' with 'THREE'
	t := AnsiReplaceStr(t,'JBBBHHJ','3');
	//Replace '4' with 'FOUR'
	t := AnsiReplaceStr(t,'JBBBBHJ','4');
	//Replace '5' with 'FIVE'
	t := AnsiReplaceStr(t,'JBBBBBJ','5');
	//Replace '6' with 'SIX'
	t := AnsiReplaceStr(t,'JHBBBBJ','6');
	//Replace '7' with 'SEVEN'
	t := AnsiReplaceStr(t,'JHHBBBJ','7');
	//Replace '8' with 'EIGHT'
	t := AnsiReplaceStr(t,'JHHHBBJ','8');
	//Replace '9' with 'NINE'
	t := AnsiReplaceStr(t,'JHHHHBJ','9');
	
	//Replace spaces with either 'ZZ' or 'XX'
	t := AnsiReplaceStr(t,'ZX',' ');
		
	//Convert entire SESTRING to lowercase	
	pt := LowerCase(t);
	
	//Signal CAPS
	pt:=CapsDecode(pt);
	//some 'kq's occasionally get left in - 
	// there's probably an obvious cause, but I haven't found it yet
	pt:=AnsiReplaceStr(pt,'kq','');
	
	Result := pt;
	
END; {PostProcessText}


// overwrite and reset a string
PROCEDURE ZeroStr(VAR str: STRING);
	VAR n: CARDINAL;
	BEGIN
		FOR n := 1 TO length(str) DO
			str[n] := chr(0);
		str := '';
	END;
	

// belt-and-braces variable cleanup
PROCEDURE SecureCleanup;
	VAR n: CARDINAL;
	BEGIN
		// overwrite keyphrase
		ZeroStr(keyphrase);
		ZeroStr(okeyphrase);
		// overwrite seed string
		ZeroStr(seed);
		// overwrite Plaintext
		ZeroStr(plaintext);
		ZeroSTr(msg);
		// overwrite Ciphertext
		ZeroStr(ciphertext);
		ZeroStr(scrtext1);
		ZeroStr(checktext);
		ZeroStr(authcode);
		// overwrite keyword-array
		FOR n := 1 TO numkeywords DO
			ZeroStr(keywords[n]);
		numkeywords := 0;
		// zeroize depth
		depth := 0;
		modulo:= 0;
		start := chr(0);
		// overwrite all generated hashes
		ZeroStr(aHash); ZeroStr(bHash); ZeroStr(eHash); ZeroStr(kHash);
		ZeroStr(mHash); ZeroStr(nHash); ZeroStr(sHash);		
		// clear all random number pools
		RandPool.Clear;
		OTPPool.Clear;
		RealOTPPool.Clear;
		// overwrite the log
		TRY
			TRY
				FOR n := 0 TO Log.Count-1 DO BEGIN
					Log[n] := StringStr(255,chr(0));
					Log[n] := '';
				END;
			EXCEPT END;
		FINALLY
			Log.Free;
		END;
	END;

INITIALIZATION

	Log := TStringList.Create;
	
FINALIZATION

	SecureCleanup;
	
END.
