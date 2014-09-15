{ Universal 32-bit RNG wrapper unit: ISAAC and Marsaglia rngs 
  ----------------------------------------------------------- 
  NOTE on MOD operation: Monte Carlo trials have shown that the use of MOD 
  on 32-bit pseudo-random values does NOT skew the distribution enough to 
  warrant using an alternative scheme for limiting the range of the RNGs. 
  
  An alternative scheme was tested and it resulted in a somewhat less
  uniform distribution than did the MOD operator alone.}

{$mode delphi}
{$define clean}
{ $define test}
unit rng;

interface

// all rngs
type TRNG = (CONG,COSWB,ISAAC,B128,B256,B512,KILFI,KISS,KISWB,LFIB4,MWC,MWSWB,SHR3,SHRSW,SWB); 
	 TRNGarray = array[CONG..SWB] of string;

const RNGs: TRNGarray = ('CONG','COSWB','ISAAC','BB128','BB256','BB512','KILFI','KISS','KISWB','LFIB4','MWC','MWSWB','SHR3','SHRSW','SWB'); 

var  baseRNG: TRNG = ISAAC;

// Stir a given RNG to a given depth
procedure rStir(r:TRNG; depth: cardinal);
// Stir all RNGs to a given depth
procedure rStirAll(depth: cardinal);
// Reset all RNGs to initial (0) state
procedure rResetAll;
// Seed all RNGs with given 32-bit seed
procedure rSeedAll(seed: Cardinal); OVERLOAD;
// Seed all RNGs with given seed array
procedure rSeedAll(seed: array of Cardinal); OVERLOAD;
// Seed all RNGs with given string
procedure rSeedAll(seed: ANSISTRING); OVERLOAD;
// Seed specified RNG with given seed
procedure rSeed(rng:TRNG; seed: Cardinal); OVERLOAD;
// Seed specified RNG with given seed array (for ISAAC)
procedure rSeed(rng:TRNG; seed: array of Cardinal); OVERLOAD;
// Seed specified RNG with given string (for ISAAC)
procedure rSeed(rng:TRNG; seed: ANSISTRING); OVERLOAD;
// Randomize on NOW
procedure rRandomize;
// Get random 32-bit number from specified RNG
function rRandom(rng:TRNG): Cardinal;
// Get random number [0..x-1] from specified RNG
function rRand0(rng:TRNG; x: cardinal): Cardinal;
// Get random number [1..x] from specified RNG
function rRand(rng:TRNG; x: cardinal): Cardinal;
// Get random number in range [min..max] from specified RNG
function rRandR(rng:TRNG; min,max: cardinal): Cardinal;
// Get a random byte in printable ASCII range 
FUNCTION rRandA(rng: TRNG): BYTE;
// Get a random byte in 0..255 
FUNCTION rRandB(rng: TRNG): BYTE;
// Get a random string of length <len> with characters mod <mod>
FUNCTION rRandStr(rng: TRNG; len,m: BYTE): STRING;
// Get random 32-bit value from a random combo of x rngs
function rRandomMix(x: word): Cardinal;
// Returns a random choice of RNG (baseRNG as selector)
function rAnyRNG: TRNG;
// Set the base RNG. For random selection use rSetBaseRNG(rAnyRNG)
procedure rSetBaseRNG(b:TRNG);


implementation

uses sysutils, myisaac, marsaglia, bb128, bb256, bb512;

const 	lastRNG = cardinal(SWB);


// Randomly set the base rng
procedure rSetBaseRNG(b:TRNG);
begin
	baseRNG := b;
end;


// Returns a random choice of rng (using baseRNG)
function rAnyRNG: TRNG;
begin
	result := TRNG(rRand0(baseRNG,lastRNG));
end;


// "Stir" all RNGs to a given depth
procedure rStirAll(depth: cardinal);
var n: cardinal;
begin
	for n := 1 to depth do begin
		mCONG;
		mFIB;
		iRandom;
		mKISS;
		mLFIB4;
		mMWC;
		mSHR3;
		mSWB;
		bb128.bbRandom;
		bb256.bbRandom;
		bb512.bbRandom;
	end;
end;


// "Stir" given RNG to a given depth
procedure rStir(r:TRNG; depth: cardinal);
var n: cardinal;
begin
	for n := 1 to depth do begin
		case r of
			CONG: 	mCONG;
			//FIB:	mFIB;
			ISAAC:	iRandom;
			KISS:	mKISS;
			LFIB4:	mLFIB4;
			MWC:	mMWC;
			SHR3:	mSHR3;
			SWB:	mSWB;
			B128:	bb128.bbRandom;
			B256:	bb256.bbRandom;
			B512:	bb512.bbRandom;
		end;
	end;
end;



// Reset all RNGs to initial (0) state
procedure rResetAll;
begin
	//Reset Marsaglia's
	mReset;
	//Reset ISAAC
	iReset;
	//Reset bb128
	bb128.bbReset;
	//Reset bb256
	bb256.bbReset;
	//Reset bb512
	bb512.bbReset;
end;


// Seed all RNGs with given seed
procedure rSeedAll(seed: Cardinal);
begin
	// Seed ISAAC
	iSeed(seed,TRUE);
	// Seed Marsaglia's
	mSeedAll(seed);
end;


// Seed all RNGs with given seed array
procedure rSeedAll(seed: array of Cardinal);
var s: Cardinal;
begin
	// Seed ISAAC
	iSeed(seed,TRUE);
	s := iPeek;
	// Seed Marsaglia's with ISAAC's next iRandom
	mSeedAll(s);
end;



// Seed all RNGs with given string
procedure rSeedAll(seed: ANSISTRING);
var s: Cardinal;
begin
	// Seed ISAAC
	iSeed(seed,TRUE);
	s := iPeek;
	// Seed Marsaglia's with ISAAC's next iRandom
	mSeedAll(s);
	// Seed bb128
	bb128.bbSeedW(seed,384);
	// Seed bb256
	bb256.bbSeedW(seed,768);
	//Seed bb512
	bb512.bbSeedW(seed,1536);
	
end;



// Seed specified RNG with given numeric seed
procedure rSeed(rng:TRNG; seed: Cardinal);
begin
	case rng of 
		ISAAC: 	iSeed(seed,TRUE);
		CONG:  	mSeedAll(seed);
		COSWB:	mSeedAll(seed);
		KILFI:  mSeedAll(seed);
		KISS:	mSeedAll(seed);
		KISWB:	mSeedAll(seed);
		LFIB4:	mSeedAll(seed);
		MWC:	mSeedAll(seed);
		MWSWB:	mSeedAll(seed);
		SHR3:	mSeedAll(seed);
		SHRSW:	mSeedAll(seed);
		SWB:	mSeedAll(seed);
	end;
end;


// Seed specified RNG with 32-bit seed array (for ISAAC)
procedure rSeed(rng:TRNG; seed: array of Cardinal);
var s: Cardinal;
begin
	s := iPeek;
	case rng of 
		ISAAC: 	iSeed(seed,TRUE);
		CONG:  	mSeedAll(s);
		COSWB:	mSeedAll(s);
		KILFI:  mSeedAll(s);
		KISS:	mSeedAll(s);
		KISWB:	mSeedAll(s);
		LFIB4:	mSeedAll(s);
		MWC:	mSeedAll(s);
		MWSWB:	mSeedAll(s);
		SHR3:	mSeedAll(s);
		SHRSW:	mSeedAll(s);
		SWB:	mSeedAll(s);
	end;
end;


// Seed specified RNG with given string (for ISAAC only)
procedure rSeed(rng:TRNG; seed: ANSISTRING);
var s: Cardinal;
begin
	s := iPeek;
	case rng of 
		ISAAC: 	iSeed(seed,TRUE);
		CONG:  	mSeedAll(s);
		COSWB:	mSeedAll(s);
		KILFI:  mSeedAll(s);
		KISS:	mSeedAll(s);
		KISWB:	mSeedAll(s);
		LFIB4:	mSeedAll(s);
		MWC:	mSeedAll(s);
		MWSWB:	mSeedAll(s);
		SHR3:	mSeedAll(s);
		SHRSW:	mSeedAll(s);
		SWB:	mSeedAll(s);
		// Seed bb128
		B128: bb128.bbSeedW(seed,384);
		// Seed bb256
		B256: bb256.bbSeedW(seed,768);
		//Seed bb512
		B512: bb512.bbSeedW(seed,1536);
	end;
end;

	
// Randomize seed on NOW
procedure rRandomize;
var nano: Cardinal;
var Q: Qword;
begin
	Q:=QWord(Now);
	nano:=Cardinal(Q);
	iSeed(nano,true);
	rSeedAll(iRandom());
	rStirAll(nano mod 1001);
	//seed:=rRandomMix(3);
	rSetBaseRNG(rAnyRNG());
end;



// Get random 32-bit number from specified RNG
function rRandom(rng:TRNG): Cardinal;
begin
	case rng of
		CONG:	result := mCONG;
		COSWB:	result := (mCONG+mSWB);
		ISAAC:	result := iRandom;
		B128:	result := bb128.bbRandom;
		B256:	result := bb256.bbRandom;
		B512:	result := bb512.bbRandom;
		KILFI:  result := (mKISS+mLFIB4);
		KISS:	result := mKISS;
		KISWB:	result := (mKISS+mSWB);
		LFIB4:	result := mLFIB4;
		MWC:	result := mMWC;
		MWSWB:	result := (mMWC+mSWB);
		SHR3:	result := mSHR3;
		SHRSW:	result := (mSHR3+mSWB);
		SWB:	result := mSWB;
	end;
end;


// Get random 32-bit value from an rng combo
function rRandomMix(x: word): Cardinal;
var c: word;
	thisR, lastR: TRNG;
begin
	result := 0; lastR:=CONG; thisR:=CONG;
	for c := 1 to x do begin
		lastR:=thisR;
		thisR:=rAnyRNG;
		if thisR=lastR then thisR := succ(thisR);
		result += rRandom(thisR);
	end;
end;

	
// Get random number [0..x-1] from specified RNG
function rRand0(rng:TRNG; x: cardinal): Cardinal;
begin
	result := rRandom(rng) mod x;
end;


// Get random number [1..x] from specified RNG
function rRand(rng:TRNG; x: cardinal): Cardinal;
begin
	result := rRandom(rng) mod x +1;
end;


// Get random number in range [min..max] from specified RNG
function rRandR(rng:TRNG; min,max: cardinal): Cardinal;
begin
	repeat
		result := rRandom(rng) mod max;
	until (result>=min);
end;


{ Get a random byte in 0..255 }
FUNCTION rRandB(rng: TRNG): BYTE;
	BEGIN
		rRandB := rRandom(rng) mod $FF;
	END;


{ Get a random byte in printable ASCII range }
FUNCTION rRandA(rng: TRNG): BYTE;
	BEGIN
		rRandA := rRandom(rng) mod 95 + 32;
	END;

	
{ Get a random string with characters mod <mod> }
FUNCTION rRandStr(rng: TRNG; len,m: BYTE): STRING;
	VAR i,st: CARDINAL;
	BEGIN
		CASE m OF
			26: st := ord('A');
			95: st := 32;
			128:st := 0;
			255:st := 0;
		ELSE st := 0;
		END;
		rRandStr := '';
		For i := 1 to len do
			rRandStr += chr(rRandom(rng) mod m + st);
	END;
	

initialization

	{$ifndef clean}
		rResetAll;
		rSeedAll(Seed);
		rStirAll(rRandom(baseRNG) shr 16);
		rRandomize;
		{$ifdef test}
			writeln('Base RNG is ', RNGs[baseRNG])
		{$endif}
	{$endif}
end.

