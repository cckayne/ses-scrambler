{$mode delphi}
{ $define distrib}
{ $define SAM}
{ $define caesar}
{ $define VERBOSE}
PROGRAM ModTrial;
{ It has been suggested that the MOD (%) operator does not offer sufficiently
	uniform distribution when limiting the range of a CSPRNG for cryptographic
	purposes, as in <mod 26> [A..Z].
  The purpose of this program is to demonstrate the practical fallacy 
	of that assertion. 
  The CSPRNG used is Bob Jenkins' 32-bit ISAAC, or a choice of 14 other PRNGs.

  Usage: >modtrial <modulo> <# of trials> <seed> <RNG>
  Example: >modtrial 26 100000000 "my seed" 2
  All command line parameters are optional.
  Default modulus: 26.
}
USES rng, MyStrUtils, SysUtils, Math, HRTimer;

TYPE PArray = ARRAY Of EXTENDED;
	 SArray = ARRAY Of STRING;
	 CArray = ARRAY Of CARDINAL;
	 QArray = ARRAY Of QWord;
	 {$ifdef caesar}
	 TMCipher = (mEncipher, mDecipher, mNone);
	 {$endif}
// Default values
CONST 	VERSION = '4.0.0';
		COPYRT  = 'ModTrial is Copyright (c) C.C.Kayne, 2014 - cckayne@gmail.com';
		PDOM	= 'ModTrial is released into the Public Domain.';
		MAXCARD = $FFFFFFFF;
		MAXQ    = $FFFFFFFFFFFFFFFF;
VAR TRIALS:	QWord = 100000000;
	{$ifdef distrib}
	MINTHROWS:	Cardinal = 1000000; // was 1000000
	{$else}
	MINTHROWS:	Cardinal = 100; // was 1000000
	{$endif}
	MODULO:		Cardinal = 26;
	MODM1:		Cardinal = 25;
	TM:			Cardinal = 25; // temp mod for set-checks
	MA:			Cardinal = 10;
	DP:			Cardinal = 7; // Decimal places for stats
	START:		Cardinal = ord('A');
	seed: STRING = 'Monte Carlo Mod';
	RNG: TRNG = B512;
	minscore: CARDINAL;
	maxscore: CARDINAL;
    minpidx : CARDINAL;
	maxpidx : CARDINAL;
	medpidx : CARDINAL;
	medval  : STRING;
	
	totals: QArray;
	values: SArray;
	probability: PArray;
	probsorted : PArray;
	expect:		 PArray;
	probtot: EXTENDED = 0.0;

	vexp,vact: EXTENDED;
	i,j,r: CARDINAL;
	q:	   QWord = 0;
{$ifdef SAM}
	BTS: CARDINAL;
{$endif}
	t: THRTimer;
	sec: Extended;
	
	
// sort an array (ascending) using the selection-sort algorithm
PROCEDURE SelectionSort(mins,maxs: CARDINAL; VAR sArr: PArray);
    VAR current, j : Cardinal;
        large : Cardinal;
	PROCEDURE Swap(x,y: Cardinal);
		VAR temp: Extended;
		BEGIN
			temp := sArr[x];
			sArr[x] := sArr[y];
			sArr[y] := temp;
		END;
    BEGIN
		FOR current := maxs DOWNTO mins DO BEGIN
			large := mins;
			FOR j := mins TO current DO
				IF (sArr[j] > sArr[large]) THEN large := j;
			Swap(large,current);
		END;
    END;

// Get the value-name at <index> from a sorted set of probabilities
FUNCTION GetVal(idx: CARDINAL; prob, probs: PArray; val: SArray): STRING;
	VAR f: boolean;
		i: Longint;
		p: Extended;
	BEGIN
		i := -1;
		p:= probs[idx];
		REPEAT
			inc(i);
			f := p = prob[i];
		UNTIL f;
		GetVal := val[i];
	END;

// Median from a set of probabilities (returns array index & sorted probabilities)
FUNCTION MedianP(mins,maxs: Cardinal; VAR prob, sprob: PArray): CARDINAL;
	VAR i: Cardinal;
	BEGIN
		SetLength(sprob, Length(prob));
		FOR i := mins TO maxs DO sprob[i] := prob[i];
		SelectionSort(mins,maxs,sprob);
		MedianP := (maxs-mins) div 2;
	END;
	
// Maximum from a set of probabilities (returns array index)
FUNCTION MaxP(mins,maxs: Cardinal; prob: PArray): CARDINAL;
	VAR i: CARDINAL;
		m: EXTENDED;
	BEGIN
		m := 0.0;
		FOR i := mins TO maxs DO
			IF prob[i]>m THEN BEGIN m:=prob[i]; MaxP := i; END;
	END;
	
// Minimum from a set of probabilities (returns array index)
FUNCTION MinP(mins,maxs: Cardinal; prob: PArray): CARDINAL;
	VAR i: CARDINAL;
		m: EXTENDED;
	BEGIN
		m:= 1.0;
		FOR i := mins TO maxs DO
			IF prob[i]<m THEN BEGIN m:=prob[i]; MinP := i; END;
	END;

	
// Mean of a set of 64-bit outcomes
FUNCTION Mean(mins,maxs: CARDINAL; prob: QArray): EXTENDED; OVERLOAD;
	VAR i,dv: Cardinal;
		t: Extended;
	BEGIN
		dv   := maxs-mins+1;
		t    := 0.0;
		FOR i:=mins TO maxs DO
			t += prob[i];
		Mean := t / dv;
	END;

	
// Mean of a set of 32-bit outcomes
FUNCTION Mean(mins,maxs: CARDINAL; prob: CArray): EXTENDED; OVERLOAD;
	VAR i,dv: Cardinal;
		t: Extended;
	BEGIN
		dv	 := maxs-mins+1;
		t    := 0.0;
		FOR i:=mins TO maxs DO
			t += prob[i];
		Mean := t / dv;
	END;

	
// Mean of a set of probabilities
FUNCTION Mean(mins,maxs: CARDINAL; prob: PArray): EXTENDED; OVERLOAD;
	VAR i,dv: Cardinal;
		t: Extended;
	BEGIN
		dv   := maxs-mins+1;
		t    := 0.0;
		FOR i:=mins TO maxs DO
			t += prob[i];
		Mean := t / dv;
	END;


// Standard deviation of a set of probabilities
FUNCTION Sigma(minx,maxs: CARDINAL; prob: PArray): EXTENDED;
	VAR i: Cardinal;
		m: Extended;
		d: PArray;
	BEGIN
		SetLength(d,Length(prob));
		m := Mean(minx,maxs,prob);
		FOR i := minx TO maxs DO
			d[i] := (prob[i]-m)**2;
		// variance
		m := Mean(minx,maxs,d);
		Sigma := sqrt(m);
	END;


// Normalize a value in range [A..B] into range [C..D]
FUNCTION Normalize(value,A,B,C,D: EXTENDED): EXTENDED;
	BEGIN
		Normalize := C + ((value-A)*(D-C)) / (B-A);
	END;
	
	
// chi-square of a set of 64-bit outcomes
FUNCTION ChiSquare(minx,maxs: CARDINAL; prob: QArray; norm: BOOLEAN): EXTENDED; OVERLOAD;
	VAR i,dof: Cardinal;
		m,chi: Extended;
	BEGIN
		chi := 0.0;
		// m-1 degrees of freedom
		dof := maxs-minx;
		// expected mean
		m := mean(minx,maxs,prob);
		FOR i := minx TO maxs DO
			chi += ((prob[i]-m)**2)/m;
		ChiSquare:=chi;
		IF norm then ChiSquare := (chi-dof)/(3*sqrt(dof));
	END;


// chi-square of a set of 32-bit outcomes
FUNCTION ChiSquare(minx,maxs: CARDINAL; prob: CArray; norm: BOOLEAN): EXTENDED; OVERLOAD;
	VAR i,dof: Cardinal;
		m,chi: Extended;
	BEGIN
		chi := 0.0;
		// m-1 degrees of freedom
		dof := maxs-minx;
		// expected mean
		m := mean(minx,maxs,prob);
		FOR i := minx TO maxs DO
			chi += ((prob[i]-m)**2)/m;
		ChiSquare:=chi;
		IF norm then ChiSquare := (chi-dof)/(3*sqrt(dof));
	END;


// chi-square of a set of probabilities
FUNCTION ChiSquare(minx,maxs: CARDINAL; prob: PArray; norm: BOOLEAN): EXTENDED; OVERLOAD;
	VAR i,dv,dof: Cardinal;
		m,chi: Extended;
	BEGIN
		chi := 0.0;
		dv  := maxs-minx+1;
		// m-1 degrees of freedom
		dof := maxs-minx;
		// expected mean
		m := 1/dv;
		FOR i := minx TO maxs DO
			chi += ((prob[i]-m)**2)/m;
		ChiSquare:=chi;
		IF norm then ChiSquare := (chi-dof)/(3*sqrt(dof));
	END;


// Normalized chi-square given expected and observed variance
FUNCTION Chi2(minx,maxs: Cardinal; varexp,varobs: EXTENDED): EXTENDED;
	VAR dof: Cardinal;
	BEGIN
		// m-1 degrees of freedom
		dof := maxs-minx;
		Chi2 := ((varexp-varobs)-dof)/(3*sqrt(dof));
	END;


// Naive variance of a set of probabilities
FUNCTION VarianceP(minx,maxs: CARDINAL; prob: PArray): EXTENDED;
	VAR i: Cardinal;
		m: Extended;
		d: PArray;
	BEGIN
		SetLength(d,Length(prob));
		m := Mean(minx,maxs,prob);
		FOR i := minx TO maxs DO
			d[i] := (prob[i]-m)**2;
		// variance
		VarianceP := Mean(minx,maxs,d);
    END;

	
{$ifdef SAM}
// count the bits needed for a given value
function bitCount (val: Cardinal): Cardinal; 
    var v,c: Cardinal;
    begin
		v := val;
		c := 0;
		while (v > 0) do begin 
			inc(c);
			v := v shr 1;
		end;
		bitCount := c;
	end;
// Obtain a value in [0..val-1] from a pseudo-random bitstream
//  by sampling b-bit segments as n until n is in range. 
// < Like other alternatives to MOD, SAM exhibits a very slightly 
//   inferior distribution over the range of possible values.
//   Given a high quality RNG, SAM is otherwise unbiased. > 
function Sam(ng: TRNG; val: Cardinal): Cardinal; OVERLOAD;
	var r,m,n,b: Cardinal;
		i      : Longint;
		f      : Boolean;
	begin
		// calculate # of bits needed for value
		b:=0; n:=val;
		while (n > 0) do begin 
			inc(b);
			n := n shr 1;
		end;
		repeat
			r := rRandom(ng);
			i := -b;
			repeat
				i+=b;
				m := (1 shl b - 1) shl i;
				n := ((r and m) shr i);
				f := n in [0..val-1];
			until (f or (i>=(32-b)));
		until(f);
		Sam := n;
	end;
// Obtain a value in [0..val-1] from a pseudo-random bitstream
//  by sampling b-bit segments as n until n is in range. 
// < Like other alternatives to MOD, SAM exhibits a very slightly 
//   inferior distribution over the range of possible values.
//   SAM passed <val> and <bts> is around 1.1 times faster than MOD. 
//   Given a high quality RNG, SAM is otherwise unbiased. > 
function Sam(ng: TRNG; val,bts: Cardinal): Cardinal; OVERLOAD;
	var r,m,n   : Cardinal;
		i       : Longint;
		f       : Boolean;
	begin
		repeat
			r := rRandom(ng);
			i := -bts;
			repeat
				i+=bts;
				m := (1 shl bts - 1) shl i;
				n := (r and m) shr i;
				f := n < val;
			until (f or (i>=(32-bts)));
		until(f);
		Sam := n;
	end;

{$endif}


{$ifdef caesar}	
{ Caesar-shift a byte <shift> places }
FUNCTION Caesar(m: TMCipher; ch, shift, modulo: Longint): Longint;
	VAR n: Longint;
	BEGIN
		IF m = mDecipher THEN shift := -shift;
		n := (ch + shift) MOD modulo;
		IF n<0 THEN n += modulo;
		Caesar := n;
	END;
{$endif}
	
	
PROCEDURE Usage;
	BEGIN
		Writeln;
		Writeln('Usage:   >modtrial <modulo> <# of trials> <seed> <RNG>');
		Writeln('Example: >modtrial 26 32 "my seed" 2');
		Writeln('Permitted range for # trials: ',MINTHROWS,'-2**64.');
		Writeln('(If # given is in [8..64], 2**# is assumed.)');
		Writeln('Permitted modulos: 26, 95, 128, 256.');
		Writeln('Available RNGs   : [0..14] 2=ISAAC 3=BB128 4=BB256 5=BB512.');
		Writeln('All command line parameters are optional.');
		Writeln('Default: mod 26 BB512 with 100000000 trials.');
		Writeln;
	END;

PROCEDURE Banner;
	BEGIN
		Writeln('ModTrial: Practical demonstration of the non-skewedness of the MOD operation ');
		Writeln(' on 32-bit random values emitted by a PRNG in a Monte Carlo simulation.');
		{$ifdef distrib}
		Writeln(COPYRT);
		Writeln(PDOM);
		Writeln('ModTrial version ',VERSION);
		{$endif}
	END;
	
BEGIN
	TRY
		// Obtain modulo, trials, seed and RNG from the command line
		IF ParamCount >= 1 THEN BEGIN
			IF ParamStr(1)[1] IN ['h','H','i','u','U','-'] THEN BEGIN Banner; Usage; HALT; END;
			IF ParamStr(1) <> '' THEN MODULO := StrToInt(ParamStr(1));
			IF ParamStr(2) <> '' THEN TRIALS := StrToInt64(ParamStr(2));
			IF ParamStr(3) <> '' THEN seed := ParamStr(3);
			IF ParamStr(4) <> '' THEN RNG := TRNG(StrToInt(ParamStr(4)));
		END;
		// if number given is in this range, assume 2**number
		IF TRIALS IN [8..64] THEN TRIALS:=2**TRIALS-1;
		// Sanity checks
		IF TRIALS<MINTHROWS THEN TRIALS:=MINTHROWS;
		TM := MODULO-1; MODM1 := TM;
		IF NOT TM IN [25,94,127,255] THEN MODULO:=26;
		// decimal places needed will vary with chosen modulo
		IF MODULO=95 THEN DP := 9;
		IF TM IN [127..255] THEN DP := 11;
		MA := DP + 3;
		// Set default start-char based on chosen modulo
		IF MODULO=26 THEN START := ord('A') ELSE
		IF MODULO=95 THEN START := ord(' ') 
		ELSE START := 0;
		// Set up dynamic arrays
		SetLength(totals,MODM1+1);
		SetLength(probability,MODM1+1);
		SetLength(expect,MODM1+1);
		SetLength(values,MODM1+1);
		// Seed the RNGs
		rSeedAll(seed);
		// warm them up
		rStirAll(65536);
		
		Banner;
		{$ifdef VERBOSE}
		Usage;
		{$else}
		Writeln;
		{$endif}
		Writeln('Experiment: ',TRIALS,' ',RNGs[RNG],{$ifdef caesar}' Caesar',{$endif}' trials using ',{$ifdef SAM}'SAM '{$else}'MOD '{$endif},IntToStr(MODULO));
		Write('Experiment starts');
    
		// zeroize totals array
		FOR i:=0 TO MODM1 DO
			totals[i]:=0;

		{$ifdef SAM}
		BTS := bitCount(MODULO);
		{$endif}
		
		starttimer(t);
		// initiate Monte Carlo experiment
		REPEAT
			inc(q);
			// Cast the die
			{$ifdef caesar}
			r:=Caesar(mDecipher, rRandom(RNG) mod MODULO, rRandom(RNG) mod MODULO, modulo);
			{$else}
			{$ifdef SAM}
			r:=Sam(RNG,MODULO,BTS);
			{$else}
			r:=rRandom(RNG) mod MODULO;
			{$endif}
			{$endif}
			// Tally the total for each outcome
			INC(totals[r]);
			{$ifdef VERBOSE}
			// show that we haven't expired...
			IF q mod 5000000 = 0 THEN Write('.');
			{$endif}
		UNTIL (q=TRIALS);
		
		sec := ReadSeconds(t);
		
		Writeln;
		Writeln('Experiment ends. (',(sec/60):5:3,' min)');

		// Calculate & display each value's outcomes & probability
		{$ifdef VERBOSE}
		Writeln;
		Writeln('Value    Outcomes   Probability');
		Writeln;
		{$endif}
		
		minscore:=0; maxscore:=MODM1;
		FOR j:=minscore TO maxscore DO BEGIN
			// expected probabilities
			expect[j] := 1/MODULO;
			// actual probabilities
			probability[j]:=totals[j]/TRIALS;
			// probtot holds total of probabilities - it should converge to 1.0
			probtot:=probtot+probability[j];
			// collect value-names & decide output format
			IF MODULO IN [26,95] THEN values[j] := chr(j mod MODULO + START)
				ELSE values[j] := Ascii2Hex(chr(j mod MODULO + START));
			{$ifdef VERBOSE}
			Writeln(values[j]:3,'      ',totals[j]:8,'    ',probability[j]:8:6)
			{$endif}
		END;
		{$ifdef VERBOSE}
		// Display totals
		Writeln('---------------------------------');
		Writeln('TOTAL    ',maxthrows:8,'   ',probtot:8:6);
		{$endif}
		// DISPLAY THE TRIAL'S RESULTS
		Writeln;
		minpidx := MinP(minscore,maxscore,probability);
		maxpidx := MaxP(minscore,maxscore,probability);
		medpidx := MedianP(minscore,maxscore,probability,probsorted);
		medval  := GetVal(medpidx,probability,probsorted,values);
		vexp	:= VarianceP(minscore,maxscore,expect);
		vact	:= VarianceP(minscore,maxscore,probability);
		Writeln('Min  probability (',values[minpidx]:2,')   =  ',probability[minpidx]:MA:DP);
		Writeln('Med  probability (',medval:2,')   =  ',probsorted[medpidx]:MA:DP);
		Writeln('Max  probability (',values[maxpidx]:2,')   =  ',probability[maxpidx]:MA:DP);
		Writeln('Range (Max-Min P)       =  ',probability[maxpidx]-probability[minpidx]:MA:DP);
		Writeln('Mean     (expect)       =  ',Mean(minscore,maxscore,expect):MA:DP);
		Writeln('Mean     (actual)       =  ',Mean(minscore,maxscore,probability):MA:DP);
		Writeln('Sigma    (expect)       =  ',Sigma(minscore,maxscore,expect):MA:DP);
		Writeln('Sigma    (actual)       =  ',Sigma(minscore,maxscore,probability):MA:DP);
		Writeln('Variance (expect) *10E4 =  ',(10000*vexp):MA:DP);
		Writeln('Variance (actual) *10E4 =  ',(10000*vact):MA:DP);
		Writeln('Chi-square              =  ',ChiSquare(minscore,maxscore,probability,TRUE):MA:DP);
		
		{$ifdef VERBOSE}
		Writeln;
		Writeln('QED.');
		{$endif}
	EXCEPT
		Writeln('Input error. Please check your command switches.');
		Usage;
	END;

END.
