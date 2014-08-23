{$mode delphi}
{$define distrib}
{ $define caesar}
PROGRAM ModTrial;
{ It has been suggested that the MOD (%) operator does not offer sufficiently
	uniform distribution when limiting the range of a CSPRNG for cryptographic
	purposes, as in <mod 26> [A..Z].
  The purpose of this program is to demonstrate the practical fallacy 
	of that assertion. 
  The CSPRNG used is Bob Jenkins' 32-bit ISAAC, or a choice of 12 other PRNGs.

  Usage: >modtrial <modulo> <# of trials> <seed> <RNG>
  Example: >modtrial 26 100000000 "my seed" 2
  All command line parameters are optional.
  Default modulus: 26.
}
USES rng, MyStrUtils, SysUtils, Math;

TYPE PArray = ARRAY Of EXTENDED;
	 SArray = ARRAY Of STRING;
	 {$ifdef caesar}
	 TMCipher = (mEncipher, mDecipher, mNone);
	 {$endif}
// Default values
CONST 	VERSION = '3.0.0';
		COPYRT  = 'ModTrial is Copyright (c) C.C.Kayne, 2014 - cckayne@gmail.com';
		PDOM	= 'ModTrial is released into the Public Domain.';
		MAXCARD = 4294967295;
VAR MAXTHROWS:	Cardinal = 100000000;
	MINTHROWS:	Cardinal = 100000; // was 1000000
	MAXINDEX:	Cardinal = 25;
	MODULO:		Cardinal = 26;
	TM:			Cardinal = 25; // temp mod for set-checks
	DP:			Cardinal = 7; // Decimal places for stats
	START:		Cardinal = ord('A');
	seed: STRING = 'Monte Carlo Mod';
	RNG: TRNG = ISAAC;
	minscore: CARDINAL;
	maxscore: CARDINAL;
    minpidx : CARDINAL;
	maxpidx : CARDINAL;
	medpidx : CARDINAL;
	medval  : STRING;
	
	totals: ARRAY of CARDINAL;
	values: ARRAY of STRING;
	probability: PArray;
	probsorted : PArray;
	probtot: EXTENDED = 0.0;

	max: CARDINAL=0;
	min: CARDINAL=MAXINT;

	i,j,r: CARDINAL;

// sort an array using the selection-sort algorithm
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

// Mean of a set of probabilities
FUNCTION Mean(mins,maxs: CARDINAL; prob: PArray): EXTENDED;
	VAR i: Cardinal;
		t: Extended;
	BEGIN
		t    := 0.0;
		FOR i:=mins TO maxs DO
			t += prob[i];
		Mean := t / i;
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
		Writeln('Example: >modtrial 26 100000000 "my seed" 2');
		Writeln('Permitted range for # trials: 10000-4294967295.');
		Writeln('Permitted modulos: 26, 95, 128, 256.');
		Writeln('Permitted RNGs   : [0..12] 2=ISAAC.');
		Writeln('All command line parameters are optional.');
		Writeln('Default: mod 26 ISAAC with 100000000 trials.');
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
			IF ParamStr(2) <> '' THEN MAXTHROWS := StrToInt64(ParamStr(2));
			IF ParamStr(3) <> '' THEN seed := ParamStr(3);
			IF ParamStr(4) <> '' THEN RNG := TRNG(StrToInt(ParamStr(4)));
		END;
		// Sanity checks
		IF MAXTHROWS<MINTHROWS THEN MAXTHROWS:=MINTHROWS;
		TM := MODULO-1; MAXINDEX := TM;
		IF NOT TM IN [25,94,127,255] THEN MODULO:=26;
		IF MODULO <> 26 THEN DP := 12;
		// Set default start-char based on chosen modulo
		IF MODULO=26 THEN START := ord('A') ELSE
		IF MODULO=95 THEN START := ord(' ') 
		ELSE START := 0;
		// Set up dynamic arrays
		SetLength(totals,MAXINDEX+1);
		SetLength(probability,MAXINDEX+1);
		SetLength(values,MAXINDEX+1);
		// Seed the RNG
		rSeedAll(seed);
		
		Banner;
		Usage;
		Writeln('Experiment: ',MAXTHROWS,' ',RNGs[RNG],' trials using MOD ',IntToStr(MODULO));
		Write('Experiment starts');
    
		// zeroize totals array
		FOR i:=0 TO MAXINDEX DO
			totals[i]:=0;
	
		// initiate Monte Carlo experiment
		FOR i:=1 TO MAXTHROWS DO BEGIN
			// Cast the die
			{$ifdef caesar}
			r:=Caesar(mDecipher, rRandom(RNG) mod MODULO, rRandom(RNG) mod MODULO, modulo);
			{$else}
			r:=rRandom(RNG) mod MODULO;
			{$endif}
			// Tally maximum and minimum outcomes
			IF (r>max) THEN max:=r;
			IF (r<min) THEN min:=r;
			// Tally the total for each outcome
			INC(totals[r]);
			// show that we haven't expired...
			IF i mod 5000000  = 0 THEN Write('.');
		END;
		Writeln;
		Writeln('Experiment ends.');

		// Calculate & display each value's outcomes & probability
		Writeln;
		Writeln('Value    Outcomes   Probability');
		Writeln;
    
		minscore:=0; maxscore:=MAXINDEX;
		FOR j:=minscore TO maxscore DO BEGIN
			probability[j]:=totals[j]/maxthrows;
			// probtot holds total of probabilities - it should converge to 1.0
			probtot:=probtot+probability[j];
			// collect value-names & decide output format
			IF MODULO IN [26,95] THEN values[j] := chr(j mod MODULO + START)
				ELSE values[j] := Ascii2Hex(chr(j mod MODULO + START));
			Writeln(values[j]:3,'      ',totals[j]:8,'    ',probability[j]:8:6)
		END;
		// Display totals
		Writeln('---------------------------------');
		Writeln('TOTAL    ',maxthrows:8,'   ',probtot:8:6);
    
		// DISPLAY THE TRIAL'S RESULTS
		Writeln;
		Writeln('Min value = ',min);
		Writeln('Max value = ',max);
		Writeln;
		minpidx := MinP(minscore,maxscore,probability);
		maxpidx := MaxP(minscore,maxscore,probability);
		medpidx := MedianP(minscore,maxscore,probability,probsorted);
		medval  := GetVal(medpidx,probability,probsorted,values);
		Writeln('Min  probability (',values[minpidx]:2,')  = ',probability[minpidx]:1:DP);
		Writeln('Med  probability (',medval:2,')  = ',probsorted[medpidx]:1:DP);
		Writeln('Max  probability (',values[maxpidx]:2,')  = ',probability[maxpidx]:1:DP);
		Writeln('Max-Min probability    = ',probability[maxpidx]-probability[minpidx]:1:DP);
		Writeln('Mean probability       = ',Mean(minscore,maxscore,probability):1:DP);
		Writeln('Sigma of probabilities = ',Sigma(minscore,maxscore,probability):1:DP);
		Writeln('Variance               = ',VarianceP(minscore,maxscore,probability):1:DP);
		Writeln;
		Writeln('QED.');
	EXCEPT
		Writeln('Input error. Please check your command switches.');
		Usage;
	END;

END.
