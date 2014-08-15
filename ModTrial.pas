{$mode delphi}
PROGRAM ModTrial;
{ It has been suggested that the MOD (%) operator does not offer sufficiently
	uniform distribution when limiting the range of a CSPRNG for cryptographic
	purposes, as in <mod 26> [A..Z].
  The purpose of this program is to demonstrate the practical fallacy 
	of that assertion. 
  The CSPRNG used is Bob Jenkins' 32-bit ISAAC.

  Usage: >mod <modulo> <# of trials> <seed>
  Example: >mod 26 100000000 "my seed"
  All command line parameters are optional.
  Default modulus: 26.
}
USES rng, MyStrUtils, SysUtils, Math;

TYPE PArray = ARRAY Of EXTENDED;
	 SArray = ARRAY Of STRING;

// Default values
CONST DP = 7; // Decimal places for stats
VAR MAXTHROWS:	Cardinal = 100000000;
	MINTHROWS:	Cardinal = 10000; // was 1000000
	MAXINDEX:	Cardinal = 25;
	MODULO:		Cardinal = 26;
	START:		Char = 'A';
	seed: STRING = 'Monte Carlo Mod';
	minscore: CARDINAL;
	maxscore: CARDINAL;
    minpidx : CARDINAL;
	maxpidx : CARDINAL;
	medpidx : CARDINAL;
	medval  : STRING;
	
	totals: ARRAY of CARDINAL;
	values: ARRAY of STRING;
	proportions: PArray;
	probsorted : PArray;
	probtot: EXTENDED = 0.0;

	max: CARDINAL=0;
	min: CARDINAL=MAXINT;

	i,j,r: CARDINAL;

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

// Median  from a set of probabilities (returns array index)
FUNCTION MedianP(mins,maxs: Cardinal; VAR prob, sprob: PArray): CARDINAL;
	VAR i: Cardinal;
	BEGIN
		SetLength(sprob, Length(prob));
		FOR i := mins TO maxs DO sprob[i] := prob[i];
		SelectionSort(mins,maxs,sprob);
		MedianP := (maxs-mins) div 2;
	end;
	
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
		for i := minx to maxs do
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
		for i := minx to maxs do
			d[i] := (prob[i]-m)**2;
		// variance
		VarianceP := Mean(minx,maxs,d);
    END;

PROCEDURE Usage;
	BEGIN
		Writeln;
		Writeln('Usage:   >modtrial <modulo> <# of trials> <seed>');
		Writeln('Example: >modtrial 26 100000000 "my seed"');
		Writeln('All command line parameters are optional.');
		Writeln('Default: mod 26 with 100000000 trials.');
		Writeln;
	END;

PROCEDURE Banner;
	BEGIN
		WRITELN('ModTrial: Practical demonstration of the non-skewedness of the MOD operation ');
		WRITELN(' on 32-bit values emitted by the ISAAC CSPRNG in a Monte Carlo simulation.');
	END;
	
BEGIN
	TRY
		// Obtain modulo, trials and seed from command line
		IF ParamCount >= 1 THEN BEGIN
			IF ParamStr(1) <> '' THEN MODULO := StrToInt(ParamStr(1));
			IF ParamStr(2) <> '' THEN MAXTHROWS := StrToInt(ParamStr(2));
			IF ParamStr(3) <> '' THEN seed := ParamStr(3);
		END;
		// Sanity check
		IF MAXTHROWS<MINTHROWS THEN MAXTHROWS:=MINTHROWS;
		// Set default start-char based on chosen modulo
		IF MODULO=26 THEN START := 'A' ELSE
		IF MODULO=95 THEN START := ' ' 
		ELSE START := chr(0);
		MAXINDEX := MODULO-1;
		// Set up dynamic arrays
		SetLength(totals,MAXINDEX+1);
		SetLength(proportions,MAXINDEX+1);
		SetLength(values,MAXINDEX+1);
		// Seed the RNG
		rSeedAll(seed);
		
		Banner;
		Usage;
		WRITELN('Experiment: ',MAXTHROWS,' ISAAC trials using MOD ',IntToStr(MODULO));
		WRITE('Experiment starts');
    
		// zeroize totals array
		FOR i:=0 TO MAXINDEX DO
			totals[i]:=0;
	
		// initiate monte-carlo experiment
		FOR i:=1 TO MAXTHROWS DO BEGIN
			// Cast the die
			r:=rRandom(ISAAC) mod MODULO;
			// Tally maximum and minimum outcomes
			IF (r>max) THEN max:=r;
			IF (r<min) THEN min:=r;
			// Tally the total for each outcome
			INC(totals[r]);
			// show that we haven't expired...
			IF i mod 5000000  = 0 THEN write('.');
		END;
		WRITELN;
		WRITELN('Experiment ends.');

		// Calculate and display each outcome's total & probability
		WRITELN;
		//WRITELN('Score      Total    Probability');
		WRITELN('Value     Outcomes  Probability');
		WRITELN;
    
		minscore:=0; maxscore:=MAXINDEX;
		FOR j:=minscore TO maxscore DO BEGIN
			proportions[j]:=totals[j]/maxthrows;
			// probtot holds total of probabilities - should converge to 1.0
			probtot:=probtot+proportions[j];
			// collect value-names
			IF MODULO=26 THEN values[j] := chr(j mod MODULO + ord(START))
				ELSE values[j] := Ascii2Hex(chr(j mod MODULO + ord(START)));
			WRITELN(values[j]:3,'      ',totals[j]:8,'    ',proportions[j]:8:6)
		END;
    
		WRITELN('---------------------------------');
		WRITELN('TOTAL    ',maxthrows:8,'    ',probtot:8:6);
    
		// TEST RESULTS
		WRITELN();
		WRITELN('Min value = ',min);
		WRITELN('Max value = ',max);
		Writeln;
		minpidx := MinP(minscore,maxscore,proportions);
		maxpidx := MaxP(minscore,maxscore,proportions);
		medpidx := MedianP(minscore,maxscore,proportions,probsorted);
		medval  := GetVal(medpidx,proportions,probsorted,values);
		Writeln('Min  probability (',values[minpidx]:2,')  = ',proportions[minpidx]:1:DP);
		Writeln('Med  probability (',medval:2,')  = ',probsorted[medpidx]:1:DP);
		Writeln('Max  probability (',values[maxpidx]:2,')  = ',proportions[maxpidx]:1:DP);
		Writeln('Max-Min probability    = ',proportions[maxpidx]-proportions[minpidx]:1:DP);
		Writeln('Mean probability       = ',Mean(minscore,maxscore,proportions):1:DP);
		Writeln('Sigma of probabilities = ',Sigma(minscore,maxscore,proportions):1:DP);
		Writeln('Variance               = ',VarianceP(minscore,maxscore,proportions):1:DP);
		Writeln;
		Writeln('QED.');
	EXCEPT
		Writeln('Input error. Please check your command switches.');
		Usage;
	END;

END.
