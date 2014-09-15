{ Conversion of readable.c to Pascal (fpc) unit.
	by cck <cckayne@gmail.com>
------------------------------------------------------------------------------
readable.c: My random number generator, ISAAC.
(c) Bob Jenkins, March 1996, Public Domain
You may use this code in any way you wish, and it is free.  No warrantee.
------------------------------------------------------------------------------

 ISAAC Stream Cipher utilities
 -----------------------------
 
  NOTE on MOD operation: Monte Carlo trials have shown that the use of MOD 
  on 32-bit pseudo-random values does NOT skew the distribution enough to 
  warrant using an alternative scheme for limiting the range of the RNGs. 
  
  An alternative scheme was tested and it resulted in a somewhat less
  uniform distribution than did the MOD operator alone.}


{ $define test}
{$mode delphi}
unit myisaac;

interface

{ Get a random 32-bit value }
function iRandom : Cardinal;
{ Get a random value (32 bits) in the range [1..x] }
function iRand(x: cardinal) : Cardinal;
{ Get a random value (32 bits) in the range [0..x-1] }
function iRand0(x: cardinal) : Cardinal;
{ Get a random byte }
function iRandB: BYTE;
{ Get a random character in printable ASCII range }
function iRandA: CHAR; OVERLOAD;
{ Get a random byte in printable ASCII range }
function iRandA: BYTE; OVERLOAD;
{ Get a random string of length <len> w/ characters mod <mod> & start <st>}
FUNCTION iRandStr(len: CARDINAL; m,st: BYTE): STRING;
{ Peek at ISAAC's current random state, but don't increment}
function iPeek: Cardinal; OVERLOAD;
{ Peek at ISAAC's current random state, but don't increment}
function iPeek(idx: BYTE): CARDINAL; OVERLOAD;
{ Initialize ISAAC with 32-bit seed - flag is usually TRUE }
procedure iSeed(seed: cardinal; flag: boolean); OVERLOAD;
{ Initialize ISAAC with 32-bit seed array - flag is usually TRUE }
procedure iSeed(seed: array of cardinal; flag: boolean); OVERLOAD;
{ Initialize ISAAC with given string - flag is usually TRUE }
procedure iSeed(seed: ANSISTRING; flag: boolean); OVERLOAD;
{ Randomize and seed on NOW }
procedure iRandomize;
{ Reset to initial zero state }
procedure iReset;


implementation

uses sysutils;

// external results 
var randrsl: array[0..256] of cardinal;
var randcnt: cardinal;

// internal state 
var mm: array[0..256] of cardinal;
var aa: cardinal=0; bb: cardinal=0; cc: cardinal=0;

// for initialization
var i: cardinal;


procedure isaac;
var i,x,y: cardinal;
begin
   cc := cc + 1;    // cc just gets incremented once per 256 results 
   bb := bb + cc;   // then combined with bb 

   for i := 0 to 255 do begin
     x := mm[i];
     case (i and 3) of
     	0: aa := aa xor (aa shl 13);
		1: aa := aa xor (aa shr 6);
		2: aa := aa xor (aa shl 2);
		3: aa := aa xor (aa shr 16);
     end; {case}
     aa := mm[(i+128) and 255] + aa;
	 y  := mm[(x shr 2) and 255] + aa + bb;
     mm[i] := y; 	
     bb := mm[(y shr 10) and 255] + x; 
     randrsl[i]:= bb; 
	 // this reset was not in original readable.c!
	 randcnt:=0;  // prepare to use the first set of results 

     { Note that bits 2..9 are chosen from x but 10..17 are chosen
        from y.  The only important thing here is that 2..9 and 10..17
        don't overlap.  2..9 and 10..17 were then chosen for speed in
        the optimized version (rand.c) 
       See http://burtleburtle.net/bob/rand/isaac.html
        for further explanations and analysis. }
   end; {for i}
end; {isaac}


// if (flag==TRUE), then use the contents of randrsl[] to initialize mm[]. 
procedure mix(var a,b,c,d,e,f,g,h: cardinal);
begin
	a := a xor b shl 11; d:=d+a; b:=b+c;
	b := b xor c shr  2; e:=e+b; c:=c+d;
	c := c xor d shl  8; f:=f+c; d:=d+e;
	d := d xor e shr 16; g:=g+d; e:=e+f;
	e := e xor f shl 10; h:=h+e; f:=f+g;
	f := f xor g shr  4; a:=a+f; g:=g+h;
	g := g xor h shl  8; b:=b+g; h:=h+a;
	h := h xor a shr  9; c:=c+h; a:=a+b;
end; {mix}


procedure iRandInit(flag: boolean);
var i,a,b,c,d,e,f,g,h: cardinal;
begin
   aa:=0; bb:=0; cc:=0;
   a:=$9e3779b9; 	// the golden ratio
   
   b:=a; c:=a; d:=a; e:=a; f:=a; g:=a; h:=a; 

   for i := 0 to 3 do          // scramble it 
        mix(a,b,c,d,e,f,g,h);
   
   i:=0;
   repeat  // fill in mm[] with messy stuff 
	if flag then begin     // use all the information in the seed 
       a+=randrsl[i  ]; b+=randrsl[i+1]; c+=randrsl[i+2]; d+=randrsl[i+3];
       e+=randrsl[i+4]; f+=randrsl[i+5]; g+=randrsl[i+6]; h+=randrsl[i+7];
    end;
     
    mix(a,b,c,d,e,f,g,h);
    mm[i  ]:=a; mm[i+1]:=b; mm[i+2]:=c; mm[i+3]:=d;
    mm[i+4]:=e; mm[i+5]:=f; mm[i+6]:=g; mm[i+7]:=h;
	i+=8;
   until i>255;

   if (flag) then begin
   // do a second pass to make all of the seed affect all of mm 
     i:=0;
     repeat
      a+=mm[i  ]; b+=mm[i+1]; c+=mm[i+2]; d+=mm[i+3];
      e+=mm[i+4]; f+=mm[i+5]; g+=mm[i+6]; h+=mm[i+7];
      mix(a,b,c,d,e,f,g,h);
      mm[i  ]:=a; mm[i+1]:=b; mm[i+2]:=c; mm[i+3]:=d;
      mm[i+4]:=e; mm[i+5]:=f; mm[i+6]:=g; mm[i+7]:=h;
      i+=8;
     until i>255; 
   end;

   isaac();           // fill in the first set of results 
   randcnt:=0;       // prepare to use the first set of results 
end; {randinit}



procedure iSeed(seed: cardinal; flag: boolean);
var i,a,b,c,d,e,f,g,h: cardinal;
begin
   aa:=0; bb:=0; cc:=0;
   a:=seed; b:=a; c:=a; d:=a; 
   e:=a; f:=a; g:=a; h:=a; 

   for i := 0 to 3 do          // scramble it 
        mix(a,b,c,d,e,f,g,h);
   
   i:=0;
   repeat  // fill in mm[] with messy stuff 
	if flag then begin     // use all the information in the seed 
       a+=randrsl[i  ]; b+=randrsl[i+1]; c+=randrsl[i+2]; d+=randrsl[i+3];
       e+=randrsl[i+4]; f+=randrsl[i+5]; g+=randrsl[i+6]; h+=randrsl[i+7];
    end; {if flag}
     
    mix(a,b,c,d,e,f,g,h);
    mm[i  ]:=a; mm[i+1]:=b; mm[i+2]:=c; mm[i+3]:=d;
    mm[i+4]:=e; mm[i+5]:=f; mm[i+6]:=g; mm[i+7]:=h;
	i+=8;
   until i>255;

   if (flag) then begin
   // do a second pass to make all of the seed affect all of mm 
     i:=0;
     repeat
      a+=mm[i  ]; b+=mm[i+1]; c+=mm[i+2]; d+=mm[i+3];
      e+=mm[i+4]; f+=mm[i+5]; g+=mm[i+6]; h+=mm[i+7];
      mix(a,b,c,d,e,f,g,h);
      mm[i  ]:=a; mm[i+1]:=b; mm[i+2]:=c; mm[i+3]:=d;
      mm[i+4]:=e; mm[i+5]:=f; mm[i+6]:=g; mm[i+7]:=h;
      i+=8;
     until i>255; 
   end; {if flag}

   isaac();           // fill in the first set of results 
   randcnt:=0;       // prepare to use the first set of results 
end; {iSeed - was randinit}



{ Seed ISAAC with a given seed.
  The array can be any size. The first 256 values will be used.
  If the array has less than 256 values, all the available values will be used.
  You can use either Cardinals or Integer with no problem. }
PROCEDURE iSeed(seed: ARRAY OF Cardinal; flag: BOOLEAN);
VAR
   i,m: integer;
BEGIN
    FOR i:= 0 TO 255 DO mm[i]:=0;
    m := High(seed);
    FOR i:= 0 TO 255 DO BEGIN
		// in case s[] has less than 256 elements.
        IF i>m THEN randrsl[i]:=0  
			ELSE randrsl[i]:=seed[i];
    END;
    // initialize with seed
	iRandInit(true);
END;


{ Initialize ISAAC with given string - flag is usually TRUE }
PROCEDURE iSeed(seed: ANSISTRING; flag: boolean);
	VAR s: ARRAY OF cardinal;
		n,l: Cardinal;
	BEGIN
		l := Length(seed);
		SetLength(s,l);
		FOR n := 0 TO l-1 DO
			s[n] := ord(seed[n+1]);
		iSeed(s, true);
	END;



{ Get a random 32-bit value }
function iRandom : Cardinal;
begin
    Result := randrsl[randcnt];
    inc(randcnt);
    if (randcnt >255) then
    begin
         Isaac();
         randcnt := 0;
    end;
end; {iRandom}


{ Get a random value (32 bits) in the range [1..x] }
function iRand(x: cardinal) : Cardinal;
begin
	// MOD: The standard, classical, accepted and time-honoured way
	//   of limiting the range of an RNG. Does not skew 32-bit results.
    Result := randrsl[randcnt] mod x +1;
    inc(randcnt);
    if (randcnt >255) then
    begin
         Isaac();
         randcnt := 0;
    end;
end; {iRand}


{ Get a random value (32 bits) in the range [0..x-1] }
function iRand0(x: cardinal) : Cardinal;
begin
	// MOD: The standard, classical, accepted and time-honoured way
	//   of limiting the range of an RNG. Does not skew 32-bit results.
    Result := randrsl[randcnt] mod x;
    inc(randcnt);
    if (randcnt >255) then
    begin
         Isaac();
         randcnt := 0;
    end;
end; {iRand0}


{ Get a random character in printable ASCII range }
function iRandA: CHAR;
begin
	// MOD: The standard, classical, accepted and time-honoured way
	//   of limiting the range of an RNG. Does not skew 32-bit results.
	Result := chr(iRandom mod 95 + 32);
end;

{ Get a random byte in printable ASCII range }
function iRandA: BYTE;
begin
	// MOD: The standard, classical, accepted and time-honoured way
	//   of limiting the range of an RNG. Does not skew 32-bit results.
	Result := iRandom mod 95 + 32;
end;

{ Get a random byte }
function iRandB: BYTE;
begin
	// MOD: The standard, classical, accepted and time-honoured way
	//   of limiting the range of an RNG. Does not skew 32-bit results.
	Result := iRandom mod $FF;
end;


{ Get a random string with characters mod <mod> and start <st>}
FUNCTION iRandStr(len: CARDINAL; m,st: BYTE): STRING;
	VAR i: CARDINAL;
	BEGIN
		iRandStr := '';
		For i := 1 to len do
	// MOD: The standard, classical, accepted and time-honoured way
	//   of limiting the range of an RNG. Does not skew 32-bit results.
			iRandStr += chr(iRandom mod m + st);
	END;



{ Randomize and seed on NOW string }
procedure iRandomize;
begin
  iSeed(FormatDateTime('YYYY-MM-DD-hh:nn:ss:zzz',Now), true);
end;


procedure iReset;
begin
  aa:=0; bb:=aa; cc:=aa;
  randcnt := 0;
  // Zeroise arrays
  for i := 0 to 255 do begin 
	randrsl[i]:=0;
	mm[i]:=0;
  end;
  // init WITHOUT the seed (speedup)
  iRandInit(false);  
end; {iReset}


{ Peek at ISAAC's current random state, but don't increment}
function iPeek: Cardinal;
begin
	Result := randrsl[randcnt];
end;

{ Peek at ISAAC's current random state, but don't increment}
function iPeek(idx: BYTE): CARDINAL;
begin
	Result := randrsl[idx];
end;



{$ifdef test}
// test main() from readable.c
procedure iTest;
	var i,j: cardinal;
	begin
		aa:=0;bb:=0;
		for i := 0 to 255 do begin 
			mm[i]:=0; randrsl[i]:=0;
		end;
		iRandinit(true);
		for i:=0 to 1 do begin
			isaac();
			for j:=0 to 255 do begin
				Write(IntToHex(randrsl[j],8));
				if ((j and 7)=7) then Writeln;
			end;
		end;
		Writeln;
	end;
{$endif}
	

initialization

	{$ifdef test}
	// test main() from readable.c
		iTest;
	{$endif}
	// Initialization
	iReset;

finalization
	iReset;
end.
