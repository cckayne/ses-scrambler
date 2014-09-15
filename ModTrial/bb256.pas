unit bb256;
// BEDBUG256 - A FLEA-inspired CSPRNG and Stream Cipher
// BEDBUG256 is a BEDBUG with a 256-byte internal state array
// BEDBUG256 may be seeded with a 1024-bit key
// BEDBUG256 Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com
// BEDBUG256 is based on FLEA and other PRNG insights by Bob Jenkins. Public Domain.
{$mode delphi}
// BB256b uses an extra pseudo-random lookup
{$define B}
// BB256 option: switch ROT constants at each bb256() call
{$define RSW}

interface

// byte size of state array
const	STSZ = 256;
		STM1 = STSZ-1;
var 	MODU:  byte = 26; 
		START: byte = ord('A');

// initialize/reset BEDBUG256
procedure bbReset;
// obtain a BEDBUG256 pseudo-random value in [0..2**32]
function bbRandom: Cardinal;
// Get a random character in printable ASCII range
function bbRandA: char;
// Seed BEDBUG256 with a 1024-bit block of 4-byte words (Bob Jenkins method) 
procedure bbSeedW(seed: string; rounds: integer);

implementation

// 2**32/phi, where phi is the golden ratio
const	GOLDEN = $9e3779b9;

// BB256 STATE
var		rsl: 	array[0..STSZ] of Cardinal;
		state: 	array[0..STSZ] of Cardinal;
		b,c,d,e,rcnt: Cardinal;

// BB256 ROT switcher
var			ri: Cardinal = 0;
type 		TRsw = record iii: Cardinal; jjj: Cardinal; kkk: Cardinal end;
var 		rsw: array[0..3] of TRsw;
procedure 	initrsw; 
begin
{$ifdef B}
rsw[0].iii := 30; rsw[0].jjj := 13; rsw[0].kkk := 24; // avalanche: 17.00 bits (worst case)
rsw[1].iii :=  5; rsw[1].jjj := 19; rsw[1].kkk := 20; // avalanche: 16.94 bits (worst case)
rsw[2].iii := 21; rsw[2].jjj :=  4; rsw[2].kkk := 10; // avalanche: 16.84 bits (worst case)
rsw[3].iii := 29; rsw[3].jjj := 15; rsw[3].kkk :=  6; // avalanche: 16.72 bits (worst case)
{$else}
rsw[0].iii := 20; rsw[0].jjj := 14; rsw[0].kkk :=  8; // avalanche: 17.00 bits (worst case)
rsw[1].iii := 20; rsw[1].jjj :=  2; rsw[1].kkk := 22; // avalanche: 16.97 bits (worst case)
rsw[2].iii := 22; rsw[2].jjj := 23; rsw[2].kkk := 24; // avalanche: 16.78 bits (worst case)
rsw[3].iii :=  7; rsw[3].jjj :=  8; rsw[3].kkk := 22; // avalanche: 16.72 bits (worst case)
{$endif}
end;

	
function rot(var x: Cardinal; const k: Cardinal): Cardinal;
	begin
		rot := (x shl k) or (x shr (32-k));
	end;

	
{$ifdef TEST}
procedure statepeek; 
	var i: Cardinal;
	begin
		for i=0 to STM1 do
			Writeln('rsl: ',
				i:3,rsl[i]:14,(rsl[i] mod MODU + START):4,(rsl[i] and 255):4,' | ',
				i:3,state[i]:14,(state[i] mod MODU + START):4,(state[i] and 255):4);
	end;
{$endif}


// BEDBUG256 is filled every 256 rounds
procedure bb256;
    var i: Cardinal;
	begin
		for i:=0 to STM1 do begin
			e := state[d and STM1] - rot(b,rsw[ri].iii);
			state[d and STM1] := b xor rot(c,rsw[ri].jjj);
			b := c + rot(d,rsw[ri].kkk);
			c := d + e;
			{$ifdef B}
			d := e + state[b and STM1];
			{$else}
			d := e + state[i];
			{$endif}
			rsl[i] := d;
		end;
		{$ifdef RSW}
		ri := (c and 3);
		{$endif}
		{$ifdef TEST}
		statepeek;
		{$endif}
	end;
	
	
// initialize/reset BEDBUG (obligatory)
procedure bbReset;
	var i: Cardinal;
	begin
		initrsw;
		b := GOLDEN; c := b; d:=c; e:=d;
		for i:=0 to STM1 do
			state[i] := GOLDEN; rsl[i] := 0;
	end;


// obtain a BEDBUG pseudo-random value in [0..2**32]
function bbRandom: Cardinal;
	begin
		bbRandom := rsl[rcnt];
		inc(rcnt);
		if (rcnt=STSZ) then begin
			bb256;
			rcnt := 0;
		end;
	end;


// Get a random character in printable ASCII range
function bbRandA: char;
	begin
		bbRandA := chr(bbRandom mod MODU + START);
	end;


// Seed BEDBUG256 with a 1024-bit block of 4-byte words (Bob Jenkins method) 
procedure bbSeedW(seed: string; rounds: integer);
	var i,l: Cardinal;
		p: pointer;
	begin
		p:=@state[0];
		l:=Length(seed);
		bbReset;
		for i:=0 to l-1 do
			byte((p+i)^) := byte(seed[i+1]);
		bb256;
		for i:=1 to rounds do bbRandom;  
	end;

end.