unit bb128;
// BEDBUG128 - A FLEA-inspired CSPRNG and Stream Cipher
// BEDBUG128 is a BEDBUG with a 128-byte internal state array
// BEDBUG128 may be seeded with a 512-bit key
// BEDBUG128 Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com
// BEDBUG128 is based on FLEA and other PRNG insights by Bob Jenkins. Public Domain.
{$mode delphi}
{ $define TEST}
// BB128b performs an extra pseudo-random lookup
{$define B}
// BB128 option: switch ROT constants at each bb128() call
{$define RSW}

interface

// byte size of state array
const	STSZ = 128;
		STM1 = STSZ-1;
var 	MODU:  byte = 26; 
		START: byte = ord('A');

// initialize/reset BEDBUG128
procedure bbReset;
// obtain a BEDBUG128 pseudo-random value in [0..2**32]
function bbRandom: Cardinal;
// obtain a random character in printable ASCII range
function bbRandA: char;
// seed BEDBUG128 with a 512-bit block of 4-byte words (Bob Jenkins method) 
procedure bbSeedW(seed: string; rounds: integer);

implementation
{$ifdef TEST}uses StrUtils;{$endif}

// 2**32/phi, where phi is the golden ratio
const	GOLDEN = $9e3779b9;

// BB128 STATE
var		rsl: 	array[0..STSZ] of Cardinal;
		state: 	array[0..STSZ] of Cardinal;
		b,c,d,e,rcnt: Cardinal;

// BB128 ROT switcher
var			ri: Cardinal = 0;
type 		TRsw = record iii: Cardinal; jjj: Cardinal; kkk: Cardinal end;
var 		rsw: array[0..3] of TRsw;
procedure 	initrsw; 
begin
ri:=0;
{$ifdef B}
rsw[0].iii := 14; rsw[0].jjj :=  9; rsw[0].kkk := 16; // avalanche: 17.31 bits (worst case)
rsw[1].iii := 17; rsw[1].jjj :=  8; rsw[1].kkk := 14; // avalanche: 17.31 bits (worst case)
rsw[2].iii := 17; rsw[2].jjj := 19; rsw[2].kkk := 30; // avalanche: 17.19 bits (worst case)
rsw[3].iii := 15; rsw[3].jjj :=  9; rsw[3].kkk := 24; // avalanche: 17.19 bits (worst case)
{$else}
rsw[0].iii := 12; rsw[0].jjj := 25; rsw[0].kkk :=  4; // avalanche: 17.50 bits (worst case)
rsw[1].iii := 18; rsw[1].jjj := 24; rsw[1].kkk :=  4; // avalanche: 17.44 bits (worst case)
rsw[2].iii := 15; rsw[2].jjj := 22; rsw[2].kkk :=  6; // avalanche: 17.31 bits (worst case)
rsw[3].iii :=  6; rsw[3].jjj :=  6; rsw[3].kkk := 26; // avalanche: 17.31 bits (worst case)
{$endif}
end;

	
function rot(var x: Cardinal; const k: Cardinal): Cardinal;
	begin
		rot := (x shl k) or (x shr (32-k));
	end;

	
{$ifdef TEST}
var bcnt: Cardinal = 0;
procedure statepeek; 
	var i, gcnt: Cardinal;
	begin
		gcnt := 0;
		inc(bcnt);
		Writeln(bcnt:3,') bb128 using rsw[',ri,']...');
		for i:=0 to STM1 do begin
			Writeln('rsl ',
				i:3,': ',rsl[i]:11,chr(rsl[i] mod MODU + START):2,
					dec2numb((rsl[i] and 255),2,16):3,'  | state ',
				i:3,': ',state[i]:11,chr(state[i] mod MODU + START):2,
					dec2numb((state[i] and 255),2,16):3);
			if (state[i]=GOLDEN) then inc(gcnt);
		end;
		Writeln('     b = ',b:11,chr(b mod MODU+START):2,dec2numb((b and 255),2,16):3);     
		Writeln('     c = ',c:11,chr(c mod MODU+START):2,dec2numb((c and 255),2,16):3);    
		Writeln('     d = ',d:11,chr(d mod MODU+START):2,dec2numb((d and 255),2,16):3);
		Writeln('     ',gcnt,' GOLDEN');
	end;
{$endif}


// BEDBUG128 is filled every 128 rounds
procedure bb128;
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
		{$ifdef TEST}
		statepeek;
		{$endif}
		{$ifdef RSW}
		ri := (c and 3);
		{$endif}
	end;
	
	
// initialize/reset BEDBUG128 (obligatory)
procedure bbReset;
	var i: Cardinal;
	begin
		rcnt:=0;
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
			bb128;
			rcnt := 0;
		end;
	end;


// obtain a random character in printable ASCII range
function bbRandA: char;
	begin
		bbRandA := chr(bbRandom mod MODU + START);
	end;


// seed BEDBUG128 with a 512-bit block of 4-byte words (Bob Jenkins method) 
procedure bbSeedW(seed: string; rounds: integer);
	var i,l: Cardinal;
		p: pointer;
	begin
		p:=@state[0];
		l:=Length(seed);
		bbReset;
		for i:=0 to l-1 do
			byte((p+i)^) := byte(seed[i+1]);
		bb128;
		for i:=1 to rounds do bbRandom;  
	end;

end.