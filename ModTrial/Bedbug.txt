    BEDBUG - A family of FLEA-inspired CSPRNGs and Stream Ciphers
    BEDBUG is a BEDBUG with a 128-word, 256-word or 512-word internal state array
    BEDBUG may be seeded with a 512-bit, a 1024-bit or a 2048-bit key
    BEDBUG is Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com
    BEDBUG is based on FLEA and other PRNG insights by Bob Jenkins. Public Domain.

What is BEDBUG?

BEDBUG is a small, fast, cryptographically secure pseudo-random number generator (CSPRNG) and stream cipher. It exhibits uniform distribution, mixes rapidly (with worst-case avalanche better than 16-bits), has no detected bias, and comes in three variants: BEDBUG128, with an internal state array of 128+3 32-bit words; BEDBUG256, with an internal state of 256+3 words; and BEDBUG512 with a 512+3-word state. The former permit seeding with a key of up to 512 or 1024 bits, the latter with a 2048-bit key.

The BEDBUG generator itself is a mere 5 lines of code:

		e = state[d % STATESIZE] - rot(b,iii);
		state[d % STATESIZE] = b ^ rot(c,jjj);
		b = c + rot(d,kkk);
		c = d + e;
		d = e + state[b % STATESIZE];
		
Three rotations, two pseudo-random lookups. 

The default BEDBUG configuration (B-RSW) alternates the rotation constants unpredictably. These four sets of three values differ between BEDBUG128, BEDBUG256 and BEDBUG512 and were selected, tuned and tested in each case for optimal avalanche, which is never less than 16.5 bits. The quantities are:

for BEDBUG128 -

1. iii = 14; jjj =  9; kkk = 16; // avalanche: 17.31 bits (worst case)
2. iii = 17; jjj =  8; kkk = 14; // avalanche: 17.31 bits (worst case)
3. iii = 17; jjj = 19; kkk = 30; // avalanche: 17.19 bits (worst case)
4. iii = 15; jjj =  9; kkk = 24; // avalanche: 17.19 bits (worst case)

for BEDBUG256 -

1. iii = 30; jjj = 13; kkk = 24; // avalanche: 17.00 bits (worst case)
2. iii =  5; jjj = 19; kkk = 20; // avalanche: 16.94 bits (worst case)
3. iii = 21; jjj =  4; kkk = 10; // avalanche: 16.84 bits (worst case)
4. iii = 29; jjj = 15; kkk =  6; // avalanche: 16.72 bits (worst case)

for BEDBUG512 -

1. iii = 17; jjj =  3; kkk =  4; // avalanche: 16.56 bits (worst case)
2. iii = 15; jjj = 18; kkk = 23; // avalanche: 16.52 bits (worst case)
3. iii =  3; jjj = 23; kkk = 24; // avalanche: 16.52 bits (worst case)
4. iii =  3; jjj = 15; kkk =  8; // avalanche: 16.53 bits (worst case)

The three BEDBUG variants presented here have been implemented identically in C and Pascal and can be found in their respective directories along with b, bb and bbb - three short programs to check their output. BEDBUG was developed and tested with the Tiny C compiler (tcc) and Free Pascal (fpc) under Win32. The NIST statistical tests were run under Linux.

To view BEDBUG's abbreviated internal state, please uncomment "#define TEST" in the C files and "{$define TEST}" in the Pascal files and "#define VERBOSE" and "{$define VERBOSE}" to display the full result & state arrays as they are generated on each call to bbXXX.

PRNG tests passed by BEDBUG:

* NIST suite of statistical tests for randomness: all tests for all seeds
* George Marsaglia's DIEHARD suite: all tests for all seeds
* Bob Jenkins' countx for sub-sequences of length 3-11
* Bob Jenkins' rngav (avalanche better than 16.5 bits average)
* John Walker's ENT test for entropy
* C.C.Kayne's GapTrial (value-spacings normal over all modulos)
* C.C.Kayne's ModTrial (value-distributions uniform over all modulos)
* C.C.Kayne's SeqTrial (same-value sequences normal over all modulos)
* C.C.Kayne's Visual 1 & 2

(Please view the Results.txt files in the relevant folders or verify the recorded results by re-running the tests themselves.)


Why BEDBUG?

Because there are not yet enough NSA- and NIST-free strong light ciphers in the world. ISAAC is one to rule them all, FLEA came and went briefly, and now hops BEDBUG into their traces, thanks to Bob Jenkins' gratifyingly published insights into the art of creating the PRNG.


Why "bedbug"?

Because it shares kinship and vocation with "flea", is tiny and mean, and is doubtless destined to irritate the bejaysus out of crackers and cryptanalysts.

Have fun with BEDBUG, and don't hesitate to get in touch if you encounter issues with any of the files included in this distribution. As always, your comments, suggestions, criticism and general feedback are welcomed.

C.C.Kayne
cckayne@gmail.com
https://github.com/cckayne/BEDBUG
https://code.google.com/p/bedbug-csprng-stream-cipher/
https://sourceforge.net/projects/bedbug-csprng-stream-cipher/
