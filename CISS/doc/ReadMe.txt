ciss - The ISAAC Stream Cipher & Scrambler in C
ISAAC Copyright (C) R.Jenkins 1996, Public Domain.
CISS  Copyright (C) C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com

This program comes with absolutely no warranty:
It is free software: You are welcome to redistribute it.

Usage  : >ciss <message> <key-phrase> <cipher mode> <cipher type> <output>
Example: >ciss "my message" "my strong key" e v a
(Encrypt "my message" on key "my strong key" with the Vernam cipher)
Maximum message length: 20480 B; maximum key length: 1024 B.
Minimum message length:     2 B; minimum key length:   12 B.

[e] cipher mode: Encrypt
[d] cipher mode: Decrypt
[c] cipher type: Caesar/MOD
[m] cipher type: Caesar/MIX
[v] cipher type: Vernam/XOR
[a] output form: ASCII A-Z
[h] output form: Hexadecimal

Arguments must be passed in the usage order, and all are required.

Please note that the nonce-scrambling and Vigenere mixing that tops off the cipher sequence is only available with MOD 26 ASCII output. MOD 95 hexadecimal and Vernam will do key-derivation but will omit the signature SES super-encryption. So if you desire the diffusion and unique-ciphertext feature of SES when using CISS, please make sure to set the "a" option with either "c" (Caesar/MOD) or "m" (Caesar/MIX).

Encipherment example:

Example: >ciss "my message" "my strong key" e m a
Output : >HDYZWRQZZTQLIMQBZWZNJTNOPVZ
(Encrypt "my message" on key "my strong key" using Caesar/MIX and ASCII output.)

Decipherment example:

Example: >ciss HDYZWRQZZTQLIMQBZWZNJTNOPVZ "my strong key" d m a
Output : >my message


Why CISS?
---------

The aim of CISS is to demonstrate the portability of the basic SES ISAAC-based cipher algorithm and its ease of implementation in a popular programming language other than Pascal. C may not have Free Pascal's advanced string-handling functions or object model, but it was surprising what could be accomplished in a couple of days translating most of the SES cipher routines to C.

CISS lacks only the inner Vigenere plus Scrambler keyword-loop and the Keccak (SHA-3) key derivation germane to SES, but these could easily be added, and may well be in a future release. Despite this, CISS still manages 4 super-encipherments on any message. The real elegance of CISS is that it uses ISAAC for everything, including hashing and key-stretching, gaining in speed by an order of magnitude while keeping it all simple and compact.

CISS is a hot-rod SES stripped down to the metal.

Cryptographically, CISS is _strong_ when measured against any of today's vaunted standards (AES and Triple DES included), though doubtless not quite as strong as SES in its present incarnation, lacking as it does the inner cipher cascade. 

However, "more than good enough" justly describes CISS, if only because it is built upon ISAAC, Bob Jenkins' masterpiece, still unbroken after more than 20 years. 

I think you'll find that CISS proves how _easy_ truly strong cryptography is to implement, while avoiding all those NIST standards with their NSA backdoors.


Compiling CISS
--------------

CISS compilation has been tested with tcc in Win32 and gcc on Linux.

To compile at the command prompt, type

tcc ciss.c mystring.c iscutils.c iscipher.c scrambler.c

or

gcc ciss.c mystring.c iscutils.c iscipher.c scrambler.c -o ciss

or run the make.bat or make.sh batch files included with the source.

If you'd like to view cipher-log output during test runs of CISS, please uncomment "//#define LOG" in isctypes.h

For convenience, pre-compiled binaries for Windows and Linux are included in the /bin directory under their respective sub-folders.
 
Enjoy!

C.C.Kayne
cckayne@gmail.com
