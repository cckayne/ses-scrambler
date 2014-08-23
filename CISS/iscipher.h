// CISS  Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com  
#ifndef ISCIPHER_H_   /* Include guard */
#define ISCIPHER_H_

/* external results */
ub4 rsl[256], randcnt;

/* internal state */
static    ub4 mm[256];
static    ub4 aa=0, bb=0, cc=0;

// Get a random 32-bit value 0..MAXINT
ub4 iRandom();
// Get a random 32-bit value in range [min..max]
ub4 iRand(ub4 min, ub4 max);
// Get a random character in printable ASCII range
char iRandA();
// Get a random byte in [1..255]
/* < for C strings - must not contain '\0' > */
char iRandB();
// Warm up the CSPRNG for <rounds>
void iStir(ub4 rounds);
// Seed ISAAC with a string
void iSeed(char *seed, int flag);
// Seed ISAAC with a 1024-bit block of 32-bit words (Bob Jenkins method) 
void iSeedW(char *seed, int flag);
// obtain a random stir-depth
ub4 iDepth(char *str);
// obtain a cryptographic ASCII hash on <str>, of length <len>
char* iHash(char *s, int len);
// obtain a cryptographic ASCII string on <str>, of length <len>
char* iString(char *s, int len);
// obtain a cryptographic nonce/IV ASCII hash of NLEN or semi-random length
char* iNonce(int israndlen);
// XOR cipher on random stream. Output: ASCII string
char* Vernam(char *msg);
// Caesar-shift a character
char Caesar(enum ciphermode m, char ch, char shift, char modulo, char start);
// Caesar shift a character, randomly selecting rectus or reversus mode 
char mCaesar(enum ciphermode m, char ch, char shift, char modulo, char start);
// Caesar shift on a random character, randomly mixing modes 
char rmCaesar(enum ciphermode m, char ch, char modulo, char start);
// Caesar-shift a string on a pseudo-random stream
char* rCaesarStr(enum ciphermode m, char *msg, char modulo, char start);
// Caesar shift a string on random characters, randomly mixing modes 
char* rmCaesarStr(enum ciphermode m, char *msg, char modulo, char start);
// Classical Vigenere: Caesar cipher on a repeated key, optionally mixing modes
char* Vig(enum ciphermode m, char *pt, char *key, char modulo, char start, int mix);

#endif
