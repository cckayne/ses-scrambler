// CISS  Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com  
// The ISAAC CSPRNG & stream cipher in C, copyright R.Jenkins, 1996. Public Domain.
// Caesar, Vernam & Vigenere ciphers, 
//  plus ISAAC-based cryptographic string and hash functions.
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include "mystring.h"
#include "isctypes.h"
#include "iscipher.h"
	
char MOD = 95; char START = 32;

/* Compiles and works with tcc in win32 and gcc on Linux (with warnings)
------------------------------------------------------------------------------
readable.c: My random number generator, ISAAC.
(c) Bob Jenkins, March 1996, Public Domain
You may use this code in any way you wish, and it is free.  No warrantee.
------------------------------------------------------------------------------
*/

void isaac()
{
   register ub4 i,x,y;

   cc = cc + 1;    /* cc just gets incremented once per 256 results */
   bb = bb + cc;   /* then combined with bb */

   for (i=0; i<256; ++i)
   {
     x = mm[i];
     switch (i&3)
     {
     case 0: aa = aa^(aa<<13); break;
     case 1: aa = aa^(aa>>6); break;
     case 2: aa = aa^(aa<<2); break;
     case 3: aa = aa^(aa>>16); break;
     }
     aa = mm[(i+128)&255] + aa;
     mm[i] = y  = mm[(x>>2)&255] + aa + bb;
     rsl[i] = bb = mm[(y>>10)&255] + x;
   }
   // not in original readable.c
   randcnt = 0;
}

/* if (flag!=0), then use the contents of rsl[] to initialize mm[]. */
#define mix(a,b,c,d,e,f,g,h) \
{ \
   a^=b<<11; d+=a; b+=c; \
   b^=c>>2;  e+=b; c+=d; \
   c^=d<<8;  f+=c; d+=e; \
   d^=e>>16; g+=d; e+=f; \
   e^=f<<10; h+=e; f+=g; \
   f^=g>>4;  a+=f; g+=h; \
   g^=h<<8;  b+=g; h+=a; \
   h^=a>>9;  c+=h; a+=b; \
}

void randinit(int flag)
{
   register int i;
   ub4 a,b,c,d,e,f,g,h;
   aa=bb=cc=0;
   a=b=c=d=e=f=g=h=0x9e3779b9;  /* the golden ratio */

   for (i=0; i<4; ++i)          /* scramble it */
   {
     mix(a,b,c,d,e,f,g,h);
   }

   for (i=0; i<256; i+=8)   /* fill in mm[] with messy stuff */
   {
     if (flag)                  /* use all the information in the seed */
	 {
       a+=rsl[i  ]; b+=rsl[i+1]; c+=rsl[i+2]; d+=rsl[i+3];
       e+=rsl[i+4]; f+=rsl[i+5]; g+=rsl[i+6]; h+=rsl[i+7];
     }
     mix(a,b,c,d,e,f,g,h);
     mm[i  ]=a; mm[i+1]=b; mm[i+2]=c; mm[i+3]=d;
     mm[i+4]=e; mm[i+5]=f; mm[i+6]=g; mm[i+7]=h;
   }

   if (flag)
   {        /* do a second pass to make all of the seed affect all of mm */
	 for (i=0; i<256; i+=8)
     {
       a+=mm[i  ]; b+=mm[i+1]; c+=mm[i+2]; d+=mm[i+3];
       e+=mm[i+4]; f+=mm[i+5]; g+=mm[i+6]; h+=mm[i+7];
       mix(a,b,c,d,e,f,g,h);
       mm[i  ]=a; mm[i+1]=b; mm[i+2]=c; mm[i+3]=d;
       mm[i+4]=e; mm[i+5]=f; mm[i+6]=g; mm[i+7]=h;
     }
   }

   isaac();            /* fill in the first set of results */
   randcnt=0;        /* prepare to use the first set of results */
}


// Get a random 32-bit value [0..MAXINT]
ub4 iRandom()
{
	ub4 r = rsl[randcnt];
	++randcnt;
	if (randcnt >255) {
		isaac();
		randcnt = 0;
	}
	return r;
}

// Get a random 32-bit value in range [min..max]
ub4 iRand(ub4 min, ub4 max) {
	ub4 r;
	do
		r=iRandom() % max;
	while(r<min);
	return r;
}


// Get a random character in printable ASCII range
char iRandA()
{	
	return iRandom() % MOD + START;
}


// Get a random byte in [1..255]
/* < for C strings - must not contain '\0' > */
char iRandB()
{	
	return iRandom() % 255 +1 ;
}


// Warm up the CSPRNG for <rounds>
void iStir(ub4 rounds) {
	register ub4 i;
	for (i=0; i<rounds; i++) iRandom();
}


// Seed ISAAC with a string
void iSeed(char *seed, int flag)
{
	register ub4 i,l;
	// zeroise mm and rsl arrays
	for (i=0; i<256; i++){mm[i]=0;rsl[i]=0;}
	l = strlen(seed);
	// fill rsl[] with seed bytes
	for (i=0; i<256; i++)
        if (i<=l) rsl[i] = seed[i];
	randinit(flag);
}

// Seed ISAAC with a 1024-bit block of words (Bob Jenkins method) 
void iSeedW(char *seed, int flag)
{
	register ub4 i,l;
	char s[MAXK];
	l=strlen(seed);
	memset(s,0,l+1);
	strcpy(s,seed);
	for (i=0; i<256; i++) {mm[i]=0;rsl[i]=0;}
	memcpy((char *)rsl, (char *)s, l);
	randinit(flag);
}


// obtain a random stir-depth
ub4 iDepth(char *str) {
	register ub4 i,l,r;
	register ub4 tot = 0;
	l = strlen(str);
	r = iRandom() % 999;
	for (i=0; i<r; i++) 
		tot += (iRandom() >> 21) * l;
	return tot / i;
}


// obtain a cryptographic ASCII hash on <str>, of length <len>
char* iHash(char *s, int len) {
	register ub4 i,d;
	char h[MAXK];
	memset(h,'\0', len+1);
	iSeed(s,TRUE);
	d = iDepth(s);
	iStir(d);
	for (i=0; i<len; i++) h[i] = iRandB();
	return h;
}
	

// obtain a cryptographic ASCII string on <str>, of length <len>
char* iString(char *s, int len) {
	register ub4 i,d;
	char str[MAXK];
	memset(str,'\0', len+1);
	iSeed(s,TRUE);
	d = iDepth(s);
	iStir(d);
	for (i=0; i<len; i++) str[i] = iRandom() % MOD + START;
	return str;
}


// obtain a cryptographic nonce/IV ASCII hash of NLEN or semi-random length
char* iNonce(int israndlen) {
	register ub4 len;
	char non[MAXK]="";
	if (israndlen) len=iRand(NLENMIN,NLENMAX);
		else len=NLEN;
	sprintf(non,"%d ",getpid());
	strcat(non,datetimestr());
	strcpy(non,iString(non,len));
	return non;
}


// XOR cipher on random stream. Output: ASCII string
char* Vernam(char *msg)
	{
		register ub4 i,l;
		l = strlen(msg);
		char v[MAXM];
		// zeroise v
		memset(v,'\0',l+1);
		// XOR message
		for (i=0; i<l; i++) 
			v[i] = iRandA() ^ msg[i];
		return v;
	}

	
// Caesar-shift a character
char Caesar(enum ciphermode m, char ch, char shift, char modulo, char start)
	{
		register int n;
		if (m == cmDecipher) shift = -shift;
		n = (ch-start) + shift;
		n = n % modulo;
		if (n<0) n += modulo;
		return start+n;
	}

// Caesar shift a character, randomly selecting rectus or reversus mode 
char mCaesar(enum ciphermode m, char ch, char shift, char modulo, char start)
	{
		if (iRandom() % 100 > 49) {
			// rectus
			if (m == cmEncipher) 
				// shift
				return Caesar(cmEncipher, ch, shift, modulo, start);
			else
				// unshift
				return Caesar(cmDecipher, ch, shift, modulo, start);
		} else {
			// reversus
			if (m == cmEncipher) 
				// unshift
				return Caesar(cmDecipher, ch, shift, modulo, start);
			else
				// shift
				return Caesar(cmEncipher, ch, shift, modulo, start);
		}
	}	
	
	
	
// Caesar shift on a random character, randomly mixing modes 
char rmCaesar(enum ciphermode m, char ch, char modulo, char start)
	{
		if (iRandom() % 100 > 49) {
			// rectus
			if (m == cmEncipher) 
				// shift
				return Caesar(cmEncipher, ch, iRandom() % modulo, modulo, start);
			else
				// unshift
				return Caesar(cmDecipher, ch, iRandom() % modulo, modulo, start);
		} else {
			// reversus
			if (m == cmEncipher) 
				// unshift
				return Caesar(cmDecipher, ch, iRandom() % modulo, modulo, start);
			else
				// shift
				return Caesar(cmEncipher, ch, iRandom() % modulo, modulo, start);
		}
	}	
	
	
// Caesar-shift a string on a pseudo-random stream
char* rCaesarStr(enum ciphermode m, char *msg, char modulo, char start)
	{
		register ub4 i,l;
		l = strlen(msg);
		char c[MAXM];
		// zeroise c
		memset(c,'\0',l+1);
		// Caesar-shift message
		for (i=0; i<l; i++) 
			c[i] = Caesar(m, msg[i], iRandA(), modulo, start);
		return c;
	}

// Caesar-shift a string, randomly mixing rectus and reversus
char* rmCaesarStr(enum ciphermode m, char *msg, char modulo, char start)
	{
		register ub4 i,l;
		l = strlen(msg);
		char c[MAXM];
		// zeroise c
		memset(c,'\0',l+1);
		// Caesar-shift message
		for (i=0; i<l; i++) 
			c[i] = rmCaesar(m, msg[i], modulo, start);
		return c;
	}

	
// Classical Vigenere: Caesar cipher on a repeated key, optionally mixing modes
char* Vig(enum ciphermode m, char *pt, char *key, char modulo, char start, int mix) {
	register ub4 c,n,lp,lk;
	char kt[MAXM]=""; char ct[MAXM]="";
	lp=strlen(pt); lk=strlen(key);
	memset(kt,'\0',lp+1); memset(ct,'\0',lp+1);
	//construct the key-string from the key
	for (c=0;c<lp;c++) { n = c % lk; kt[c]=key[n]; }
	#ifdef TEST
	puts(kt);
	#endif
	//cipher on the constructed key, optionally mixing modes
	if (mix) for (c=0;c<lp;c++) 
		ct[c] = mCaesar(m, pt[c], kt[c], modulo, start);
	else	 for (c=0;c<lp;c++)
		ct[c] =  Caesar(m, pt[c], kt[c], modulo, start);
	return ct;
}
