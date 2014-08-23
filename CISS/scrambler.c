// CISS  Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com  
// Cryptographically secure pseudo-random number pool and scrambler
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include "mystring.h"
#include "isctypes.h"
#include "iscipher.h"
#include "scrambler.h"

typedef struct randpair { ub4 element1; ub4 element2; } randpair;
struct randpair RandPair[MAXDEPTH];

// Randpool variables
ub4 PIdx = 0;
ub4 Pool[MAXDEPTH];

// RandPool methods

// Fill pool with random values
int RandPool_Fill(void) {
	register ub4 i;
	PIdx = 0;
	for(i=0;i<MAXDEPTH;i++)
		Pool[i] = iRandom();
	return TRUE;
}
	

// pull the next value from pool	
ub4 RandPool_Next(void) {
	if (PIdx < MAXDEPTH)
			PIdx++;
		else
			RandPool_Fill();
	return Pool[PIdx];
}


// pull from pool by external index	
ub4 RandPool_Get(ub4 i) {
	return Pool[i];
}

	
// Reset the index into Pool	
void RandPool_Reset(void) {
	PIdx = 0;
}
	
	
// Zeroise the Random pool
void RandPool_Clear(void) {
	register ub4 i;
	for (i=0;i<MAXDEPTH;i++) Pool[i] = 0;
}


// Record pairs for swapping
void InitRandPairs(ub4 dep, ub4 len) {
	register ub4 c;
	for (c=0;c<dep;c++) {
		RandPair[c].element1 = RandPool_Next() % len;
		RandPair[c].element2 = RandPool_Next() % len;
	}
}


// Swap two ASCII chars
void cswap(char *value1, char* value2) {
	char interim1;
	interim1 = *value1;
	*value1  = *value2;
	*value2  = interim1;
}


// Scramble by repeatedly swapping elements in string
// according to pattern preconfigured in InitRandPairs
char* Scrambled(char *t, ub4 depth) {
	register ub4 c;
	char temp[MAXM]="";
	if (depth>=MAXDEPTH) return temp;
	strcpy(temp,t);
	for (c=0;c<depth;c++) 
		cswap(&temp[RandPair[c].element1], &temp[RandPair[c].element2]);
	return temp;
}

// unScramble by repeatedly swapping elements in string
// according to pattern preconfigured in InitRandPairs
char* unScrambled(char *t, ub4 depth) {
	register long int c;
	char temp[MAXM]="";
	if (depth>=MAXDEPTH) return temp;
	strcpy(temp,t);
	for (c=depth-1;c>-1;--c)
		cswap(&temp[RandPair[c].element1], &temp[RandPair[c].element2]);
	return temp;
}

// set the mean random scramble-depth based on key-length
ub4 SetDepth(ub4 len) {
	register ub4 i,tot=0;
	for (i=0;i<1000;i++)
		tot += (iRandom() >> 19) * len;
	if ((tot / i) >= MAXDEPTH) return MAXDEPTH-1;
		else return (tot/i);
}

