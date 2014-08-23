#ifndef SCRAMBLER_H_   /* Include guard */
#define SCRAMBLER_H_

// Fill pool with random values
int RandPool_Fill(void);
// pull the next value from pool	
ub4 RandPool_Next(void);
// pull from pool by external index	
ub4 RandPool_Get(ub4 i);
// Reset the index into Pool	
void RandPool_Reset(void);
// Zeroise the Random pool
void RandPool_Clear(void);
// Record pairs for swapping
void InitRandPairs(ub4 dep, ub4 len);
// Scramble by repeatedly swapping elements in string
// according to pattern preconfigured in InitRandPairs
char* Scrambled(char *t, ub4 depth);
// unScramble by repeatedly swapping elements in string
// according to pattern preconfigured in InitRandPairs
char* unScrambled(char *t, ub4 depth);
// set the mean random scramble-depth
ub4 SetDepth(ub4 l);

#endif
