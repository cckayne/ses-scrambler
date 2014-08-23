// ciss - The ISAAC Stream Cipher & Scrambler in C
// ISAAC Copyright R.Jenkins 1996, Public Domain
// CISS  Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com  

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include "isctypes.h"
#include "mystring.h"
#include "iscipher.h"
#include "iscutils.h"
#include "scrambler.h"

int SCRAMBLER=TRUE; int NONCE=TRUE; int MIX=TRUE;

#ifdef LOG
// log and counter
typedef struct LOGENTRY { char Title[4]; char Entry[LINE+1]; } LOGENTRY;
struct LOGENTRY Log[20]; 
ub4 lcnt = 0;

// add log entry
void log_add(char *title, char *entry) {
	char e[LINE+1]="";
	if (strlen(entry)>LINE) strcpy(e,leftstr(entry,LINE));
		else strcpy(e,entry);
	strcpy(Log[lcnt].Title,title);
	strcpy(Log[lcnt].Entry,e);
	lcnt++;
}

// display log entries
void log_show(void) {
	register ub4 i;
	for (i=0;i<lcnt;i++) {
		printf("%s : ",Log[i].Title);
		printf("%s\n",Log[i].Entry);
	}
}

// securely clear log entries
void log_clear(void) {
	register ub4 i;
	for (i=0;i<lcnt;i++) {
		memset(Log[i].Title,0,sizeof(Log[i].Title));
		memset(Log[i].Entry,0,sizeof(Log[i].Entry));
		memset(Log[i].Entry,0xFF,sizeof(Log[i].Entry));
		memset(Log[i].Entry,0,sizeof(Log[i].Entry));
	}
}
#endif


int banner(void)
	{
		puts("ciss - The ISAAC Stream Cipher & Scrambler in C");
		puts("ISAAC Copyright (C) R.Jenkins 1996, Public Domain.");
		puts("CISS  Copyright (C) C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com");
		printf("\n%s\n", "This program comes with absolutely no warranty:"); 
		puts("It is free software: You are welcome to redistribute it."); 
		return 0;
	}

int options(void)
	{
		printf("\n");
		puts("[e] cipher mode: Encrypt");
		puts("[d] cipher mode: Decrypt");
		puts("[c] cipher type: Caesar/MOD");
		puts("[m] cipher type: Caesar/MIX");
		puts("[v] cipher type: Vernam/XOR");
		puts("[a] output form: ASCII A-Z");
		puts("[h] output form: Hexadecimal");
		printf("\n");
		printf("%s\n","Arguments must be passed in the usage order, and all are required.");
		puts("Please see ReadMe.txt for further details and background.");
		return 0;
	}

int usage(void)
	{
		printf("\n");
		printf("%s\n","Usage  : >ciss <message> <key-phrase> <cipher mode> <cipher type> <output>");
		printf("%s\n","Example: >ciss \"my message\" \"my strong key\" e v a");
		printf("%s\n","(Encrypt \"my message\" on key \"my strong key\" with the Vernam cipher)"); 
		printf("Maximum message length: %5d B; maximum key length: %4d B.\n",MAXM,MAXK-1);
		printf("Minimum message length: %5d B; minimum key length: %4d B.\n",MINM,MINK  );
		options();
		return 0;
	}
	
int info(void)
	{
		banner(); usage(); return 0;
	}


int main(int argc, char *argv[])
{
	// stir depth and nonce length
	ub4 dep, sdep = MAXM, lnce = NLEN;
	enum ciphermode cmode   = cmNone;
	enum ciphertype ctype	= ctNone;
	enum outputform oform	= ofHEX;
	// input: message & key-phrase
	char msg[MAXM] = ""; 
	char key[MAXM] = "";
	// ciphertext & plaintext
	char ctx[MAXM], ptx[MAXM];
	// derived & stretched key
	char kdf[MAXK] = "";
	// IV/nonce
	char nce[MAXK] = "";
	// check the command line
	if (argc >= 5) {
		if ((argc>=2) && strlen(argv[1])<MAXM) strcpy(msg,argv[1]);
		if ((argc>=3) && strlen(argv[1])<MAXK) strcpy(key,argv[2]);
		if (argc>=4)
			if ((strcmp(argv[3],"d")==0) || (strcmp(argv[3],"D")==0))
				 cmode = cmDecipher; else
			if ((strcmp(argv[3],"e")==0) || (strcmp(argv[3],"E")==0)) 
				 cmode = cmEncipher; else cmode = cmNone;
		if (argc>=5)
			if ((strcmp(argv[4],"v")==0) || (strcmp(argv[4],"V")==0))
				 ctype = ctVernam; else
			if ((strcmp(argv[4],"c")==0) || (strcmp(argv[4],"C")==0))
				 ctype = ctCaesar; else
			if ((strcmp(argv[4],"m")==0) || (strcmp(argv[4],"M")==0)) 
				ctype = ctCaesarM; else ctype = ctNone;
		if (argc>=6) 
			if ((strcmp(argv[5],"a")==0) || (strcmp(argv[4],"A")==0))
				oform = ofASC; else oform = ofHEX;
	}
	// sanity checks
	if (TRUE) {
		if ((strlen(msg)<MINM) || (strlen(key)<MINK)) { info(); exit(0); }
		if ((cmode==cmNone) || (ctype==ctNone))       { info(); exit(0); }
		// only hex output available for Vernam
		if (ctype==ctVernam) oform=ofHEX;
		// output mode MOD 26? (not possible with Vernam)
		if ((oform==ofASC) && (ctype!=ctVernam)) { MOD=26; START='A'; }
		// no nonce scrambling or mixing available with hex output
		if (oform==ofHEX) SCRAMBLER=NONCE=MIX=FALSE;
	}
	// B E G I N  P R E P A R A T I O N
	// ISAAC: preliminary seeding
	iSeedW(key,TRUE);
	
	if (SCRAMBLER) {
		sdep = SetDepth(strlen(key));
		#ifdef LOG
		char tmp[12]=""; sprintf(tmp,"%d",sdep);
		log_add("DEP",tmp);
		#endif
	}
	
	if (NONCE) {
		// obtain nonce/IV hash of fixed or random length
		strcpy(nce,iNonce(FALSE));
		// note nonce length for later
		lnce = strlen(nce);
	}

	// Key-derivation starts:
	if (TRUE) {
		// 1) seed ISAAC with a key-derived hash
		strcpy(kdf,iHash(key,MAXK-1));
		iSeedW(kdf,TRUE);
		// 2) calculate stir-depth
		dep = iDepth(kdf);
		// 3) warm up ISAAC with <dep> rounds
		iStir(dep); 
	}
	#ifdef TEST
		#ifdef LOG
		log_add("DKY",leftstr(kdf,LINE));
		#endif
	#endif
	// Key-derivation ends.
	
	if (SCRAMBLER) {
		// prepare scrambler's random pool
		RandPool_Fill();
	}
	// E N D  P R E P A R A T I O N.
	
	// B E G I N  M A I N  C I P H E R  S E Q U E N C E
	// Mode: Encipher
	if (cmode==cmEncipher) {
		// pre-process message if output is mod 26
		if (oform==ofASC) strcpy(msg, PreProcessText(msg));
			#ifdef LOG
			if (oform==ofASC) log_add("MSG",msg);
			#endif
		// Encrypt: Vernam XOR
		if (ctype==ctVernam)  strcpy(ctx, Vernam(msg));
		// Encrypt: Caesar MOD
		if (ctype==ctCaesar)  strcpy(ctx, rCaesarStr(cmEncipher, msg, MOD, START));
		// Encrypt: Caesar MIX
		if (ctype==ctCaesarM) strcpy(ctx, rmCaesarStr(cmEncipher, msg, MOD, START));
		// convert to hexadecimal as appropriate
		if (oform==ofHEX) strcpy(ctx,ascii2hex(ctx));
			#ifdef LOG
			log_add(" CT",ctx);
			#endif
		if (MIX) {
			// Mix: Vigenere-cipher the ciphertext on the nonce
			strcpy(ctx,Vig(cmode,ctx,nce,MOD,START,FALSE));
				#ifdef LOG
				log_add("NCE",nce);
				log_add("VCT",ctx);
				#endif
		}
		if (NONCE) {
			// append ciphertext to nonce
			strcat(nce,ctx); strcpy(ctx,nce);
				#ifdef LOG
				log_add("NCT",ctx);
				#endif
		}
		if (SCRAMBLER) {
			// prepare scrambler context & scramble ciphertext
			InitRandPairs(sdep,strlen(ctx));
			strcpy(ctx,Scrambled(ctx,sdep));
				#ifdef LOG
				log_add("SCT",ctx);
				#endif
		}
	}
	// Mode: Decipher
	if (cmode==cmDecipher) {
		// Convert hexadecimal ciphertext to ASCII (not in mod 26)
		if (oform==ofHEX) strcpy(ctx, hex2ascii(msg)); else strcpy(ctx, msg);
		if (SCRAMBLER) {
				#ifdef LOG
				log_add("SCT",ctx);
				#endif
			// prepare scrambler context & unscramble ciphertext
			InitRandPairs(sdep,strlen(ctx));
			strcpy(ctx,unScrambled(ctx,sdep));
				#ifdef LOG
				log_add("UST",ctx);
				#endif
		}
		if (NONCE) {
			// detach ciphertext from nonce
			strcpy(nce,leftstr(ctx,lnce));
			strcpy(ctx,rightstr(ctx,strlen(msg)-lnce));
				#ifdef LOG
				log_add("VCT",ctx);
				log_add("NCE",nce);
				#endif
		}
		if (MIX) {
			// Un-mix: Vigenere-decipher the ciphertext on the nonce
			strcpy(ctx,Vig(cmode,ctx,nce,MOD,START,FALSE));
				#ifdef LOG
				log_add("UVC",ctx);
				#endif
		}
		// Decrypt: Vernam XOR
		if (ctype==ctVernam) strcpy(ptx, Vernam(ctx));
		// Decrypt: Caesar MOD
		if (ctype==ctCaesar)  strcpy(ptx, rCaesarStr(cmDecipher, ctx, MOD, START));
		// Decrypt: Caesar MIX
		if (ctype==ctCaesarM) strcpy(ptx, rmCaesarStr(cmDecipher,ctx, MOD, START));
		// post-process plaintext if output is mod 26
		if (oform==ofASC) strcpy(ptx, PostProcessText(ptx));
	}  
	// E N D  M A I N  C I P H E R  S E Q U E N C E .
	#ifdef LOG
	log_show ();
	log_clear();
	#endif
	// P R O G R A M  O U T P U T
	if ((strcmp(ptx,"") != 0) || (strcmp(ctx,"") != 0)) {
		// Mode: Encipher
		if (cmode==cmEncipher) puts(ctx);
		// Mode: Decipher
		if (cmode==cmDecipher) puts(ptx);
		
		// belt'n'braces variable cleanup
		if (TRUE) {
			iSeed("",FALSE);
			memset(rsl,0,sizeof(rsl));memset(rsl,0xFF,sizeof(rsl));memset(rsl,0,sizeof(rsl));
			memset(mm ,0,sizeof(mm ));memset( mm,0xFF,sizeof( mm));memset( mm,0,sizeof( mm));
			memset(msg,0,sizeof(msg));memset(msg,0xFF,sizeof(msg));memset(msg,0,sizeof(msg));
			memset(key,0,sizeof(key));memset(key,0xFF,sizeof(key));memset(key,0,sizeof(key));
			memset(kdf,0,sizeof(kdf));memset(kdf,0xFF,sizeof(kdf));memset(kdf,0,sizeof(kdf));
			memset(ctx,0,sizeof(ctx));memset(ctx,0xFF,sizeof(ctx));memset(ctx,0,sizeof(ctx));
			memset(ptx,0,sizeof(ptx));memset(ptx,0xFF,sizeof(ptx));memset(ptx,0,sizeof(ctx));
			dep=0;
		}
		
	} else info();

	return 0;
}
