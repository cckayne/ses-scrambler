// General non-ANSI string & char functions
// Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com
// replace_str copyright Laird Shaw. Public Domain.  
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <time.h>
#include "mystring.h"
#include "isctypes.h"

/*
Description:	Replaces in the string str all the occurrences of the source string old with 
the destination string new. The lengths of the strings old and new may differ. The string new 
may be of any length, but the string "old" must be of non-zero length - the penalty for 
providing an empty string for the "old" parameter is an infinite loop. 
In addition, none of the three parameters may be NULL.

Returns:	The post-replacement string, or NULL if memory for the new string could not be 
allocated. Does not modify the original string. The memory for the returned post-replacement 
string may be deallocated with the standard library function free() when it is no longer 
required.

Dependencies:	For this function to compile, you will need to also #include the following 
files: <string.h>, <stdlib.h> and <stddef.h>.

Licence:	Public domain. You may use this code in any way you see fit, optionally crediting 
its author (me, Laird Shaw, with assistance from comp.lang.c).
*/
char *replace_str(const char *str, const char *old, const char *new)
{
	// cckayne made ret fixed-length (was: char *ret)
	char ret[MAXM], *r;
	const char *p, *q;
	size_t oldlen = strlen(old);
	size_t count, retlen, newlen = strlen(new);

	if (oldlen != newlen) {
		for (count = 0, p = str; (q = strstr(p, old)) != NULL; p = q + oldlen)
			count++;
		/* this is undefined if p - str > PTRDIFF_MAX */
		retlen = p - str + strlen(p) + count * (newlen - oldlen);
	} else
		retlen = strlen(str);
	
	// cckayne removed the malloc and made ret[] fixed-length
	/* if ((ret = malloc(retlen + 1)) == NULL)
		return NULL; */

	for (r = ret, p = str; (q = strstr(p, old)) != NULL; p = q + oldlen) {
		/* this is undefined if q - p > PTRDIFF_MAX */
		ptrdiff_t l = q - p;
		memcpy(r, p, l);
		r += l;
		memcpy(r, new, newlen);
		r += newlen;
	}
	strcpy(r, p);

	return ret;
}

	
// hex nibble to integer	
int htoi(char c) {
    int first = c / 16 - 3;
    int second = c % 16;
    int result = first*10 + second;
    if(result > 9) result--;
    return result;
}

// hex nibble to ASCII char
int htoa(char c, char d) {
    int high = htoi(c) * 16;
    int low = htoi(d);
    return high+low;
}
	
// Convert a hexadecimal string to a string of ASCII characters
char* hex2ascii(char *hstr)
	{
		register unsigned long int i, l;
		l = strlen(hstr);
		char c = 0;
		char a[MAXM], t[4];
		memset(a,'\0',l+1);
        for (i = 0; i < l; i++) {
            if (i % 2 != 0) {
                sprintf(t, "%c", htoa(c, hstr[i]));
				strcat(a,t);
            } else c = hstr[i];
        }
		return a;
	}
	
// Convert an ASCII string to a string of hexadecimal digits
char* ascii2hex(char *astr)
	{
		register unsigned long int i, l;
		l = strlen(astr);
		char c = 0;
		char h[MAXM], t[4];
		memset(h,'\0',l+1);
        for (i = 0; i < l; i++) {
            sprintf(t, "%02X", astr[i]);
			strcat(h,t);
        }
		return h;
	}
	
// is the char in given range?
int	inrange(char ch, char lo, char hi) {
	if ((ch>=lo) && (ch<=hi)) return 1; else return 0;	
}	

// convert a character to uppercase
char uch(char ch) {
	register char uch;
	uch = ch;
	if (inrange(ch,'a','z'))
		uch = ch & 0x5F;
	return uch;
}
 
// convert a character to lowercase
char lch(char ch) {
	register char lch;
	lch = ch;
	if (inrange(ch,'A','Z'))
		lch = ch | 0x20;
	return lch;
}

	
// convert a string to uppercase
char* ucase(char *str) {
	register unsigned long int i,l;
	char uc[MAXM];
	l=strlen(str);
	memset(uc,'\0',l+1);
	for (i=0; i<l; i++)
		uc[i]=uch(str[i]);
	return uc;
}
 
 
// convert a string to lowercase
char* lcase(char *str) {
	register unsigned long int i,l;
	char lc[MAXM];
	l=strlen(str);
	memset(lc,'\0',l+1);
	for (i=0; i<l; i++) 
		lc[i]=lch(str[i]);
	return lc;
}


// get the date and time (seconds resolution) as a string
char* datetimestr(void) {
	struct tm *tm;
	time_t t;
	char str_time[100];
	char str_date[100];

	t = time(NULL);
	tm = localtime(&t);

	strftime(str_time, sizeof(str_time), "%H:%M:%S ", tm);
	strftime(str_date, sizeof(str_date), "%Y-%m-%d ", tm);

	return strcat(str_date,str_time);
}


// get the leftmost <len> characters of a string
char* leftstr(char *str, int len) {
	register ub4 i,l;
	char lstr[MAXM]="";
	l=strlen(str);
	if (len>=l) return str;
	for (i=0; i<len; i++)
		lstr[i] = str[i];
	lstr[i+1]='\0';
	return lstr;
}

// get the rightmost <len> characters of a string
char* rightstr(char *str, int len) {
	register long int i,j,l;
	char rstr[MAXM]="";
	l=strlen(str);
	if (len>=l) return str;
	j=-1;
	for (i=l-len; i<l; i++) {
		j++;
		rstr[j] = str[i];
	}
	rstr[j+1]='\0';
	return rstr;
}

