// General non-ANSI string & char functions
// Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com  

#ifndef MYSTRING_H_   /* Include guard */
#define MYSTRING_H_

// search and replace, string <old> for string <new>, in <str>
char *replace_str(const char *str, const char *old, const char *new);
// Convert a hexadecimal string to a string of ASCII characters
char* hex2ascii(char *hstr);
// Convert an ASCII string to a string of hexadecimal digits
char* ascii2hex(char *astr);
// is the char in given range?
int	inrange(char ch, char lo, char hi);
// convert a character to uppercase
char uch(char ch);
// convert a character to lowercase
char lch(char ch);
// convert a string to uppercase
char* ucase(char *str);
// convert a string to lowercase
char* lcase(char *str);
// get the date and time (seconds resolution) as string
char* datetimestr(void);
// get the leftmost <len> characters of a string
char* leftstr(char *str, int len);
// get the rightmost <len> characters of a string
char* rightstr(char *str, int len);

#endif
