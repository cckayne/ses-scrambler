// CISS  Copyright C.C.Kayne 2014, GNU GPL V.3, cckayne@gmail.com  
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include "mystring.h"
#include "isctypes.h"
#include "iscutils.h"
	
char* CapsEncode(char *txt) {
	char t[MAXM] = "";
	strcpy(t, txt);
	//Signal CAPS
	strcpy(t,replace_str(t,"A","KQA"));
	strcpy(t,replace_str(t,"B","KQB"));
	strcpy(t,replace_str(t,"C","KQC"));
	strcpy(t,replace_str(t,"D","KQD"));
	strcpy(t,replace_str(t,"E","KQE"));
	strcpy(t,replace_str(t,"F","KQF"));
	strcpy(t,replace_str(t,"G","KQG"));
	strcpy(t,replace_str(t,"H","KQH"));
	strcpy(t,replace_str(t,"I","KQI"));
	strcpy(t,replace_str(t,"J","KQJ"));
	strcpy(t,replace_str(t,"K","KQK"));
	strcpy(t,replace_str(t,"L","KQL"));
	strcpy(t,replace_str(t,"M","KQM"));
	strcpy(t,replace_str(t,"N","KQN"));
	strcpy(t,replace_str(t,"Ñ","KQNT"));
	strcpy(t,replace_str(t,"O","KQO"));
	strcpy(t,replace_str(t,"P","KQP"));
	strcpy(t,replace_str(t,"Q","KQQ"));
	strcpy(t,replace_str(t,"R","KQR"));
	strcpy(t,replace_str(t,"S","KQS"));
	strcpy(t,replace_str(t,"T","KQT"));
	strcpy(t,replace_str(t,"U","KQU"));
	strcpy(t,replace_str(t,"V","KQV"));
	strcpy(t,replace_str(t,"W","KQW"));
	strcpy(t,replace_str(t,"X","KQX"));
	strcpy(t,replace_str(t,"Y","KQY"));
	strcpy(t,replace_str(t,"Z","KQZ"));
	return t;
}


char* CapsDecode(char *txt) {
	char t[MAXM] = "";
	strcpy(t,txt);
	//Signal CAPS
	strcpy(t,replace_str(t,"kqa","A"));
	strcpy(t,replace_str(t,"kqb","B"));
	strcpy(t,replace_str(t,"kqc","C"));
	strcpy(t,replace_str(t,"kqd","D"));
	strcpy(t,replace_str(t,"kqe","E"));
	strcpy(t,replace_str(t,"kqf","F"));
	strcpy(t,replace_str(t,"kqg","G"));
	strcpy(t,replace_str(t,"kqh","H"));
	strcpy(t,replace_str(t,"kqi","I"));
	strcpy(t,replace_str(t,"kqj","J"));
	strcpy(t,replace_str(t,"kqk","K"));
	strcpy(t,replace_str(t,"kql","L"));
	strcpy(t,replace_str(t,"kqm","M"));
	strcpy(t,replace_str(t,"kqn","N"));
	strcpy(t,replace_str(t,"kqnt","Ñ"));
	strcpy(t,replace_str(t,"kqo","O"));
	strcpy(t,replace_str(t,"kqp","P"));
	strcpy(t,replace_str(t,"kqq","Q"));
	strcpy(t,replace_str(t,"kqr","R"));
	strcpy(t,replace_str(t,"kqs","S"));
	strcpy(t,replace_str(t,"kqt","T"));
	strcpy(t,replace_str(t,"kqu","U"));
	strcpy(t,replace_str(t,"kqv","V"));
	strcpy(t,replace_str(t,"kqw","W"));
	strcpy(t,replace_str(t,"kqx","X"));
	strcpy(t,replace_str(t,"kqy","Y"));
	strcpy(t,replace_str(t,"kqz","Z"));
	return t;
}

	
char* PreProcessText(char *txt) {
	ub4 c;
	char t[MAXM];
	strcpy(t,txt);
	
	//Signal CAPS
	strcpy(t,CapsEncode(t));
	//Kludge - sometimes replace_str doubles letters
	strcpy(t,replace_str(t,"KKQQ","KQ"));

	//P U N C T U A T I O N
	strcpy(t,replace_str(t,".","ZSTX"));
	strcpy(t,replace_str(t,",","ZCOX"));
	strcpy(t,replace_str(t,"'","ZQTX"));
	strcpy(t,replace_str(t,"\"","ZDQX"));
	strcpy(t,replace_str(t,"-","DXDX"));
	strcpy(t,replace_str(t,":","ZCLX"));
	strcpy(t,replace_str(t,";","ZSCX"));
	strcpy(t,replace_str(t,"/","ZSLX"));
	strcpy(t,replace_str(t,"?","ZQQX"));
	strcpy(t,replace_str(t,"!","ZLCX"));
	strcpy(t,replace_str(t,"(","ZBOX"));
	strcpy(t,replace_str(t,")","ZBCX"));
	strcpy(t,replace_str(t,"{","OBRACE"));
	strcpy(t,replace_str(t,"}","CBRACE"));
	strcpy(t,replace_str(t,"_","ZULX"));
	strcpy(t,replace_str(t,"*","ZASX"));
	strcpy(t,replace_str(t,"#","ZHSX"));
	strcpy(t,replace_str(t,"&","ZAMX"));
	strcpy(t,replace_str(t,"@","ZMPX"));
	strcpy(t,replace_str(t,"$","ZDLX"));
	strcpy(t,replace_str(t,"%","ZPCX"));
	strcpy(t,replace_str(t,"\\","ZBSX"));
	strcpy(t,replace_str(t,"=","ZEQX"));
	strcpy(t,replace_str(t,"+","ZPLX"));
	strcpy(t,replace_str(t,"[","OSQBRACE"));
	strcpy(t,replace_str(t,"]","CSQUBRACE"));
	strcpy(t,replace_str(t,"<","OABRACKET"));
	strcpy(t,replace_str(t,">","CABRACKET"));
	
	//N U M B E R S
	// Morse-encoded - B = dit, H = dah
	strcpy(t,replace_str(t,"0","JHHHHHJ"));
	strcpy(t,replace_str(t,"1","JBHHHHJ"));
	strcpy(t,replace_str(t,"2","JBBHHHJ"));
	strcpy(t,replace_str(t,"3","JBBBHHJ"));
	strcpy(t,replace_str(t,"4","JBBBBHJ"));
	strcpy(t,replace_str(t,"5","JBBBBBJ"));
	strcpy(t,replace_str(t,"6","JHBBBBJ"));
	strcpy(t,replace_str(t,"7","JHHBBBJ"));
	strcpy(t,replace_str(t,"8","JHHHBBJ"));
	strcpy(t,replace_str(t,"9","JHHHHBJ"));
	
	// ACCENTED CHARACTERS & DIACRITICS
	
	//Replace spaces
	strcpy(t,replace_str(t," ","ZX"));
		
	//Convert entire string to uppercase	
	strcpy(t,ucase(t));
	
	//Finally, replace any other extraneous chars with "X"
	for (c = 0; c<strlen(t); c++) 
		if (!((t[c]>='A') && (t[c]<='Z'))) t[c]='X';
	return t;
}



char* PostProcessText(char *txt) {
	char t[MAXM];
	strcpy(t,txt);
		
	//P U N C T U A T I O N
	strcpy(t,replace_str(t,"ZSTX","."));
	strcpy(t,replace_str(t,"ZCOX",","));
	strcpy(t,replace_str(t,"ZQTX","'"));
	strcpy(t,replace_str(t,"ZDQX","\""));
	strcpy(t,replace_str(t,"DXDX","-"));
	strcpy(t,replace_str(t,"ZCLX",":"));
	strcpy(t,replace_str(t,"ZSCX",";"));
	strcpy(t,replace_str(t,"ZSLX","/"));
	strcpy(t,replace_str(t,"ZQQX","?"));
	strcpy(t,replace_str(t,"ZLCX","!"));
	strcpy(t,replace_str(t,"ZBOX","("));
	strcpy(t,replace_str(t,"ZBCX",")"));
	strcpy(t,replace_str(t,"OBRACE","{"));
	strcpy(t,replace_str(t,"CBRACE","}"));
	strcpy(t,replace_str(t,"ZULX","_"));
	strcpy(t,replace_str(t,"ZASX","*"));
	strcpy(t,replace_str(t,"ZHSX","#"));
	strcpy(t,replace_str(t,"ZAMX","&"));
	strcpy(t,replace_str(t,"ZMPX","@"));
	strcpy(t,replace_str(t,"ZDLX","$"));
	strcpy(t,replace_str(t,"ZPCX","%"));
	strcpy(t,replace_str(t,"ZBSX","\\"));
	strcpy(t,replace_str(t,"ZEQX","="));
	strcpy(t,replace_str(t,"ZPLX","+"));
	strcpy(t,replace_str(t,"OSQBRACE","["));
	strcpy(t,replace_str(t,"CSQUBRACE","]"));
	strcpy(t,replace_str(t,"OABRACKET","<"));
	strcpy(t,replace_str(t,"CABRACKET",">"));
	
	//N U M B E R S
	// Morse-encoded - B = dit, H = dah
	strcpy(t,replace_str(t,"JHHHHHJ","0"));
	strcpy(t,replace_str(t,"JBHHHHJ","1"));
	strcpy(t,replace_str(t,"JBBHHHJ","2"));
	strcpy(t,replace_str(t,"JBBBHHJ","3"));
	strcpy(t,replace_str(t,"JBBBBHJ","4"));
	strcpy(t,replace_str(t,"JBBBBBJ","5"));
	strcpy(t,replace_str(t,"JHBBBBJ","6"));
	strcpy(t,replace_str(t,"JHHBBBJ","7"));
	strcpy(t,replace_str(t,"JHHHBBJ","8"));
	strcpy(t,replace_str(t,"JHHHHBJ","9"));
	
	//Replace spaces
	strcpy(t,replace_str(t,"ZX"," "));
		
	//Convert entire string to lowercase	
	strcpy(t,lcase(t));

	//Signal CAPS
	strcpy(t,CapsDecode(t));
	//some "kq"s occasionally get left in - 
	// there"s probably an obvious cause, but I haven"t found it yet
	strcpy(t,replace_str(t,"kq",""));
	
	return t;
}

