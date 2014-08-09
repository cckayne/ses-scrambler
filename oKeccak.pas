// NB: Keccak doesn't work in Delphi mode
{$i std.inc}
{ $define test}
UNIT oKeccak;
// OOP Keccak interface to Wolfgang Ehrhardt's _preliminary_ port V0.17
INTERFACE

USES sesTypes, keccak_n;

CONST 	kBITS		= 512; // options are 224, 256, 384, and 512
		kNIBBLES	= kBITS DIV 4;
		kBYTES		= kNIBBLES DIV 2;

		
TYPE TKeccak	= OBJECT
	PUBLIC
		FUNCTION  Go(str: SESTRING): SESTRING;
		FUNCTION  GetBits: SESTRING; OVERLOAD;
		FUNCTION  GetBits: CARDINAL; OVERLOAD;
		FUNCTION  GetNibbles: SESTRING; OVERLOAD;
		FUNCTION  GetNibbles: CARDINAL; OVERLOAD;
		FUNCTION  GetBytes: SESTRING; OVERLOAD;
		FUNCTION  GetBytes: CARDINAL; OVERLOAD;
	PRIVATE
		state	: THashState;
		msgbits	: CARDINAL;
		kmsg	: STRING;
		buf		: ARRAY[0..kBITS-1] OF BYTE;
		i		: ARRAY[1..3] OF INTEGER;
		digest	: SESTRING;
		FUNCTION  Start: BOOLEAN;
		FUNCTION  isGo(str: SESTRING): BOOLEAN;
END;

IMPLEMENTATION

USES SysUtils, BTypes, Mem_Util; 


FUNCTION TKeccak.isGo(str: SESTRING): BOOLEAN;
	BEGIN
		kmsg := str;
		msgbits	:= 8*sizeof(kmsg);
		i[1] 	:= Init(state,kBITS);
		i[2] 	:= Update(state, @kmsg, msgbits);
		i[3] 	:= Final(state,@buf);
		digest	:= Uppercase(HexStr(@buf,kBYTES));
		isGo	:= i[1] + i[2] + i[3] = 0;
	END;

	
FUNCTION TKeccak.Go(str: SESTRING): SESTRING;
	BEGIN
		IF isGo(str) THEN Go := digest
			ELSE Go := 'KECCAK HASHING ERROR';
	{$ifdef test}
		Writeln(digest);
	{$endif}
	END;

	
FUNCTION TKeccak.Start: BOOLEAN;
	BEGIN
		kmsg	:= '';
		msgbits	:= 8*sizeof(kmsg);
		i[1] 	:= Init(state,kBITS);
		i[2] 	:= Update(state, @kmsg, msgbits);
		i[3] 	:= Final(state,@buf);
		digest	:= Uppercase(HexStr(@buf,kBYTES));
		Start	:= i[1] + i[2] + i[3] = 0;
	END;

	
FUNCTION  TKeccak.GetBits: SESTRING;
	BEGIN
		GetBits := IntToStr(kBITS);
	END;
	
FUNCTION  TKeccak.GetBits: CARDINAL;
	BEGIN
		GetBits := kBITS;
	END;
	
FUNCTION  TKeccak.GetNibbles: SESTRING;
	BEGIN
		GetNibbles := IntToStr(kNIBBLES);
	END;
	
FUNCTION  TKeccak.GetNibbles: CARDINAL;
	BEGIN
		GetNibbles := kNIBBLES;
	END;
	
FUNCTION  TKeccak.GetBytes: SESTRING;
	BEGIN
		GetBytes := IntToStr(kBYTES);
	END;
	
FUNCTION  TKeccak.GetBytes: CARDINAL;
	BEGIN
		GetBytes := kBYTES;
	END;
	

INITIALIZATION

FINALIZATION

	
END.
