{$mode delphi}
{$define distrib}
// Versioning unit for SES
UNIT sesVer;

INTERFACE

(* Full version format is 
     VERSION-            -BUILD          -STATUS 
Major.Minor.Patch-BYear-BMonth-BDay-BTime-Status

You zero digits to the right of any you increment, 
so if you fix a bug and introduce a new feature after version 5.3.6 
then the new version is 5.4.0.
*)

CONST	BANNER1			= 'A Super-Encypherment Scrambler';
CONST	VERSION 		= '6.0.0';			 	// Major.Minor.Patch		
CONST	STATUS 			= 'stable';				// may be alpha, beta, or stable
VAR		BUILD: STRING	= '2014-08-03-0521'; 	// YYYY-MM-DD-TTTT
VAR		PNAME			: STRING;				// the program's name
VAR		VBS				: STRING;				// holds Version-Build-Status

FUNCTION 	doBuild	: STRING;
PROCEDURE 	doBanner;

IMPLEMENTATION

USES SysUtils, MyStrUtils;

FUNCTION doBuild;
	VAR ft: TDateTime;
	VAR fa: Longint;
	BEGIN
		TRY
			fa := FileAge(ExtractFilename(ParamStr(0)));
			ft := FileDateToDateTime(fa);
			result := FormatDateTime('YYYY-MM-DD-hhnn',ft);
		EXCEPT
			result := BUILD;
		END;
	END;

PROCEDURE doBanner;
	BEGIN
		Writeln(PNAME,': ',BANNER1,'.');
		Writeln('Version & build: ',VBS);
		{$ifdef distrib}
			Writeln;
			Writeln('This program comes with absolutely no warranty:'); 
			Writeln('For details use -w. This is free software: '); 
			Writeln('You are welcome to redistribute it.'); 
			Writeln('Use -W for complete terms & conditions.');
			Writeln('Copyright (C) C.Kayne 2013, cckayne@gmail.com');
			Writeln;
		{$endif}
	END;
	

INITIALIZATION

	PNAME := HeadAnsi(ExtractFileName(ParamStr(0)),'.');
	BUILD := doBuild;
	VBS	  := VERSION+'-'+BUILD+'-'+STATUS;

FINALIZATION


END.
