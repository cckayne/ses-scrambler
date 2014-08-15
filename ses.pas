{$mode delphi}
{$define distrib}
{ $define test}
{$define interactive}
{ 

SES: A Super-Encipherment Scrambler.
This program comes with absolutely no warranty:
For details use -w. This is free software:
You are welcome to redistribute it.
Use -W for complete terms & conditions.
Copyright (C) C.Kayne 2013, cckayne@gmail.com

}

PROGRAM ses;

USES MyStrUtils, Print, sesTypes, sesCiphers, sesParams, sesMessages, sesHash, uFiles;
{$ifdef distrib}
	// the GPL license must always be present
	{$I 'gpl.pas'}
{$endif}


BEGIN
	// P R E L I M I N A R I E S
	
	// show version, options, info, usage, etc.
	ShowMessages;
	
	// 0) Mode
	CipherMode := GetMode;
	
	// Option to securely delete a file and exit
	IF CipherMode=mNone THEN
		IF doFile AND (GetFile<>'') AND doDelete THEN
			IF FSDel(GetFile, GetPath, GetPasses) THEN BEGIN
				Writeln(GetFile,' securely deleted (',IntToStr(GetPasses),' passes).');
				HALT;
			END;
	

	// I N T E R A C T I V E   &   S W I T C H   M O D E
	// 1) Keyphrase
	IF NOT doOTP AND (GetOTP='') THEN
		IF GetKeyphrase<>'' THEN keyphrase:= GetKeyphrase ELSE {$ifdef interactive}
			REPEAT Write('Enter keyphrase: '); Readln(keyphrase); UNTIL keyphrase<>''{$else} Exit {$endif};

	CASE sesParams.CipherMode OF
		mEncipher: BEGIN
			// 2a) Plaintext
			IF ((NOT doFile) AND (NOT doFileT)) AND ((GetFile='') AND (GetFileT='')) THEN
				IF GetPlaintext<>'' THEN msg := GetPlaintext ELSE {$ifdef interactive}
					REPEAT Write('Enter plaintext: '); Readln(msg); UNTIL msg<>''{$else} Exit {$endif};
		END; 
		mDecipher: BEGIN
			// 2a) Ciphertext
			IF ((NOT doFile) AND (NOT doFileT)) AND ((GetFile='') AND (GetFileT='')) THEN
				IF GetCiphertext<>'' THEN ciphertext := GetCiphertext ELSE {$ifdef interactive}
					REPEAT Write('Enter ciphertext: '); Readln(ciphertext); UNTIL ciphertext<>''{$else} Exit {$endif};
		END; 
		mNone: BEGIN
			// 2a) Plaintext
			// assume Encryption mode
			CipherMode := mEncipher;
			IF ((NOT doFile) AND (NOT doFileT)) AND ((GetFile='') AND (GetFileT='')) THEN
				IF GetPlaintext<>'' THEN msg := GetPlaintext ELSE {$ifdef interactive}
					REPEAT Write('Enter plaintext: '); Readln(msg); UNTIL msg<>''{$else} Exit {$endif};
		END; 	
	END; 
	
	// 3 )   T H E   S E S   M A C H I N E   G O E S   T O   W O R K . . .
	CASE sesParams.CipherMode OF
		mEncipher: 
			TRY
				// B E G I N   E N C I P H E R
				// a) isc-encipher a file of bytes
				IF doFile AND (GetFile<>'') THEN
					Writeln(iscCipherF(GetFile, GetPath, Keyphrase, mEncipher, doDelete))
				ELSE
				// b) ses-enciphter a text file
				IF doFileT AND (GetFileT<>'') THEN
					FileEncipher(keyphrase, GetFileT, GetPath, doDelete)
				ELSE
				// c) simple true-OTP encipher a message
				IF doOTP THEN BEGIN
					RealOTPPool.Fill(GetOTP);
					checktext := enRealOTP(msg);
					IF doGroups THEN
						PrintLn(Groups(checktext,GetGrouping))
					ELSE Writeln(checktext);
					// optionally, print the authentication code
					IF doAuth THEN BEGIN
						aHash := aKeccak.Go(checktext);
						Writeln(aHash);
					END;
				END ELSE BEGIN
				// d) ses-encipher a message
					prepared := false;
					checktext := sesEncipher(keyphrase,msg);
					// optionally, generate the authentication code
					IF doAuth THEN
						aHash := aKeccak.Go(checktext);
					IF doVerbose THEN ShowLog ELSE
						IF doGroups THEN
							PrintLn(Groups(checktext,GetGrouping))
						ELSE Writeln(checktext);
					// optionally, print the authentication code
					IF doAuth THEN Writeln(aHash);
				END;
				// E N D   E N C I P H E R
			EXCEPT
				doError(CipherMode, msg);
			END;
		mDecipher: 
			TRY
				// optional ciphertext authentication
				IF doAuth AND (NOT doFile) THEN BEGIN
					IF GetAuth='' THEN BEGIN
						REPEAT Write('Enter auth-code: '); Readln(authcode); UNTIL authcode<>'';
					END ELSE authcode := GetAuth;
					// if authentication fails, decryption cannot proceed
					IF NOT isAuthentic(ciphertext,authcode) THEN BEGIN
						Writeln('< ciphertext is not authenticated >');
						EXIT;
					END;
				END;
				// B E G I N   D E C I P H E R
				// a) isc-decipher a file of bytes
				IF doFile AND (GetFile<>'') THEN
					Writeln(iscCipherF(GetFile, GetPath, Keyphrase, mDecipher, doDelete))
				ELSE
				// b) ses-decipher a text file
				IF doFileT AND (GetFileT<>'') THEN
					FileDecipher(keyphrase, GetFileT, GetPath)
				ELSE
				// c) simple true-OTP decipher a ciphertext
				IF doOTP THEN BEGIN
					RealOTPPool.Fill(GetOTP);
					Writeln(deRealOTP(ciphertext))
				// d) ses-decipher a ciphertext
				END ELSE BEGIN
					prepared := false;
					checktext := sesDecipher(keyphrase,ciphertext);
					IF doVerbose THEN ShowLog ELSE
						Writeln(checktext);
				END;
				// E N D   D E C I P H E R
			EXCEPT
				{$ifdef interactive}doError(CipherMode, ciphertext);{$endif}
			END;
	END;

	msg := LICENSE[1]; // avoid a "not used" compiler warning
	
END.

