{$mode delphi}
UNIT uZip;

INTERFACE

USES Classes;

// Compress file(s) using the Zip deflate algorithm	
FUNCTION Zip(zfile: STRING; files: TStringList): BOOLEAN;
// Decompress zf.zip into path or, if empty, the current directory
FUNCTION unZip(zf, path: STRING): BOOLEAN;


IMPLEMENTATION

USES Zipper;


// Compress file(s) using the Zip deflate algorithm	
FUNCTION Zip(zfile: STRING; files: TStringList): BOOLEAN;
	VAR n: CARDINAL;
		z: TZipper;
	BEGIN
		TRY
			TRY
				z := TZipper.Create;
				z.FileName := zfile; 
				FOR n := 0 TO files.Count-1 DO
					z.Entries.AddFileEntry(files[n], files[n]);
				z.ZipAllFiles;
				Zip := TRUE;
			EXCEPT
				Zip := FALSE;
			END;
		FINALLY
			z.FREE;
		END;
	END;


// Decompress file.zip into <path> or, if empty, the current directory
FUNCTION unZip(zf,path: STRING): BOOLEAN;
	VAR u: TUnZipper;
	BEGIN
		u := TUnZipper.Create;
		TRY    
			TRY
				u.FileName := zf;
				IF path <> '' THEN
					u.OutputPath := path;
				u.Examine;
				u.UnZipAllFiles;
				unZip := TRUE;
			EXCEPT
				unZip := FALSE;
			END;
		FINALLY
			u.Free;
		END;
	END;
	
END.
