MODULE Files; (*file manager*) 
IMPORT SYSTEM;

CONST
  MaxPathLength* = 1023;

TYPE
	File* = POINTER TO FileDesc; 
  FileDesc = RECORD
    name: ARRAY MaxPathLength +1 OF CHAR;
    handle: LONGINT;
    pos: LONGINT
  END;  

	Rider* = RECORD 
    res*: INTEGER; 
    eof*: BOOLEAN;
    pos: LONGINT;
    file: File;
  END;

VAR
  StdIn*, StdOut*, StdErr*: File;

PROCEDURE Old*(name: ARRAY OF CHAR): File; 
  VAR f: File;
BEGIN NEW(f); f.handle := 0; f.pos := 0; COPY(name, f.name);
  RETURN f
END Old;
(*the file with the given name. NIL if the name is not in the directory*) 

PROCEDURE New*(name: ARRAY OF CHAR): File; 
  VAR f: File;
BEGIN NEW(f); f.handle := 0; f.pos := 0; COPY(name, f.name);
  RETURN f
END New;
(*a new file with given name*) 

(*Close file f and register it under its name in the directory.
If the name exists already, the corresponding old file is unregistered*)
PROCEDURE Register*(f: File); 
BEGIN f := NIL
END Register;

PROCEDURE Close*(f: File);
BEGIN f := NIL
END Close;

PROCEDURE Purge*(f: File); 
BEGIN f := NIL
END Purge;

PROCEDURE Length*(f: File): LONGINT;
END Length;
(*the number of bytes in the file*) 

PROCEDURE Set*(VAR r: Rider; f: File; pos: LONGINT); 
END Set;
(*Associate rider r with file f at position pos. r.eof := FALSE*) 

PROCEDURE Read*(VAR r: Rider; VAR x: SYSTEM.BYTE); 
END Read;
(*read byte and advance rider by one position. If at end, r.eof := TRUE and x := 
0X*) 

PROCEDURE ReadBytes*(VAR r: Rider; VAR x: ARRAY OF SYSTEM.BYTE; n: LONGINT); 
END ReadBytes;
(*read n bytes and advance rider by n positions.
If at end, r.eof := TRUE and r.res := no. of bytes requested but not read.*) 

PROCEDURE Write*(VAR r: Rider; x: SYSTEM.BYTE); 
END Write;
(*write byte and advance rider by one position*) 

(*write n bytes and advance rider by n positions*) 
PROCEDURE WriteBytes*(VAR r: Rider; VAR x: ARRAY OF SYSTEM.BYTE; n: LONGINT);
END WriteBytes;

PROCEDURE Pos*(VAR r: Rider): LONGINT; 
BEGIN RETURN r.pos
END Pos;

PROCEDURE Base*(VAR r: Rider): File; 
BEGIN RETURN r.file
END Base;

(*res = 0: renamed;
  res = 1: new name existed already and now denotes the renamed file; 
  res = 2: old name not in directory;
  res = 3: name is illegal;
  res = 4: name is too long *) 
PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER); 
END Rename;

(*res = 0: deleted;
  res = 2: name not in directory; 
  res = 3: name is illegal;
  res = 4: name is too long *) 
PROCEDURE Delete*(name: ARRAY OF CHAR; VAR res: INTEGER); 
END Delete;

BEGIN 
  NEW(StdIn); StdIn.pos := 0; StdIn.name := ""; StdIn.handle := 0;
  NEW(StdOut); StdOut.pos := 0; StdOut.name := ""; StdOut.handle := 0;
  NEW(StdErr); StdErr.pos := 0; StdErr.name := ""; StdErr.handle := 0;
END Files.
