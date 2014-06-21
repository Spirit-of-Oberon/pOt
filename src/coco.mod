MODULE COCO; (* DVD 04 09 1993 00:12 *)
  IMPORT Files, Strings, COCS;  (* to mark *)
  (* C Output *)
  CONST
    UnxNL = 0AX;  MacNL = 0DX;
    IndWidth = 2;
		CLineLength = 80;

  VAR
    fold*, linenum*, macwrap*, unxwrap*: BOOLEAN;
    NL: ARRAY 2 OF CHAR;
    NLlen: INTEGER;
    ofile: Files.File;
    Output: Files.Rider;
    indlevel: INTEGER;
    newline: BOOLEAN;
    txtline: LONGINT;
		cpos: INTEGER;

  PROCEDURE Size*(): LONGINT;
  BEGIN RETURN Files.Pos(Output) + 1
  END Size;

  PROCEDURE Open*(VAR name: ARRAY OF CHAR);
  BEGIN  ofile := Files.New(name); IF ofile = NIL THEN HALT(21H) END; 
    Files.Set(Output,ofile,0);
    indlevel := 0; txtline := 1; newline := TRUE; cpos := 0;
    NLlen := 0;
    IF macwrap THEN NL[NLlen] := MacNL; INC(NLlen) END;
    IF unxwrap THEN NL[NLlen] := UnxNL; INC(NLlen) END;
    IF NLlen = 0 THEN NL[NLlen] := UnxNL; INC(NLlen) END
  END Open;

  PROCEDURE Close*;
  BEGIN Files.Set(Output, NIL, 0); Files.Close(ofile)
  END Close;

  PROCEDURE Purge*;
  BEGIN Files.Set(Output, NIL, 0); Files.Purge(ofile)
  END Purge; 

  PROCEDURE Indent*;
  BEGIN INC(indlevel, IndWidth)
  END Indent;

  PROCEDURE Undent*;
  BEGIN IF indlevel >= IndWidth THEN DEC(indlevel, IndWidth) END
  END Undent;

  PROCEDURE Separate*;
  BEGIN Files.Write(Output, " ");
  END Separate;

  PROCEDURE Wrap*;
    VAR i: INTEGER;
      linepgm: ARRAY 7 OF CHAR;
      lineno: ARRAY 11 OF CHAR;
  BEGIN
    newline := TRUE;
    Files.WriteBytes(Output, NL, NLlen);
    IF linenum & (txtline # COCS.txtpos.line) THEN
      linepgm := "#line "; 
      Strings.FromLInt(COCS.txtpos.line, 10, lineno);
      Files.WriteBytes(Output, linepgm, Strings.Length(linepgm));
      Files.WriteBytes(Output, lineno, Strings.Length(lineno));
      Files.Write(Output, " "); Files.Write(Output, 22X); 
      Files.WriteBytes(Output, COCS.txtpos.name, Strings.Length(COCS.txtpos.name)); 
      Files.Write(Output, 22X); Files.WriteBytes(Output, NL, NLlen);
      txtline := COCS.txtpos.line
    END;
    INC(txtline)
  END Wrap;

  PROCEDURE PutSeq*(s: ARRAY OF CHAR);
    VAR i, j: INTEGER;
  BEGIN 
    IF newline THEN
      newline := FALSE; i := 0; 
      WHILE i # indlevel DO Files.Write(Output, " "); INC(i) END;
			cpos := i
    END;  
    i := 0; j := 0; WHILE s[i] = " " DO INC(i) END;
		IF fold THEN
			LOOP
	     	IF i # j THEN s[j] := s[i] END;
				IF s[j] = 0X THEN EXIT END;
				INC(i); INC(j);
				IF (cpos >= CLineLength) & ((s[i] = "(") OR (s[i] = ")")) THEN
	    		Files.WriteBytes(Output, s, j);
					Files.Write(Output, "\"); Files.WriteBytes(Output, NL, NLlen);
					j := 0; cpos := 0
				ELSE INC(cpos)
				END
	    END
		ELSIF i # j THEN
			LOOP s[j] := s[i]; IF s[j] = 0X THEN EXIT END;
				INC(i); INC(j)
			END
		ELSE j := SHORT(Strings.Length(s))
		END;
    Files.WriteBytes(Output, s, j)
  END PutSeq;

  PROCEDURE PutPP*(s: ARRAY OF CHAR);
    VAR i, j: INTEGER;
  BEGIN 
    Wrap; 
    Files.Write(Output, "#");
    IF indlevel # 0 THEN i := IndWidth END;
    WHILE i # indlevel DO Files.Write(Output, " "); INC(i) END;
    Files.WriteBytes(Output, s, Strings.Length(s));
    Wrap
  END PutPP;

  PROCEDURE PutComment*(s: ARRAY OF CHAR);
    VAR i: INTEGER;
      str: ARRAY 4 OF CHAR;
  BEGIN
    i := 0;
    str := "/* "; Files.WriteBytes(Output, str, Strings.Length(str));
    WHILE s[i] # 0X DO
      Files.Write(Output, s[i]);
      IF (s[i] = "*") & (s[i+1] = "/") THEN Files.Write(Output, "@") END;
      INC(i)
    END;
    str := " */"; Files.WriteBytes(Output, str, Strings.Length(str))
  END PutComment;

BEGIN fold := FALSE; linenum := FALSE; macwrap := FALSE; unxwrap := TRUE
END COCO.
