MODULE COCQ; (* DT 21 10 1993 23:39 *)
  IMPORT Strings, COCS, COCT, COCO;

  CONST MaxCSeqLen = 4095; 

  VAR
    CSeq, Dup: ARRAY MaxCSeqLen + 1 OF CHAR; (* designator buffer *)
    cslen*: INTEGER;

 (* Error 215  - simplify expression *)
 
  PROCEDURE Append*(sfx: ARRAY OF CHAR);
    VAR i, sfxlen: INTEGER;
  BEGIN sfxlen := SHORT(Strings.Length(sfx));
    IF sfxlen + cslen > MaxCSeqLen THEN COCS.Mark(215); RETURN END;
    i := 0; 
    LOOP CSeq[i + cslen] := sfx[i]; 
      IF i = sfxlen THEN EXIT END;
      INC(i) 
    END;
    INC(cslen, sfxlen)
  END Append;
    
  PROCEDURE Prepend*(pfx: ARRAY OF CHAR; pos: INTEGER; VAR nextpos: INTEGER); 
    VAR i, pfxlen: INTEGER;
  BEGIN IF pos = cslen THEN Append(pfx); nextpos := cslen; RETURN END;
    pfxlen := SHORT(Strings.Length(pfx));
    IF pfxlen + cslen > MaxCSeqLen THEN COCS.Mark(215); i := pos
    ELSE
      i := cslen + 1; WHILE i # pos DO DEC(i); CSeq[i + pfxlen] := CSeq[i] END;
      WHILE i # pos + pfxlen DO CSeq[i] := pfx[i - pos]; INC(i) END;
      INC(cslen, pfxlen);
    END;
    nextpos := i
  END Prepend;

  PROCEDURE Rewind*;
  BEGIN CSeq[0] := 0X; cslen := 0 
  END Rewind;
  
  PROCEDURE Drop*(VAR x: COCT.Item);     
  BEGIN cslen := x.qoffs; CSeq[cslen] := 0X; x.qoffs := -1
  END Drop;
                    
  PROCEDURE Link*(VAR x: COCT.Item);
  BEGIN x.qoffs := cslen
  END Link;

  PROCEDURE Unlink*(VAR x: COCT.Item);
  BEGIN IF x.qoffs = 0 THEN COCO.PutSeq(CSeq); Rewind END; x.qoffs := -1
  END Unlink;
  
  PROCEDURE Mark*(VAR x: COCT.Item);
  BEGIN x.qoffs := cslen
  END Mark;

  PROCEDURE Release*(VAR x: COCT.Item);
    VAR i: INTEGER;
  BEGIN cslen := x.qoffs;
    IF cslen # 0 THEN
      i := 0;
      LOOP Dup[i] := CSeq[i + cslen];
        IF Dup[i] = 0X THEN EXIT END;
        INC(i)
      END;
      CSeq[cslen] := 0X;
      COCO.PutSeq(Dup)
    ELSE COCO.PutSeq(CSeq); CSeq[0] := 0X
    END;
    x.qoffs := -1
  END Release;

  PROCEDURE Dummy*();
  BEGIN IF cslen = 0 THEN Append(" ") END
  END Dummy;

END COCQ.
