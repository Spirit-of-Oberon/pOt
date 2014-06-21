MODULE COCX; (* DVD 04 09 1993 01:55 *)
 (* C eXpressions *)
  IMPORT Strings, COCT, COCQ, COCN, COCJ;

  CONST
   (*structure forms*)    
    Undef = 0; Byte = 1; Bool = 2; Char = 3; SInt = 4; Int = 5; LInt = 6;    
    Real = 7; LReal = 8; Set = 9; String = 10; NilTyp = 11; NoTyp = 12;    
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;    

    intSet = {SInt .. LInt}; realSet = {Real .. LReal};
    numSet = intSet + realSet;

 (* Expressions *)

  PROCEDURE SubExprPrefix*;
  BEGIN COCQ.Append("(")
  END SubExprPrefix;

  PROCEDURE SubExprSuffix*;
  BEGIN COCQ.Append(")")
  END SubExprSuffix;

  PROCEDURE Set00*(VAR x: COCT.Item; rngchk: BOOLEAN);
    VAR np: INTEGER;
  BEGIN COCQ.Prepend("((unsigned long)1<<", x.qoffs, np); 
    IF rngchk THEN COCQ.Prepend("pOt__rngchk_se(__FILE__,__LINE__,", np, np)
    ELSE COCQ.Prepend("(", np, np)
    END;
    COCQ.Append("))")
  END Set00;

  PROCEDURE Set10*(VAR x: COCT.Item; rngchk: BOOLEAN); 
   (*the name is spelled one-zero, not ten*)
    VAR np: INTEGER; 
  BEGIN COCQ.Prepend("((~(unsigned long)0<<", x.qoffs, np);
    IF rngchk THEN COCQ.Prepend("pOt__rngchk_se(__FILE__,__LINE__,", np, np)
    ELSE COCQ.Prepend("(", np, np)
    END;
    COCQ.Append("))&(~(~(unsigned long)1<<");
    IF rngchk THEN COCQ.Append("pOt__rngchk_se(__FILE__,__LINE__,")
    ELSE COCQ.Append("(")
    END;
  END Set10; 

  PROCEDURE Set11*(VAR x: COCT.Item; rngchk: BOOLEAN);
  BEGIN COCQ.Append("))))")
  END Set11;

  PROCEDURE InPfx*(VAR x: COCT.Item; rngchk: BOOLEAN);
    VAR np: INTEGER;
  BEGIN COCQ.Prepend("((unsigned long)0!=((unsigned long)1<<", x.qoffs, np); 
    IF rngchk THEN COCQ.Prepend("pOt__rngchk_se(__FILE__,__LINE__,", np, np)
    ELSE COCQ.Prepend("(", np, np)
    END;                   
    COCQ.Append(")&") (* set operations in Oberon and C have different priority, so they are always surrounded by parentheses *)
  END InPfx;

  PROCEDURE InSfx*;
  BEGIN COCQ.Append("))")
  END InSfx;

  PROCEDURE TypTest*(VAR x,y: COCT.Item);
    VAR np: INTEGER; s: ARRAY 9 OF CHAR;
  BEGIN COCQ.Prepend("pOt__typtest((pOt__RecTypDsc**)", x.qoffs, np);
    IF x.typ.form = Record THEN COCQ.Prepend("&", np, np) END;
    COCQ.Append(",(pOt__RecTypDsc*)&");
    IF y.typ.form = Record THEN COCN.CTDName(y.typ, COCQ.cslen, np)
    ELSE (*Pointer*) COCN.CTDName(y.typ.BaseTyp, COCQ.cslen, np)
    END;
    COCQ.Append(",0x");
    IF y.typ.form = Record THEN Strings.FromLInt(y.typ.n, 16, s)
    ELSE (*Pointer*)  Strings.FromLInt(y.typ.BaseTyp.n, 16, s)
    END;
    COCQ.Append(s); COCQ.Append("L)")
  END TypTest;

  PROCEDURE MOp*(op: INTEGER; VAR x: COCT.Item; rngchk: BOOLEAN);
    VAR np: INTEGER; f: INTEGER;
  BEGIN f := x.typ.form;
    CASE op OF
      6  (*+*): 
    | 7  (*-*): 
      IF f IN numSet THEN COCQ.Prepend("-", x.qoffs, np)
      ELSIF f = Set THEN COCQ.Prepend("~", x.qoffs, np) 
      END
    | 32 (*~*): COCQ.Prepend("!", x.qoffs, np)
    END
  END MOp;

  PROCEDURE NumOp*(op:INTEGER; convert:SET; VAR x,y:COCT.Item; rngchk:BOOLEAN);
    VAR np: INTEGER; s: ARRAY 4 OF CHAR;
    
    PROCEDURE CastX;
    BEGIN 
      COCQ.Prepend("))", y.qoffs, np); 
      COCQ.Prepend("((", x.qoffs, np); 
      COCN.CTDenoter(x.typ, np, np); 
      COCQ.Prepend(")(", np, np)
    END CastX;
  
  BEGIN
    IF rngchk & (x.typ.form IN intSet) & (op # 2) THEN
      Strings.FromLInt(x.typ.form,10,s);
      COCQ.Append("),"); COCQ.Append(s); COCQ.Append(")");
      COCQ.Prepend("),(pOt_LONGINT)(", y.qoffs, np);
      COCQ.Prepend("chk(__FILE__,__LINE__,(pOt_LONGINT)(", x.qoffs, np);
      CASE op OF 1 (***): COCQ.Prepend("mul", x.qoffs, np)
      | 3 (*DIV*): COCQ.Prepend("div", x.qoffs, np)
      | 4 (*MOD*): COCQ.Prepend("mod", x.qoffs, np)
      | 6 (*+*): COCQ.Prepend("add", x.qoffs, np)
      | 7 (*-*): COCQ.Prepend("sub", x.qoffs, np)
      END;
      COCQ.Prepend("(", x.qoffs, np);
      COCN.CTDenoter(x.typ, np, np); 
      COCQ.Prepend(")pOt__", np, np)
    ELSE
      IF 1 IN convert THEN COCJ.Cast(y) END;
      CASE op OF 1 (***): 
	COCQ.Prepend("*", y.qoffs, np);
	IF 0 IN convert THEN CastX END
      | 2 (*/*):
	COCQ.Prepend("/", y.qoffs, np);
	IF 0 IN convert THEN CastX END
      | 3 (*DIV*):
	COCQ.Prepend(",", y.qoffs, np); COCQ.Append(")");
	IF 0 IN convert THEN CastX END;
	COCQ.Prepend("(", x.qoffs, np);
	COCN.CTDenoter(x.typ, np, np); 
	COCQ.Prepend(")pOt__div(", np, np)
      | 4 (*MOD*):
	COCQ.Prepend(",", y.qoffs, np); COCQ.Append(")");
	IF 0 IN convert THEN CastX END;
	COCQ.Prepend("(", x.qoffs, np);
	COCN.CTDenoter(x.typ, np, np); 
	COCQ.Prepend(")pOt__mod(", np, np)
      | 6 (*+*):
	COCQ.Prepend("+", y.qoffs, np);
	IF 0 IN convert THEN CastX END
      | 7 (*-*):
	COCQ.Prepend("-", y.qoffs, np);
	IF 0 IN convert THEN CastX END
      END
    END
  END NumOp;

  PROCEDURE SetOp*(op: INTEGER; VAR x, y: COCT.Item);
    VAR np: INTEGER;
  BEGIN
    CASE op OF 
      1 (***): COCQ.Prepend("&", y.qoffs, np) 
    | 2 (*/*): COCQ.Prepend("^", y.qoffs, np) 
    | 6 (*+*): COCQ.Prepend("|", y.qoffs, np) 
    | 7 (*-*): COCQ.Prepend("&~", y.qoffs, np) 
    END;
    COCQ.Prepend("(", x.qoffs, np); COCQ.Append(")")
  END SetOp;

  PROCEDURE BoolOp*(op: INTEGER; VAR x, y: COCT.Item);
    VAR np: INTEGER;
  BEGIN
    CASE op OF
      5 (*&*): COCQ.Prepend("&&", y.qoffs, np)
    | 8 (*OR*): COCQ.Prepend("||", y.qoffs, np)
    END;
    COCQ.Prepend("(", x.qoffs, np); COCQ.Append(")")
  END BoolOp;

  PROCEDURE NumRel*(op: INTEGER; convert: SET; VAR x, y: COCT.Item);
    VAR np: INTEGER;

    PROCEDURE CastX;
    BEGIN 
      COCQ.Prepend("))", y.qoffs, np); 
      COCQ.Prepend("((", x.qoffs, np); 
      COCN.CTDenoter(x.typ, np, np); 
      COCQ.Prepend(")(", np, np)
    END CastX;
  
  BEGIN
    IF 1 IN convert THEN COCJ.Cast(y) END;
    CASE op OF
      9 (*=*): COCQ.Prepend("==", y.qoffs, np)
    | 10 (*#*): COCQ.Prepend("!=", y.qoffs, np)
    | 11 (*<*): COCQ.Prepend("<", y.qoffs, np)
    | 12 (*<=*): COCQ.Prepend("<=", y.qoffs, np)
    | 13 (*>*): COCQ.Prepend(">", y.qoffs, np)
    | 14 (*>=*): COCQ.Prepend(">=", y.qoffs, np)
    END;
    IF 0 IN convert THEN CastX END;
    COCQ.Prepend("(", x.qoffs, np); COCQ.Append(")")
  END NumRel;
  
  PROCEDURE StrRel*(op: INTEGER; VAR x, y: COCT.Item);
    VAR np: INTEGER; s: ARRAY 9 OF CHAR;
  BEGIN
    s[0] := "s"; s[1] := "s"; s[2] := 0X;
    COCQ.Prepend(",", y.qoffs, np);
    IF y.typ.form = Char THEN s[1] := "c"
    ELSE
      IF y.typ.form # String THEN COCQ.Prepend("(pOt_CHAR*)&", np, np) END;
      IF x.typ.form = Char THEN s[0] := "c"
      ELSIF x.typ.form # String THEN COCQ.Prepend("(pOt_CHAR*)&", x.qoffs, np) 
      END
    END;
    COCQ.Prepend("pOt__cmp", x.qoffs, np);
    COCQ.Prepend(s, np, np); COCQ.Prepend("(", np, np);
    COCQ.Append(",0x"); Strings.FromLInt(LONG(op), 16, s);
    COCQ.Append(s); COCQ.Append(")")
  END StrRel;

  PROCEDURE CharRel*(op: INTEGER; VAR x, y: COCT.Item);
    VAR np: INTEGER;
  BEGIN
    COCQ.Prepend("(unsigned char)", y.qoffs, np);
    CASE op OF
      9 (*=*): COCQ.Prepend("==", y.qoffs, np)
    | 10 (*#*): COCQ.Prepend("!=", y.qoffs, np)
    | 11 (*<*): COCQ.Prepend("<", y.qoffs, np)
    | 12 (*<=*): COCQ.Prepend("<=", y.qoffs, np)
    | 13 (*>*): COCQ.Prepend(">", y.qoffs, np)
    | 14 (*>=*): COCQ.Prepend(">=", y.qoffs, np)
    END;
    COCQ.Prepend("((unsigned char)", x.qoffs, np); COCQ.Append(")")
  END CharRel;

  PROCEDURE StProcPfx*(fctno: INTEGER; rngchk: BOOLEAN);
  BEGIN
    CASE fctno OF 0 (*HALT*): COCQ.Append("pOt__halt(__FILE__,__LINE__,")
    | 1  (*NEW*): COCQ.Append("pOt__new(__FILE__,__LINE__,(pOt__TypDsc***)&")
    | 2  (*CC*): COCQ.Append("(") (* not imp. *)
    | 3  (*ABS*):
    | 4  (*CAP*): COCQ.Append("pOt__cap((unsigned char)")
    | 5  (*ORD*): COCQ.Append("(pOt_INTEGER)((unsigned char)")
    | 6  (*ENTIER*): 
      IF rngchk THEN COCQ.Append("pOt__rngchk_li(__FILE__,__LINE__,") 
      ELSE COCQ.Append("pOt__entier(") 
      END
    | 7  (*SIZE*): COCQ.Append("(pOt_LONGINT)(sizeof(")
    | 8  (*ODD*): COCQ.Append("(pOt_BOOLEAN)(0x1&(")
    | 9  (*ADR*): COCQ.Append("((pOt_LONGINT)&")
    | 10,11 (*MIN,MAX*): 
    | 12 (*CHR*): COCQ.Append("(pOt_CHAR)");
      IF rngchk THEN COCQ.Append("pOt__rngchk_cn(__FILE__,__LINE__,") END;
      COCQ.Append("(unsigned char)(")         
    | 13,14 (*SHORT LONG*): 
    | 15 (*OVFL*): COCQ.Append("(")
    | 16, 17, 18, 19 (*INC DEC INCL EXCL*):
    | 20 (*LEN*): COCQ.Append("(*((pOt__ArrTypDsc**)&")
    | 21 (*ASH*): COCQ.Append("pOt__ash(")
    | 22 (*LSH*): COCQ.Append("pOt__lsh(")
    | 23 (*ROT*): COCQ.Append("pOt__rot(")
    | 24 (*GET*): COCQ.Append("pOt__get((pOt_BYTE_SYSTEM*)")
    | 25 (*PUT*): COCQ.Append("pOt__put((pOt_BYTE_SYSTEM*)")
    | 26 (*BIT*): COCQ.Append("(*(pOt_BYTE_SYSTEM *)(")
    | 27 (*VAL*): 
    | 28 (*SYSTEM.NEW*):
    | 29 (*COPY*):
      IF rngchk THEN COCQ.Append("pOt__copychk(__FILE__,__LINE__,(pOt_CHAR*)&")
      ELSE COCQ.Append("pOt__copy((pOt_CHAR*)&")
      END
    | 30 (*MOVE*): COCQ.Append("pOt__move((pOt_BYTE_SYSTEM*)(")
    END
  END StProcPfx;

  PROCEDURE StPar1Sfx*(VAR x:COCT.Item; fctno: INTEGER; rngchk: BOOLEAN);
    VAR np: INTEGER;
  BEGIN
    CASE fctno OF 0 (*HALT*):
    | 1  (*NEW*): COCQ.Append(",(pOt__TypDsc*)&"); COCN.CTDName(x.typ,COCQ.cslen,np)
    | 2  (*CC*): 
    | 3  (*ABS*): 
      COCQ.Prepend("(", x.qoffs, np);
      COCN.CTDenoter(x.typ, np, np);
      IF x.typ.form IN intSet THEN COCQ.Prepend(")pOt__abs(", np, np)
      ELSIF x.typ.form IN realSet THEN COCQ.Prepend(")pOt__fabs(", np, np)
      END
    | 4 .. 6 (*CAP ORD ENTIER*): 
    | 7 (*SIZE*): 
      IF x.typ.form IN {Array, DynArr, Record} THEN
	COCQ.Append(")-sizeof(pOt__TypDsc*)")
      ELSE COCQ.Append(")")
      END
    | 8  (*ODD*): COCQ.Append(")")
    | 9  (*ADR*): 
      IF x.typ.form IN {Array, DynArr, Record} THEN 
	COCQ.Append("+sizeof(pOt__TypDsc*)") 
      END
    | 10,11 (*MIN,MAX*): 
    | 12 (*CHR*): IF rngchk THEN COCQ.Append(")") END
    | 13 (*SHORT*): 
      COCQ.Prepend("(",x.qoffs,np); 
      COCN.CTDenoter(x.typ,np,np); 
      COCQ.Prepend(")",np,np);
      IF rngchk THEN COCQ.Prepend("pOt__rngchk_",np,np);
	CASE x.typ.form OF Int: COCQ.Prepend("si",np,np)
	| LInt: COCQ.Prepend("i",np,np)
	| LReal: COCQ.Prepend("r",np,np)
	END;
	COCQ.Prepend("(__FILE__,__LINE__,",np,np)
      ELSE COCQ.Prepend("(",np,np)
      END
    | 14 (*LONG*): 
      COCQ.Prepend("(",x.qoffs,np); 
      COCN.CTDenoter(x.typ,np,np); 
      COCQ.Prepend(")(",np,np);
    | 15 (*OVFL*):
    | 16 (*INC*): COCQ.Append("+=(");
    | 17 (*DEC*): COCQ.Append("-=(");
    | 18 (*INCL*): COCQ.Append("|=((unsigned long)1<<(");
    | 19 (*EXCL*): COCQ.Append("&=~((unsigned long)1<<(")
    | 20 (*LEN*): COCQ.Append("+")
    | 21, 22 (*ASH,LSH*): COCQ.Append(",")
    | 23 (*ROT*): COCQ.Append(","); COCN.CTSize(x.typ,COCQ.cslen,np); COCQ.Append("*8,");
    | 24,25 (*GET PUT*): COCQ.Append(",(pOt_BYTE_SYSTEM*)&")
    | 26 (*BIT*): COCQ.Append(")&");
      IF rngchk THEN COCQ.Append("pOt__rngchk_cn(__FILE__,__LINE__,1<<(")
      ELSE COCQ.Append("(1<<(")
      END
    | 27 (*VAL*): COCQ.Prepend("((",x.qoffs,np); COCQ.Append(")(")
    | 28 (*SYSTEM.NEW*): COCQ.Append("=("); COCN.CTDenoter(x.typ,COCQ.cslen,np);
      COCQ.Append(")pOt__alloc(__FILE__,__LINE__,");
    | 29 (*COPY*): COCQ.Append(",(pOt_CHAR*)&")
    | 30 (*MOVE*): COCQ.Append("),(pOt_BYTE_SYSTEM*)(")
    END
  END StPar1Sfx;

  PROCEDURE StPar2Sfx*(VAR x:COCT.Item; fctno: INTEGER; rngchk: BOOLEAN);
    VAR np: INTEGER;
  BEGIN
    CASE fctno OF 16, 17 (*INC DEC*):
    | 18, 19 (*INCL EXCL*): COCQ.Append(")")
    | 20 .. 23 (*LEN ASH LSH ROT*):
    | 24, 25 (*GET PUT*): COCQ.Append(","); COCN.CTSize(x.typ,COCQ.cslen,np)
    | 26 (*BIT*): COCQ.Append("))")
    | 27 (*VAL*): COCQ.Append(")")
    | 28, 29 (*SYSTEM.NEW COPY*):
    | 30 (*MOVE*): COCQ.Append("),")
    END
  END StPar2Sfx;

  PROCEDURE StPar3Sfx*(VAR x: COCT.Item; fctno: INTEGER; rngchk: BOOLEAN);
    VAR np: INTEGER;
  BEGIN 
    CASE fctno OF 30:
    END
  END StPar3Sfx;

  PROCEDURE StFakeSfx*(fctno: INTEGER; rngchk: BOOLEAN);
  BEGIN 
    CASE fctno OF 16,17 (*INC DEC*): COCQ.Append("1")
    | 20 (*LEN*): COCQ.Append("0")
    END
  END StFakeSfx;

  PROCEDURE StFctSfx*(fctno:INTEGER; rngchk:BOOLEAN);
  BEGIN 
    IF (fctno = 10) OR (fctno = 11) THEN (*nothing*)
    ELSIF fctno = 20 THEN COCQ.Append("))->nofel")
    ELSE COCQ.Append(")")
    END
  END StFctSfx;

END COCX.

