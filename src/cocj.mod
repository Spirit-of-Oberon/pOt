MODULE COCJ; (* DT 22 10 1993 00:06 *)
  IMPORT SYSTEM, Strings, Reals, COCT, COCQ, COCN;

  CONST
   (*object and item modes*)
    Var   =  1; Ind   =  3; Con   =  8; Reg = 11;   Fld   = 12; 
    Typ   = 13; LProc = 14; XProc = 15; SProc = 16; CProc = 17; 
    IProc = 18; Mod   = 19; Head  = 20;    

   (*structure forms*)    
    Bool = 2; Char = 3; SInt = 4; Int = 5; LInt = 6;    
    Real = 7; LReal = 8; Set = 9; String = 10; NilTyp = 11;
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;    

		RLen = 9;
    LRLen = 17;

  PROCEDURE DeRef*(VAR x: COCT.Item);
    VAR np: INTEGER;
  BEGIN COCQ.Prepend("(*", x.qoffs, np); COCQ.Append(")")
  END DeRef;

  PROCEDURE NilPtr*(VAR x: COCT.Item);
    VAR np: INTEGER;
  BEGIN COCQ.Prepend("((", x.qoffs, np);
    COCN.CTDenoter(x.typ, np, np); 
    COCQ.Prepend(")pOt__nilchk(__FILE__,__LINE__,", np, np);
    COCQ.Append("))")
  END NilPtr;   

  PROCEDURE InRef*(VAR x: COCT.Item);
    VAR np: INTEGER;
  BEGIN COCQ.Prepend("(&", x.qoffs, np); COCQ.Append(")")
  END InRef;

  PROCEDURE Cast*(VAR x: COCT.Item);
    VAR np: INTEGER;
  BEGIN
    COCQ.Prepend("((", x.qoffs, np);
    COCN.CTDenoter(x.typ, np, np);
    IF x.mode = Ind THEN COCQ.Prepend("*)(", np, np) 
    ELSE COCQ.Prepend(")(", np, np) 
    END;
    COCQ.Append("))")
  END Cast;

   (* array indexes *)
  PROCEDURE ArrInxPfx*(VAR x: COCT.Item; inxchk: BOOLEAN);
    VAR len: LONGINT;
      s: ARRAY 9 OF CHAR; 
  BEGIN COCQ.Append(".arr[");
    IF inxchk THEN 
      COCQ.Append("pOt__inxchk(__FILE__,__LINE__,0x"); 
      Strings.FromLInt(x.typ.n, 16, s); COCQ.Append(s); 
      COCQ.Append(", ")
    ELSE 
      COCQ.Append("(")
    END
  END ArrInxPfx;

  PROCEDURE ArrInxSfx*;
  BEGIN COCQ.Append(")]") 
  END ArrInxSfx;

  PROCEDURE DynArrInxPfx*(VAR x: COCT.Item; inxchk: BOOLEAN);
    VAR np: INTEGER; s: ARRAY 9 OF CHAR; btyp: COCT.Struct; y: COCT.Item;
  BEGIN
    COCQ.Prepend("(", x.qoffs, np);
    IF x.intval = 0 THEN COCQ.Prepend("(char *)", np, np) END;
    COCQ.Append("+sizeof(pOt__ArrTypDsc*)+");
    btyp := x.typ.BaseTyp;
    IF btyp.form = DynArr THEN
      COCQ.Append("(*(");
      COCN.CObjName(x, COCQ.cslen, np);
      IF x.intval # 0 THEN
        COCQ.Append("+0x");
        Strings.FromLInt(x.intval, 16, s);
        COCQ.Append("L");
        COCQ.Append(s)
      END;
      COCQ.Append("))->elsize")
    ELSE COCN.CTSize(btyp, COCQ.cslen, np)
    END;
    IF inxchk THEN 
      COCQ.Append("*pOt__inxchk(__FILE__,__LINE__,(*(");
      COCN.CObjName(x, COCQ.cslen, np);
      IF x.intval # 0 THEN
        COCQ.Append("+0x");
        Strings.FromLInt(x.intval, 16, s);
        COCQ.Append(s)
      END;
      COCQ.Append("))->nofel,")
    ELSE COCQ.Append("*(")
    END         
  END DynArrInxPfx;   

  PROCEDURE DynArrInxSfx*;
  BEGIN COCQ.Append("))") 
  END DynArrInxSfx;

  PROCEDURE BytArrInxPfx*(VAR x: COCT.Item; inxchk: BOOLEAN);
    VAR np: INTEGER;
  BEGIN
    IF inxchk THEN 
      COCQ.Append(".data[pOt__inxchk(__FILE__,__LINE__,"); 
      COCN.CObjName(x, COCQ.cslen, np); 
      COCQ.Append(".len,")
    ELSE COCQ.Append(".data[(")
    END
  END BytArrInxPfx;

  PROCEDURE BytArrInxSfx*;
  BEGIN
    COCQ.Append(")")
  END BytArrInxSfx;

  PROCEDURE Field*(VAR x: COCT.Item; y: COCT.Object);
    VAR np, ix: INTEGER; z: COCT.Item;
  BEGIN
    COCQ.Append(".");
    ix := y.mnolev; WHILE ix # x.typ.n DO COCQ.Append("base."); INC(ix) END;
    z.mode := Fld; z.obj := y; z.mnolev := y.mnolev;
    COCN.CObjName(z, COCQ.cslen, np)
  END Field;

  PROCEDURE TypGuard*(VAR x, y: COCT.Item; typchk: BOOLEAN);
    VAR np: INTEGER; s: ARRAY 9 OF CHAR;
  BEGIN
    COCQ.Prepend("((", x.qoffs, np);
    COCN.CObjName(y, np, np);
    IF x.typ.form = Record THEN COCQ.Prepend("*)", np, np)
    ELSE COCQ.Prepend(")", np, np)
    END;
    IF typchk THEN 
      COCQ.Prepend("pOt__typchk(__FILE__,__LINE__,(pOt__RecTypDsc**)", np, np);
      COCQ.Append(",(pOt__RecTypDsc*)&");
      IF y.typ.form = Record THEN COCN.CTDName(y.typ, COCQ.cslen, np)
      ELSE (*Pointer*) COCN.CTDName(y.typ.BaseTyp, COCQ.cslen, np)
      END;
      COCQ.Append(",0x");
      IF y.typ.form = Record THEN Strings.FromLInt(y.typ.n, 16, s)
      ELSE (*Pointer*)  Strings.FromLInt(y.typ.BaseTyp.n, 16, s)
      END;
      COCQ.Append(s); COCQ.Append("L")
    ELSE COCQ.Prepend("(", np, np);
    END;
    COCQ.Append("))")
  END TypGuard;

 (* Constants *)
                                          
  PROCEDURE CConstValue*(VAR x: COCT.Item; pos: INTEGER; VAR nextpos: INTEGER);

    CONST CConstMaxLen = 127; (* enough *)
    VAR CConstBuf: ARRAY CConstMaxLen + 1 OF CHAR; 

    PROCEDURE WriteChar(c: CHAR);
      VAR i: INTEGER;
    BEGIN
      CConstBuf[0] := "'";
      IF (c = "\") OR (c = "'") OR (c = 22X) THEN
        CConstBuf[1] := "\"; CConstBuf[2] := c; i := 3
      ELSIF (0X <= c) & (c <= 1FX) OR (7FX <= c) & (c <= 0FFX) THEN
        CConstBuf[1] := "\"; CConstBuf[2] := 0X;
        COCQ.Prepend(CConstBuf, nextpos, nextpos);
        Strings.FromLInt(ORD(c), 8, CConstBuf); 
        i := 0; WHILE CConstBuf[i] # 0X DO INC(i) END
      ELSE 
        CConstBuf[1] := c; i := 2
      END;
      CConstBuf[i] := "'"; CConstBuf[i+1] := 0X;
      COCQ.Prepend(CConstBuf, nextpos, nextpos)
    END WriteChar;

    PROCEDURE WriteLInt(form: INTEGER; li: LONGINT);
    BEGIN
      IF form = Bool THEN COCQ.Prepend("(pOt_BOOLEAN)", nextpos, nextpos) 
      ELSIF form IN {Pointer, ProcTyp} THEN COCQ.Prepend("(void*)", nextpos, nextpos)
      END;
      IF li # MIN(LONGINT) THEN 
        Strings.FromLInt(li, 10, CConstBuf);
				IF li < 0 THEN COCQ.Prepend("(", nextpos, nextpos) END;
        COCQ.Prepend(CConstBuf, nextpos, nextpos);
        IF form = LInt THEN COCQ.Prepend("L", nextpos, nextpos) END;
				IF li < 0 THEN COCQ.Prepend(")", nextpos, nextpos) END
      ELSE COCQ.Prepend("(pOt_LONGINT)", nextpos, nextpos);
        Strings.FromLInt(li, 16, CConstBuf);
        COCQ.Prepend("0x", nextpos, nextpos);
        COCQ.Prepend(CConstBuf, nextpos, nextpos);
        COCQ.Prepend("L", nextpos, nextpos)
      END  
    END WriteLInt;

    PROCEDURE WriteLReal(form: INTEGER; lr: LONGREAL);
      VAR expo: INTEGER;
        mant: LONGINT;
        i,len: INTEGER;
    BEGIN
			CASE form OF Real: len := RLen; COCQ.Prepend("(pOt_REAL)", nextpos, nextpos)
			| LReal: len := LRLen
			END;
      IF lr # 0 THEN
        i := 0;
        IF lr < 0.0 THEN CConstBuf[i] := "-"; INC(i); lr := -lr END;
        expo := 0;
        IF lr > 1.0 THEN WHILE lr >= 10.0 DO lr := lr/10.0; INC(expo) END
        ELSE WHILE lr < 1.0 DO lr := lr*10.0; DEC(expo) END
        END;
        mant := ENTIER(lr);
        CConstBuf[i] := CHR(mant + ORD("0")); INC(i);
        CConstBuf[i] := "."; INC(i);
        WHILE i # len DO
          lr := (lr - mant)*10.0;
          mant := ENTIER(lr);
          CConstBuf[i] := CHR(mant + ORD("0"));
          INC(i)
        END;
        CConstBuf[i] := 0X;
        COCQ.Prepend(CConstBuf, nextpos, nextpos);
        IF expo # 0 THEN
          Strings.FromLInt(expo, 10, CConstBuf);
          COCQ.Prepend("E", nextpos, nextpos);
          COCQ.Prepend(CConstBuf, nextpos, nextpos)
        END;  
      ELSE COCQ.Prepend("0.0", nextpos, nextpos)
      END
    END WriteLReal;

    PROCEDURE WriteSet(s: LONGINT);
    BEGIN
      COCQ.Prepend("(pOt_SET)0x", nextpos, nextpos);
      Strings.FromLInt(s, 16, CConstBuf);
      Strings.Append(CConstBuf, "L");
      COCQ.Prepend(CConstBuf, nextpos, nextpos)
    END WriteSet;
      
    PROCEDURE WriteString(inx: LONGINT);
    BEGIN
      COCQ.Prepend("&pOt__strcon_buf[0x", nextpos, nextpos);
      Strings.FromLInt(inx-1, 16, CConstBuf);
      COCQ.Prepend(CConstBuf, nextpos, nextpos);
      COCQ.Prepend("L]", nextpos, nextpos)
    END WriteString;
      
  BEGIN (* x.mode = Const *)
    nextpos := pos;
    CASE x.typ.form OF 
    | Char: WriteChar(CHR(x.intval MOD 100H))
    | Bool, SInt .. LInt, Pointer, ProcTyp: WriteLInt(x.typ.form, x.intval)
    | Real, LReal: WriteLReal(x.typ.form, x.fltval)
    | Set: WriteSet(x.intval)
    | String: IF (x.intval DIV 100H) = 0 THEN COCN.CObjName(x, nextpos, nextpos) 
      ELSE WriteString(x.intval DIV 100H)
      END
    | NilTyp: COCQ.Prepend("pOt_NIL", nextpos, nextpos)
    END
  END CConstValue;

  PROCEDURE SetStrTD*(VAR x: COCT.Item; typ: COCT.Struct);
    VAR np: INTEGER; s: ARRAY 5 OF CHAR;
  BEGIN
    COCQ.Prepend("pOt__set_str_td(", x.qoffs, np); COCQ.Append(",&");
    IF (typ # NIL) & (typ.form = Array) THEN COCN.CTDName(typ, COCQ.cslen, np)
    ELSE  COCQ.Append("pOt__str_td[0x"); 
      Strings.FromLInt(x.intval MOD 100H, 16, s); COCQ.Append(s);
      COCQ.Append("]")
    END;
    COCQ.Append(")")
  END SetStrTD;

END COCJ.
