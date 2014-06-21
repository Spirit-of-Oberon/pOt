MODULE COCN; (* DT 22 10 1993 00:03 *)
  IMPORT SYSTEM, Strings, Reals, COCT, COCQ;

  CONST
   (* name resolution modes *)
    ordObj = 0; stdObj = 1; sysObj = 2;

   (*object and item modes*)
    Var = 1; Ind =3; Con = 8; Fld = 12; Typ = 13; CProc = 17;

   (*structure forms*)    
    Undef = 0; Bool = 2; Set = 9; String = 10; NilTyp = 11; NoTyp = 12;    
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;    

 (* name resolution and designating *)
  PROCEDURE CObjBaseName*(VAR x: COCT.Item; pos: INTEGER; VAR nextpos: INTEGER);
    VAR head: COCT.Object; 
  BEGIN
    nextpos := pos;
    IF x.obj = NIL THEN RETURN END;
		IF x.mode = CProc THEN
			COCQ.Prepend(x.obj.name, nextpos, nextpos) (* directly called external C function *)
		ELSE
	    COCQ.Prepend("pOt_", nextpos, nextpos);
	    COCQ.Prepend(x.obj.name, nextpos, nextpos);
	    IF x.mode # Fld THEN
	      IF x.mnolev < 0 THEN
	        COCQ.Prepend("_", nextpos, nextpos); 
	        COCQ.Prepend(COCT.GlbMod[-x.mnolev - 1].name, nextpos, nextpos)
	      ELSIF x.mnolev = 0 THEN
	        CASE x.obj.mnolev OF ordObj:
	          head := COCT.topScope;
	          WHILE head.mnolev # 0 DO head := head.dsc END;
	          COCQ.Prepend("_", nextpos, nextpos);
	          COCQ.Prepend(head.name, nextpos, nextpos)
	        | stdObj:
	        | sysObj: COCQ.Prepend("_SYSTEM", nextpos, nextpos)
	        END
	      ELSE
	        IF (x.mode >= Con) OR (x.mnolev < COCT.level) THEN
	          head := COCT.topScope;
	          WHILE x.mnolev # head.mnolev DO head := head.dsc END;
	          LOOP
	            COCQ.Prepend("_", nextpos, nextpos); 
	            COCQ.Prepend(head.name, nextpos, nextpos); 
	            IF head.mnolev = 0 THEN EXIT  END;
	            head := head.dsc
	          END
	        ELSIF x.mnolev > COCT.level THEN
	          COCQ.Prepend("_", nextpos, nextpos) (* twin *)
	        END
	      END
			END
    END
  END CObjBaseName;

  PROCEDURE CObjName*(VAR x: COCT.Item; pos: INTEGER; VAR nextpos: INTEGER);
    VAR viaref: BOOLEAN;
  BEGIN nextpos := pos;
    viaref := (x.mode = Var) & (x.mnolev > 0) & (x.mnolev # COCT.level);
    IF viaref THEN COCQ.Prepend("(*", nextpos, nextpos) END;
    CObjBaseName(x, nextpos, nextpos);
    IF viaref THEN COCQ.Prepend(")", nextpos, nextpos) END
  END CObjName;

  PROCEDURE CRetName*(pos: INTEGER; VAR nextpos: INTEGER);
  BEGIN COCQ.Prepend("pOt__retval", pos, nextpos)
  END CRetName;
    
  PROCEDURE CTDName*(typ: COCT.Struct; pos: INTEGER; VAR nextpos: INTEGER);
    VAR
      head: COCT.Object;
      s: ARRAY 5 OF CHAR;
  BEGIN (* provided typ.form = Record or typ.form = Array *)
    nextpos := pos;
    COCQ.Prepend("pOt__td_", nextpos, nextpos);
    Strings.FromLInt(typ.descr, 16, s); COCQ.Prepend(s, nextpos, nextpos);
    COCQ.Prepend("_", nextpos, nextpos);
    IF typ.mno = 0 THEN 
      head := COCT.topScope;
      WHILE head.mnolev # 0 DO head := head.dsc END;
      COCQ.Prepend(head.name, nextpos, nextpos)
    ELSE COCQ.Prepend(COCT.GlbMod[typ.mno - 1].name, nextpos, nextpos)
    END
  END CTDName;

  PROCEDURE CTagName*(typ: COCT.Struct; pos: INTEGER; VAR nextpos: INTEGER);
    VAR
      head: COCT.Object;
      s: ARRAY 5 OF CHAR;
  BEGIN (* provided typ.form = Record or typ.form = Array *)
    nextpos := pos;
    COCQ.Prepend("struct pOt__tag_", nextpos, nextpos); (* each record has a symbolic tag *)
    Strings.FromLInt(typ.descr, 16, s); COCQ.Prepend(s, nextpos, nextpos);
    COCQ.Prepend("_", nextpos, nextpos);
    IF typ.mno = 0 THEN 
      head := COCT.topScope;
      WHILE head.mnolev # 0 DO head := head.dsc END;
      COCQ.Prepend(head.name, nextpos, nextpos)
    ELSE COCQ.Prepend(COCT.GlbMod[typ.mno - 1].name, nextpos, nextpos)
    END
  END CTagName;

  PROCEDURE CTDenoter*(typ: COCT.Struct; pos: INTEGER; VAR nextpos: INTEGER);
    VAR y: COCT.Item;
    
  BEGIN
    nextpos := pos;
    CASE typ.form OF Undef:
    | Bool .. Set: 
      y.mode := Typ; y.typ := typ; y.obj := typ.strobj;
      IF typ.mno > 0 THEN y.mnolev := -typ.mno
      ELSE COCT.FindObj(y.obj, y.mnolev)
      END;
      CObjName(y, nextpos, nextpos)
    | String .. NilTyp: 
		| NoTyp: COCQ.Prepend("void", nextpos, nextpos)
    | Pointer: CTDenoter(typ.BaseTyp, nextpos, nextpos); COCQ.Prepend("*",nextpos,nextpos)  
    | ProcTyp: CTDenoter(typ.BaseTyp, nextpos, nextpos); COCQ.Prepend("(*)()",nextpos,nextpos)
    | DynArr: COCQ.Prepend("pOt__ArrTypDsc**", nextpos, nextpos)
    | Array, Record: CTagName(typ, nextpos, nextpos)
    END
  END CTDenoter;

  PROCEDURE CTSize*(typ: COCT.Struct; pos: INTEGER; VAR nextpos: INTEGER);
    VAR y: COCT.Item;
  BEGIN 
    COCQ.Prepend("sizeof(", pos, nextpos); 
    CTDenoter(typ, nextpos, nextpos);
    COCQ.Prepend(")", nextpos, nextpos)  
  END CTSize;

  PROCEDURE CBodyName*(obj: COCT.Object; pos: INTEGER; VAR nextpos: INTEGER);
  BEGIN COCQ.Prepend("pOt_", pos, nextpos); 
    IF obj.mnolev = 0 THEN COCQ.Prepend(obj.name, nextpos, nextpos)
    ELSE COCQ.Prepend(COCT.GlbMod[obj.mnolev-1].name, nextpos, nextpos)
    END;
    COCQ.Prepend("__body", nextpos, nextpos)
  END CBodyName;  
 
  PROCEDURE CBodyFlagName*(obj: COCT.Object; pos: INTEGER; VAR nextpos: INTEGER);
  BEGIN COCQ.Prepend("pOt_", pos, nextpos); 
    IF obj.mnolev = 0 THEN COCQ.Prepend(obj.name, nextpos, nextpos)
    ELSE COCQ.Prepend(COCT.GlbMod[obj.mnolev-1].name, nextpos, nextpos)
    END;
    COCQ.Prepend("__loaded", nextpos, nextpos)
  END CBodyFlagName;
  
END COCN.
