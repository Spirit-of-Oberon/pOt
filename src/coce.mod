MODULE COCE;  (*NW 7.6.87 / 5.3.91*) (*DT 27 12 1993 23:50*)
  IMPORT SYSTEM, COCS, COCT, COCQ, COCN, COCJ, COCX;
    
  CONST
   (*object and item modes*)  
    Var = 1; Ind = 3; Con = 8; Reg = 11; Fld = 12; Typ = 13;  
    
   (*structure forms*)
    Undef = 0; Byte = 1; Bool = 2; Char = 3; SInt = 4; Int = 5; LInt = 6;  
    Real = 7; LReal = 8; Set = 9; String = 10; NilTyp = 11; NoTyp = 12;  
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;  

    intSet = {SInt .. LInt}; realSet = {Real .. LReal};
    numSet = intSet + realSet;

  VAR
    inxchk*, rngchk*, nilchk*: BOOLEAN;

   (*SYSTEM Dependant*)
    MinChar, MaxChar,
    MinBool, MaxBool,
    MinSInt, MaxSInt,
    MinInt, MaxInt,
    MinLInt, MaxLInt,
    MinSet, MaxSet: LONGINT;
    MinReal, MaxReal,
    MinLReal, MaxLReal: LONGREAL;

  PROCEDURE SetIntType*(VAR x: COCT.Item);
    VAR v: LONGINT;  
  BEGIN v := x.intval;  
    IF (MinSInt <= v) & (v <= MaxSInt) THEN x.typ := COCT.sinttyp  
    ELSIF (MinInt <= v) & (v <= MaxInt) THEN x.typ := COCT.inttyp  
    ELSE x.typ := COCT.linttyp  
    END  
  END SetIntType;  
    
  PROCEDURE StartObj*(VAR x: COCT.Item): INTEGER;    
    VAR qoffs, np: INTEGER; 
  BEGIN qoffs := x.qoffs;
    COCQ.Link(x); COCN.CObjName(x, x.qoffs, np); 
    RETURN qoffs  
  END StartObj;  

  PROCEDURE StopObj*(VAR x: COCT.Item; qoffs: INTEGER);
  BEGIN IF x.mode = Ind THEN COCJ.DeRef(x); x.mode := Var END;
    COCQ.Unlink(x);
    x.qoffs := qoffs
  END StopObj;  
  
  PROCEDURE StartExpr*(VAR x: COCT.Item): INTEGER;  
    VAR qoffs: INTEGER;
  BEGIN qoffs := x.qoffs;
    COCQ.Link(x); IF x.qoffs = 0 THEN COCQ.Dummy END; 
    RETURN qoffs  
  END StartExpr;  

  PROCEDURE StopExpr*(VAR x: COCT.Item; qoffs: INTEGER);     
    VAR np: INTEGER;
  BEGIN
    IF (x.mode = Con) & (x.typ # COCT.notyp) THEN COCQ.Drop(x);
       COCQ.Link(x); COCJ.CConstValue(x, x.qoffs, np)
    END;
    COCQ.Unlink(x);
    x.qoffs := qoffs  
  END StopExpr;  
  
  PROCEDURE StopConstExpr*(VAR x: COCT.Item; qoffs: INTEGER);  
  BEGIN COCQ.Drop(x); x.qoffs := qoffs  
  END StopConstExpr;    
  
  PROCEDURE StopStringExpr*(VAR x: COCT.Item; qoffs: INTEGER; typ: COCT.Struct);    
    VAR np: INTEGER;
  BEGIN    
    IF x.typ = COCT.stringtyp THEN
      IF x.intval MOD 100H = 1 THEN COCQ.Drop(x); 
	COCQ.Link(x); COCJ.CConstValue(x, x.qoffs, np)
      END;  
      COCJ.SetStrTD(x, typ);
      COCJ.DeRef(x)
    END;
    COCQ.Unlink(x); x.qoffs := qoffs
  END StopStringExpr;  

  PROCEDURE SubExprPrefix*;
  BEGIN COCX.SubExprPrefix
  END SubExprPrefix;

  PROCEDURE SubExprSuffix*;
  BEGIN COCX.SubExprSuffix
  END SubExprSuffix;

  PROCEDURE HookExpr*(VAR x: COCT.Item): INTEGER;  
    VAR qoffs: INTEGER;
  BEGIN qoffs := x.qoffs; COCQ.Link(x); RETURN qoffs  
  END HookExpr;  

  PROCEDURE IndexPrefix*(VAR x: COCT.Item);
  BEGIN
    IF x.mode >= Con THEN COCS.Mark(127) END;
    IF x.typ.form = Array THEN
      IF x.mode = Ind THEN COCJ.DeRef(x); x.mode := Var END;
      COCJ.ArrInxPfx(x, inxchk)
    ELSIF x.typ.form = DynArr THEN  
      IF (x.intval=0) & (x.typ.BaseTyp.form=Byte) THEN 
	COCJ.BytArrInxPfx(x, inxchk)
      ELSE COCJ.DynArrInxPfx(x, inxchk)
      END
    ELSE COCS.Mark(82)  
    END
  END IndexPrefix;

  PROCEDURE Index*(VAR x, y: COCT.Item);
    VAR  f, n: INTEGER; 
  BEGIN f := y.typ.form;  
    IF ~(y.typ.form IN intSet) THEN COCS.Mark(80); y.typ := COCT.inttyp END ;  
    IF y.mode > Reg THEN COCS.Mark(126) END;
    IF x.typ.form = Array THEN  
      COCJ.ArrInxSfx;
      IF (y.mode = Con) & ((0 > y.intval) OR (y.intval >= x.typ.n)) THEN COCS.Mark(81) END;
      x.typ := x.typ.BaseTyp;
      x.obj := NIL;
    ELSIF x.typ.form = DynArr THEN  
      IF (x.intval=0) & (x.typ.BaseTyp.form=Byte) THEN 
	COCJ.BytArrInxSfx(); x.typ := x.typ.BaseTyp; x.obj := NIL
      ELSE COCJ.DynArrInxSfx();
	x.typ := x.typ.BaseTyp;
	IF x.typ.form = DynArr THEN INC(x.intval) 
	ELSE COCJ.Cast(x); COCJ.DeRef(x); x.mode := Var; x.obj := NIL
	END
      END
    END
  END Index;  
    
  PROCEDURE Field*(VAR x: COCT.Item; y: COCT.Object);
  BEGIN (*x.typ.form = Record*)  
    IF (y # NIL) & (y.mode = Fld) THEN  
      IF x.mode = Ind THEN COCJ.DeRef(x); x.mode := Var 
      ELSIF x.mode # Var THEN COCS.Mark(127)
      END;
      COCJ.Field(x,y);
      x.typ := y.typ
    ELSE COCS.Mark(83); x.typ := COCT.undftyp
    END;
    x.obj := NIL
  END Field;  
    
  PROCEDURE DeRef*(VAR x: COCT.Item);
  BEGIN IF x.mode >= Con THEN COCS.Mark(127) END;
    IF x.typ.form = Pointer THEN  
      IF x.mode = Var THEN x.mode := Ind  
      ELSE COCJ.DeRef(x)
      END ;  
      IF nilchk THEN COCJ.NilPtr(x) END;
      x.typ := x.typ.BaseTyp; x.obj := COCT.wasderef  
    ELSE COCS.Mark(84)  
    END 
  END DeRef;

  PROCEDURE GTT(t0, t1: COCT.Struct);  
    VAR t: COCT.Struct;   
  BEGIN    
    IF t0 # t1 THEN t := t1;    
      REPEAT t := t.BaseTyp UNTIL (t = NIL) OR (t = t0);    
      IF t = NIL THEN COCS.Mark(85) END  
    END    
  END GTT;    
    
  PROCEDURE TypGuard*(VAR x, y: COCT.Item);
  BEGIN
    IF x.typ.form = Pointer THEN  
      IF x.mode = Ind THEN COCJ.DeRef(x); x.mode := Var END;
      IF y.typ.form = Pointer THEN  
	GTT(x.typ.BaseTyp, y.typ.BaseTyp);
	COCJ.TypGuard(x, y, COCT.typchk) 
      ELSE COCS.Mark(86)  
      END  
    ELSIF (x.typ.form = Record) & 
	  (x.obj # NIL) & (x.obj # COCT.wasderef) & (x.obj.mode = Ind) &
	  (y.typ.form = Record) THEN  
      GTT(x.typ, y.typ);  
      COCJ.TypGuard(x, y, COCT.typchk)
    ELSE COCS.Mark(87)  
    END;
    x.typ := y.typ
  END TypGuard;  

  PROCEDURE TypTest*(VAR x,y: COCT.Item);
  BEGIN
    IF x.typ.form = Pointer THEN  
      IF y.typ.form = Pointer THEN  
	GTT(x.typ.BaseTyp, y.typ.BaseTyp);
	COCX.TypTest(x, y)
      ELSE COCS.Mark(86)  
      END  
    ELSIF (x.typ.form = Record) & 
	  (x.obj # NIL) & (x.obj # COCT.wasderef) & (x.obj.mode = Ind) &
	  (y.typ.form = Record) THEN  
      GTT(x.typ, y.typ);  
      COCX.TypTest(x, y)
    ELSE COCS.Mark(87)  
    END;
    x.typ := COCT.booltyp
  END TypTest;

  PROCEDURE Const*(VAR x: COCT.Item);
    VAR np: INTEGER;
  BEGIN COCJ.CConstValue(x, COCQ.cslen, np)
  END Const;

  PROCEDURE Set00*(VAR x, y: COCT.Item); (*(1<<(y))*)
    VAR one: LONGINT;  
  BEGIN x.mode := Reg; x.intval := 0; x.typ := COCT.settyp;  
    IF y.typ.form IN intSet THEN  
      IF y.mode = Con THEN x.mode := Con;  
	IF (MinSet <= y.intval) & (y.intval <= MaxSet) THEN 
	  one := 1; x.intval := SYSTEM.LSH(one, y.intval)  
	ELSE COCS.Mark(202)  
	END  
      END;
      COCX.Set00(x,rngchk)
    ELSE COCS.Mark(93)  
    END  
  END Set00;  
    
  PROCEDURE Set10*(VAR x, y: COCT.Item); (*((~0UL<<(y))&(~0UL>>(31-(z))))*)
    VAR all: LONGINT;
  BEGIN x.mode :=  Reg; x.intval := 0; x.typ := COCT.settyp;
    IF y.typ.form IN intSet THEN
      IF y.mode = Con THEN x.mode := Con;
	IF (MinSet <= y.intval) & (y.intval <= MaxSet) THEN 
	  all := -1; x.intval := SYSTEM.LSH(all, y.intval)  
	ELSE COCS.Mark(202)  
	END  
      END;
      COCX.Set10(x,rngchk)
    ELSE COCS.Mark(93)  
    END  
  END Set10;
    
  PROCEDURE Set11*(VAR x, y, z: COCT.Item);
    VAR s: LONGINT;
  BEGIN   
    IF z.typ.form IN intSet THEN  
      IF x.mode = Con THEN  
	IF z.mode = Con THEN  
	  IF (z.intval > MaxSet) OR (z.intval < 0) THEN COCS.Mark(202); x.intval := 0
	  ELSIF y.intval <= z.intval THEN
	    s := -2; x.intval := x.intval - SYSTEM.LSH(s, z.intval)
	  ELSE x.intval := 0 (*ok*)
	  END
	ELSE x.mode := Reg
	END  
      ELSIF (z.mode = Con) & ((0 > z.intval) OR (z.intval > MaxSet)) THEN 
	COCS.Mark(202)
      END;
      COCX.Set11(x, rngchk)
    ELSE COCS.Mark(93)  
    END
  END Set11; 

  PROCEDURE InPrefix*(VAR x: COCT.Item);
  BEGIN 
    IF x.mode > Reg THEN COCS.Mark(126) END;
    IF x.typ.form IN intSet THEN
      IF x.mode = Con THEN COCX.InPfx(x, FALSE);
	IF (0 > x.intval) OR (x.intval > MaxSet) THEN COCS.Mark(202) END
      ELSE COCX.InPfx(x, rngchk); x.mode := Reg
      END
    ELSE COCS.Mark(92); x.mode := Reg
    END
  END InPrefix;

  PROCEDURE In*(VAR x, y: COCT.Item);
  BEGIN 
    IF y.mode > Reg THEN COCS.Mark(126) END;
    IF (x.typ.form IN intSet) & (y.typ.form = Set) THEN  
      COCX.InSfx;
      IF (x.mode = Con) & (y.mode = Con) THEN 
	IF x.intval IN SYSTEM.VAL(SET, y.intval) THEN x.intval := 1
	ELSE x.intval := 0
	END
      ELSE x.mode := Reg
      END
    ELSE COCS.Mark(92); x.mode := Reg  
    END ;  
    x.typ := COCT.booltyp  
  END In;  

  PROCEDURE MOp*(op: INTEGER; VAR x: COCT.Item); (* monadic plus, minus and negation *)
    VAR f: INTEGER; a: LONGINT;
  BEGIN 
    f := x.typ.form;  
    IF x.mode > Reg THEN COCS.Mark(126) END;
    CASE op OF 
      5 (*&*): 
      IF f # Bool THEN
	COCS.Mark(94); x.mode := Con; x.intval := 0; x.typ := COCT.booltyp
      END;
    | 6 (*+*): 
      IF f IN numSet THEN COCX.MOp(op, x, rngchk)
      ELSE COCS.Mark(96); x.mode := Reg
      END  
    | 7 (*-*): 
      IF f IN numSet THEN  
	COCX.MOp(op, x, rngchk);
	IF x.mode = Con THEN 
	  IF f IN intSet THEN x.intval := -x.intval; SetIntType(x)  
	  ELSE x.fltval := -x.fltval
	  END
	ELSE x.mode := Reg
	END
      ELSIF f = Set  THEN 
	COCX.MOp(op, x, rngchk);
	IF x.mode = Con THEN a := -1; x.intval := a - x.intval
	ELSE x.mode := Reg
	END
      ELSE COCS.Mark(97); x.mode := Reg
      END  
    | 8 (*OR*): 
      IF f # Bool THEN
	COCS.Mark(95); x.mode := Con; x.typ := COCT.booltyp; x.intval := 1
      END
    | 32: (*~*)  
      IF f = Bool THEN  
	COCX.MOp(op, x, rngchk);
	IF x.mode = Con THEN x.intval := 1 - x.intval  
	ELSE x.mode := Reg
	END
      ELSE COCS.Mark(98); x.mode := Reg
      END  
    END  
  END MOp;  

  PROCEDURE convertII(VAR x: COCT.Item; typ: COCT.Struct);
  BEGIN IF x.mode < Con THEN x.mode := Reg END; x.typ := typ
  END convertII;  
    
  PROCEDURE convertRI(VAR x: COCT.Item; typ: COCT.Struct);
  BEGIN IF x.mode < Con THEN x.mode := Reg END;
    IF x.mode = Con THEN x.fltval := x.intval END; x.typ := typ
  END convertRI;  
    
  PROCEDURE convertRR(VAR x: COCT.Item);
  BEGIN IF x.mode < Con THEN x.mode := Reg END; x.typ := COCT.lrltyp
  END convertRR;  
      
  PROCEDURE Op*(op: INTEGER; VAR x, y: COCT.Item);
    VAR f, g: INTEGER; p, q, r: COCT.Struct; consts: BOOLEAN;
      convert: SET; (*0 - first, 1 - second*)
    
    PROCEDURE strings(): BOOLEAN;
      VAR first, second: BOOLEAN;
    BEGIN 
      first := ((((f=Array) OR (f=DynArr)) & (x.typ.BaseTyp.form=Char)) OR (f=String));
      second := ((((g=Array) OR (g=DynArr)) & (y.typ.BaseTyp.form=Char)) OR (g=String));
      RETURN 
	first & second OR 
	first & (y.mode = Con) & (y.typ.form = Char) OR
	(x.mode = Con) & (x.typ.form = Char) & second
    END strings;  
    
  BEGIN 
    IF x.mode > Reg THEN COCS.Mark(126) END; 
    IF y.mode > Reg THEN COCS.Mark(126) END; 
    convert := {};
    IF x.typ # y.typ THEN (* conversions *)
      f := x.typ.form; g := y.typ.form;  
      CASE f OF  
	Undef:  
      | SInt: 
	IF g = Int THEN convertII(x, y.typ)  
	ELSIF g = LInt THEN convertII(x, y.typ)  
	ELSIF g = Real THEN convertRI(x, y.typ)  
	ELSIF g = LReal THEN convertRI(x, y.typ)  
	ELSE COCS.Mark(100)  
	END  
      | Int:  
	IF g = SInt THEN convertII(y, x.typ)  
	ELSIF g = LInt THEN convertII(x, y.typ)  
	ELSIF g = Real THEN convertRI(x, y.typ)  
	ELSIF g = LReal THEN convertRI(x, y.typ)  
	ELSE COCS.Mark(100)  
	END  
      | LInt: 
	IF g = SInt THEN convertII(y, x.typ)  
	ELSIF g = Int THEN convertII(y, x.typ)  
	ELSIF g = Real THEN convertRI(x, y.typ)  
	ELSIF g = LReal THEN convertRI(x, y.typ)  
	ELSE COCS.Mark(100)  
	END  
      | Real: 
	IF g = SInt THEN convertRI(y, x.typ)  
	ELSIF g = Int THEN convertRI(y, x.typ)  
	ELSIF g = LInt THEN convertRI(y, x.typ)  
	ELSIF g = LReal THEN convertRR(x)  
	ELSE COCS.Mark(100)  
	END  
      | LReal: 
	IF g = SInt THEN convertRI(y, x.typ)  
	ELSIF g = Int THEN convertRI(y, x.typ)  
	ELSIF g = LInt THEN convertRI(y, x.typ)  
	ELSIF g = Real THEN convertRR(y)  
	ELSE COCS.Mark(100)  
	END  
      | NilTyp: IF g # Pointer THEN COCS.Mark(100) END  
      | Pointer: 
	IF g = Pointer THEN  
	  p := x.typ.BaseTyp; q := y.typ.BaseTyp;  
	  IF (p.form = Record) & (q.form = Record) THEN  
	    IF p.n < q.n THEN r := p; p := q; q := r; 
	      INCL(convert,1); y.typ := x.typ
	    ELSE INCL(convert,0); x.typ := y.typ
	    END;  
	    WHILE (p # q) & (p # NIL) DO p := p.BaseTyp END;  
	    IF p = NIL THEN COCS.Mark(100) END  
	  ELSE COCS.Mark(100)  
	  END  
	ELSIF g # NilTyp THEN COCS.Mark(100)
	END  
      | ProcTyp: IF g # NilTyp THEN COCS.Mark(100) END  
      | Array, DynArr, Char, String:  
      | Byte, Bool, Set, NoTyp, Record: COCS.Mark(100)
      END;
      IF f IN numSet THEN 
	IF f = x.typ.form THEN INCL(convert,1)
	ELSE INCL(convert,0)
	END
      END
    END;  
    f := x.typ.form; g := y.typ.form; consts := (x.mode = Con) & (y.mode = Con);
    CASE op OF 
     (*multiplication*) 
      1 (***):  
      IF f IN numSet THEN
	COCX.NumOp(op,convert,x,y,rngchk);
	IF (f IN intSet) & consts THEN (*ovfl test missing*)  
	  x.intval := x.intval * y.intval; SetIntType(x)  
	ELSE x.mode := Reg
	END
      ELSIF f = Set THEN  
	COCX.SetOp(op,x,y);
	IF consts THEN 
	  x.intval := SYSTEM.VAL(LONGINT,SYSTEM.VAL(SET,x.intval)*SYSTEM.VAL(SET,y.intval))
	ELSE x.mode := Reg
	END
      ELSIF f # Undef THEN COCS.Mark(101); x.mode := Reg
      END  
    
    | 2 (*/*):  
      IF f IN numSet THEN
	IF x.typ.form IN intSet THEN convert := convert + {0,1}; 
	  convertRI(x, COCT.realtyp); convertRI(y, COCT.realtyp)
	END;
	COCX.NumOp(op,convert,x,y,rngchk); x.mode := Reg
      ELSIF f = Set THEN
	COCX.SetOp(op,x,y);
	IF consts THEN 
	  x.intval := SYSTEM.VAL(LONGINT,SYSTEM.VAL(SET,x.intval)/SYSTEM.VAL(SET,y.intval))
	ELSE x.mode := Reg 
	END 
      ELSIF f # Undef THEN COCS.Mark(102); x.mode := Reg
      END  
    
    | 3 (*DIV*):  
      IF f IN intSet THEN  
	COCX.NumOp(op,convert,x,y,rngchk);
	IF consts THEN  
	  IF y.intval # 0 THEN x.intval := x.intval DIV y.intval; SetIntType(x)  
	  ELSE COCS.Mark(205)  
	  END  
	ELSE x.mode := Reg
	END
      ELSIF f # Undef THEN COCS.Mark(103); x.mode := Reg
      END  
    
    | 4 (*MOD*):  
      IF f IN intSet THEN  (*MOD*)
	COCX.NumOp(op,convert,x,y,rngchk);
	IF consts THEN  
	  IF y.intval # 0 THEN x.intval := x.intval MOD y.intval; x.typ := y.typ  
	  ELSE COCS.Mark(205)  
	  END  
	ELSE x.mode := Reg
	END
      ELSIF f # Undef THEN COCS.Mark(104); x.mode := Reg
      END  
    
    | 5 (*&*):  
      COCX.BoolOp(op,x,y);
      IF consts THEN x.intval := x.intval*y.intval ELSE x.mode := Reg END

   (* addition *)
    | 6 (*+*):  
      IF f IN numSet THEN
	COCX.NumOp(op,convert,x,y,rngchk);
	IF (f IN intSet) & consts THEN
	  INC(x.intval, y.intval); SetIntType(x)  (*ovfl test missing*)  
	ELSE x.mode := Reg
	END
      ELSIF f = Set THEN
	COCX.SetOp(op,x,y);
	IF consts THEN
	  x.intval := SYSTEM.VAL(LONGINT,SYSTEM.VAL(SET,x.intval)+SYSTEM.VAL(SET,y.intval))  
	ELSE x.mode := Reg
	END
      ELSIF f # Undef THEN COCS.Mark(105); x.mode := Reg
      END  
    
    | 7 (*-*): 
      IF f IN numSet THEN
	COCX.NumOp(op,convert,x,y,rngchk);
	IF (f IN intSet) & consts THEN
	  DEC(x.intval, y.intval); SetIntType(x)  (*ovfl test missing*)  
	ELSE x.mode := Reg
	END
      ELSIF f = Set THEN
	COCX.SetOp(op,x,y);
	IF consts THEN
	  x.intval := SYSTEM.VAL(LONGINT,SYSTEM.VAL(SET,x.intval)-SYSTEM.VAL(SET,y.intval))  
	ELSE x.mode := Reg
	END
      ELSIF f # Undef THEN COCS.Mark(106); x.mode := Reg
      END  

    | 8 (*OR*): 
      COCX.BoolOp(op,x,y);
      IF consts & (x.intval = 0) THEN x.intval := y.intval 
      ELSE x.mode := Reg 
      END
    
   (* relations *)
    | 9, 10 (*=,#*):  
      IF f IN {Bool, SInt .. LInt, Set, NilTyp, Pointer, ProcTyp} THEN
	COCX.NumRel(op,convert,x,y);
	IF consts THEN
	  IF (x.intval = y.intval) = (op = 9) THEN x.intval := 1 ELSE x.intval := 0 END
	ELSE x.mode := Reg
	END
      ELSIF f IN realSet THEN COCX.NumRel(op,convert,x,y); x.mode := Reg
      ELSIF strings() THEN COCX.StrRel(op,x,y); x.mode := Reg
      ELSIF (f = Char) # (g = Char) THEN COCS.Mark(100); x.mode := Reg
      ELSIF f = Char THEN
	COCX.CharRel(op,x,y);
	IF consts THEN
	  IF (x.intval = y.intval) = (op = 9) THEN x.intval := 1 ELSE x.intval := 0 END
	ELSE x.mode := Reg
	END
      ELSIF f # Undef THEN COCS.Mark(107); x.mode := Reg
      END;
      x.typ := COCT.booltyp
    
    | 11,14 (*<,>=*): 
      IF f IN intSet THEN
	COCX.NumRel(op,convert,x,y);
	IF consts THEN
	  IF (x.intval < y.intval) = (op = 11) THEN x.intval := 1 ELSE x.intval := 0 END
	ELSE x.mode := Reg
	END
      ELSIF f IN realSet THEN COCX.NumRel(op,convert,x,y); x.mode := Reg
      ELSIF strings() THEN COCX.StrRel(op,x,y); x.mode := Reg
      ELSIF (f = Char) # (g = Char) THEN COCS.Mark(100); x.mode := Reg
      ELSIF f = Char THEN
	COCX.CharRel(op,x,y);
	IF consts THEN
	  IF (x.intval < y.intval) = (op = 11) THEN x.intval := 1 ELSE x.intval := 0 END
	ELSE x.mode := Reg
	END
      ELSIF f # Undef THEN COCS.Mark(108); x.mode := Reg
      END;
      x.typ := COCT.booltyp
    
    | 12,13 (*<=,>*): 
      IF f IN intSet THEN
	COCX.NumRel(op,convert,x,y);
	IF consts THEN
	  IF (x.intval <= y.intval) = (op = 12) THEN x.intval := 1 ELSE x.intval := 0 END
	ELSE x.mode := Reg
	END
      ELSIF f IN realSet THEN COCX.NumRel(op,convert,x,y); x.mode := Reg
      ELSIF strings() THEN COCX.StrRel(op,x,y); x.mode := Reg
      ELSIF (f = Char) # (g = Char) THEN COCS.Mark(100); x.mode := Reg
      ELSIF f = Char THEN
	COCX.CharRel(op,x,y);
	IF consts THEN
	  IF (x.intval <= y.intval) = (op = 12) THEN x.intval := 1 ELSE x.intval := 0 END
	ELSE x.mode := Reg
	END                                      
      ELSIF f # Undef THEN COCS.Mark(108); x.mode := Reg
      END;
      x.typ := COCT.booltyp
    END  
  END Op;  

  PROCEDURE TkFct*(VAR x: COCT.Item; fctno: INTEGER);
  BEGIN COCX.StProcPfx(fctno,rngchk);
  END TkFct;
    
  PROCEDURE StPar1*(VAR x: COCT.Item; fctno: INTEGER);
    VAR f: INTEGER; s: LONGINT;
  BEGIN f := x.typ.form;
    CASE fctno OF 0: (*HALT*)  
      IF (f = SInt) & (x.mode = Con) THEN  
	IF x.intval < 20H THEN COCS.Mark(218) ELSE COCX.StPar1Sfx(x,fctno,rngchk) END  
      ELSE COCS.Mark(217)  
      END ;  
      x.typ := COCT.notyp  
    | 1: (*NEW*) 
      IF x.mode >= Con THEN COCS.Mark(112)
      ELSIF f = Pointer THEN  
	x.typ := x.typ.BaseTyp; f := x.typ.form;  
	IF f IN {Record, Array} THEN COCX.StPar1Sfx(x,fctno,rngchk)
	ELSE COCS.Mark(111)  
	END  
      ELSE COCS.Mark(111)  
      END ;  
      x.typ := COCT.notyp  
    | 2: (*CC*)  
      IF (f = SInt) & (x.mode = Con) THEN  
	IF (0 <= x.intval) & (x.intval < 16) THEN  COCX.StPar1Sfx(x,fctno,rngchk) 
	ELSE COCS.Mark(219) 
	END  
      ELSE COCS.Mark(217)  
      END;
      x.typ := COCT.notyp
    | 3: (*ABS*) 
      IF f IN numSet THEN 
	COCX.StPar1Sfx(x,fctno,rngchk);
	IF x.mode = Con THEN
	  CASE f OF SInt: 
	    IF x.intval < 0 THEN 
	      IF x.intval # MinSInt THEN x.intval := -x.intval
	      ELSE COCS.Mark(203)
	      END
	    END
	  | Int: 
	    IF x.intval < 0 THEN 
	      IF x.intval # MinInt THEN x.intval := -x.intval
	      ELSE COCS.Mark(203)
	      END
	    END
	  | LInt: 
	    IF x.intval < 0 THEN 
	      IF x.intval # MinLInt THEN x.intval := -x.intval
	      ELSE COCS.Mark(203)
	      END
	    END
	  | Real,LReal: IF x.fltval < 0.0 THEN  x.intval := -x.intval END
	  END
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg
	END
      ELSE COCS.Mark(111); x.mode := Reg
      END;
    | 4: (*CAP*) 
      IF f = Char THEN COCX.StPar1Sfx(x,fctno,rngchk);
	IF x.mode = Con THEN x.intval := ORD(CAP(CHR(x.intval))) 
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg
	END
      ELSE COCS.Mark(111); x.typ := COCT.chartyp; x.mode := Reg
      END;
    | 5: (*ORD*)   
      IF f IN {Byte, Char} THEN COCX.StPar1Sfx(x,fctno,rngchk); 
	IF x.mode # Con THEN IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg END
      ELSE COCS.Mark(111); x.mode := Reg 
      END;
      x.typ := COCT.inttyp
    | 6: (*ENTIER*)  
      IF f IN realSet THEN 
	COCX.StPar1Sfx(x,fctno,rngchk);
	IF x.mode = Con THEN 
	  IF (MinLInt <= x.fltval) & (x.fltval <= MaxLInt) THEN x.intval := ENTIER(x.fltval)
	  ELSE COCS.Mark(203)
	  END
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg
	END
      ELSE COCS.Mark(111); x.mode := Reg 
      END;  
      x.typ := COCT.linttyp
    | 7: (*SIZE*)  
      IF x.mode = Typ THEN COCX.StPar1Sfx(x,fctno,rngchk) ELSE COCS.Mark(110) END;
      x.typ := COCT.linttyp; x.mode := Reg
    | 8: (*ODD*)  
      IF f IN intSet THEN COCX.StPar1Sfx(x,fctno,rngchk);
	IF x.mode = Con THEN
	  IF ODD(x.intval) THEN x.intval := 1 ELSE x.intval := 0 END 
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg
	END
      ELSE COCS.Mark(111); x.mode := Reg 
      END;
      x.typ := COCT.booltyp
    | 9: (*ADR*) 
      IF x.mode < Con THEN COCX.StPar1Sfx(x,fctno,rngchk)
      ELSE COCS.Mark(127)
      END;
      x.typ := COCT.linttyp; x.mode := Reg
    | 10: (*MIN*)  
      IF x.mode = Typ THEN COCX.StPar1Sfx(x,fctno,rngchk); x.mode := Con;      
	CASE f OF      
	  Bool, Char:  x.intval := MinChar      
	| SInt:  x.intval := MinSInt      
	| Int:   x.intval := MinInt       
	| LInt:  x.intval := MinLInt      
	| Real:  x.fltval := MinReal      
	| LReal: x.fltval := MinLReal      
	| Set:   x.intval := MinSet; x.typ := COCT.inttyp        
	| Undef, NilTyp .. Record: COCS.Mark(111)        
	END      
      ELSE COCS.Mark(110); x.mode := Reg      
      END      
    | 11: (*MAX*)  
      IF x.mode = Typ THEN COCX.StPar1Sfx(x,fctno,rngchk); x.mode := Con;      
	CASE f OF      
	  Bool:  x.intval := MaxBool    
	| Char:  x.intval := MaxChar      
	| SInt:  x.intval := MaxSInt      
	| Int:   x.intval := MaxInt      
	| LInt:  x.intval := MaxLInt     
	| Real:  x.fltval := MaxReal      
	| LReal: x.fltval := MaxLReal     
	| Set:   x.intval := MaxSet; x.typ := COCT.inttyp      
	| Undef, NilTyp .. Record: COCS.Mark(111)      
	END      
      ELSE COCS.Mark(110); x.mode := Reg
      END       
    | 12: (*CHR*)   
      IF f IN intSet THEN COCX.StPar1Sfx(x,fctno,rngchk);
	IF x.mode = Con THEN
	  IF (0 > x.intval) OR (x.intval >= 100H) THEN COCS.Mark(203) END
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg
	END
      ELSE COCS.Mark(111) ; x.mode := Reg
      END;  
      x.typ := COCT.chartyp  
    | 13: (*SHORT*)  
      IF f IN {Int,LInt,LReal} THEN 
	COCX.StPar1Sfx(x,fctno,rngchk);
	CASE f OF Int: 
	  IF x.mode = Con THEN SetIntType(x);  
	    IF x.typ.form # SInt THEN COCS.Mark(203); x.mode := Reg END  
	  ELSE IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg
	  END; 
	  x.typ := COCT.sinttyp
	| LInt:
	  IF x.mode = Con THEN SetIntType(x);  
	    IF x.typ.form = LInt THEN COCS.Mark(203); x.mode := Reg END  
	  ELSE IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg
	  END;
	  x.typ := COCT.inttyp  
	| LReal:
	  IF x.mode = Con THEN 
	    IF (x.fltval > MaxReal) OR (x.fltval < MinReal) THEN COCS.Mark(203); x.mode := Reg END
	  ELSE IF x.mode > Reg THEN COCS.Mark(126) END; x.mode := Reg
	  END;
	  x.typ := COCT.realtyp
	END
      ELSE COCS.Mark(111); x.mode := Reg
      END  
    | 14: (*LONG*)  
      IF f IN {SInt,Int,Real} THEN COCX.StPar1Sfx(x,fctno,rngchk);
	IF x.mode > Reg THEN COCS.Mark(126) END;
	CASE f OF SInt: convertII(x, COCT.inttyp)
	| Int: convertII(x, COCT.linttyp)
	| Real: convertRR(x)
	END
      ELSE COCS.Mark(111); x.mode := Reg
      END  
    | 15: (*OVFL*)  
      IF (f = Bool) & (x.mode = Con) THEN COCX.StPar1Sfx(x,fctno,rngchk)
      ELSE COCS.Mark(111)  
      END;  
      x.typ := COCT.notyp  
    | 16,17: (*INC DEC*)   
      IF x.mode >= Con THEN COCS.Mark(112)  
      ELSIF f IN intSet THEN COCX.StPar1Sfx(x,fctno,rngchk)
      ELSE COCS.Mark(111)  
      END  
    | 18,19: (*INCL EXCL*)  
      IF x.mode >= Con THEN COCS.Mark(112)  
      ELSIF x.typ = COCT.settyp THEN COCX.StPar1Sfx(x,fctno,rngchk)
      ELSE COCS.Mark(111); x.typ := COCT.settyp  
      END  
    | 20: (*LEN*)  
      IF (f = DynArr) OR (f = Array) THEN 
	IF x.mode >= Con THEN COCS.Mark(127) END;
	COCX.StPar1Sfx(x,fctno,rngchk)
      ELSE COCS.Mark(131) 
      END  
    | 21: (*ASH*)  
      IF f IN intSet THEN 
	IF x.mode > Reg THEN COCS.Mark(126) END; 
	COCX.StPar1Sfx(x,fctno,rngchk); x.typ := COCT.linttyp
      ELSE COCS.Mark(111)
      END  
    | 22, 23: (*LSH ROT*)  
      IF f IN {SInt, Int, LInt, Set} THEN IF x.mode > Reg THEN COCS.Mark(126) END; COCX.StPar1Sfx(x,fctno,rngchk) 
      ELSE COCS.Mark(111) 
      END  
    | 24,25,26: (*GET, PUT, BIT*)  
      IF f IN intSet THEN COCX.StPar1Sfx(x,fctno,rngchk);
	IF x.mode = Con THEN x.typ := COCT.linttyp
	ELSIF f = LInt THEN IF x.mode > Reg THEN COCS.Mark(126) END
	ELSE COCS.Mark(111)
	END
      ELSE COCS.Mark(111)  
      END  
    | 27: (*VAL*)  
      IF x.mode = Typ THEN 
	IF f <= ProcTyp THEN COCX.StPar1Sfx(x,fctno,rngchk)
	ELSE COCS.Mark(109)
	END
      ELSE COCS.Mark(110) 
      END  
    | 28: (*SYSTEM.NEW*)  
      IF x.mode >= Con THEN COCS.Mark(112)
      ELSIF f = Pointer THEN  COCX.StPar1Sfx(x,fctno,rngchk)
      ELSE COCS.Mark(111)  
      END  
    | 29: (*COPY*)  
      IF (((f=Array) OR (f=DynArr)) & (x.typ.BaseTyp.form = Char)) 
	 OR (f = String) THEN IF x.mode > Reg THEN COCS.Mark(126) END; COCX.StPar1Sfx(x,fctno,rngchk)
      ELSE COCS.Mark(111)  
      END  
    | 30: (*MOVE*)  
      IF f = LInt THEN IF x.mode > Reg THEN COCS.Mark(126) END; COCX.StPar1Sfx(x,fctno,rngchk)
      ELSE COCS.Mark(111)  
      END  
    END  
  END StPar1;  
    
  PROCEDURE StPar2*(VAR p, x: COCT.Item; fctno: INTEGER);
    VAR f, L: INTEGER; y: COCT.Item; typ: COCT.Struct;  
  BEGIN f := x.typ.form;  
    IF fctno < 16 THEN COCS.Mark(64); RETURN END;  
    CASE fctno OF 16, 17: (*INC DEC*)  
      IF x.typ # p.typ THEN  
	IF (x.mode = Con) & (x.typ.form IN intSet) THEN 
	  COCX.StPar2Sfx(p,fctno,rngchk); x.typ := p.typ  
	ELSE COCS.Mark(111)  
	END  
      ELSE IF x.mode > Reg THEN COCS.Mark(126) END
      END;
      p.typ := COCT.notyp  
    | 18, 19: (*INCL EXCL*) 
      IF f IN intSet THEN
	IF x.mode = Con THEN
	  IF (x.intval < 0) OR (MaxSet < x.intval) THEN COCS.Mark(202) END
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END
	END;
	COCX.StPar2Sfx(p,fctno,rngchk)
      ELSE COCS.Mark(111)
      END;
      p.typ := COCT.notyp 
    | 20: (*LEN*)  
      p.mode := Reg;
      IF (x.mode = Con) & (f = SInt) THEN  
	L := SHORT(x.intval); typ := p.typ;  
	WHILE (L > 0) & (typ.form IN {DynArr, Array}) DO typ := typ.BaseTyp; DEC(L) END;  
	IF (L # 0) OR ~(typ.form IN {DynArr, Array}) THEN COCS.Mark(132)
	ELSE COCX.StPar2Sfx(p,fctno,rngchk); 
	  IF typ.form = DynArr THEN p.typ := COCT.linttyp
	  ELSE p.mode := Con; p.intval := typ.n; SetIntType(p)  
	  END  
	END  
      ELSE COCS.Mark(111)  
      END  
    | 21, 22, 23: (*ASH LSH ROT*)  
      IF f IN intSet THEN COCX.StPar2Sfx(p,fctno,rngchk);
	IF (p.mode = Con) & (x.mode = Con) THEN
	  CASE fctno OF 21: p.intval := ASH(p.intval,x.intval)
	  | 22: p.intval := SYSTEM.LSH(p.intval,x.intval)
	  | 23: 
	    CASE p.typ.form OF SInt: p.intval := SYSTEM.ROT(SHORT(SHORT(p.intval)),x.intval)
	    | Int: p.intval := SYSTEM.ROT(SHORT(p.intval), x.intval)
	    | LInt,Set: p.intval := SYSTEM.ROT(p.intval, x.intval)
	    END
	  END
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END; p.mode := Reg
	END
      ELSE COCS.Mark(111); p.mode := Reg
      END
    | 24,25: (*GET PUT*)  
      IF x.mode >= Con THEN COCS.Mark(127)  
      ELSIF f IN {Array, DynArr, Record} THEN COCS.Mark(111)
      ELSE COCX.StPar2Sfx(p,fctno,rngchk)
      END;  
      p.typ := COCT.notyp  
    | 26: (*BIT*)  
      IF f IN intSet THEN COCX.StPar2Sfx(p,fctno,rngchk);
	IF x.mode = Con THEN
	  IF (x.intval < 0) OR (7 < x.intval) THEN COCS.Mark(203) END 
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END
	END
      ELSE COCS.Mark(111)
      END;  
      p.mode := Reg; p.typ := COCT.booltyp
    | 27: (*VAL*)  
      COCX.StPar2Sfx(p,fctno,rngchk); 
      IF x.mode > Reg THEN COCS.Mark(126) END; x.typ := p.typ; x.qoffs := p.qoffs; p := x 
    | 28: (*SYSTEM.NEW*)  
      IF f IN intSet THEN COCX.StPar2Sfx(p,fctno,rngchk);
	IF x.mode = Con THEN
	ELSE IF x.mode > Reg THEN COCS.Mark(126) END
	END
      ELSE COCS.Mark(111)  
      END ;  
      p.typ := COCT.notyp  
    | 29: (*COPY*)  
      IF ((f = Array) OR (f = DynArr)) & (x.typ.BaseTyp.form = Char) THEN  
	IF x.mode >= Con THEN COCS.Mark(112) END; 
	COCX.StPar2Sfx(p,fctno,rngchk)
      ELSE COCS.Mark(111)  
      END;  
      p.typ := COCT.notyp  
    | 30: (*MOVE*)  
      IF f = LInt THEN IF x.mode > Reg THEN COCS.Mark(126) END; COCX.StPar2Sfx(p,fctno,rngchk)
      ELSE COCS.Mark(111)  
      END  
    END  
  END StPar2;  
    
  PROCEDURE StPar3*(VAR p, x: COCT.Item; fctno: INTEGER);
    VAR f: INTEGER; 
  BEGIN f := x.typ.form;  
    IF fctno = 30 THEN (*MOVE*)  
      IF f IN intSet THEN IF x.mode > Reg THEN COCS.Mark(126) END; 
      COCX.StPar3Sfx(p,fctno,rngchk)
      ELSE COCS.Mark(111)  
      END;  
      p.typ := COCT.notyp  
    ELSE COCS.Mark(64)  
    END  
  END StPar3;  
    
  PROCEDURE StFct*(VAR p: COCT.Item; fctno, parno: INTEGER);
    VAR np: INTEGER;
  BEGIN   
    IF fctno >= 16 THEN  
      IF ((fctno = 16) OR (fctno = 17)) & (parno = 1) THEN (*INC DEC*)
	COCX.StFakeSfx(fctno,rngchk); p.typ := COCT.notyp
      ELSIF (fctno = 20) & (parno = 1) THEN (*LEN*)
	COCX.StFakeSfx(fctno,rngchk);
	IF p.typ.form = DynArr THEN p.mode := Reg; p.typ := COCT.linttyp
	ELSE p.mode := Con; p.intval := p.typ.n; SetIntType(p)  
	END  
      ELSIF (parno < 2) OR (fctno = 30) & (parno < 3) THEN COCS.Mark(65)  
      END  
    ELSIF parno < 1 THEN COCS.Mark(65)  
    END;
    COCX.StFctSfx(fctno, rngchk)
  END StFct;  
    
BEGIN inxchk := TRUE; rngchk := TRUE; nilchk := TRUE;

 (* SYSTEM Dependant *)
  MinChar := 0H; MaxChar := 0FFH;
  MinBool := 0; MaxBool := 1;
  MinSInt := -128; MaxSInt := 127;
  MinInt := -32768; MaxInt := 32767;
  MinLInt := 80000000H; MaxLInt := 7FFFFFFFH;
  MinReal := -3.402823E+38; MaxReal := 3.402823E+38;
  MinLReal := -1.79769313486231D+308; MaxLReal := 1.79769313486231D+308;
  MinSet := 0; MaxSet := 31  

END COCE.  
