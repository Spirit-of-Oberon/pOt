MODULE POT;   (*NW 7.6.87 / 19.7.92*) (* DT $Date: 2003/08/17 18:13:55 $ *)
  IMPORT OS, Files, Texts, COCS, COCO, COCT, COCE, COCH, COCD, COCC; 
  
  CONST 
    NofCases = 2048; ModNameLen = 31; 
   (*symbol values*)
    times = 1; slash = 2; div = 3; mod = 4;  
    and = 5; plus = 6; minus = 7; or = 8; eql = 9;  
    neq = 10; lss = 11; leq = 12; gtr = 13; geq = 14;  
    in = 15; is = 16; arrow = 17; period = 18; comma = 19;  
    colon = 20; upto = 21; rparen = 22; rbrak = 23; rbrace = 24;  
    of = 25; then = 26; do = 27; to = 28; lparen = 29;  
    lbrak = 30; lbrace = 31; not = 32; becomes = 33; number = 34;  
    nil = 35; string = 36; ident = 37; semicolon = 38; bar = 39;  
    end = 40; else = 41; elsif = 42; until = 43; if = 44;  
    case = 45; while = 46; repeat = 47; loop = 48; with = 49;  
    exit = 50; return = 51; array = 52; record = 53; pointer = 54;  
    begin = 55; const = 56; type = 57; var = 58; procedure = 59;  
    import = 60; module = 61;  
  
   (*object and item modes*)  
    Var = 1; Ind = 3; Con = 8; Fld = 12; Typ = 13;  
    LProc = 14; XProc = 15; SProc = 16; CProc = 17; IProc = 18; Mod = 19;  
  
   (*structure forms*)
    Undef = 0; Char = 3; SInt = 4; Int = 5; LInt = 6;  
    NoTyp = 12;  
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;  

    intSet = {SInt .. LInt}; labeltyps = {Char .. LInt};

  TYPE
     LabelRange = RECORD low, high: LONGINT END;
	
  VAR MaxArrLen: LONGINT; (* SYSTEM dependant *)  
    W: Texts.Writer;  
    sym: INTEGER;  
    symchg, hchg, newSF, newHF: BOOLEAN;  
    LoopLevel, LoopNo: INTEGER;  
    TmpFName, FName: ARRAY Files.MaxPathLength + 1 OF CHAR;
    CaseTab: ARRAY NofCases OF LabelRange;  
    BofCTab: INTEGER;

  PROCEDURE^ Type(VAR typ: COCT.Struct);  
  PROCEDURE^ FormalType(VAR typ: COCT.Struct);  
  PROCEDURE^ Expression(VAR x: COCT.Item);  
  PROCEDURE^ Block(proc: COCT.Object);  

  PROCEDURE CheckSym(s: INTEGER);  
  BEGIN  
    IF sym = s THEN COCS.Get(sym) ELSE COCS.Mark(s) END  
  END CheckSym;  
  
  PROCEDURE qualident(VAR x: COCT.Item);  
    VAR mnolev: INTEGER; obj: COCT.Object;  
  BEGIN (*sym = ident*)  
    COCT.Find(obj, mnolev); COCS.Get(sym);  
    IF (sym = period) & (obj # NIL) & (obj.mode = Mod) THEN  
      COCS.Get(sym); mnolev := -obj.mnolev;  
      IF sym = ident THEN  
	COCT.FindImport(obj, obj); COCS.Get(sym)  
      ELSE COCS.Mark(10); obj := NIL  
      END  
    END;  
    x.mnolev := mnolev; x.obj := obj;  
    IF obj # NIL THEN  
      x.mode := obj.mode; x.typ := obj.typ;   
      x.intval := obj.intval; x.fltval := obj.fltval;
      IF x.mode <= Ind THEN COCT.VarMode(x) END
    ELSE   
      COCS.Mark(0);   
      x.mode := Var; x.typ := COCT.undftyp; x.obj := NIL  
    END  
  END qualident;  
  
  PROCEDURE ConstExpression(VAR x: COCT.Item);  
    VAR qoffs: INTEGER;
  BEGIN 
    qoffs := COCE.StartExpr(x); 
    Expression(x); 
    COCE.StopConstExpr(x, qoffs);  
    IF x.mode # Con THEN  
      COCS.Mark(50); x.mode := Con; x.typ := COCT.inttyp; x.intval := 1  
    ELSIF (x.typ = COCT.stringtyp) & (x.obj # NIL) & (x.mnolev < 0) THEN
      COCS.Mark(221); INC(x.intval, 100H)
    END
  END ConstExpression;  

  PROCEDURE StringExpression(VAR x: COCT.Item; typ: COCT.Struct);  
    VAR qoffs: INTEGER; s: ARRAY 2 OF CHAR;  
  BEGIN 
    qoffs := COCE.StartExpr(x); 
    Expression(x);  
    IF (x.mode = Con) & (x.typ.form = Char) THEN  
      IF x.intval < 100H THEN 
	s[0] := CHR(x.intval); s[1] := 0X;
	COCD.AllocString(s, x);   
	IF x.obj # NIL THEN x.obj.intval := x.intval END  
      END;  
      x.intval := x.intval - (x.intval MOD 100H) + 1;
      x.typ := COCT.stringtyp
    END;
    COCE.StopStringExpr(x, qoffs, typ)
  END StringExpression;  
  
  PROCEDURE NewStr(VAR typ: COCT.Struct; form: SHORTINT);  
  BEGIN NEW(typ);  
    typ.form := form; typ.mno := 0; typ.ref := 0;  
    typ.BaseTyp := COCT.undftyp; typ.strobj := NIL
  END NewStr;  
  
  PROCEDURE CheckMark(VAR mk: BOOLEAN);  
  BEGIN COCS.Get(sym);  
    IF sym = times THEN  
      IF COCT.level = 0 THEN mk := TRUE ELSE mk := FALSE; COCS.Mark(47) END;  
      COCS.Get(sym)  
    ELSE mk := FALSE  
    END  
  END CheckMark;  
  
  PROCEDURE CheckUndefPointerTypes;  
    VAR obj: COCT.Object;  
  BEGIN obj := COCT.topScope.next;  
    WHILE obj # NIL DO  
      IF obj.mode = Undef THEN COCS.Mark(48) END;  
      obj := obj.next  
    END  
  END CheckUndefPointerTypes;  
  
  PROCEDURE RecordType(VAR typ: COCT.Struct);  
    VAR fld, fld0, fld1: COCT.Object;  
      ftyp: COCT.Struct;  
      base: COCT.Item;  
      name: ARRAY 1 OF CHAR;
  BEGIN NewStr(typ, Record); typ.BaseTyp := NIL; typ.n := 0;  
    IF sym = lparen THEN  
      COCS.Get(sym); (*record extension*)  
      IF sym = ident THEN  
	qualident(base);  
	IF (base.mode = Typ) & (base.typ.form = Record) THEN  
	  typ.BaseTyp := base.typ; typ.n := base.typ.n + 1  
	ELSE COCS.Mark(52)  
	END  
      ELSE COCS.Mark(10)  
      END;  
      CheckSym(rparen)  
    END;  
    name := ""; COCT.OpenScope(0, name); 
    fld := NIL; fld1 := COCT.topScope;  
    LOOP  
      IF sym = ident THEN  
	LOOP  
	  IF sym = ident THEN  
	    IF typ.BaseTyp # NIL THEN  
	      COCT.FindField(typ.BaseTyp, fld0);  
	      IF fld0 # NIL THEN COCS.Mark(1) END  
	    END;  
	    COCT.Insert(COCS.name, fld); CheckMark(fld.marked); fld.mode := Fld;  
	    fld.mnolev := SHORT(typ.n)   
	  ELSE COCS.Mark(10)  
	  END;  
	  IF sym = comma THEN COCS.Get(sym)  
	  ELSIF sym = ident THEN COCS.Mark(19)  
	  ELSE EXIT  
	  END  
	END;  
	CheckSym(colon); Type(ftyp);   
	WHILE fld1.next # NIL DO  
	  fld1 := fld1.next; fld1.typ := ftyp  
	END  
      END;  
      IF sym = semicolon THEN COCS.Get(sym)  
      ELSIF sym = ident THEN COCS.Mark(38)  
      ELSE EXIT  
      END  
    END;  
    typ.link := COCT.topScope.next;  
    CheckUndefPointerTypes; COCT.CloseScope;  
    COCD.AllocTypDesc(typ)  
  END RecordType;  
  
  PROCEDURE ArrayType(VAR typ: COCT.Struct);  
    VAR x: COCT.Item; f, n: INTEGER;  
  BEGIN NewStr(typ, Array); ConstExpression(x); f := x.typ.form;  
    IF f IN intSet THEN  
      IF (0 < x.intval) & (x.intval <= MaxArrLen) THEN n := SHORT(x.intval)  
      ELSE n := 1; COCS.Mark(63)  
      END  
    ELSE COCS.Mark(51); n := 1  
    END;  
    typ.n := n;   
    IF sym = of THEN  
      COCS.Get(sym); Type(typ.BaseTyp)  
    ELSIF sym = comma THEN  
      COCS.Get(sym); ArrayType(typ.BaseTyp)  
    ELSE COCS.Mark(34)  
    END;  
    COCD.AllocTypDesc(typ)  
  END ArrayType;  
  
  PROCEDURE FormalParameters(VAR resTyp: COCT.Struct);  
    VAR mode: SHORTINT; res: COCT.Item;  
	par, par1: COCT.Object; typ: COCT.Struct;  
  BEGIN par1 := COCT.topScope;   
    IF (sym = ident) OR (sym = var) THEN  
      LOOP  
	IF sym = var THEN COCS.Get(sym); mode := Ind ELSE mode := Var END;  
	LOOP  
	  IF sym = ident THEN  
	    COCT.Insert(COCS.name, par); COCS.Get(sym); par.mode := mode  
	  ELSE COCS.Mark(10)  
	  END;  
	  IF sym = comma THEN COCS.Get(sym)  
	  ELSIF sym = ident THEN COCS.Mark(19)  
	  ELSIF sym = var THEN COCS.Mark(19); COCS.Get(sym)  
	  ELSE EXIT  
	  END  
	END;  
	CheckSym(colon); FormalType(typ);  
	WHILE par1.next # NIL DO  
	  par1 := par1.next; par1.typ := typ; par1.intval := 1 (* par mark *)  
	END;  
	IF sym = semicolon THEN COCS.Get(sym)  
	ELSIF sym = ident THEN COCS.Mark(38)  
	ELSE EXIT  
	END  
      END  
    END;  
    CheckSym(rparen);  
    IF sym = colon THEN  
      COCS.Get(sym); resTyp := COCT.undftyp;  
      IF sym = ident THEN qualident(res);  
	IF res.mode = Typ THEN  
	  IF (res.typ.form <= ProcTyp) & (res.typ.form # NoTyp) THEN resTyp := res.typ ELSE COCS.Mark(54) END  
	ELSE COCS.Mark(52)  
	END  
      ELSE COCS.Mark(10)  
      END  
    ELSE resTyp := COCT.notyp  
    END  
  END FormalParameters;  
  
  PROCEDURE ProcType(VAR typ: COCT.Struct);  
    VAR name: ARRAY 1 OF CHAR;
  BEGIN NewStr(typ, ProcTyp);   
    IF sym = lparen THEN  
      COCS.Get(sym); name := ""; 
      COCT.OpenScope(COCT.level, name);  
      FormalParameters(typ.BaseTyp); typ.link := COCT.topScope.next;  
      COCT.CloseScope
    ELSE typ.BaseTyp := COCT.notyp; typ.link := NIL  
    END  
  END ProcType;  
  
  PROCEDURE SetPtrBase(ptyp, btyp: COCT.Struct);  
  BEGIN  
    IF (btyp.form = Record) OR (btyp.form = Array) THEN  
      ptyp.BaseTyp := btyp  
    ELSE ptyp.BaseTyp := COCT.undftyp; COCS.Mark(57)  
    END  
  END SetPtrBase;  
  
  PROCEDURE Type(VAR typ: COCT.Struct);  
    VAR lev: INTEGER; obj: COCT.Object; x: COCT.Item;  
  BEGIN typ := COCT.undftyp;  
    IF sym < lparen THEN COCS.Mark(12);  
      REPEAT COCS.Get(sym) UNTIL sym >= lparen  
    END;  
    IF sym = ident THEN qualident(x);  
      IF x.mode = Typ THEN typ := x.typ;  
	IF typ = COCT.notyp THEN COCS.Mark(58) END  
      ELSE COCS.Mark(52)  
      END  
    ELSIF sym = array THEN  
      COCS.Get(sym); ArrayType(typ)  
    ELSIF sym = record THEN  
      COCS.Get(sym); RecordType(typ); CheckSym(end)  
    ELSIF sym = pointer THEN  
      COCS.Get(sym); NewStr(typ, Pointer); typ.link := NIL;   
      CheckSym(to);  
      IF sym = ident THEN COCT.Find(obj, lev);  
	IF obj = NIL THEN (*forward ref*)  
	  COCT.Insert(COCS.name, obj); typ.BaseTyp := COCT.undftyp;  
	  obj.mode := Undef; obj.typ := typ; COCS.Get(sym)  
	ELSE qualident(x);  
	  IF x.mode = Typ THEN SetPtrBase(typ, x.typ)  
	  ELSE typ.BaseTyp := COCT.undftyp; COCS.Mark(52)  
	  END  
	END  
      ELSE Type(x.typ); SetPtrBase(typ, x.typ)  
      END  
    ELSIF sym = procedure THEN  
      COCS.Get(sym); ProcType(typ)  
    ELSE COCS.Mark(12)  
    END;  
    IF (sym < semicolon) OR (else < sym) THEN COCS.Mark(15);  
      WHILE (sym < ident) OR (else < sym) & (sym < begin) DO  
	COCS.Get(sym)  
      END  
    END  
  END Type;  
  
  PROCEDURE FormalType(VAR typ: COCT.Struct);  
    VAR x: COCT.Item; typ0: COCT.Struct; n: LONGINT;  
  BEGIN typ := COCT.undftyp; n := 0;  
    WHILE sym = array DO  
      COCS.Get(sym); CheckSym(of); INC(n)  
    END;  
    IF sym = ident THEN qualident(x);  
      IF x.mode = Typ THEN typ := x.typ;  
	IF typ = COCT.notyp THEN COCS.Mark(58) END  
      ELSE COCS.Mark(52)  
      END  
    ELSIF sym = procedure THEN COCS.Get(sym); ProcType(typ)  
    ELSE COCS.Mark(10)  
    END;  
    WHILE n > 0 DO   
      NewStr(typ0, DynArr); typ0.BaseTyp := typ;   
      typ0.mno := 0; typ := typ0; DEC(n)  
    END  
  END FormalType;  
  
  PROCEDURE selector(VAR x: COCT.Item);  
    VAR fld: COCT.Object; y: COCT.Item; qoffs: INTEGER;
  BEGIN  
    qoffs := COCE.StartObj(x);  
    LOOP  
      IF sym = lbrak THEN COCS.Get(sym);  
	LOOP  
	  IF (x.typ # NIL) & (x.typ.form = Pointer) THEN COCE.DeRef(x) END;  
	  COCE.IndexPrefix(x); Expression(y); COCE.Index(x, y);  
	  IF sym = comma THEN COCS.Get(sym) ELSE EXIT END  
	END;  
	CheckSym(rbrak)  
      ELSIF sym = period THEN COCS.Get(sym);  
	IF sym = ident THEN  
	  IF x.typ # NIL THEN  
	    IF x.typ.form = Pointer THEN COCE.DeRef(x) END;  
	    IF x.typ.form = Record THEN  
	      COCT.FindField(x.typ, fld); COCE.Field(x, fld)  
	    ELSE COCS.Mark(53)  
	    END  
	  ELSE COCS.Mark(52)  
	  END;  
	  COCS.Get(sym)  
	ELSE COCS.Mark(10)  
	END  
      ELSIF sym = arrow THEN  
	COCS.Get(sym); COCE.DeRef(x)  
      ELSIF (sym = lparen) & (x.mode < Typ) & (x.typ.form # ProcTyp) THEN  
	COCS.Get(sym);  
	IF sym = ident THEN  
	  qualident(y);  
	  IF y.mode = Typ THEN COCE.TypGuard(x, y)  
	  ELSE COCS.Mark(52)  
	  END  
	ELSE COCS.Mark(10)  
	END;  
	CheckSym(rparen)  
      ELSE EXIT  
      END  
    END;  
    COCE.StopObj(x, qoffs)
  END selector;  
  
  PROCEDURE ActualParameters(fpar: COCT.Object);  
    VAR apar: COCT.Item;  qoffs: INTEGER;
  BEGIN  
    IF sym # rparen THEN  
      LOOP   
	IF ~COCT.IsParam(fpar) THEN COCS.Mark(64); Expression(apar)  
	ELSE
	  COCH.ParamPrefix(fpar);
	  IF (fpar.mode = Var) &
	       ((fpar.typ.form = Array) OR (fpar.typ.form = DynArr)) & 
		 (fpar.typ.BaseTyp.form = Char) THEN 
	    StringExpression(apar, fpar.typ)  
	  ELSE Expression(apar)  
	  END;  
	  COCH.Param(apar, fpar); 
	  fpar := fpar.next
	END;  
	IF sym = comma THEN COCS.Get(sym)  
	ELSIF (lparen <= sym) & (sym <= ident) THEN COCS.Mark(19)  
	ELSE EXIT  
	END;
	COCH.NextParam
      END  
    END;  
    IF COCT.IsParam(fpar) THEN COCS.Mark(65) END  
  END ActualParameters;  
  
  PROCEDURE StandProcCall(VAR x: COCT.Item);  
    VAR y: COCT.Item; m, n: INTEGER; qoffs: INTEGER;
  BEGIN m := SHORT(x.intval); n := 0;  
    qoffs := COCE.StartExpr(x);
    COCE.TkFct(x, m);
    IF sym = lparen THEN COCS.Get(sym);  
      IF sym # rparen THEN  
	LOOP  
	  IF n = 0 THEN 
	    IF m = 29 THEN StringExpression(x,NIL) ELSE Expression(x) END;
	    COCE.StPar1(x, m); n := 1  
	  ELSIF n = 1 THEN Expression(y); COCE.StPar2(x, y, m); n := 2  
	  ELSIF n = 2 THEN Expression(y); COCE.StPar3(x, y, m); n := 3  
	  ELSE COCS.Mark(64); Expression(y)  
	  END;  
	  IF sym = comma THEN COCS.Get(sym)  
	  ELSIF (lparen <= sym) & (sym <= ident) THEN COCS.Mark(19)  
	  ELSE EXIT  
	  END  
	END;  
	CheckSym(rparen)  
      ELSE COCS.Get(sym)  
      END 
    ELSE COCS.Mark(29)  
    END;  
    COCE.StFct(x, m, n);
    COCE.StopExpr(x,qoffs)
  END StandProcCall;  
  
  PROCEDURE Sets(VAR x: COCT.Item);  
    
    VAR y: COCT.Item; xqoffs, yqoffs: INTEGER;

    PROCEDURE Element(VAR x: COCT.Item);  
      VAR e1, e2: COCT.Item; qoffs: INTEGER;
    BEGIN qoffs := COCE.StartExpr(x); 
      Expression(e1);  
      IF sym = upto THEN 
	COCS.Get(sym); COCE.Set10(x, e1); Expression(e2); COCE.Set11(x, e1, e2)  
      ELSE COCE.Set00(x, e1)  
      END;  
      COCE.StopExpr(x,qoffs)
    END Element;  
    
  BEGIN 
    x.obj := NIL; x.typ := COCT.settyp;
    y.obj := NIL; y.typ := COCT.settyp;
    xqoffs := COCE.StartExpr(x);
    IF sym # rbrace THEN  
      Element(x);  
      LOOP  
	IF sym = comma THEN COCS.Get(sym)  
	ELSIF (lparen <= sym) & (sym <= ident) THEN COCS.Mark(19)  
	ELSE EXIT  
	END;  
	yqoffs := COCE.HookExpr(y);
	Element(y); COCE.Op(plus, x, y)  (*x := x+y*)  
      END  
    ELSE x.mode := Con; x.intval := 0  
    END;  
    COCE.StopExpr(x, xqoffs);                              
    CheckSym(rbrace)
  END Sets;  
  
  PROCEDURE Factor(VAR x: COCT.Item);  
    VAR fpar: COCT.Object; qoffs: INTEGER;
  BEGIN  
    IF sym < lparen THEN COCS.Mark(13);  
      REPEAT COCS.Get(sym) UNTIL sym >= lparen  
    END;  
    IF sym = ident THEN  
      qualident(x);   
      IF x.mode = SProc THEN StandProcCall(x)  
      ELSIF x.mode = Con THEN COCE.Const(x)
      ELSE
	selector(x);                                 
	IF sym = lparen THEN  
	  COCS.Get(sym); 
	  qoffs := COCH.PrepCall(x, fpar); ActualParameters(fpar); 
	  COCH.Call(x, qoffs);   
	  CheckSym(rparen)  
	END
      END
    ELSIF sym = number THEN  
      COCS.Get(sym); 
      x.mode := Con; x.obj := NIL;
      CASE COCS.numtyp OF  
	1: x.typ := COCT.chartyp; x.intval := COCS.intval  
      | 2: x.intval := COCS.intval; COCE.SetIntType(x)  
      | 3: x.typ := COCT.realtyp; x.fltval := COCS.realval 
      | 4: x.typ := COCT.lrltyp; x.fltval := COCS.lrlval
      END;
      COCE.Const(x)
    ELSIF sym = string THEN  
      x.mode := Con; x.obj := NIL; x.typ := COCT.stringtyp;
      x.intval := COCS.intval;
      COCD.AllocString(COCS.name, x); COCS.Get(sym);
      COCE.Const(x)
    ELSIF sym = nil THEN  
      COCS.Get(sym); 
      x.mode := Con; x.obj := NIL; x.typ := COCT.niltyp;
      x.intval := 0;
      COCE.Const(x)
    ELSIF sym = lparen THEN 
      COCS.Get(sym);
      COCE.SubExprPrefix; Expression(x); COCE.SubExprSuffix;
      CheckSym(rparen)
    ELSIF sym = lbrak THEN  
      COCS.Get(sym); COCS.Mark(29); 
      COCE.SubExprPrefix; Expression(x); COCE.SubExprSuffix;
      CheckSym(rparen)  
    ELSIF sym = lbrace THEN COCS.Get(sym); Sets(x)  
    ELSIF sym = not THEN COCS.Get(sym); 
      qoffs := COCE.StartExpr(x); 
      Factor(x); COCE.MOp(not, x);
      COCE.StopExpr(x, qoffs)
    ELSE COCS.Mark(13); COCS.Get(sym); x.typ := COCT.undftyp; x.mode := Var; x.intval := 0  
    END  
  END Factor;  
  
  PROCEDURE Term(VAR x: COCT.Item);  
    VAR y: COCT.Item; mulop: INTEGER; xqoffs, yqoffs: INTEGER;
  BEGIN xqoffs := COCE.StartExpr(x);
    Factor(x);  
    WHILE (times <= sym) & (sym <= and) DO  
      mulop := sym; COCS.Get(sym);  
      IF mulop = and THEN COCE.MOp(and, x) END;
      yqoffs := COCE.HookExpr(y);
      Factor(y); COCE.Op(mulop, x, y)
    END;
    COCE.StopExpr(x, xqoffs)
  END Term;  
  
  PROCEDURE SimpleExpression(VAR x: COCT.Item);  
    VAR y: COCT.Item; addop: INTEGER; xqoffs, yqoffs: INTEGER; 
  BEGIN xqoffs := COCE.StartExpr(x);
    IF sym = minus THEN COCS.Get(sym); Term(x); COCE.MOp(minus, x)  
    ELSIF sym = plus THEN COCS.Get(sym); Term(x); COCE.MOp(plus, x)  
    ELSE Term(x)  
    END;  
    WHILE (plus <= sym) & (sym <= or) DO  
      addop := sym; COCS.Get(sym);
      IF addop = or THEN COCE.MOp(or, x) END;
      yqoffs := COCE.HookExpr(y);
      Term(y); COCE.Op(addop, x, y)
    END;
    COCE.StopExpr(x, xqoffs)
  END SimpleExpression;  
  
  PROCEDURE Expression(VAR x: COCT.Item);  
    VAR y: COCT.Item; relation: INTEGER; xqoffs,yqoffs: INTEGER;
  BEGIN 
    xqoffs := COCE.StartExpr(x);  
    SimpleExpression(x);  
    IF (eql <= sym) & (sym <= geq) THEN  
      relation := sym; COCS.Get(sym);  
      yqoffs := COCE.HookExpr(y);
      SimpleExpression(y); COCE.Op(relation, x, y)
    ELSIF sym = in THEN  
      COCS.Get(sym); COCE.InPrefix(x); 
      yqoffs := COCE.HookExpr(y);
      SimpleExpression(y); COCE.In(x, y)
    ELSIF sym = is THEN  
      IF x.mode >= Typ THEN COCS.Mark(112) END;  
      COCS.Get(sym);  
      IF sym = ident THEN  
	qualident(y);  
	IF y.mode = Typ THEN COCE.TypTest(x, y) ELSE COCS.Mark(52) END  
      ELSE COCS.Mark(10)  
      END  
    END;  
    COCE.StopExpr(x, xqoffs)
  END Expression;  
  
  PROCEDURE ProcedureDeclaration;  
    VAR proc, proc1, par: COCT.Object;  
      L1: INTEGER;  
      mode: SHORTINT; body: BOOLEAN;  
  BEGIN proc := NIL; body := TRUE;  
    IF (sym # ident) & (COCT.level = 0) THEN  
      IF sym = times THEN mode := XProc  
      ELSIF sym = arrow THEN (*forward*) mode := XProc; body := FALSE  
      ELSIF sym = plus THEN mode := IProc  
      ELSIF sym = minus THEN mode := CProc; body := FALSE  
      ELSE mode := LProc; COCS.Mark(10)  
      END;  
      COCS.Get(sym)  
    ELSE mode := LProc  
    END;  
    IF sym = ident THEN  
      IF COCT.level = 0 THEN COCT.Find(proc1, L1) ELSE proc1 := NIL END;  
      IF (proc1 # NIL) & (proc1.mode = XProc) & (proc1.intval = 0) THEN  
	(*there exists a corresponding forward declaration*)  
	COCT.Remove(proc1); COCT.Insert(COCS.name, proc);  
	CheckMark(proc.marked); mode := XProc
      ELSE  
	IF proc1 # NIL THEN COCS.Mark(1); proc1 := NIL END;  
	COCT.Insert(COCS.name, proc); CheckMark(proc.marked); proc.intval := 0;  
	IF proc.marked & (mode = LProc) THEN mode := XProc END
      END;  
      proc.mode := mode; proc.typ := COCT.notyp; proc.dsc := NIL;  
      INC(COCT.level); COCT.OpenScope(COCT.level, proc.name);  
      IF sym = lparen THEN  
	COCS.Get(sym); FormalParameters(proc.typ); proc.dsc := COCT.topScope.next  
      END;  
      IF proc1 # NIL THEN  (*forward*)  
	COCH.CompareParLists(proc.dsc, proc1.dsc);  
	IF proc.typ # proc1.typ THEN COCS.Mark(118) END  
      END;  
      IF mode = CProc THEN  
	IF sym = number THEN proc.intval := COCS.intval; COCS.Get(sym) ELSE COCS.Mark(17) END  
      END;  
      IF body THEN CheckSym(semicolon); COCT.topScope.typ := proc.typ; 
	proc.intval := 1; par := proc.dsc;  
	Block(proc); proc.dsc := COCT.topScope.next;  (*update*)  
	IF sym = ident THEN  
	  IF COCS.name # proc.name THEN COCS.Mark(4) END;  
	  COCS.Get(sym)  
	ELSE COCS.Mark(10)  
	END  
      ELSE COCC.ForwardDeclaration(proc)
      END;  
      DEC(COCT.level); COCT.CloseScope  
    END  
  END ProcedureDeclaration;  
  
  PROCEDURE CaseLabelList(LabelForm: INTEGER; VAR n: INTEGER);  
    VAR x, y: COCT.Item; i, f: INTEGER;  
  BEGIN  
    IF ~(LabelForm IN labeltyps) THEN COCS.Mark(61) END;  
    LOOP ConstExpression(x); f := x.typ.form;  
      IF f IN intSet THEN  
	IF LabelForm < f THEN COCS.Mark(60) END (*inclusion*) 
      ELSIF f # LabelForm THEN COCS.Mark(60) (*CHAR*) 
      END;  
      IF sym = upto THEN  
	COCS.Get(sym); ConstExpression(y);  
	IF (y.typ.form # f) &
	  (~((f IN intSet) & (y.typ.form IN intSet))) THEN 
	  COCS.Mark(60) 
	END;  
	IF y.intval < x.intval THEN COCS.Mark(63); y.intval := x.intval END  
      ELSE y := x  
      END;  
      COCC.CaseLabelList(x,y);
      (*enter label range into ordered CaseTable*)  
      i := n;
      IF i < NofCases THEN  
	LOOP  
	  IF i = BofCTab THEN EXIT END;  
	  IF CaseTab[i-1].low <= y.intval THEN  
	    IF CaseTab[i-1].high >= x.intval THEN COCS.Mark(62) END;  
	    EXIT  
	  END;  
	  CaseTab[i] := CaseTab[i-1]; DEC(i)
	END;  
	CaseTab[i].low := SHORT(x.intval); CaseTab[i].high := SHORT(y.intval);  
	INC(n)  
      ELSE COCS.Mark(213)  
      END;  
      IF sym = comma THEN COCS.Get(sym)  
      ELSIF (sym = number) OR (sym = ident) THEN COCS.Mark(19)  
      ELSE EXIT  
      END  
    END  
  END CaseLabelList;  
  
  PROCEDURE StatSeq(thisloop: INTEGER);  
    VAR fpar, wobj: COCT.Object; xtyp, rtyp: COCT.Struct;  
	x, y: COCT.Item; nextloop: INTEGER;  
	qoffs: INTEGER;
  
    PROCEDURE CasePart;  
      VAR x: COCT.Item; prev, n: INTEGER;  
    BEGIN n := BofCTab; prev := BofCTab;
      COCC.CasePfx; Expression(x); COCC.CaseSfx; CheckSym(of);  
      COCC.OpenScope;
      LOOP  
	IF sym < bar THEN  
	  CaseLabelList(x.typ.form, n); 
	  BofCTab := n;
	  CheckSym(colon); StatSeq(thisloop);
	  BofCTab := prev
	END;  
	IF sym = bar THEN COCC.CaseBar; COCS.Get(sym) ELSE EXIT END  
      END;  
      COCC.CaseElse;
      IF sym = else THEN COCS.Get(sym); StatSeq(thisloop)
      ELSE COCH.Trap(16); COCC.TermStmt
      END;
      COCC.CloseScope
    END CasePart;  
  
  BEGIN  
    LOOP
      IF sym < ident THEN COCS.Mark(14);  
	REPEAT COCS.Get(sym) UNTIL sym >= ident  
      END;  
      IF sym = ident THEN  
	qualident(x);  
	IF x.mode = SProc THEN  
	  StandProcCall(x);  
	  IF x.typ # COCT.notyp THEN COCS.Mark(55) END  
	ELSE
	  qoffs := COCH.StartLinStmt(x);
	  selector(x);  
	  IF sym = eql THEN COCS.Mark(33); sym := becomes END;  
	  IF sym = becomes THEN COCS.Get(sym); 
	    COCH.AssignPrefix(x); 
	    IF ((x.typ.form = Array) OR (x.typ.form = DynArr)) & 
	      (x.typ.BaseTyp.form = Char) THEN StringExpression(y, x.typ)  
	    ELSE Expression(y)  
	    END;  
	    COCH.Assign(x, y);
	    COCH.StopLinStmt(x, qoffs)
	  ELSE COCH.StopLinStmt(x, qoffs); (* function call, no prefices *)
	    qoffs := COCH.PrepCall(x, fpar);  
	    IF sym = lparen THEN  
	      COCS.Get(sym); ActualParameters(fpar); CheckSym(rparen)  
	    ELSIF COCT.IsParam(fpar) THEN COCS.Mark(65)  
	    END;  
	    COCH.Call(x, qoffs);  
	    IF x.typ # COCT.notyp THEN COCS.Mark(55) END  
	  END  
	END;
	COCC.TermStmt
      ELSIF sym = if THEN COCS.Get(sym); 
	COCC.IfPfx; 
	Expression(x); IF x.typ # COCT.booltyp THEN COCS.Mark(120) END; 
	COCC.IfSfx;
	CheckSym(then); COCC.OpenScope; StatSeq(thisloop);
	WHILE sym = elsif DO COCS.Get(sym);
	  COCC.Else; 
	  COCC.IfPfx; 
	  Expression(x); IF x.typ # COCT.booltyp THEN COCS.Mark(120) END; 
	  COCC.IfSfx; 
	  CheckSym(then); COCC.OpenScope; StatSeq(thisloop)  
	END;  
	IF sym = else THEN COCS.Get(sym); 
	  COCC.Else; COCC.OpenScope; StatSeq(thisloop)  
	END;  
	COCC.CloseScope;
	CheckSym(end) 
      ELSIF sym = case THEN  
	COCS.Get(sym); CasePart; CheckSym(end)  
      ELSIF sym = while THEN COCS.Get(sym);
	COCC.Loop; COCC.OpenScope;
	COCC.LoopCondPfx; 
	Expression(x); IF x.typ # COCT.booltyp THEN COCS.Mark(120) END; 
	COCC.LoopCondSfx(TRUE);
	CheckSym(do); StatSeq(thisloop);
	COCC.CloseScope;
	CheckSym(end)  
      ELSIF sym = repeat THEN COCS.Get(sym);
	COCC.Loop; COCC.OpenScope;
	StatSeq(thisloop);  
	IF sym = until THEN COCS.Get(sym);
	  COCC.LoopCondPfx; 
	  Expression(x); IF x.typ # COCT.booltyp THEN COCS.Mark(120) END;
	  COCC.LoopCondSfx(FALSE)
	ELSE COCS.Mark(43)  
	END;
	COCC.CloseScope
      ELSIF sym = loop THEN COCS.Get(sym); 
	INC(LoopNo); nextloop := LoopNo; INC(LoopLevel); 
	COCC.Loop; COCC.OpenScope;
	StatSeq(nextloop); 
	COCC.CloseScope;
	COCC.LoopLabel(nextloop);
	DEC(LoopLevel);  
	CheckSym(end)  
      ELSIF sym = with THEN COCS.Get(sym); x.obj := NIL; xtyp := NIL;  
	IF sym = ident THEN  
	  qualident(x); CheckSym(colon);  
	  qoffs := COCE.StartObj(x);
	  IF sym = ident THEN qualident(y);  
	    IF y.mode = Typ THEN  
	      IF x.obj # NIL THEN  
		IF x.typ.form = Pointer THEN COCS.Mark(-2) END;  
		xtyp := x.typ; COCE.TypGuard(x, y); x.obj.typ := x.typ  
	      ELSE COCS.Mark(130)  
	      END  
	    ELSE COCS.Mark(52)  
	    END  
	  ELSE COCS.Mark(10)  
	  END;
	  COCE.StopObj(x, qoffs);
	  COCC.TermStmt
	ELSE COCS.Mark(10)  
	END;  
	CheckSym(do);
	IF x.obj # NIL THEN
	  rtyp := COCT.topScope.typ; COCT.OpenScope(COCT.level, COCT.topScope.name); 
	  COCT.topScope.typ := rtyp;
	  COCT.Insert(x.obj.name, wobj);
	  wobj.typ := x.obj.typ; wobj.mode := Ind;
	  wobj.intval := x.obj.intval;
  
	  COCC.OpenScope;
	  COCC.With(x, wobj);
	  StatSeq(thisloop); 
	  COCC.CloseScope;
  
	  COCT.CloseScope;
	ELSE StatSeq(thisloop)
	END;
	CheckSym(end);  
	IF xtyp # NIL THEN x.obj.typ := xtyp END  
      ELSIF sym = exit THEN COCS.Get(sym);
	IF LoopLevel = 0 THEN COCS.Mark(45)  
	ELSE COCC.Exit(thisloop)
	END  
      ELSIF sym = return THEN COCS.Get(sym);  
	IF COCT.level > 0 THEN  
	  IF sym < semicolon THEN  
	    x.typ := COCT.topScope.typ; x.mode := Var; x.mnolev := COCT.level;
	    qoffs := COCH.StartLinStmt(x);
	    COCC.Result(x); COCH.AssignPrefix(x); Expression(y); COCH.Assign(x, y);
	    COCH.StopLinStmt(x, qoffs); COCC.TermStmt
	  ELSIF COCT.topScope.typ # COCT.notyp THEN COCS.Mark(124)  
	  END;  
	  COCC.Return
	ELSE (*return from module body*)  
	  IF sym < semicolon THEN Expression(x); COCS.Mark(124) END;  
	  COCC.Return
	END  
      END;  
      IF sym = semicolon THEN COCS.Get(sym)  
      ELSIF (sym <= ident) OR (if <= sym) & (sym <= return) THEN COCS.Mark(38)  
      ELSE EXIT  
      END  
    END  
  END StatSeq;  
  
  PROCEDURE Block(proc: COCT.Object);  
    VAR typ, forward: COCT.Struct;  
      obj, first: COCT.Object;  
      x: COCT.Item;  
      L0: INTEGER;  
      mk: BOOLEAN;  
      id0: ARRAY 32 OF CHAR;  
      big: BOOLEAN;
  
  BEGIN obj := COCT.topScope;  
    WHILE obj.next # NIL DO obj := obj.next END;  
    LOOP  
      IF sym = const THEN  
	COCS.Get(sym);  
	WHILE sym = ident DO  
	  COPY(COCS.name, id0); CheckMark(mk);  
	  IF sym = eql THEN COCS.Get(sym); ConstExpression(x)  
	  ELSIF sym = becomes THEN COCS.Mark(9); COCS.Get(sym); ConstExpression(x)  
	  ELSE COCS.Mark(9); obj.mode := Con; obj.typ := COCT.inttyp; obj.intval := 0
	  END;  
	  COCT.Insert(id0, obj); obj.mode := x.mode;  
	  obj.typ := x.typ; obj.intval := x.intval; obj.fltval := x.fltval; obj.marked := mk;  
	  CheckSym(semicolon)  
	END  
      END;  
      IF sym = type THEN  
	COCS.Get(sym);  
	WHILE sym = ident DO  
	  typ := COCT.undftyp; COCT.Insert(COCS.name, obj); forward := obj.typ;              
	  obj.mode := Typ; obj.typ := COCT.notyp; CheckMark(obj.marked);  
	  IF sym = eql THEN COCS.Get(sym); Type(typ)  
	  ELSIF (sym = becomes) OR (sym = colon) THEN COCS.Mark(9); COCS.Get(sym); Type(typ)  
	  ELSE COCS.Mark(9)  
	  END;  
	  obj.typ := typ;  
	  IF typ.strobj = NIL THEN typ.strobj := obj END;  
	  IF forward # NIL THEN (*fixup*) SetPtrBase(forward, typ) END;  
	  CheckSym(semicolon)  
	END  
      END;  
      IF sym = var THEN  
	COCS.Get(sym);  
	WHILE sym = ident DO  
	  COCT.Insert(COCS.name, obj); first := obj; CheckMark(obj.marked); obj.mode := Var;  
	  LOOP  
	    IF sym = comma THEN COCS.Get(sym)  
	    ELSIF sym = ident THEN COCS.Mark(19)  
	    ELSE EXIT  
	    END;  
	    IF sym = ident THEN  
	      COCT.Insert(COCS.name, obj); CheckMark(obj.marked); obj.mode := Var  
	    ELSE COCS.Mark(10)  
	    END  
	  END;  
	  CheckSym(colon); Type(typ);   
	  WHILE first # NIL DO  
	    first.typ := typ; first.intval := 0;  (* non-parameter mark *)  
	    first := first.next  
	  END;  
	  CheckSym(semicolon)  
	END  
      END;  
      IF (sym < const) OR (sym > var) THEN EXIT END;  
    END;  
    CheckUndefPointerTypes;  
    IF COCT.level = 0 THEN COCC.ModulePrologue 
    ELSE big := sym = procedure; COCC.OuterPrologue(proc, big)
    END;  
    WHILE sym = procedure DO  
      COCS.Get(sym); ProcedureDeclaration; CheckSym(semicolon)  
    END;  
    IF COCT.level = 0 THEN COCC.BodyPrologue
    ELSE COCC.InnerPrologue(proc, big)
    END;
    IF sym = begin THEN COCS.Get(sym); 
      LoopLevel := 0; LoopNo := 0; BofCTab := 0;
      StatSeq(0) 
    END;  
    CheckSym(end);
    IF COCT.level = 0 THEN COCC.BodyEpilogue
    ELSE COCC.Epilogue(proc, big)
    END
  END Block;  
  
  PROCEDURE CompilationUnit(SrcName: ARRAY OF CHAR);  
    VAR L0: INTEGER; ch: CHAR;  
      time, date, key, tm: LONGINT;  
      modid, impid: ARRAY 32 OF CHAR;  
      linenum: BOOLEAN;
      res: INTEGER;
      
    PROCEDURE MakeFileName(VAR name, FName: ARRAY OF CHAR; ext: ARRAY OF CHAR);  
      VAR i, j: INTEGER; ch: CHAR;  
    BEGIN i := 0;  
      LOOP ch := name[i];  
	IF ch = 0X THEN EXIT END;  
	FName[i] := ch; 
	IF i # Files.MaxPathLength THEN INC(i) END 
      END;  
      j := 0;  
      REPEAT ch := ext[j]; FName[i] := ch; 
	IF i # Files.MaxPathLength THEN INC(i) END; INC(j)  
      UNTIL ch = 0X  
    END MakeFileName;  

  BEGIN tm := OS.Time();
    COCD.Init; COCT.Init; 
    OS.GC; (* The right place is here, because at the time of GC it's good to have all pointers initialized *)
    COCS.Open(SrcName); 
    COCS.Get(sym);  
    Texts.WriteString(W, "  compiling ");  
    IF sym = module THEN COCS.Get(sym) ELSE COCS.Mark(16) END;  
    IF sym = ident THEN  
      Texts.WriteString(W, COCS.name);
      Texts.Append(Files.StdOut, W.buf);
      L0 := 0; ch := COCS.name[0];  
      WHILE (ch # 0X) & (L0 # ModNameLen) DO modid[L0] := ch; INC(L0); ch := COCS.name[L0] END;  
      modid[L0] := 0X;  
      IF ch # 0X THEN COCS.Mark(228) END;  
      COCT.OpenScope(0, modid); COCS.Get(sym);  
      CheckSym(semicolon); 
      IF sym = import THEN  
	COCS.Get(sym);  
	LOOP  
	  IF sym = ident THEN  
	    COPY(COCS.name, impid); COCS.Get(sym);  
	    MakeFileName(impid, FName, ".Cym");  
	    IF sym = becomes THEN COCS.Get(sym);  
	      IF sym = ident THEN  
		MakeFileName(COCS.name, FName, ".Cym"); COCS.Get(sym)  
	      ELSE COCS.Mark(10)  
	      END  
	    END;  
	    COCT.Import(impid, modid, FName)  
	  ELSE COCS.Mark(10)  
	  END;  
	  IF sym = comma THEN COCS.Get(sym)  
	  ELSIF sym = ident THEN COCS.Mark(19)  
	  ELSE EXIT  
	  END  
	END;  
	CheckSym(semicolon)  
      END;  
      IF ~COCS.scanerr THEN  
	MakeFileName(modid, FName, ".c"); COCO.Open(FName);
	Block(NIL);
	IF sym = ident THEN  
	  IF COCS.name # modid THEN COCS.Mark(4) END;  
	  COCS.Get(sym)  
	ELSE COCS.Mark(10)  
	END;  
	IF sym # period THEN COCS.Mark(18) END;  
	IF ~COCS.scanerr THEN  
	  Texts.Write(W, " ");
	  Texts.WriteInt(W, COCO.Size(), 6);
	  Texts.Append(Files.StdOut, W.buf);
	  COCO.Close;

	  linenum := COCO.linenum; COCO.linenum := FALSE;

	  MakeFileName(modid, FName, ".hi");
	  COCO.Open(FName);
	  COCC.InitData;
	  Texts.Write(W, " ");
	  Texts.WriteInt(W, COCO.Size(), 6);
	  Texts.Append(Files.StdOut, W.buf);
	  COCO.Close;

	  MakeFileName(modid, TmpFName, ".$$$");

	  MakeFileName(modid, FName, ".Cym");  
	  newSF := symchg;
	  OS.GetClock(time, date); key := (date MOD 4000H) * 20000H + time;
	  COCT.Export(modid, TmpFName, FName, newSF, key);  
	  IF ~COCS.scanerr THEN
	    IF newSF THEN 
	      Texts.WriteString(W, " new symbol file"); 
	      Texts.Append(Files.StdOut, W.buf)
	    END;
  
	    COCO.Open(TmpFName);
	    COCC.CExport;
	    Texts.Write(W, " ");
	    Texts.WriteInt(W, COCO.Size(), 6);
	    Texts.Append(Files.StdOut, W.buf);
	    COCO.Close;
  
	    MakeFileName(modid, FName, ".h");
	    newHF := hchg;
	    COCC.CommitCExport(TmpFName, FName, newHF);
	    IF ~COCS.scanerr THEN
	      IF newHF THEN
		Texts.WriteString(W, " new h-file");
		Texts.Append(Files.StdOut, W.buf)
	      END;

	      Texts.WriteString(W, " ");
	      Texts.WriteInt(W, OS.Time() - tm, 6); Texts.WriteLn(W);
	      Texts.Append(Files.StdOut, W.buf)
	    END
	  END;
  
	  IF COCS.scanerr THEN (* remove the code if errors in exort *)
	    MakeFileName(modid, FName, ".hi"); 
	    Files.Delete(FName, res); 
	    MakeFileName(modid, FName, ".c"); 
	    Files.Delete(FName, res) 
	  END;

	  COCO.linenum := linenum
	ELSE COCO.Purge
	END  
      END;  
      COCT.CloseScope  
    ELSE COCS.Mark(10)  
    END;  
    COCS.Close; COCT.Close
  END CompilationUnit;  
  
  PROCEDURE Compile*; 
    VAR parfile: Texts.Text; srcfile: Files.File; 
      Par: Texts.Reader;
      i: INTEGER; ch: CHAR;
  BEGIN 
    OS.GetParFile(FName);
    Texts.Open(parfile,FName); Texts.OpenReader(Par, parfile, 0);
    Texts.Read(Par, ch);
    WHILE ch # 0X DO
      IF ch = "-" THEN Texts.Read(Par, ch);
	LOOP
	  CASE CAP(ch) OF
	    "F": COCO.fold := ch = "F"
	  | "H": hchg := ch = "H"
	  | "I": COCE.inxchk := ch = "I"
	  | "L": COCS.linecol := ch = "L" 
	  | "M": COCO.macwrap := ch = "M"
	  | "N": COCO.linenum := ch = "N"
	  | "O": COCE.nilchk := ch = "O"
	  | "P": COCC.tempsafe := ch = "P"
	  | "R": COCE.rngchk := ch = "R"
	  | "S": symchg := ch = "S"
	  | "T": COCT.typchk := ch = "T"
	  | "U": COCO.unxwrap := ch = "U"
	  ELSE 
	    IF ch > " " THEN
	      Texts.WriteString(W, "wrong parameter -"); 
	      Texts.Write(W, ch); Texts.WriteLn(W); 
	      Texts.Append(Files.StdOut, W.buf)
	    ELSE EXIT
	    END
	  END;
	  Texts.Read(Par, ch)
	END
      ELSIF ch > " " THEN i := 0;
	LOOP
	  IF i = Files.MaxPathLength THEN 
	    REPEAT Texts.Read(Par, ch) UNTIL ch <= " "; 
	    EXIT 
	  END;
	  FName[i] := ch; INC(i);  
	  Texts.Read(Par, ch);
	  IF ch <= " " THEN EXIT END
	END;
	FName[i] := 0X; srcfile := Files.Old(FName);
	IF srcfile # NIL THEN Files.Close(srcfile); 
	  CompilationUnit(FName)
	ELSE Texts.WriteString(W, FName); 
	  Texts.WriteString(W, " not found"); Texts.WriteLn(W); 
	  Texts.Append(Files.StdOut, W.buf)
	END
      ELSE Texts.Read(Par, ch)
      END
    END;
    Texts.Close(parfile)
  END Compile;  

BEGIN MaxArrLen := 7FFFFFFFH; (* SYSTEM Dependant *)
  symchg := FALSE; hchg := FALSE;
  Texts.OpenWriter(W);
  Texts.WriteString(W, "Portable Oberon Translator $Revision: 1.1.1.1 $ NW 19.7.92 / $Date: 2003/08/17 18:13:55 $ "); 
  Texts.WriteLn(W);
  Texts.Append(Files.StdOut, W.buf)
END POT.

(*
$Log: pot.mod,v $
Revision 1.1.1.1  2003/08/17 18:13:55  dvd


Revision 1.1.1.1  2001/12/19 02:28:26  dvd


# Revision 1.35  1995/07/28  12:10:32  dvd
# bug with RETURN inside WITH is corrected
#
# Revision 1.34  1995/06/29  07:54:36  dvd
# *** empty log message ***
#
# Revision 1.33  1995/01/27  13:46:48  dvd
# Code procedures for C functions
#
# Revision 1.32  1995/01/25  00:57:44  dvd
# Uncaught error 120 is fixed.
#
# Revision 1.31  1995/01/20  22:01:49  dvd
# <item>.obj is now initialized to NIL
#
# Revision 1.30  1994/12/05  19:37:00  dvd
# Error reporting is improved.
#
# Revision 1.21  1994/10/31  10:02:53  dvd
# Memory usage is improved by moving explicit call to GC after
# the initialization of static structures.
#
*)
