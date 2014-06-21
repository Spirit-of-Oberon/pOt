MODULE COCH;    (*NW 7.6.87 / 19.7.92 *) (* DT 19 10 1993 21:07 *)

  IMPORT COCS, COCT, COCD, COCQ, COCP;  
  
  CONST  
   (*object and item modes*)  
    Var   =  1; Ind   =  3; Con   =  8; Stk   =  9; Reg   = 11; Fld   = 12;
		Typ = 13; LProc = 14; XProc = 15;  CProc = 17; IProc = 18; Mod   = 19;
  
   (*structure forms*)  
    Undef = 0; Byte = 1; Bool = 2; Char = 3; SInt = 4; Int = 5; LInt = 6;  
    Real = 7; LReal = 8; Set = 9; String = 10; NilTyp = 11; NoTyp = 12;  
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;  
  
  PROCEDURE StartLinStmt*(VAR x: COCT.Item): INTEGER;  
    VAR qoffs: INTEGER;
  BEGIN qoffs := x.qoffs; COCQ.Link(x); 
    IF x.qoffs = 0 THEN COCQ.Dummy END; 
    RETURN qoffs  
  END StartLinStmt;  

  PROCEDURE StopLinStmt*(VAR x: COCT.Item; qoffs: INTEGER);     
    VAR np: INTEGER;
  BEGIN COCQ.Unlink(x); x.qoffs := qoffs  
  END StopLinStmt;  

  PROCEDURE DynArrBnd(ftyp, atyp: COCT.Struct; varpar: BOOLEAN);  
    VAR f: INTEGER; 
  BEGIN (* ftyp.form = DynArr *)  
    IF varpar & (ftyp.BaseTyp.form = Byte) THEN  (* byte array *)
      IF atyp.form # DynArr THEN  
        IF (atyp.form # Array) OR (atyp.BaseTyp.form > SInt) THEN COCS.Mark(-1) END;  
      ELSE atyp := atyp.BaseTyp;  
        IF atyp.form # DynArr THEN  
          IF atyp.form > SInt THEN COCS.Mark(-1) END  
        ELSE COCS.Mark(-1);  
          REPEAT atyp := atyp.BaseTyp UNTIL atyp.form # DynArr
        END 
      END  
    ELSE  
      LOOP f := atyp.form;  
        IF (f # Array) & (f # DynArr) THEN COCS.Mark(66); EXIT END ;  
        ftyp := ftyp.BaseTyp; atyp := atyp.BaseTyp;  
        IF ftyp.form # DynArr THEN  
          IF ftyp # atyp THEN COCS.Mark(67) END ;  
          EXIT  
        END  
      END  
    END  
  END DynArrBnd;  
  
  PROCEDURE CompareParLists*(x, y: COCT.Object);  
    VAR xt, yt: COCT.Struct;  
  BEGIN  
    WHILE x # NIL DO  
      IF y # NIL THEN  
        xt := x.typ; yt := y.typ;  
        WHILE (xt.form = DynArr) & (yt.form = DynArr) DO  
          xt := xt.BaseTyp; yt := yt.BaseTyp  
        END ;  
        IF x.mode # y.mode THEN COCS.Mark(115)  
        ELSIF xt # yt THEN  
          IF (xt.form = ProcTyp) & (yt.form = ProcTyp) THEN  
            CompareParLists(xt.link, yt.link)  
          ELSE COCS.Mark(115)  
          END  
        END ;  
        y := y.next  
      ELSE COCS.Mark(116)  
      END ;  
      x := x.next  
    END ;  
    IF (y # NIL) & (y.mode <= Ind) & (y.intval = 1) THEN COCS.Mark(117) END  
  END CompareParLists;  
  
  PROCEDURE AssignPrefix*(VAR x: COCT.Item);
  BEGIN 
    IF x.mode >= Con THEN COCS.Mark(56)
    ELSIF (x.mode = Var) & (x.mnolev < 0) THEN COCS.Mark(-3)
    END;
    COCP.AssignPfx(x, COCT.typchk)
  END AssignPrefix;
  
  PROCEDURE Assign*(VAR x, y: COCT.Item);  
    VAR f, g, L: INTEGER;
        p, q: COCT.Struct;  
  BEGIN IF y.mode = Typ THEN COCS.Mark(126) END;
		f := x.typ.form; g := y.typ.form;  
    CASE f OF  
      Undef, String:  
    | Byte: IF ~(g IN {Undef, Byte, Char, SInt}) THEN COCS.Mark(113) END
    | Bool, Char, SInt, Set: IF g # f THEN COCS.Mark(113) END 
    | Int:  IF ~(g IN {SInt, Int}) THEN  COCS.Mark(113) END
    | LInt: IF ~(g IN {SInt .. LInt}) THEN COCS.Mark(113) END
    | Real: IF ~(g IN {SInt .. Real}) THEN COCS.Mark(113) END
    | LReal:IF ~(g IN {SInt .. LReal}) THEN COCS.Mark(113) END
    | Pointer:  
      IF (x.typ = y.typ) OR (g = NilTyp) THEN (* OK *)      
      ELSIF g = Pointer THEN        
        p := x.typ.BaseTyp; q := y.typ.BaseTyp;        
        IF (p.form = Record) & (q.form = Record) THEN        
          WHILE (q # p) & (q # NIL) DO q := q.BaseTyp END ;        
          IF q = NIL THEN COCS.Mark(113) END        
        ELSE COCS.Mark(113)        
        END        
      ELSE COCS.Mark(113)        
      END        
    | Array: 
      IF x.typ = y.typ THEN (* OK *)
      ELSIF (g = String) & (x.typ.BaseTyp = COCT.chartyp) THEN        
        L := SHORT(y.intval MOD 100H) + 1;
        IF L > x.typ.n THEN COCS.Mark(114) 
        ELSIF L + COCD.Overhead < x.typ.n THEN COCS.Mark(244)
        END
      ELSE COCS.Mark(113)        
      END        
    | DynArr: 
      IF (g = String) & (x.typ.BaseTyp.form = Char) THEN (* OK *)
      ELSIF y.mode > Ind THEN COCS.Mark(59)          
      ELSE DynArrBnd(x.typ, y.typ, FALSE)          
      END           
    | Record: 
      IF x.typ # y.typ THEN        
        IF g = Record THEN        
          q := y.typ.BaseTyp;        
          WHILE (q # NIL) & (q # x.typ) DO q := q.BaseTyp END ;        
          IF q = NIL THEN COCS.Mark(113) END        
        ELSE COCS.Mark(113)        
        END        
      END
    | ProcTyp:  
      IF (x.typ = y.typ) OR (y.typ = COCT.niltyp) THEN (* OK *)
      ELSIF (y.mode = XProc) OR (y.mode = IProc) THEN        
        (*procedure y to proc. variable x; check compatibility*)        
        IF x.typ.BaseTyp = y.typ THEN        
          CompareParLists(x.typ.link, y.obj.dsc)
        ELSE COCS.Mark(118)        
        END        
      ELSIF y.mode = LProc THEN COCS.Mark(119)        
      ELSE COCS.Mark(111)        
      END        
    | NoTyp, NilTyp: COCS.Mark(111)  
		END;
    COCP.AssignSfx(x,y)
  END Assign;  
  
  PROCEDURE PrepCall*(VAR x: COCT.Item; VAR fpar: COCT.Object): INTEGER;  
    VAR qoffs: INTEGER;
  BEGIN qoffs := x.qoffs; COCQ.Link(x); COCP.ParamListPfx;
    IF (x.mode = LProc) OR (x.mode = XProc) OR (x.mode = CProc) THEN  
      fpar := x.obj.dsc  
    ELSIF (x.typ # NIL) & (x.typ.form = ProcTyp) THEN  
      fpar := x.typ.link  
    ELSE COCS.Mark(121); fpar := NIL; x.typ := COCT.undftyp  
    END;
    RETURN qoffs
  END PrepCall;  

  PROCEDURE ParamPrefix*(f: COCT.Object);
  BEGIN COCP.ParamPfx(f)
  END ParamPrefix;
  
  PROCEDURE Param*(VAR ap: COCT.Item; f: COCT.Object);  
    
    VAR q: COCT.Struct; fp: COCT.Item;  

    PROCEDURE ValParam(VAR x,y: COCT.Item);
      VAR f, g: INTEGER; L: INTEGER; p: COCT.Struct;
    BEGIN 
			IF y.mode = Typ THEN COCS.Mark(126) END;
			f := x.typ.form; g := y.typ.form;  
      CASE f OF  
        Undef, String:  
      | Byte: IF ~(g IN {Undef, Byte, Char, SInt}) THEN COCS.Mark(113) END
      | Bool, Char, SInt, Set: IF g # f THEN COCS.Mark(113) END 
      | Int:  IF ~(g IN {SInt, Int}) THEN  COCS.Mark(113) END
      | LInt: IF ~(g IN {SInt .. LInt}) THEN COCS.Mark(113) END
      | Real: IF ~(g IN {SInt .. Real}) THEN COCS.Mark(113) END
      | LReal:IF ~(g IN {SInt .. LReal}) THEN COCS.Mark(113) END
      | Pointer:  
        IF (x.typ = y.typ) OR (g = NilTyp) THEN (* OK *)      
        ELSIF g = Pointer THEN        
          p := x.typ.BaseTyp; q := y.typ.BaseTyp;        
          IF (p.form = Record) & (q.form = Record) THEN        
            WHILE (q # p) & (q # NIL) DO q := q.BaseTyp END ;        
            IF q = NIL THEN COCS.Mark(113) END        
          ELSE COCS.Mark(113)        
          END        
        ELSE COCS.Mark(113)        
        END        
      | Array: 
        IF x.typ = y.typ THEN (* OK *)
        ELSIF (g = String) & (x.typ.BaseTyp = COCT.chartyp) THEN        
          L := SHORT(y.intval MOD 100H) + 1;
          IF L > x.typ.n THEN COCS.Mark(114) 
          ELSIF L + COCD.Overhead < x.typ.n THEN COCS.Mark(244)
          END
        ELSE COCS.Mark(113)        
        END        
      | DynArr: 
        IF (g = String) & (x.typ.BaseTyp.form = Char) THEN (* OK *)
        ELSIF y.mode > Ind THEN COCS.Mark(59)          
        ELSE DynArrBnd(x.typ, y.typ, FALSE)          
        END           
      | Record: 
        IF x.typ # y.typ THEN        
          IF g = Record THEN        
            q := y.typ.BaseTyp;        
            WHILE (q # NIL) & (q # x.typ) DO q := q.BaseTyp END ;        
            IF q = NIL THEN COCS.Mark(113) END        
          ELSE COCS.Mark(113)        
          END        
        END
      | ProcTyp:  
        IF (x.typ = y.typ) OR (y.typ = COCT.niltyp) THEN (* OK *)
        ELSIF (y.mode = XProc) OR (y.mode = IProc) THEN        
          (*procedure y to proc. variable x; check compatibility*)        
          IF x.typ.BaseTyp = y.typ THEN        
            CompareParLists(x.typ.link, y.obj.dsc)
          ELSE COCS.Mark(118)        
          END        
        ELSIF y.mode = LProc THEN COCS.Mark(119)        
        ELSE COCS.Mark(111)        
        END        
      | NoTyp, NilTyp: COCS.Mark(111)  
      END  
    END ValParam;

  BEGIN fp.mode := Stk; fp.typ := f.typ;  
    IF f.mode = Ind THEN (*VAR parameter*)  
      IF ap.mode >= Con THEN COCS.Mark(122) END ;  
      IF fp.typ.form = DynArr THEN  
        DynArrBnd(fp.typ, ap.typ, TRUE)
      ELSIF (f.typ.form = Record) & (ap.typ.form = Record) THEN  
        q := ap.typ;  
        WHILE (q # f.typ) & (q # NIL) DO q := q.BaseTyp END ;  
        IF q = NIL THEN COCS.Mark(111) END 
      ELSIF (ap.typ = f.typ) OR ((f.typ.form = Byte) & (ap.typ.form IN {Char, SInt})) THEN  
      ELSE COCS.Mark(123)  
      END  
    ELSE ValParam(fp, ap)  
    END;
    COCP.ParamSfx(ap, f)
  END Param;  

  PROCEDURE NextParam*;
  BEGIN COCP.NextParam 
  END NextParam;
  
  PROCEDURE Call*(VAR x: COCT.Item; qoffs: INTEGER);  
  BEGIN COCP.ParamListSfx; COCQ.Unlink(x); x.qoffs := qoffs;
    IF (x.mode < Con) & (x.typ # COCT.undftyp) THEN x.typ := x.typ.BaseTyp
    ELSIF ~(x.mode IN {LProc .. IProc}) THEN COCS.Mark(121)
    END;
		IF x.typ # COCT.notyp THEN x.mode := Reg END
  END Call;  

  PROCEDURE Trap*(num: SHORTINT);
    VAR x: COCT.Item; qoffs: INTEGER;
  BEGIN IF num >= 20H THEN HALT(20H) END;
    x.intval := num;
    qoffs := StartLinStmt(x); COCP.Trap(x); StopLinStmt(x, qoffs)
  END Trap;

END COCH.  
