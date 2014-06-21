MODULE COCY; (*DT 09 01 1993*)
 (* Objects' declarations *)
  IMPORT Strings, COCT, COCQ, COCN, COCJ, COCO;
  
  CONST
   (*object modes*)
    Var = 1; Ind = 3; Con = 8; Fld = 12; Typ = 13;
    LProc = 14; XProc = 15; SProc = 16; CProc = 17; IProc = 18;

   (*structure forms*)
    Undef = 0; Set = 9; String = 10; NoTyp = 12;
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;
    
   (*variable modes*)
    Defi = 0; Refe = 1; Twin = 2; Decl = 3;

   (*modifiers*)
    Extern = 0; Static = 1; Interrupt = 2; Typedef = 3;
    
  VAR
    prevtyp: COCT.Struct;
    prevmarked: BOOLEAN;

  PROCEDURE ContDecl;
  BEGIN COCO.PutSeq(","); COCO.Separate
  END ContDecl;

  PROCEDURE TermDecl;
  BEGIN COCO.PutSeq(";"); COCO.Wrap
  END TermDecl;
  
  PROCEDURE Modifier(m: INTEGER);
  BEGIN 
    CASE m OF Extern: COCO.PutSeq("extern")
    | Static: COCO.PutSeq("static")
    | Interrupt: COCO.PutSeq("pOt__interrupt")
    | Typedef: COCO.PutSeq("typedef")
    END;
    COCO.Separate
  END Modifier;
  
  PROCEDURE ^Type*(str: COCT.Struct; def: BOOLEAN);
  PROCEDURE ^ParList(par: COCT.Object; fwd: BOOLEAN);

  PROCEDURE Obj(VAR x: COCT.Item);
    VAR np: INTEGER; typ: COCT.Struct;
  BEGIN typ := x.typ;
    IF (typ.form = ProcTyp) & 
      ((typ.strobj = x.obj) OR (typ.strobj = NIL)) THEN
      COCQ.Prepend("(*", x.qoffs, np); COCQ.Append(")");
      typ := typ.BaseTyp
    END;
    IF (typ.form = Pointer) &
      ((typ.strobj = x.obj) OR (typ.strobj = NIL)) THEN 
      COCQ.Prepend("*", x.qoffs, np); typ := typ.BaseTyp
    END;
    IF x.mode <= Ind THEN
      IF typ = prevtyp THEN 
        IF (x.mnolev = 0) & (x.obj.marked # prevmarked) THEN
          TermDecl;
          IF ~x.obj.marked THEN Modifier(Static) END;
          Type(typ, FALSE);
          prevmarked := x.obj.marked
        ELSE ContDecl
        END; 
        COCQ.Release(x)
      ELSE 
        IF prevtyp # NIL THEN TermDecl END;
        CASE x.intval OF 
          Defi: 
          IF (~COCT.IsParam(x.obj)) & (x.mnolev = 0) & (~x.obj.marked) THEN 
            Modifier(Static) 
          END
        | Refe: Modifier(Static)
        | Twin:
        | Decl: Modifier(Extern)
        END;
        Type(typ, FALSE);
        COCQ.Release(x); prevtyp := typ; prevmarked := x.obj.marked
      END
    ELSIF x.mode = Fld THEN
      IF typ = prevtyp THEN ContDecl; COCQ.Release(x)
      ELSE IF prevtyp # NIL THEN TermDecl END;
        Type(typ, FALSE);
        COCQ.Release(x); prevtyp := typ
      END  
    ELSE Type(typ, FALSE); COCQ.Release(x)
    END;
    IF (x.typ.form = ProcTyp) &
      ((x.typ.strobj = x.obj) OR (x.typ.strobj = NIL))  THEN
      ParList(x.typ.link, TRUE) 
    END
  END Obj;

  PROCEDURE ObjToItem*(obj: COCT.Object; VAR x: COCT.Item);
  BEGIN x.mnolev := COCT.topScope.mnolev;
    x.mode := obj.mode; x.obj := obj; x.typ := obj.typ;
    x.intval := obj.intval; x.fltval := obj.fltval;
    IF x.mode <= Ind THEN COCT.VarMode(x) END
  END ObjToItem;

  PROCEDURE ConstObj*(c: COCT.Object; cmode: INTEGER);
    VAR np: INTEGER; x: COCT.Item;
  BEGIN ObjToItem(c, x); COCQ.Mark(x); COCN.CObjName(x, x.qoffs, np);
    IF (x.typ = COCT.stringtyp) & (x.mnolev = 0) & x.obj.marked THEN
      IF cmode = Decl THEN Modifier(Extern) END;
      COCO.PutSeq("pOt__String"); COCO.Separate; COCQ.Release(x);
      IF cmode = Defi THEN COCQ.Mark(x); COCJ.CConstValue(x, x.qoffs, np);
        COCO.PutSeq("="); COCQ.Release(x)
      END;
      TermDecl
    ELSE COCQ.Drop(x)  
    END  
  END ConstObj;
        
  PROCEDURE TypeObj*(t: COCT.Object);
    VAR np: INTEGER; x: COCT.Item;
  BEGIN ObjToItem(t, x);
    COCQ.Mark(x); COCN.CObjName(x, x.qoffs, np);
    Modifier(Typedef); Obj(x);
    TermDecl
  END TypeObj;

  PROCEDURE Struct*(typ: COCT.Struct);
  BEGIN Type(typ, TRUE); TermDecl
  END Struct;

  PROCEDURE StartVOList*;
  BEGIN prevtyp := NIL; prevmarked := FALSE
  END StartVOList;

  PROCEDURE VarObj*(v: COCT.Object; vmode: INTEGER);
    VAR np: INTEGER; x: COCT.Item; 
  BEGIN ObjToItem(v, x); x.intval := vmode;
    COCQ.Mark(x); 
    IF x.intval = Refe THEN 
      INC(COCT.level); COCN.CObjName(x, x.qoffs, np); DEC(COCT.level)
    ELSIF x.intval = Twin THEN 
      DEC(COCT.level); COCN.CObjName(x, x.qoffs, np); INC(COCT.level)
    ELSE COCN.CObjName(x, x.qoffs, np)
    END;
    IF x.mode = Ind THEN COCQ.Prepend("*", x.qoffs, np) END;
    Obj(x)
  END VarObj;

  PROCEDURE StopVOList*;
  BEGIN IF prevtyp # NIL THEN TermDecl END; prevtyp := NIL
  END StopVOList;
  
  PROCEDURE ProcObj*(p: COCT.Object; pmode: INTEGER);
    VAR np: INTEGER; x: COCT.Item;
  BEGIN ObjToItem(p, x); x.intval := pmode;
    IF x.intval # Decl THEN DEC(x.mnolev) END;
    COCQ.Mark(x);
    COCN.CObjName(x, x.qoffs, np);
    CASE x.mode OF 
      LProc: Modifier(Static)
    | XProc: Modifier(Extern)
    | CProc:
    | IProc: Modifier(Interrupt)
    END;
    Type(x.typ, FALSE); COCQ.Release(x);
    ParList(x.obj.dsc, x.intval # Defi);
    IF x.intval # Defi THEN TermDecl END
  END ProcObj;

  PROCEDURE RetObj*(proc: COCT.Object);
    VAR np: INTEGER; x: COCT.Item;
  BEGIN COCQ.Mark(x); COCN.CRetName(x.qoffs, np);
    Type(proc.typ, FALSE); COCQ.Release(x); TermDecl
  END RetObj;

  PROCEDURE BodyObj*(obj: COCT.Object; pmode: INTEGER);
    VAR np: INTEGER; x: COCT.Item;
  BEGIN IF pmode = Decl THEN COCO.PutSeq("extern ") END;
    COCO.PutSeq("void "); 
    COCQ.Mark(x); COCN.CBodyName(obj, x.qoffs, np); COCQ.Release(x);
    IF pmode = Defi THEN COCO.PutSeq("()"); COCO.Wrap
    ELSE COCO.Separate; COCO.PutSeq("pOt__ARGS((void))"); TermDecl
    END
  END BodyObj;

  PROCEDURE StrTypeDef(str: COCT.Struct);
    VAR s: ARRAY 9 OF CHAR; 
      np: INTEGER; x: COCT.Item; fld: COCT.Object;
  BEGIN 
    COCO.PutSeq("{"); COCO.Wrap; COCO.Indent; 
    CASE str.form OF 
      Array:
      CASE str.BaseTyp.form OF 
        Undef .. Set: COCO.PutSeq("pOt__ArrTypDsc")
      | Pointer, ProcTyp: COCO.PutSeq("pOt__PtrArrTypDsc")
      | String .. NoTyp:
      | Array, Record: COCO.PutSeq("pOt__StrArrTypDsc")
      | DynArr:
      END;
      COCO.Separate; COCO.PutSeq("*td"); TermDecl;
      x.mode := Fld; x.typ := str.BaseTyp;
      COCQ.Mark(x); COCQ.Append("arr[0x");
      Strings.FromLInt(str.n, 16, s); COCQ.Append(s); COCQ.Append("L]");
      StartVOList; Obj(x); StopVOList
    | Record:
      IF str.BaseTyp = NIL THEN COCO.PutSeq("pOt__RecTypDsc *td")
      ELSE Type(str.BaseTyp, FALSE); COCO.PutSeq("base")
      END; TermDecl;
      fld := str.link; 
      IF fld # NIL THEN
        StartVOList;
        WHILE fld # NIL DO 
          IF fld.name # "" THEN
            VarObj(fld, Defi); fld := fld.next 
          END
        END;
        StopVOList
      END
    END;
    COCO.Undent; COCO.PutSeq("}") 
  END StrTypeDef;

  PROCEDURE Type*(str: COCT.Struct; def: BOOLEAN);
    VAR np: INTEGER; x: COCT.Item; 
  BEGIN 
    x.mnolev := -str.mno; 
    x.mode := Typ; x.typ := str; x.obj := str.strobj;
    COCQ.Mark(x);
    IF str.form = NoTyp THEN
      COCQ.Append("void"); COCQ.Release(x)
    ELSIF str.form = DynArr THEN
      IF str.BaseTyp = COCT.bytetyp THEN COCQ.Append("pOt__BytArr")
      ELSE COCQ.Append("pOt__DynArr")
      END;
      COCQ.Release(x)
    ELSIF str.form IN {Array, Record} THEN 
      COCN.CTagName(str, x.qoffs, np); COCQ.Release(x);
      IF def THEN COCO.Separate; StrTypeDef(str) END
    ELSE
      IF x.mnolev = 0 THEN COCT.FindObj(x.obj, x.mnolev) END;
      IF str.form # Undef THEN COCN.CObjName(x, x.qoffs, np) END;
      COCQ.Release(x)
    END; 
    COCO.Separate
  END Type;
      
  PROCEDURE ParList(par: COCT.Object; fwd: BOOLEAN);
    VAR np: INTEGER; x: COCT.Item; parorg, parend: COCT.Object;
      prevtyp0: COCT.Struct;
  BEGIN 
    prevtyp0 := prevtyp;
    IF (par # NIL) & (par.mode <= Ind) & (par.intval = 1) THEN
      IF fwd THEN COCO.Separate; COCO.PutSeq("pOt__ARGS((");
        LOOP
          ObjToItem(par, x); x.intval := Defi;
          COCQ.Mark(x); IF x.mode = Ind THEN COCQ.Prepend("*", x.qoffs, np) END;
          prevtyp := NIL; Obj(x); 
          par := par.next;
          IF (par = NIL) OR (par.mode > Ind) OR (par.intval = 0) THEN EXIT END;
          ContDecl
        END;
        COCO.PutSeq("))")
      ELSE parorg := par; 
        COCO.Indent; COCO.PutPP("if pOt__ANSI_C");
        COCO.PutSeq("(");
        LOOP
          ObjToItem(par, x); x.intval := Defi;
          COCQ.Mark(x); COCN.CObjName(x, x.qoffs, np);
          IF x.mode = Ind THEN COCQ.Prepend("*", x.qoffs, np) END;
          prevtyp := NIL; Obj(x); 
          par := par.next;
          IF (par = NIL) OR (par.mode > Ind) OR (par.intval = 0) THEN 
            parend := par; 
            EXIT 
          END;
          ContDecl
        END;
        COCO.PutSeq(")");
        COCO.PutPP("else");
        COCO.PutSeq("(");
        par := parorg;
        LOOP
          ObjToItem(par, x); COCQ.Mark(x); COCN.CObjName(x, x.qoffs, np); COCQ.Release(x);
          par := par.next;
          IF par = parend THEN EXIT END;
          ContDecl
        END; COCO.PutSeq(")"); 
        COCO.Wrap; 
        par := parorg; StartVOList;
        REPEAT VarObj(par, Defi); par := par.next UNTIL par = parend;
        StopVOList;
        COCO.PutPP("endif"); COCO.Undent;
      END
    ELSE 
      IF fwd THEN COCO.Separate; COCO.PutSeq("pOt__ARGS((void))")
      ELSE COCO.PutSeq("()"); COCO.Wrap
      END
    END;
    prevtyp := prevtyp0
  END ParList;
  
  PROCEDURE GCNode*(nptr, nstr: INTEGER; firstvar: COCT.Object);
    VAR obj: COCT.Object; s: ARRAY 9 OF CHAR;
      x: COCT.Item; np: INTEGER;
      dummyGC: BOOLEAN;
    PROCEDURE Ptr(iptr: INTEGER);
    BEGIN COCO.PutSeq("pOt__gc_ptrs.vars[0x");
      Strings.FromLInt(iptr, 16, s); COCO.PutSeq(s);
      COCO.PutSeq("]=")
    END Ptr;

    PROCEDURE Str(istr: INTEGER);
    BEGIN COCO.PutSeq("pOt__gc_strs.vars[0x");
      Strings.FromLInt(istr, 16, s); COCO.PutSeq(s);
      COCO.PutSeq("]=")
    END Str;
    
  BEGIN
    IF COCT.level = 0 THEN Modifier(Static);
      COCO.PutSeq("struct {void *next, *vars[0x");
      Strings.FromLInt(nptr+1, 16, s); COCO.PutSeq(s);
      COCO.PutSeq("];} pOt__gc_ptrs = {pOt_NIL,{"); COCO.Wrap; 
      COCO.Indent;
      obj := firstvar;
      WHILE (obj # NIL) & (obj.mode <= Typ) DO
        IF (obj.mode = Var) & (obj.typ.form = Pointer) THEN
          ObjToItem(obj, x);
          IF x.mode = Var THEN COCO.PutSeq("&") END;
          COCQ.Mark(x); COCN.CObjName(x, x.qoffs, np); COCQ.Release(x);
          COCO.PutSeq(","); COCO.Wrap
        END;
        obj := obj.next
      END;
      COCO.PutSeq("pOt_NIL"); COCO.Wrap; 
      COCO.Undent; 
      COCO.PutSeq("}}"); TermDecl;
  
      Modifier(Static);
      COCO.PutSeq("struct {void *next, *vars[0x");
      Strings.FromLInt(nstr+1, 16, s); COCO.PutSeq(s);
      COCO.PutSeq("];} pOt__gc_strs = {&pOt__gc_ptrs,{"); COCO.Wrap; 
      COCO.Indent;
      obj := firstvar;
      WHILE (obj # NIL) & (obj.mode <= Typ) DO
        IF (obj.mode = Var) & 
          (obj.typ.form IN {Array .. Record}) &
          COCT.HasPtr(obj.typ) THEN
          ObjToItem(obj, x);
          IF x.mode = Var THEN COCO.PutSeq("&") END;
          COCQ.Mark(x); COCN.CObjName(x, x.qoffs, np); COCQ.Release(x);
          COCO.PutSeq(","); COCO.Wrap
        END;
        obj := obj.next
      END;
      COCO.PutSeq("pOt_NIL"); COCO.Wrap; 
      COCO.Undent; 
      COCO.PutSeq("}}"); TermDecl
    ELSE dummyGC := (nstr = 0) & (nptr = 0);
      IF dummyGC THEN 
        COCO.PutSeq("struct {void*next;} pOt__gc_ptrs"); TermDecl
      ELSE  
        COCO.PutSeq("struct {void *next, *vars[0x");
        Strings.FromLInt(nptr+1, 16, s); COCO.PutSeq(s);
        COCO.PutSeq("];} pOt__gc_ptrs"); TermDecl; 

        COCO.PutSeq("struct {void *next, *vars[0x");
        Strings.FromLInt(nstr+1, 16, s); COCO.PutSeq(s);
        COCO.PutSeq("];} pOt__gc_strs"); TermDecl
      END;
   
      COCO.PutSeq("pOt__gc_ptrs.next = pOt__gc_root"); TermDecl;

      IF ~dummyGC THEN
        COCO.PutSeq("pOt__gc_strs.next = &pOt__gc_ptrs"); TermDecl;
   
        Ptr(nptr); COCO.PutSeq("pOt_NIL"); TermDecl;
        Str(nstr); COCO.PutSeq("pOt_NIL"); TermDecl;
        
        obj := firstvar;
        WHILE (obj # NIL) & (obj.mode <= Typ) DO
          IF obj.mode = Var THEN
            ObjToItem(obj, x);
            IF obj.typ.form = Pointer THEN DEC(nptr); 
              Ptr(nptr); IF x.mode = Var THEN COCO.PutSeq("&") END;
              COCQ.Mark(x); COCN.CObjName(x, x.qoffs, np); COCQ.Release(x);
              TermDecl
            ELSIF (obj.typ.form IN {Array .. Record}) & COCT.HasPtr(obj.typ) THEN DEC(nstr);
              Str(nstr); IF x.mode = Var THEN COCO.PutSeq("&") END;
              COCQ.Mark(x); COCN.CObjName(x, x.qoffs, np); COCQ.Release(x);
              TermDecl
            END
          END;
          obj := obj.next
        END;
        COCO.PutSeq("pOt__gc_root=(struct pOt__tag_gc_node*)&pOt__gc_strs"); 
        TermDecl
      END
    END  
  END GCNode;

  PROCEDURE GCLock*;
  BEGIN COCO.PutSeq("int pOt__gc_enabled_prev"); TermDecl;
    COCO.PutSeq("pOt__gc_enabled_prev=pOt__gc_enabled"); TermDecl;
    COCO.PutSeq("pOt__gc_enabled=0"); TermDecl
  END GCLock;
    
END COCY.
