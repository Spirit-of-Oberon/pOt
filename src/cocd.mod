MODULE COCD; (* DVD 03 09 1993 21:10 *)
 (*module's data and definitions*)
  IMPORT Strings, COCS, COCT, COCQ, COCN, COCY, COCO;
  
  CONST 
    ConstLength = 16384; Overhead* = 2048;
    MaxRecs = 128; MaxExts = 16;
  
   (*object modes*)
    Fld = 12; 

   (*structure forms*)
    Undef = 0; Set = 9; String = 10; NoTyp = 12;
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;

  VAR
    PtrSize: LONGINT; (* SYSTEM Dependant *)

    constant: ARRAY ConstLength OF CHAR; 
    RecTab: ARRAY MaxRecs OF COCT.Struct;
    conx: LONGINT;
    bofrec, nofrec, recnum: INTEGER;

  PROCEDURE Init*;
		VAR iofrec: INTEGER;
  BEGIN conx := 0; bofrec := 0; nofrec := 0; recnum := 0;
		iofrec := 0; WHILE iofrec # MaxRecs DO RecTab[iofrec] := NIL; INC(iofrec) END
  END Init;

 (*descriptors and string constants*)
  PROCEDURE AllocString*(VAR s: ARRAY OF CHAR; VAR x: COCT.Item);  
    VAR start, rem: LONGINT; i: INTEGER; ch: CHAR;  

    PROCEDURE FindString;
      VAR slen, clen, i: INTEGER;
    BEGIN start := 0; slen := SHORT(Strings.Length(s));
      LOOP
        IF start = conx THEN EXIT END;
        clen := ORD(constant[start]);
        IF slen = clen THEN
          i := slen;
          LOOP
            DEC(i);
            IF i = -1 THEN EXIT END;
            IF constant[start + PtrSize + i] # s[i] THEN EXIT END
          END;
          IF i = -1 THEN EXIT END
        END;
        INC(start, PtrSize + clen + 1); rem := start MOD PtrSize;
        IF rem # 0 THEN INC(start, PtrSize - rem) END
      END
    END FindString;  
            
  BEGIN FindString; INC(x.intval, (start+1)*100H);
    IF start = conx THEN
      i := -1; INC(conx, PtrSize);
      REPEAT INC(i); ch := s[i];
        IF conx >= ConstLength THEN COCS.Mark(230); conx := 0 END ;  
        constant[conx] := ch; INC(conx)  
      UNTIL ch = 0X;
      rem := conx MOD PtrSize; 
      IF rem # 0 THEN
        INC(conx, PtrSize - rem);
        IF conx >= ConstLength THEN COCS.Mark(230); conx := 0 END
      END;
      constant[start] := CHR(i)
    END  
  END AllocString;  

  PROCEDURE InitStrings*;  
    VAR s: ARRAY 9 OF CHAR;
      i: LONGINT;
    
    PROCEDURE WriteChar(c: CHAR);
      VAR i: INTEGER;
    BEGIN
      s[0] := "'";
      IF (c = "\") OR (c = "'") OR (c = 22X) THEN
        s[1] := "\"; s[2] := c; i := 3
      ELSIF (0X <= c) & (c <= 1FX) OR (7FX <= c) & (c <= 0FFX) THEN
        s[1] := "\"; s[2] := 0X;
        COCO.PutSeq(s);
        Strings.FromLInt(ORD(c), 8, s); 
        i := 0; WHILE s[i] # 0X DO INC(i) END
      ELSE 
        s[1] := c; i := 2
      END;
      s[i] := "'"; s[i+1] := 0X;
      COCO.PutSeq(s)
    END WriteChar;

  BEGIN
    IF conx # 0 THEN  
      COCO.PutSeq("static pOt_CHAR pOt__strcon_buf[0x");
      Strings.FromLInt(conx + Overhead, 16, s); 
      COCO.PutSeq(s); COCO.PutSeq("L]={"); COCO.Wrap;
      COCO.Indent;
      i := 0;
      WHILE i # conx DO WriteChar(constant[i]); INC(i);
        COCO.PutSeq(","); IF i MOD 16 = 0 THEN COCO.Wrap END
      END;
      COCO.PutSeq("0"); COCO.Wrap;
      COCO.Undent;
      COCO.PutSeq("};"); COCO.Wrap
    END  
  END InitStrings;  
  
  PROCEDURE AllocTypDesc*(typ: COCT.Struct);     
  BEGIN typ.descr := recnum; INC(recnum); 
    IF (typ.form = Record) & (typ.n > MaxExts) THEN COCS.Mark(233)  
    ELSIF nofrec < MaxRecs THEN  
      RecTab[nofrec] := typ; INC(nofrec)
    ELSE COCS.Mark(223)  
    END  
  END AllocTypDesc;  
  
  PROCEDURE DeclTypDescs*;
    VAR iofrec: INTEGER; typ: COCT.Struct;
      np: INTEGER; x: COCT.Item;
  BEGIN iofrec := 0;
    WHILE iofrec # nofrec DO
      typ := RecTab[iofrec];
      IF typ.ref # 0 THEN
        COCY.Struct(typ);
        COCO.PutSeq("extern"); COCO.Separate;
        CASE typ.form OF 
          Array:
          CASE typ.BaseTyp.form OF 
            Undef .. Set: COCO.PutSeq("pOt__ArrTypDsc")
          | Pointer, ProcTyp: COCO.PutSeq("pOt__PtrArrTypDsc")
          | String .. NoTyp:
          | Array, Record: COCO.PutSeq("pOt__StrArrTypDsc")
          | DynArr:
          END
        | Record: COCO.PutSeq("pOt__RecTypDsc")
        END; COCO.Separate;
        COCQ.Mark(x); COCN.CTDName(typ, x.qoffs, np); COCQ.Release(x);
        COCO.PutSeq(";"); COCO.Wrap
      END;
      INC(iofrec)
    END
  END DeclTypDescs;

  PROCEDURE InitTypDescs*;  
    VAR iofrec: INTEGER; typ: COCT.Struct;
      np: INTEGER; x: COCT.Item; s: ARRAY 9 OF CHAR;
      mode, nstr, nptr, npro, ifld, nfld: INTEGER;
      fld: COCT.Object;
      iext: INTEGER;
      base: COCT.Struct; basetyps: ARRAY MaxExts OF COCT.Struct;
  BEGIN iofrec := bofrec;
    WHILE iofrec # nofrec DO
      typ := RecTab[iofrec];
      COCY.Struct(typ);
      IF COCT.level # 0 THEN COCO.PutSeq("static"); COCO.Separate END;
      CASE typ.form OF 
        Array:
        CASE typ.BaseTyp.form OF 
          Undef .. Set: COCO.PutSeq("pOt__ArrTypDsc"); mode := 1
        | Pointer: COCO.PutSeq("pOt__PtrArrTypDsc"); mode := 2
        | ProcTyp: COCO.PutSeq("pOt__PtrArrTypDsc"); mode := 3
        | String .. NoTyp:
        | Array, Record: COCO.PutSeq("pOt__StrArrTypDsc"); mode := 4
        | DynArr:
        END
      | Record: mode := 0;
        COCO.PutSeq("struct {"); COCO.Wrap; 
        COCO.Indent;
        COCO.PutSeq("pOt_INTEGER mode;"); COCO.Wrap;
        COCO.PutSeq("pOt_LONGINT size;"); COCO.Wrap;
        COCO.PutSeq("pOt_INTEGER extlev, nstr, nptr, npro;"); COCO.Wrap;
        COCO.PutSeq("pOt__RecTypDsc *base_td[pOt__MaxExts];"); COCO.Wrap;
        nstr := 0; nptr := 0; npro := 0; fld := typ.link;
        WHILE fld # NIL DO
          IF fld.name # "" THEN
            IF fld.typ.form IN {Array, Record} THEN INC(nstr)
            ELSIF fld.typ.form = Pointer THEN INC(nptr)
            ELSIF fld.typ.form = ProcTyp THEN INC(npro)
            END
          END;
          fld := fld.next
        END;
        nfld := nstr+nptr+npro;
        IF nfld # 0 THEN
          COCO.PutSeq("struct {pOt_LONGINT poffs; pOt__TypDsc *fld_td;} tab[0x");
          Strings.FromLInt(nstr+nptr+npro, 16, s); COCO.PutSeq(s); COCO.PutSeq("];"); COCO.Wrap
        END;
        COCO.Undent;
        COCO.PutSeq("}")
      END; COCO.Separate;
      COCQ.Mark(x); COCN.CTDName(typ, x.qoffs, np); COCQ.Release(x);
      COCO.PutSeq("= {"); COCO.Wrap;
      COCO.Indent;
      Strings.FromLInt(mode, 10, s); COCO.PutSeq(s); 
      COCO.PutSeq(","); COCO.Wrap;
      CASE typ.form OF
        Array:
        COCO.PutSeq("0x");
        Strings.FromLInt(typ.n, 16, s); COCO.PutSeq(s); COCO.PutSeq("L, ");
        COCQ.Link(x); COCN.CTSize(typ.BaseTyp, x.qoffs, np); COCQ.Unlink(x);
        IF mode = 4 THEN
          COCO.PutSeq(","); COCO.Wrap;
          COCO.PutSeq("(pOt__TypDsc*)&"); COCQ.Link(x); COCN.CTDName(typ.BaseTyp, x.qoffs, np); COCQ.Unlink(x)
        END;
        COCO.Wrap
      | Record:
        COCQ.Link(x); COCN.CTSize(typ, x.qoffs, np); COCQ.Unlink(x); 
        COCO.PutSeq(","); COCO.Wrap;
        COCO.PutSeq("0x"); Strings.FromLInt(typ.n, 16, s); COCO.PutSeq(s); 
        COCO.PutSeq(", ");
        COCO.PutSeq("0x"); Strings.FromLInt(nstr, 16, s); COCO.PutSeq(s); 
        COCO.PutSeq(", ");
        COCO.PutSeq("0x"); Strings.FromLInt(nptr, 16, s); COCO.PutSeq(s); 
        COCO.PutSeq(", ");
        COCO.PutSeq("0x"); Strings.FromLInt(npro, 16, s); COCO.PutSeq(s); 
        COCO.PutSeq(","); COCO.Wrap;
        COCO.PutSeq("{");
        iext := SHORT(typ.n); base := typ.BaseTyp;
        WHILE iext # 0 DO DEC(iext);
          basetyps[iext] := base;
          base := base.BaseTyp
        END;  
        IF typ.n # 0 THEN
          LOOP COCO.PutSeq("(pOt__RecTypDsc*)&"); 
            COCQ.Link(x); COCN.CTDName(basetyps[iext], x.qoffs, np); COCQ.Unlink(x);
            INC(iext); IF iext = SHORT(typ.n) THEN EXIT END;
            COCO.PutSeq(", ")
          END
        END;  
        IF iext # MaxExts THEN
          IF iext = 0 THEN COCO.PutSeq("pOt_NIL"); INC(iext) END;
          WHILE iext # MaxExts DO COCO.PutSeq(", pOt_NIL"); INC(iext) END
        END;
        COCO.PutSeq("}"); 
        IF nfld # 0 THEN COCO.PutSeq(","); COCO.Wrap; 
          COCO.PutSeq("{"); COCO.Wrap;
          COCO.Indent;
          x.mode := Fld; x.mnolev := 0;
          ifld := 0;
          IF nstr # 0 THEN
            fld := typ.link;
            LOOP
              IF (fld.name # "") & (fld.typ.form IN {Array, Record}) THEN
                COCO.PutSeq("{(pOt_LONGINT)&((");
                COCQ.Link(x); COCN.CTDenoter(typ, x.qoffs, np); COCQ.Unlink(x);
                COCO.PutSeq("*)0)->");
                x.obj := fld; x.typ := fld.typ;
                COCQ.Link(x); COCN.CObjName(x, x.qoffs, np); COCQ.Unlink(x);
                COCO.PutSeq(", (pOt__TypDsc*)&");
                COCQ.Link(x); COCN.CTDName(fld.typ, x.qoffs, np); COCQ.Unlink(x);
                COCO.PutSeq("}");
                INC(ifld);
                IF ifld # nfld THEN COCO.PutSeq(",") END
              END;  
              IF ifld = nstr THEN EXIT END;
              fld := fld.next
            END;
            COCO.Wrap
          END;
          IF nptr # 0 THEN
            INC(nptr, nstr);
            fld := typ.link;
            LOOP
              IF (fld.name # "") & (fld.typ.form = Pointer) THEN
                COCO.PutSeq("{(pOt_LONGINT)&((");
                COCQ.Link(x); COCN.CTDenoter(typ, x.qoffs, np); COCQ.Unlink(x);
                COCO.PutSeq("*)0)->");
                x.obj := fld; x.typ := fld.typ;
                COCQ.Link(x); COCN.CObjName(x, x.qoffs, np); COCQ.Unlink(x);
                COCO.PutSeq(", pOt_NIL}");
                INC(ifld);
                IF ifld # nfld THEN COCO.PutSeq(",") END
              END;  
              IF ifld = nptr THEN EXIT END;
              fld := fld.next
            END;
            COCO.Wrap
          END;
          IF npro # 0 THEN
            INC(npro, nptr);
            fld := typ.link;
            LOOP
              IF (fld.name # "") & (fld.typ.form = ProcTyp) THEN
                COCO.PutSeq("{(pOt_LONGINT)&((");
                COCQ.Link(x); COCN.CTDenoter(typ, x.qoffs, np); COCQ.Unlink(x);
                COCO.PutSeq("*)0)->");
                x.obj := fld; x.typ := fld.typ;
                COCQ.Link(x); COCN.CObjName(x, x.qoffs, np); COCQ.Unlink(x);
                COCO.PutSeq(", pOt_NIL}");
                INC(ifld);
                IF ifld # nfld THEN COCO.PutSeq(",") END
              END;  
              IF ifld = npro THEN EXIT END;
              fld := fld.next
            END;
            COCO.Wrap
          END;
          COCO.Undent;
          COCO.PutSeq("}")
        END;
        COCO.Wrap
      END;
      COCO.Undent;
      COCO.PutSeq("};"); COCO.Wrap;
      INC(iofrec)
    END;
    IF COCT.level = 0 THEN bofrec := nofrec
    ELSE nofrec := bofrec
    END
  END InitTypDescs;  

BEGIN PtrSize := 4
END COCD.  
