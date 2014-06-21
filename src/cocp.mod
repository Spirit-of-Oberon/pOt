MODULE COCP; (* DT 23 10 1993 00:23 *)
  IMPORT Strings, COCT, COCQ, COCN;

  CONST
   (*object and item modes*)
    Var = 1; Ind = 3; Con = 8; Fld = 12; Typ = 13;

   (*structure forms*)    
    Undef = 0; Byte = 1; Bool = 2; Char = 3; SInt = 4; Int = 5; LInt = 6;    
    Real = 7; LReal = 8; Set = 9; String = 10; NilTyp = 11; NoTyp = 12;    
    Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;    

  PROCEDURE AssignPfx*(VAR x: COCT.Item; typchk: BOOLEAN);
    VAR np: INTEGER;
  BEGIN
    CASE x.typ.form OF
      Undef:
    | Byte .. SInt: COCQ.Append("=")
    | Int .. LReal, Pointer:
      COCQ.Append("=(");
      COCN.CTDenoter(x.typ, COCQ.cslen, np);
      COCQ.Append(")(")
    | Set, ProcTyp: COCQ.Append("=")
    | String .. NoTyp:
    | Array: 
      COCQ.Prepend("pOt__arr_assign((pOt__ArrTypDsc**)&", x.qoffs, np);
      COCQ.Append(",(pOt__ArrTypDsc**)&");
    | DynArr:
    | Record:
      IF typchk & ((x.obj = COCT.wasderef) OR (x.obj # NIL) & (x.obj.mode = Ind)) THEN
        COCQ.Prepend("pOt__varrec_assign(__FILE__,__LINE__,(pOt__RecTypDsc**)&", x.qoffs, np);
        COCQ.Append(",(pOt__RecTypDsc**)&")
      ELSE
        COCQ.Prepend("pOt__rec_assign((pOt__RecTypDsc**)&", x.qoffs, np);
        COCQ.Append(",(pOt__RecTypDsc**)&")
      END
    END
  END AssignPfx;  

  PROCEDURE AssignSfx*(VAR x, y: COCT.Item);
  BEGIN
    CASE x.typ.form OF
      Undef:
    | Byte .. SInt:
    | Int .. LReal, Pointer: COCQ.Append(")")
    | Set, ProcTyp:
    | String .. NoTyp:
    | Array: COCQ.Append(")")
    | DynArr:
    | Record: COCQ.Append(")")
    END
  END AssignSfx;

  PROCEDURE ParamPfx*(f: COCT.Object);
    VAR np: INTEGER;
  BEGIN
    IF f.mode = Ind THEN
      CASE f.typ.form OF Undef:
      | Byte: COCQ.Append("(pOt_BYTE_SYSTEM*)&")
      | Bool .. Array: COCQ.Append("&");
      | DynArr: 
        IF f.typ.BaseTyp.form = Byte THEN COCQ.Append("pOt__make_byte_arr((void*)&")
        ELSE COCQ.Append("(pOt__ArrTypDsc**)&")
        END;
      | Record: 
        COCQ.Append("("); 
        COCN.CTDenoter(f.typ, COCQ.cslen, np); 
        COCQ.Append("*)&")
      END
    ELSE
      CASE f.typ.form OF 
        Undef .. SInt:
      | Int .. LReal, Pointer:  
        COCQ.Append("("); 
        COCN.CTDenoter(f.typ, COCQ.cslen, np);
        COCQ.Append(")(")
      | Set, ProcTyp:
      | String .. NoTyp:
      | Array: 
        COCQ.Append("("); 
        COCN.CTDenoter(f.typ, COCQ.cslen, np);
        COCQ.Append("*)");
        COCQ.Append("pOt__dup_arr(__FILE__,__LINE__,(pOt__ArrTypDsc**)&")
      | DynArr: 
        IF f.typ.BaseTyp.form = Byte THEN COCQ.Append("pOt__dup_byte_arr(__FILE__,__LINE__,(void*)&")
        ELSE COCQ.Append("pOt__dup_arr(__FILE__,__LINE__,(pOt__ArrTypDsc**)&")
        END
      | Record: 
        COCQ.Append("("); 
        COCN.CTDenoter(f.typ, COCQ.cslen, np);
        COCQ.Append("*)");
        COCQ.Append("pOt__dup_rec(__FILE__,__LINE__,(pOt__RecTypDsc**)&")
      END
    END
  END ParamPfx;

  PROCEDURE ParamSfx*(VAR ap: COCT.Item; f: COCT.Object);
    VAR np: INTEGER;
  BEGIN
    IF f.mode = Ind THEN
      CASE f.typ.form OF Undef, Byte:  
      | Bool .. Array: 
      | DynArr: 
        IF f.typ.BaseTyp.form = Byte THEN 
          COCQ.Append(",");
          CASE ap.typ.form OF Byte .. Set: 
            COCQ.Append("0,"); 
            COCN.CTSize(ap.typ, COCQ.cslen, np);
            COCQ.Append(")")
          | Array, DynArr: 
            IF (ap.typ.form = DynArr) & 
                 (ap.intval = 0) & (ap.typ.BaseTyp.form = Byte) THEN 
              COCQ.Append("0,0)")
            ELSIF ap.typ.BaseTyp.form IN {Byte .. SInt} THEN 
              COCQ.Append("1,0)")
            ELSE 
              COCQ.Append("2,0)")
            END
          | Record: COCQ.Append("3,0)")
          END
        END
      | Record:
      END
    ELSE
      CASE f.typ.form OF 
        Undef .. SInt:
      | Int .. LReal, Pointer: COCQ.Append(")")
      | Set, ProcTyp:
      | String .. NoTyp:
      | Array: COCQ.Append(")")
      | DynArr: 
        IF f.typ.BaseTyp.form = Byte THEN 
          IF (ap.typ.form = DynArr) & (ap.intval = 0) THEN 
            COCQ.Append(",0)") 
          ELSE 
            COCQ.Append(",1)")
          END
        ELSE COCQ.Append(")")
        END
      | Record: 
        COCQ.Append(",(pOt__RecTypDsc*)&"); COCN.CTDName(f.typ, COCQ.cslen, np); 
        COCQ.Append(")")
      END
    END
  END ParamSfx;

  PROCEDURE ParamListPfx*;
  BEGIN COCQ.Append("(")
  END ParamListPfx;

  PROCEDURE NextParam*;
  BEGIN COCQ.Append(",")
  END NextParam;

  PROCEDURE ParamListSfx*;
  BEGIN COCQ.Append(")")
  END ParamListSfx;

  PROCEDURE Trap*(VAR x: COCT.Item);
    VAR s: ARRAY 9 OF CHAR;
  BEGIN Strings.FromLInt(x.intval, 16, s);
    COCQ.Append("pOt__halt(__FILE__,__LINE__,0x");
    COCQ.Append(s); COCQ.Append(")")
  END Trap;

END COCP.
