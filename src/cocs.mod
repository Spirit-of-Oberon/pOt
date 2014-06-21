MODULE COCS;  (*NW 7.6.87 / 20.12.90*) (* DVD 05 08 1993 01:05 *)
  
    IMPORT Files, Reals, Texts;
  
    (*symbols:
        |  0          1          2           3            4
     ---|--------------------------------------------------------
      0 |  null       *          /           DIV          MOD
      5 |  &          +          -           OR           =
     10 |  #          <          <=          >            >=
     15 |  IN         IS         ^           .            ,
     20 |  :          ..         )           ]            }
     25 |  OF         THEN       DO          TO           (
     30 |  [          {          ~           :=           number
     35 |  NIL        string     ident       ;            |
     40 |  END        ELSE       ELSIF       UNTIL        IF
     45 |  CASE       WHILE      REPEAT      LOOP         WITH      
     50 |  EXIT       RETURN     ARRAY       RECORD       POINTER
     55 |  BEGIN      CONST      TYPE        VAR          PROCEDURE
     60 |  IMPORT     MODULE     eof *)
  
    CONST KW = 43;  (*size of hash table*)
          maxDig = 32;
          maxInt = 7FFFH;
          maxShInt = 7FH;
          maxExp = 38; maxLExp = 308;
          maxStrLen = 127;
      
    (*name, numtyp, intval, realval, lrlval are implicit results of Get*)
  
    VAR
      linecol*: BOOLEAN;

      numtyp* : INTEGER; (* 1 = char, 2 = integer, 3 = real, 4 = longreal*)
      intval* : LONGINT;
      realval*: REAL;
      lrlval* : LONGREAL;
      scanerr*: BOOLEAN;
      name*   : ARRAY maxStrLen + 1 OF CHAR;

      txtpos*: RECORD
        name*: ARRAY Files.MaxPathLength + 1 OF CHAR;
        line*: LONGINT;
        col*: INTEGER
      END;
  
      ifile: Files.File;
      Input: Files.Rider;
      Diag: Texts.Writer;
  
      ch: CHAR;     (*current character*)
      lastpos: LONGINT; (*error position in source file*)

      i: INTEGER;
      keyTab  : ARRAY KW OF RECORD 
        symb, alt: INTEGER; 
        id: ARRAY 12 OF CHAR 
      END;

    PROCEDURE Mark*(n: INTEGER);
      VAR pos:  LONGINT;
    BEGIN 
      pos := Files.Pos(Input);
      IF lastpos + 8 < pos THEN
        Texts.WriteLn(Diag); 
        IF linecol THEN
          Texts.WriteString(Diag, txtpos.name); 
          Texts.WriteString(Diag, "("); Texts.WriteInt(Diag, txtpos.line, 1);
          Texts.WriteString(Diag, ", "); Texts.WriteInt(Diag, txtpos.col, 1);
          Texts.WriteString(Diag, "): ")
        ELSE
          Texts.WriteString(Diag, "  pos"); Texts.WriteInt(Diag, pos, 8)
        END;
        IF n < 0 THEN Texts.WriteString(Diag, "  warning")
        ELSE Texts.WriteString(Diag, "  err"); scanerr := TRUE
        END ;
        Texts.WriteInt(Diag, ABS(n), 4); 
        Texts.WriteString(Diag, ": ");
	CASE n OF
	| -3: Texts.WriteString(Diag,"structure is passed as byte array")
	| -2: Texts.WriteString(Diag,"pointer in WITH")
	| -1: Texts.WriteString(Diag,"assigment to imported variable")
	| 0: Texts.WriteString(Diag,"undeclared identifier")
	| 1: Texts.WriteString(Diag,"multiply defined identifier")
	| 2: Texts.WriteString(Diag,"illegal character in number")
	| 3: Texts.WriteString(Diag,"illegal character in string")
	| 4: Texts.WriteString(Diag,"identifier does not match procedure name")
	| 5: Texts.WriteString(Diag,"comment not closed")
	| 6: Texts.WriteString(Diag,"")
	| 7: Texts.WriteString(Diag,"")
	| 8: Texts.WriteString(Diag,"")
	| 9: Texts.WriteString(Diag,"'=' expected")
	| 10: Texts.WriteString(Diag,"identifier expected")
	| 11: Texts.WriteString(Diag,"")
	| 12: Texts.WriteString(Diag,"type definition starts with incorrect symbol")
	| 13: Texts.WriteString(Diag,"factor starts with incorrect symbol")
	| 14: Texts.WriteString(Diag,"statement starts with incorrect symbol")
	| 15: Texts.WriteString(Diag,"declaration followed by incorrect symbol")
	| 16: Texts.WriteString(Diag,"MODULE expected")
	| 17: Texts.WriteString(Diag,"number expected")
	| 18: Texts.WriteString(Diag,"'.' missing")
	| 19: Texts.WriteString(Diag,"',' missing")
	| 20: Texts.WriteString(Diag,"':' missing")
	| 21: Texts.WriteString(Diag,"")
	| 22: Texts.WriteString(Diag,"')' missing")
	| 23: Texts.WriteString(Diag,"']' missing")
	| 24: Texts.WriteString(Diag,"'}' missing")
	| 25: Texts.WriteString(Diag,"OF missing")
	| 26: Texts.WriteString(Diag,"THEN missing")
	| 27: Texts.WriteString(Diag,"DO missing")
	| 28: Texts.WriteString(Diag,"TO missing")
	| 29: Texts.WriteString(Diag,"'(' missing")
	| 30: Texts.WriteString(Diag,"")
	| 31: Texts.WriteString(Diag,"")
	| 32: Texts.WriteString(Diag,"")
	| 33: Texts.WriteString(Diag,"':=' missing")
	| 34: Texts.WriteString(Diag,"',' or OF expected")
	| 35: Texts.WriteString(Diag,"")
	| 36: Texts.WriteString(Diag,"")
	| 37: Texts.WriteString(Diag,"identifier expected")
	| 38: Texts.WriteString(Diag,"';' missing")
	| 39: Texts.WriteString(Diag,"")
	| 40: Texts.WriteString(Diag,"END missing")
	| 41: Texts.WriteString(Diag,"")
	| 42: Texts.WriteString(Diag,"")
	| 43: Texts.WriteString(Diag,"UNTIL missing")
	| 44: Texts.WriteString(Diag,"")
	| 45: Texts.WriteString(Diag,"EXIT not within loop statement")
	| 46: Texts.WriteString(Diag,"")
	| 47: Texts.WriteString(Diag,"illegally marked identifier")
	| 48: Texts.WriteString(Diag,"unsatisfied forward reference")
	| 49: Texts.WriteString(Diag,"recursive import not allowed")
	| 50: Texts.WriteString(Diag,"expression should be constant")
	| 51: Texts.WriteString(Diag,"constant not an integer")
	| 52: Texts.WriteString(Diag,"identifier does not denote a type")
	| 53: Texts.WriteString(Diag,"identifier does not denote a record type")
	| 54: Texts.WriteString(Diag,"result type of procedure is not a basic type")
	| 55: Texts.WriteString(Diag,"procedure call of a function")
	| 56: Texts.WriteString(Diag,"assignment to non-variable")
	| 57: Texts.WriteString(Diag,"pointer not bound to record or array type")
	| 58: Texts.WriteString(Diag,"recursive type definition")
	| 59: Texts.WriteString(Diag,"illegal open array parameter")
	| 60: Texts.WriteString(Diag,"wrong type of case label")
	| 61: Texts.WriteString(Diag,"inadmissible type of case label")
	| 62: Texts.WriteString(Diag,"case label defined more than once")
	| 63: Texts.WriteString(Diag,"index out of bounds")
	| 64: Texts.WriteString(Diag,"more actual than formal parameters")
	| 65: Texts.WriteString(Diag,"fewer actual than formal parameters")
	| 66: Texts.WriteString(Diag,"element types of actual array and formal open array differ")
	| 67: Texts.WriteString(Diag,"actual parameter corresponding to open array is not an array")
	| 68: Texts.WriteString(Diag,"")
	| 69: Texts.WriteString(Diag,"parameter must be an integer constant")
	| 70: Texts.WriteString(Diag,"")
	| 71: Texts.WriteString(Diag,"")
	| 72: Texts.WriteString(Diag,"")
	| 73: Texts.WriteString(Diag,"procedure must have level 0")
	| 74: Texts.WriteString(Diag,"")
	| 75: Texts.WriteString(Diag,"")
	| 76: Texts.WriteString(Diag,"")
	| 77: Texts.WriteString(Diag,"object is not a record")
	| 78: Texts.WriteString(Diag,"dereferenced object is not a variable")
	| 79: Texts.WriteString(Diag,"indexed object is not a variable")
	| 80: Texts.WriteString(Diag,"index expression is not an integer")
	| 81: Texts.WriteString(Diag,"index out of specified bounds")
	| 82: Texts.WriteString(Diag,"indexed variable is not an array")
	| 83: Texts.WriteString(Diag,"undefined record field")
	| 84: Texts.WriteString(Diag,"dereferenced variable is not a pointer")
	| 85: Texts.WriteString(Diag,"guard or test type is not an extension of variable type")
	| 86: Texts.WriteString(Diag,"guard or testtype is not a pointer")
	| 87: Texts.WriteString(Diag,"guarded or tested variable is neither a pointer nor a VAR-parameter record")
	| 88: Texts.WriteString(Diag,"")
	| 89: Texts.WriteString(Diag,"")
	| 90: Texts.WriteString(Diag,"")
	| 91: Texts.WriteString(Diag,"")
	| 92: Texts.WriteString(Diag,"operand of IN not an integer, or not a set")
	| 93: Texts.WriteString(Diag,"set element type is not an integer")
	| 94: Texts.WriteString(Diag,"operand of & is not of type BOOLEAN")
	| 95: Texts.WriteString(Diag,"operand of OR is not of type BOOLEAN")
	| 96: Texts.WriteString(Diag,"operand not applicable to (unary) +")
	| 97: Texts.WriteString(Diag,"operand not applicable to (unary) -")
	| 98: Texts.WriteString(Diag,"operand of ~ is not of type BOOLEAN")
	| 99: Texts.WriteString(Diag,"")
	| 100: Texts.WriteString(Diag,"incompatible operands of dyadic operator")
	| 101: Texts.WriteString(Diag,"operand type inapplicable to *")
	| 102: Texts.WriteString(Diag,"operand type inapplicable to /")
	| 103: Texts.WriteString(Diag,"operand type inapplicable to DIV")
	| 104: Texts.WriteString(Diag,"operand type inapplicable to MOD")
	| 105: Texts.WriteString(Diag,"operand type inapplicable to +")
	| 106: Texts.WriteString(Diag,"operand type inapplicable to -")
	| 107: Texts.WriteString(Diag,"operand type inapplicable to = or #")
	| 108: Texts.WriteString(Diag,"operand type inapplicable to relation")
	| 109: Texts.WriteString(Diag,"operand is not a basic type")
	| 110: Texts.WriteString(Diag,"operand is not a type")
	| 111: Texts.WriteString(Diag,"operand inapplicable to (this) function")
	| 112: Texts.WriteString(Diag,"operand is not a variable")
	| 113: Texts.WriteString(Diag,"incompatible assignment")
	| 114: Texts.WriteString(Diag,"string too long")
	| 115: Texts.WriteString(Diag,"parameter discrepancy between type (or name) of variable (or forward procedure) and this procedure")
	| 116: Texts.WriteString(Diag,"type of variable (or forward procedure) has more parameters than this procedure")
	| 117: Texts.WriteString(Diag,"type of variable (or forward procedure) has fewer parameters than this procedure")
	| 118: Texts.WriteString(Diag,"procedure result type of variable (or of forward declaration) differs from result type of this procedure")
	| 119: Texts.WriteString(Diag,"assigned procedure is not global")
	| 120: Texts.WriteString(Diag,"type of expression following IF, WHILE, or UNTIL is not BOOLEAN")
	| 121: Texts.WriteString(Diag,"called object is not a procedure (or is an interrupt procedure)")
	| 122: Texts.WriteString(Diag,"actual VAR-parameter is not a variable")
	| 123: Texts.WriteString(Diag,"type of actual parameter is not identical with that of formal VAR-parameter")
	| 124: Texts.WriteString(Diag,"type of result expression differs from that of procedure")
	| 125: Texts.WriteString(Diag,"type of case expression is neither INTEGER nor CHAR")
	| 126: Texts.WriteString(Diag,"this expression cannot be a type or a procedure")
	| 127: Texts.WriteString(Diag,"illegal use of object")
	| 128: Texts.WriteString(Diag,"")
	| 129: Texts.WriteString(Diag,"unsatisfied forward procedure")
	| 130: Texts.WriteString(Diag,"WITH clause does not specify a variable")
	| 131: Texts.WriteString(Diag,"LEN not applied to array")
	| 132: Texts.WriteString(Diag,"dimension in LEN too large or negative")
	| 133: Texts.WriteString(Diag,"procedure declaration don't match forward declaration")
	| 150: Texts.WriteString(Diag,"key inconsistency of imported module")
	| 151: Texts.WriteString(Diag,"incorrect symbol file")
	| 152: Texts.WriteString(Diag,"symbol file of imported module not found")
	| 153: Texts.WriteString(Diag,"object or symbol file not opened (disk full?)")
	| 154: Texts.WriteString(Diag,"")
	| 155: Texts.WriteString(Diag,"generation of new symbol file not allowed")
	| 156: Texts.WriteString(Diag,"generation of new h-file is not allowed")
	| 200: Texts.WriteString(Diag,"not yet implemented")
	| 201: Texts.WriteString(Diag,"lower bound of set range greater than higher bound")
	| 202: Texts.WriteString(Diag,"set element greater  than MAX(SET) or less than 0")
	| 203: Texts.WriteString(Diag,"number too large")
	| 204: Texts.WriteString(Diag,"product too large")
	| 205: Texts.WriteString(Diag,"division by zero")
	| 206: Texts.WriteString(Diag,"sum too large")
	| 207: Texts.WriteString(Diag,"difference too large")
	| 208: Texts.WriteString(Diag,"overflow in arithmetic shift")
	| 209: Texts.WriteString(Diag,"")
	| 210: Texts.WriteString(Diag,"")
	| 211: Texts.WriteString(Diag,"")
	| 212: Texts.WriteString(Diag,"")
	| 213: Texts.WriteString(Diag,"")
	| 214: Texts.WriteString(Diag,"")
	| 215: Texts.WriteString(Diag,"not enough registers: simplify expression")
	| 216: Texts.WriteString(Diag,"")
	| 217: Texts.WriteString(Diag,"parameter must be an integer constant")
	| 218: Texts.WriteString(Diag,"illegal value of parameter  (32 <= p < 256)")
	| 219: Texts.WriteString(Diag,"illegal value of parameter  (0 <= p < 16)")
	| 220: Texts.WriteString(Diag,"illegal value of parameter")
	| 221: Texts.WriteString(Diag,"imported string is not a constant")
	| 222: Texts.WriteString(Diag,"")
	| 223: Texts.WriteString(Diag,"too many record types")
	| 224: Texts.WriteString(Diag,"too many pointer types")
	| 225: Texts.WriteString(Diag,"")
	| 226: Texts.WriteString(Diag,"")
	| 227: Texts.WriteString(Diag,"too many imported modules")
	| 228: Texts.WriteString(Diag,"too many exported structures")
	| 229: Texts.WriteString(Diag,"too many nested records for import")
	| 230: Texts.WriteString(Diag,"too many constants (strings) in module")
	| 231: Texts.WriteString(Diag,"")
	| 232: Texts.WriteString(Diag,"")
	| 233: Texts.WriteString(Diag,"record extension hierarchy too high")
	| 234: Texts.WriteString(Diag,"")
	| 240: Texts.WriteString(Diag,"identifier too long")
	| 241: Texts.WriteString(Diag,"string too long")
	| 244: Texts.WriteString(Diag,"character array too long")
	ELSE
	END;
        Texts.Append(Files.StdErr, Diag.buf);
        lastpos := pos
      END
    END Mark;

    PROCEDURE Read(VAR ch: CHAR);
    BEGIN Files.Read(Input, ch);
      IF ch = 0AX THEN 
        INC(txtpos.line); txtpos.col := 0 
      ELSE 
        INC(txtpos.col) 
      END
    END Read;
  
    PROCEDURE String(VAR sym: INTEGER);
      VAR i: INTEGER;
    BEGIN i := 0;
      LOOP Read(ch);
        IF ch = 22X THEN EXIT END ;
        IF ch < " " THEN Mark(3); EXIT END ;
        IF i < maxStrLen THEN name[i] := ch; INC(i) ELSE Mark(212); i := 0 END
      END ;
      Read(ch);
      IF i = 1 THEN sym := 34; numtyp := 1; intval := ORD(name[0]) (*character*)
      ELSE sym := 36; intval := i; name[i] := 0X (*string,intval holds length*)
      END
    END String;
  
    PROCEDURE Identifier(VAR sym: INTEGER);
      VAR i, k: INTEGER;
    BEGIN i := 0; k := 0;
      REPEAT
        IF i < 31 THEN name[i] := ch; INC(i); INC(k, ORD(ch)) END ;
        Read(ch)
      UNTIL (ch < "0") OR ("9" < ch) & (CAP(ch) < "A") OR ("Z" < CAP(ch));
      name[i] := 0X;
      k := (k+i) MOD KW;  (*hash function*)
      IF (keyTab[k].symb # 0) & (keyTab[k].id = name) THEN sym := keyTab[k].symb
      ELSE k := keyTab[k].alt;
        IF (keyTab[k].symb # 0) & (keyTab[k].id = name) THEN sym := keyTab[k].symb
        ELSE sym := 37 (*ident*)
        END
      END
    END Identifier;
  
    PROCEDURE Hval(ch: CHAR): INTEGER;
      VAR d: INTEGER;
    BEGIN d := ORD(ch) - 30H; (*d >= 0*)
      IF d >= 10 THEN
        IF (d >= 17) & (d < 23) THEN DEC(d, 7) ELSE d := 0; Mark(2) END
      END ;
      RETURN d
    END Hval;
  
    PROCEDURE Number;
      VAR i, j, h, d, e, n: INTEGER;
      x, f:   REAL;
      y, g: LONGREAL;
      lastCh: CHAR; neg: BOOLEAN;
      dig:    ARRAY maxDig OF CHAR;
  
      PROCEDURE ReadScaleFactor;
      BEGIN Read(ch);
        IF ch = "-" THEN neg := TRUE; Read(ch)
        ELSE neg := FALSE;
          IF ch = "+" THEN Read(ch) END
        END ;
        IF ("0" <= ch) & (ch <= "9") THEN
          REPEAT e := e*10 + ORD(ch)-30H; Read(ch)
          UNTIL (ch < "0") OR (ch >"9")
        ELSE Mark(2)
        END
      END ReadScaleFactor;
  
    BEGIN i := 0;
      REPEAT dig[i] := ch; INC(i); Read(ch)
      UNTIL (ch < "0") OR ("9" < ch) & (CAP(ch) < "A") OR ("Z" < CAP(ch));
      lastCh := ch; j := 0;
      WHILE (j < i-1) & (dig[j] = "0") DO INC(j) END;
      IF (dig[j] = "H") OR (dig[j] = "X") THEN DEC(j) END;
      IF ch = "." THEN Read(ch);
        IF ch = "." THEN lastCh := 0X; ch := 7FX END
      END ;
      IF lastCh = "." THEN (*decimal point*)
        h := i;
        WHILE ("0" <= ch) & (ch <= "9") DO (*read fraction*)
          IF i < maxDig THEN dig[i] := ch; INC(i) END ;
          Read(ch)
        END ;
        IF ch = "D" THEN
          y := 0; g := 1; e := 0;
          WHILE j < h DO y := y*10 + (ORD(dig[j])-30H); INC(j) END ;
          WHILE j < i DO g := g/10; y := (ORD(dig[j])-30H)*g + y; INC(j) END ;
          ReadScaleFactor;
          IF neg THEN
            IF e <= maxLExp THEN y := y / Reals.TenL(e) ELSE y := 0 END
          ELSIF e > 0 THEN
            IF e <= maxLExp THEN y := Reals.TenL(e) * y ELSE y := 0; Mark(203) END
          END ;
          numtyp := 4; lrlval := y
        ELSE x := 0; f := 1; e := 0;
          WHILE j < h DO x := x*10 + (ORD(dig[j])-30H); INC(j) END ;
          WHILE j < i DO f := f/10; x := (ORD(dig[j])-30H)*f + x; INC(j) END ;
          IF ch = "E" THEN ReadScaleFactor END ;
          IF neg THEN
            IF e <= maxExp THEN x := x / Reals.Ten(e) ELSE x := 0 END
          ELSIF e > 0 THEN
            IF e <= maxExp THEN x := Reals.Ten(e) * x ELSE x := 0; Mark(203) END
          END ;
          numtyp := 3; realval := x
        END
      ELSE (*integer*)
        lastCh := dig[i-1]; intval := 0;
        IF lastCh = "H" THEN
          IF j < i THEN
            DEC(i); intval := Hval(dig[j]); INC(j);
            IF i-j <= 7 THEN
              IF (i-j = 7) & (intval >= 8) THEN DEC(intval, 16) END ;
              WHILE j < i DO intval := Hval(dig[j]) + intval * 10H; INC(j) END
            ELSE Mark(203)
            END
          END
        ELSIF lastCh = "X" THEN
          DEC(i);
          WHILE j < i DO
            intval := Hval(dig[j]) + intval*10H; INC(j);
            IF intval > 0FFH THEN Mark(203); intval := 0 END
          END
        ELSE (*decimal*)
          WHILE j < i DO
            d := ORD(dig[j]) - 30H;
            IF d < 10 THEN
              IF intval <= (MAX(LONGINT) - d) DIV 10 THEN intval := intval*10 + d
              ELSE Mark(203); intval := 0
              END
            ELSE Mark(2); intval := 0
            END ;
            INC(j)
          END
        END ;
        IF lastCh = "X" THEN numtyp := 1 ELSE numtyp := 2 END
      END
    END Number;
  
    PROCEDURE Get*(VAR sym: INTEGER);
      VAR s: INTEGER; xch: CHAR;
  
      PROCEDURE Comment;  (* do not read after end of file *)
      BEGIN Read(ch);
        LOOP
          LOOP
            WHILE ch = "(" DO Read(ch);
              IF ch = "*" THEN Comment END
            END ;
            IF ch = "*" THEN Read(ch); EXIT END ;
            IF ch = 0X THEN EXIT END ;
            Read(ch)
          END ;
          IF ch = ")" THEN Read(ch); EXIT END ;
          IF ch = 0X THEN Mark(5); EXIT END
        END
      END Comment;
  
    BEGIN
      LOOP (*ignore control characters*)
        IF ch <= " " THEN
          IF ch = 0X THEN ch := " "; EXIT
          ELSE Read(ch)
          END
        ELSIF ch > 7FX THEN Read(ch)
        ELSE EXIT
        END
      END ;
      CASE ch OF   (* " " <= ch <= 7FX *)
          " "  : s := 62; ch := 0X (*eof*)
        | "!", "$", "%", "'", "?", "@", "\", "_", "`": s :=  0; Read(ch)
        | 22X  : String(s)
        | "#"  : s := 10; Read(ch)
        | "&"  : s :=  5; Read(ch)
        | "("  : Read(ch);
                 IF ch = "*" THEN Comment; Get(s)
                   ELSE s := 29
                 END
        | ")"  : s := 22; Read(ch)
        | "*"  : s :=  1; Read(ch)
        | "+"  : s :=  6; Read(ch)
        | ","  : s := 19; Read(ch)
        | "-"  : s :=  7; Read(ch)
        | "."  : Read(ch);
                 IF ch = "." THEN Read(ch); s := 21 ELSE s := 18 END
        | "/"  : s := 2;  Read(ch)
        | "0".."9": Number; s := 34
        | ":"  : Read(ch);
                 IF ch = "=" THEN Read(ch); s := 33 ELSE s := 20 END
        | ";"  : s := 38; Read(ch)
        | "<"  : Read(ch);
                 IF ch = "=" THEN Read(ch); s := 12 ELSE s := 11 END
        | "="  : s :=  9; Read(ch)
        | ">"  : Read(ch);
                 IF ch = "=" THEN Read(ch); s := 14 ELSE s := 13 END
        | "A".."Z": Identifier(s)
        | "["  : s := 30; Read(ch)
        | "]"  : s := 23; Read(ch)
        | "^"  : s := 17; Read(ch)
        | "a".."z": Identifier(s)
        | "{"  : s := 31; Read(ch)
        | "|"  : s := 39; Read(ch)
        | "}"  : s := 24; Read(ch)
        | "~"  : s := 32; Read(ch)
        | 7FX  : s := 21; Read(ch)
      END ;
      sym := s
    END Get;
  
    PROCEDURE Open*(name: ARRAY OF CHAR);
    BEGIN
      COPY(name, txtpos.name);
      ch := " "; scanerr := FALSE; 
      lastpos := -8; 
      txtpos.line := 1; txtpos.col := 0;
      ifile := Files.Old(name);
      Files.Set(Input, ifile, 0)
    END Open;

    PROCEDURE Close*;
    BEGIN Files.Close(ifile);
      IF scanerr THEN Texts.WriteLn(Diag) END;
      Texts.Append(Files.StdErr, Diag.buf)
    END Close;
  
    PROCEDURE EnterKW(sym: INTEGER; name: ARRAY OF CHAR);
      VAR j, k: INTEGER;
    BEGIN j := 0; k := 0;
      REPEAT INC(k, ORD(name[j])); INC(j)
      UNTIL name[j] = 0X;
      k := (k+j) MOD KW;  (*hash function*)
      IF keyTab[k].symb # 0 THEN
        j := k;
        REPEAT INC(k) UNTIL keyTab[k].symb = 0;
        keyTab[j].alt := k
      END ;
      keyTab[k].symb := sym; COPY(name, keyTab[k].id)
    END EnterKW;
  
  BEGIN linecol := FALSE;
    i := KW;
    WHILE i > 0 DO
      DEC(i); keyTab[i].symb := 0; keyTab[i].alt := 0
    END ;
    keyTab[0].id := "";
    EnterKW(27, "DO");
    EnterKW(44, "IF");
    EnterKW(15, "IN");
    EnterKW(16, "IS");
    EnterKW(25, "OF");
    EnterKW( 8, "OR");
    EnterKW(40, "END");
    EnterKW( 4, "MOD");
    EnterKW(35, "NIL");
    EnterKW(58, "VAR");
    EnterKW(45, "CASE");
    EnterKW(41, "ELSE");
    EnterKW(50, "EXIT");
    EnterKW(26, "THEN");
    EnterKW(49, "WITH");
    EnterKW(52, "ARRAY");
    EnterKW(55, "BEGIN");
    EnterKW(56, "CONST");
    EnterKW(42, "ELSIF");
    EnterKW(43, "UNTIL");
    EnterKW(46, "WHILE");
    EnterKW(53, "RECORD");
    EnterKW(47, "REPEAT");
    EnterKW(51, "RETURN");
    EnterKW(59, "PROCEDURE");
    EnterKW(28, "TO");
    EnterKW( 3, "DIV");
    EnterKW(48, "LOOP");
    EnterKW(57, "TYPE");
    EnterKW(60, "IMPORT");
    EnterKW(61, "MODULE");
    EnterKW(54, "POINTER");
    Texts.OpenWriter(Diag)
  END COCS.
