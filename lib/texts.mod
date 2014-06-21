MODULE Texts;  (** CAS 30-Sep-91 / interface based on Texts by JG / NW 6.12.91**) 
 (* Simplified subset DT 04-Feb-94 *)
  IMPORT Files, Reals;

  CONST
    TAB = 9X; NL = 0AX; maxD = 9;
    BufLen = 512;
   (*Scanner.class*)
    Inval* = 0; Name* = 1; String* = 2; Int* = 3; 
    Real* = 4; LongReal* = 5; Char* = 6;

  TYPE
    Text* = Files.File;
    
    Buffer* = POINTER TO BufDesc;

    BufDesc = RECORD
      next: Buffer;
      len: LONGINT;
      data: ARRAY BufLen OF CHAR
    END;

    Reader* = RECORD (Files.Rider)
      eot*: BOOLEAN
    END;

    Scanner* = RECORD (Reader)
      nextCh*: CHAR;
      line*, class*: INTEGER;
      i*: LONGINT;
      x*: REAL;
      y*: LONGREAL;
      c*: CHAR;
      len*: SHORTINT;
      s*: ARRAY 32 OF CHAR
    END;

    Writer* = RECORD 
      buf*: Buffer
    END;

  PROCEDURE RdString (VAR r: Files.Rider; VAR s: ARRAY OF CHAR);
    VAR i: INTEGER; ch: CHAR;
  BEGIN i := 0;
    REPEAT Files.Read(r, ch); s[i] := ch; INC(i) UNTIL (ch = 0X) OR (i = LEN(s));
    WHILE ~r.eof & (ch # 0X) DO Files.Read(r, ch) END;  (*synch if string too long*)
    s[i] := 0X
  END RdString;

  PROCEDURE RdInt (VAR r: Files.Rider; VAR n: INTEGER);
    VAR c0: CHAR; s1: SHORTINT;
  BEGIN Files.Read(r, c0); Files.Read(r, s1);
    n := LONG(s1) * 100H + ORD(c0)
  END RdInt;

  PROCEDURE RdLong (VAR r: Files.Rider; VAR n: LONGINT);
    VAR c0, c1, c2: CHAR; s3: SHORTINT;
  BEGIN Files.Read(r, c0); Files.Read(r, c1); Files.Read(r, c2); Files.Read(r, s3);
    n := ( (LONG(s3) * 100H + LONG(ORD(c2))) * 100H + ORD(c1) ) * 100H + ORD(c0)
  END RdLong;


  PROCEDURE WrtString (VAR r: Files.Rider; VAR s: ARRAY OF CHAR);
    VAR i: INTEGER;
  BEGIN i := 0;
    REPEAT INC(i) UNTIL s[i] = 0X;
    Files.WriteBytes(r, s, i + 1)
  END WrtString;

  PROCEDURE WrtInt (VAR r: Files.Rider; n: INTEGER);
  BEGIN Files.Write(r, CHR(n MOD 100H)); Files.Write(r, SHORT(n DIV 100H))
  END WrtInt;

  PROCEDURE WrtLong (VAR r: Files.Rider; n: LONGINT);
  BEGIN Files.Write(r, CHR(n MOD 100H)); Files.Write(r, CHR(n DIV 100H MOD 100H));
    Files.Write(r, CHR(n DIV 10000H MOD 100H)); Files.Write(r, SHORT(SHORT((n DIV 1000000H))) )
  END WrtLong;

 (* buffers *)
 
  PROCEDURE OpenBuf* (VAR B: Buffer);
  BEGIN NEW(B); B.next := NIL; B.len := 0
  END OpenBuf;

  PROCEDURE Append* (T: Text; VAR B: Buffer);
    VAR R: Files.Rider;
    
    PROCEDURE Invert;
      VAR b1, b2: Buffer;
    BEGIN b1 := NIL;
      REPEAT b2 := b1; b1 := B; B := B.next; b1.next := b2 UNTIL B = NIL;
      B := b1
    END Invert;  
  
  BEGIN Files.Set(R, T, Files.Length(T)); Invert;
    LOOP IF B.len = 0 THEN EXIT END;
      Files.WriteBytes(R, B.data, B.len);
      IF B.next = NIL THEN B.len := 0; EXIT END;
      B := B.next
    END;
    Files.Set(R, T, Files.Pos(R))
  END Append;  

 (** Readers **)

  PROCEDURE OpenReader* (VAR R: Reader; T: Text; pos: LONGINT);
  BEGIN Files.Set(R, T, pos); R.eot := FALSE
  END OpenReader;

  PROCEDURE Read* (VAR R: Reader; VAR ch: CHAR);
  BEGIN Files.Read(R, ch); R.eot := R.eof OR (ch = 0X)
  END Read;

  PROCEDURE Pos* (VAR R: Reader): LONGINT;
  BEGIN RETURN Files.Pos(R)
  END Pos;

 (* Scanners NW-DT *)

  PROCEDURE OpenScanner* (VAR S: Scanner; T: Text; pos: LONGINT);
  BEGIN OpenReader(S, T, pos); S.line := 0; S.nextCh := " "
  END OpenScanner;

  (*IEEE floating point formats:
    x = 2^(e-127) * 1.m    bit 0: sign, bits 1- 8: e, bits  9-31: m
    x = 2^(e-1023) * 1.m   bit 0: sign, bits 1-11: e, bits 12-63: m *)

  PROCEDURE Scan* (VAR S: Scanner);
    CONST maxD = 32;
    VAR ch, term: CHAR;
      neg, negE, hex: BOOLEAN;
      i, j, h: SHORTINT;
      e: INTEGER; k: LONGINT;
      x, f: REAL; y, g: LONGREAL;
      d: ARRAY maxD OF CHAR;

    PROCEDURE ReadScaleFactor;
    BEGIN Read(S, ch);
      IF ch = "-" THEN negE := TRUE; Read(S, ch)
      ELSE negE := FALSE;
        IF ch = "+" THEN Read(S, ch) END
      END;
      WHILE ("0" <= ch) & (ch <= "9") DO
        e := e*10 + ORD(ch) - 30H; Read(S, ch)
      END
    END ReadScaleFactor;

  BEGIN ch := S.nextCh; i := 0;
    LOOP
      IF ch = NL THEN INC(S.line)
      ELSIF (ch # " ") & (ch # TAB) THEN EXIT
      END ;
      Read(S, ch)
    END;
    IF ("A" <= CAP(ch)) & (CAP(ch) <= "Z") THEN (*name*)
      REPEAT S.s[i] := ch; INC(i); Read(S, ch)
      UNTIL (CAP(ch) > "Z")
        OR ("A" > CAP(ch)) & (ch > "9")
        OR ("0" > ch) & (ch # ".")
        OR (i = 31);
      S.s[i] := 0X; S.len := i; S.class := 1
    ELSIF ch = 22X THEN (*literal string*)
      Read(S, ch);
      WHILE (ch # 22X) & (ch >= " ") & (i # 31) DO
        S.s[i] := ch; INC(i); Read(S, ch)
      END;
      S.s[i] := 0X; S.len := i+1; Read(S, ch); S.class := 2
    ELSE
      IF ch = "-" THEN neg := TRUE; Read(S, ch) ELSE neg := FALSE END ;
      IF ("0" <= ch) & (ch <= "9") THEN (*number*)
        hex := FALSE; j := 0;
        LOOP d[i] := ch; INC(i); Read(S, ch);
          IF ch < "0" THEN EXIT END;
          IF "9" < ch THEN
            IF ("A" <= ch) & (ch <= "F") THEN hex := TRUE; ch := CHR(ORD(ch)-7)
            ELSIF ("a" <= ch) & (ch <= "f") THEN hex := TRUE; ch := CHR(ORD(ch)-27H)
            ELSE EXIT
            END
          END
        END;
        IF ch = "H" THEN (*hex number*)
          Read(S, ch); S.class := 3;
          IF i-j > 8 THEN j := i-8 END ;
          k := ORD(d[j]) - 30H; INC(j);
          IF (i-j = 7) & (k >= 8) THEN DEC(k, 16) END ;
          WHILE j < i DO k := k*10H + (ORD(d[j]) - 30H); INC(j) END ;
          IF neg THEN S.i := -k ELSE S.i := k END 
        ELSIF ch = "." THEN (*read real*)
          Read(S, ch); h := i;
          WHILE ("0" <= ch) & (ch <= "9") DO d[i] := ch; INC(i); Read(S, ch) END ;
          IF ch = "D" THEN
            e := 0; y := 0; g := 1;
            REPEAT y := y*10 + (ORD(d[j]) - 30H); INC(j) UNTIL j = h;
            WHILE j < i DO g := g/10; y := (ORD(d[j]) - 30H)*g + y; INC(j) END ;
            ReadScaleFactor;
            IF negE THEN
              IF e <= 308 THEN y := y / Reals.TenL(e) ELSE y := 0 END
            ELSIF e > 0 THEN
              IF e <= 308 THEN y := Reals.TenL(e) * y ELSE HALT(40) END 
            END ;
            IF neg THEN y := -y END ;
            S.class := 5; S.y := y
          ELSE e := 0; x := 0; f := 1;
            REPEAT x := x*10 + (ORD(d[j]) - 30H); INC(j) UNTIL j = h;
            WHILE j < i DO f := f/10; x := (ORD(d[j])-30H)*f + x; INC(j) END;
            IF ch = "E" THEN ReadScaleFactor END ;
            IF negE THEN
              IF e <= 38 THEN x := x / Reals.Ten(e) ELSE x := 0 END
            ELSIF e > 0 THEN
              IF e <= 38 THEN x := Reals.Ten(e) * x ELSE HALT(40) END
            END ;
            IF neg THEN x := -x END ;
            S.class := 4; S.x := x
          END ;
          IF hex THEN S.class := 0 END
        ELSE (*decimal integer*)
          S.class := 3; k := 0;
          REPEAT k := k*10 + (ORD(d[j]) - 30H); INC(j) UNTIL j = i;
          IF neg THEN S.i := -k ELSE S.i := k END;
          IF hex THEN S.class := 0 ELSE S.class := 3 END
        END
      ELSE S.class := 6;
        IF neg THEN S.c := "-" ELSE S.c := ch; Read(S, ch) END
      END
    END;
    S.nextCh := ch
  END Scan;

  (** Writers **)

  PROCEDURE OpenWriter* (VAR W: Writer);
  BEGIN OpenBuf(W.buf)
  END OpenWriter;

  PROCEDURE Write* (VAR W: Writer; ch: CHAR);
    VAR B: Buffer;
  BEGIN IF W.buf.len = BufLen THEN OpenBuf(B); B.next := W.buf; W.buf := B END;
    W.buf.data[W.buf.len] := ch; INC(W.buf.len)
  END Write;

  PROCEDURE WriteLn* (VAR W: Writer);
  BEGIN Write(W, NL)
  END WriteLn;

  PROCEDURE WriteString* (VAR W: Writer; s: ARRAY OF CHAR);
    VAR i: INTEGER;
  BEGIN i := 0;
    WHILE s[i] >= " " DO Write(W, s[i]); INC(i) END
  END WriteString;

  PROCEDURE WriteInt* (VAR W: Writer; x, n: LONGINT);
    VAR i: INTEGER; x0: LONGINT;
      a: ARRAY 11 OF CHAR;
  BEGIN i := 0;
    IF x < 0 THEN
      IF x = MIN(LONGINT) THEN WriteString(W, " -2147483648"); RETURN
      ELSE DEC(n); x0 := -x
      END
    ELSE x0 := x
    END;
    REPEAT
      a[i] := CHR(x0 MOD 10 + 30H); x0 := x0 DIV 10; INC(i)
    UNTIL x0 = 0;
    WHILE n > i DO Write(W, " "); DEC(n) END;
    IF x < 0 THEN Write(W, "-") END;
    REPEAT DEC(i); Write(W, a[i]) UNTIL i = 0
  END WriteInt;

  PROCEDURE WriteHex* (VAR W: Writer; x: LONGINT);
    VAR i: INTEGER; y: LONGINT;
      a: ARRAY 10 OF CHAR;
  BEGIN i := 0; Write(W, " ");
    REPEAT y := x MOD 10H;
      IF y < 10 THEN a[i] := CHR(y + 30H) ELSE a[i] := CHR(y + 37H) END;
      x := x DIV 10H; INC(i)
    UNTIL i = 8;
    REPEAT DEC(i); Write(W, a[i]) UNTIL i = 0
  END WriteHex;

  PROCEDURE WriteReal* (VAR W: Writer; x: REAL; n: INTEGER);
    VAR e: INTEGER; x0: REAL;
      d: ARRAY maxD OF CHAR;
  BEGIN e := Reals.Expo(x);
    IF e = 0 THEN
      WriteString(W, "  0");
      REPEAT Write(W, " "); DEC(n) UNTIL n <= 3
    ELSIF e = 255 THEN
      WriteString(W, " NaN");
      WHILE n > 4 DO Write(W, " "); DEC(n) END
    ELSE
      IF n <= 9 THEN n := 3 ELSE DEC(n, 6) END;
      REPEAT Write(W, " "); DEC(n) UNTIL n <= 8;
      (*there are 2 < n <= 8 digits to be written*)
      IF x < 0.0 THEN Write(W, "-"); x := -x ELSE Write(W, " ") END;
      e := (e - 127) * 77  DIV 256;
      IF e >= 0 THEN x := x / Reals.Ten(e) ELSE x := Reals.Ten(-e) * x END;
      IF x >= 10.0 THEN x := 0.1*x; INC(e) END;
      x0 := Reals.Ten(n-1); x := x0*x + 0.5;
      IF x >= 10.0*x0 THEN x := x*0.1; INC(e) END;
      Reals.Convert(x, n, d);
      DEC(n); Write(W, d[n]); Write(W, ".");
      REPEAT DEC(n); Write(W, d[n]) UNTIL n = 0;
      Write(W, "E");
      IF e < 0 THEN Write(W, "-"); e := -e ELSE Write(W, "+") END;
      Write(W, CHR(e DIV 10 + 30H)); Write(W, CHR(e MOD 10 + 30H))
    END
  END WriteReal;

  PROCEDURE WriteRealFix* (VAR W: Writer; x: REAL; n, k: INTEGER);
    VAR e, i: INTEGER; sign: CHAR; x0: REAL;
      d: ARRAY maxD OF CHAR;

    PROCEDURE seq(ch: CHAR; n: INTEGER);
    BEGIN WHILE n > 0 DO Write(W, ch); DEC(n) END
    END seq;

    PROCEDURE dig(n: INTEGER);
    BEGIN
      WHILE n > 0 DO
        DEC(i); Write(W, d[i]); DEC(n)
      END
    END dig;

  BEGIN e := Reals.Expo(x);
    IF k < 0 THEN k := 0 END;
    IF e = 0 THEN seq(" ", n-k-2); Write(W, "0"); seq(" ", k+1)
    ELSIF e = 255 THEN WriteString(W, " NaN"); seq(" ", n-4)
    ELSE e := (e - 127) * 77 DIV 256;
      IF x < 0 THEN sign := "-"; x := -x ELSE sign := " " END;
      IF e >= 0 THEN  (*x >= 1.0,  77/256 = log 2*) x := x/Reals.Ten(e)
        ELSE (*x < 1.0*) x := Reals.Ten(-e) * x
      END;
      IF x >= 10.0 THEN x := 0.1*x; INC(e) END;
      (* 1 <= x < 10 *)
      IF k+e >= maxD-1 THEN k := maxD-1-e
        ELSIF k+e < 0 THEN k := -e; x := 0.0
      END;
      x0 := Reals.Ten(k+e); x := x0*x + 0.5;
      IF x >= 10.0*x0 THEN INC(e) END;
      (*e = no. of digits before decimal point*)
      INC(e); i := k+e; Reals.Convert(x, i, d);
      IF e > 0 THEN
        seq(" ", n-e-k-2); Write(W, sign); dig(e);
        Write(W, "."); dig(k)
      ELSE seq(" ", n-k-3);
        Write(W, sign); Write(W, "0"); Write(W, ".");
        seq("0", -e); dig(k+e)
      END
    END
  END WriteRealFix;

  PROCEDURE WriteRealHex* (VAR W: Writer; x: REAL);
    VAR i: INTEGER;
      d: ARRAY 8 OF CHAR;
  BEGIN Reals.ConvertH(x, d); i := 0;
    REPEAT Write(W, d[i]); INC(i) UNTIL i = 8
  END WriteRealHex;

  PROCEDURE WriteLongReal* (VAR W: Writer; x: LONGREAL; n: INTEGER);
    CONST maxD = 16;
    VAR e: INTEGER; x0: LONGREAL;
      d: ARRAY maxD OF CHAR;
  BEGIN e := Reals.ExpoL(x);
    IF e = 0 THEN
      WriteString(W, "  0");
      REPEAT Write(W, " "); DEC(n) UNTIL n <= 3
    ELSIF e = 2047 THEN
      WriteString(W, " NaN");
      WHILE n > 4 DO Write(W, " "); DEC(n) END
    ELSE
      IF n <= 10 THEN n := 3 ELSE DEC(n, 7) END;
      REPEAT Write(W, " "); DEC(n) UNTIL n <= maxD;
      (*there are 2 <= n <= maxD digits to be written*)
      IF x < 0 THEN Write(W, "-"); x := -x ELSE Write(W, " ") END;
      e := SHORT(LONG(e - 1023) * 77 DIV 256);
      IF e >= 0 THEN x := x / Reals.TenL(e) ELSE x := Reals.TenL(-e) * x END ;
      IF x >= 10.0D0 THEN x := 0.1D0 * x; INC(e) END ;
      x0 := Reals.TenL(n-1); x := x0*x + 0.5D0;
      IF x >= 10.0D0*x0 THEN x := 0.1D0 * x; INC(e) END ;
      Reals.ConvertL(x, n, d);
      DEC(n); Write(W, d[n]); Write(W, ".");
      REPEAT DEC(n); Write(W, d[n]) UNTIL n = 0;
      Write(W, "D");
      IF e < 0 THEN Write(W, "-"); e := -e ELSE Write(W, "+") END;
      Write(W, CHR(e DIV 100 + 30H)); e := e MOD 100;
      Write(W, CHR(e DIV 10 + 30H));
      Write(W, CHR(e MOD 10 + 30H))
    END
  END WriteLongReal;

  PROCEDURE WriteLongRealHex* (VAR W: Writer; x: LONGREAL);
    VAR i: INTEGER;
      d: ARRAY 16 OF CHAR;
  BEGIN Reals.ConvertHL(x, d); i := 0;
    REPEAT Write(W, d[i]); INC(i) UNTIL i = 16
  END WriteLongRealHex;

  PROCEDURE WriteDate* (VAR W: Writer; t, d: LONGINT);

    PROCEDURE WritePair(ch: CHAR; x: LONGINT);
    BEGIN Write(W, ch);
      Write(W, CHR(x DIV 10 + 30H)); Write(W, CHR(x MOD 10 + 30H))
    END WritePair;

  BEGIN
    WritePair(" ", d MOD 32); WritePair(".", d DIV 32 MOD 16); WritePair(".", d DIV 512 MOD 128);
    WritePair(" ", t DIV 4096 MOD 32); WritePair(":", t DIV 64 MOD 64); WritePair(":", t MOD 64)
  END WriteDate;

  PROCEDURE Open* (VAR T: Text; name: ARRAY OF CHAR);
  BEGIN T := Files.Old(name); 
    IF T = NIL THEN T := Files.New(name) END
  END Open;

  PROCEDURE Create* (VAR T: Text; name: ARRAY OF CHAR);
  BEGIN T := Files.New(name)
  END Create;

  PROCEDURE Close* (T: Text);
  BEGIN Files.Close(T)
  END Close;

END Texts.
