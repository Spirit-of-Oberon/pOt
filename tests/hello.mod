MODULE Hello;
  IMPORT Files, Texts;

  TYPE prtyp = PROCEDURE(a: ARRAY OF CHAR);
    prtypa = PROCEDURE(a: CHAR): INTEGER;
  VAR W: Texts.Writer;
    a: prtyp;
    b: prtypa;
    c: PROCEDURE(a: ARRAY OF CHAR): CHAR;
    d: ARRAY 4 OF PROCEDURE(a: INTEGER): CHAR;

  PROCEDURE *p0(a: ARRAY OF CHAR);
  BEGIN Texts.WriteString(W,a); Texts.WriteLn(W);
  END p0;

  PROCEDURE *p1(a: CHAR): INTEGER;
  BEGIN RETURN ORD(a)
  END p1;

  PROCEDURE *p2(a: ARRAY OF CHAR): CHAR;
  BEGIN p0(a);  RETURN "a"
  END p2;


  PROCEDURE Say*;
    VAR ch: CHAR; ii: INTEGER;
  BEGIN a("It'me");  ch := c("Abc"); ii :=  b("a");
    Texts.WriteString(W, "Hello, World"); Texts.WriteLn(W);
    Texts.Append(Files.StdOut, W.buf)
  END Say;

BEGIN Texts.OpenWriter(W); a := p0; b := p1; c := p2
END Hello.
  
