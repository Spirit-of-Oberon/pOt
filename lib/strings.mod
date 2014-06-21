MODULE Strings;

  PROCEDURE Length*(s: ARRAY OF CHAR): LONGINT;
    VAR l: LONGINT;
  BEGIN l := 0; WHILE s[l] # 0X DO INC(l) END; RETURN l
  END Length;

  PROCEDURE Append*(VAR s: ARRAY OF CHAR; tail: ARRAY OF CHAR);
    VAR i, j: LONGINT;
  BEGIN
    i := 0; WHILE s[i] # 0X DO INC(i) END; j := i;
    LOOP
      s[i] := tail[i - j];
      IF s[i] = 0X THEN EXIT END;
      INC(i)
    END
  END Append;

 (* we suppose Base to be either extent of 2 or 10 *)
  PROCEDURE FromLInt*(li: LONGINT; Base: SHORTINT; VAR s: ARRAY OF CHAR);
    VAR i: INTEGER;
      
    PROCEDURE Tail(li: LONGINT);  
      
    BEGIN  
      IF li >= Base THEN Tail(li DIV Base); li := li MOD Base END;  
      IF li < 10 THEN s[i] := CHR(li + ORD("0"))  
      ELSE s[i] := CHR(li - 10 + ORD("A"))  
      END;  
      INC(i)  
    END Tail;  

  BEGIN
    i := 0;
    IF li < 0 THEN 
      IF Base = 10 THEN (* only for base ten *)
        s[0] := "-"; li := -li; i := 1
      ELSE (* two's complement arithmetics (should import SYSTEM) *)
        Tail(MAX(LONGINT) DIV (Base DIV 2) + (li + Base) DIV Base); 
        li := li MOD Base
      END
    END;    
    Tail(li);
    s[i] := 0X
  END FromLInt;

END Strings.
