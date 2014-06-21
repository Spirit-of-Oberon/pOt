MODULE OS; (* DT Mon Jan 24 1994 *)
  
  PROCEDURE Time*(): LONGINT;
  END Time;

  PROCEDURE GetClock*(VAR date,time: LONGINT);
  END GetClock;

  PROCEDURE GetParFile*(VAR fname: ARRAY OF CHAR);
  END GetParFile; 
  
  PROCEDURE GC*;
  END GC;

END OS.
