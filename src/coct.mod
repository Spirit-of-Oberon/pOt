MODULE COCT;  (*NW 28.5.87 / 5.3.91*) (* DVD 15 08 1993 15:33 *)

	IMPORT Files, COCS;

	CONST maxImps = 64; SFtag = 0FBX; firstStr = 16;
			maxStr = 80; maxUDP = 16; maxMod = 32; maxParLev = 16; maxFldLev = 16;
			NotYetExp = 0; PrivExp = -1;

		(* name resolution modes *)
			ordObj = 0; stdObj = 1; sysObj = 2;

		(*object modes*)
			Var = 1; Ind = 3; Con = 8; Fld = 12; Typ = 13;
			XProc = 15; SProc = 16; CProc = 17; Mod = 19; Head = 20;

		(*structure forms*)
			Undef = 0; Byte = 1; Bool = 2; Char = 3; SInt = 4; Int = 5; LInt = 6;
			Real = 7; LReal = 8; Set = 9; String = 10; NilTyp = 11; NoTyp = 12;
			Pointer = 13; ProcTyp = 14; Array = 15; DynArr = 16; Record = 17;

	TYPE
		Object* = POINTER TO ObjDesc;
		Struct* = POINTER TO StrDesc;

		ObjDesc* = RECORD
			dsc*, next*: Object;
			typ*:  Struct;
			mnolev*: INTEGER; (* name resolution mode for all types, level for heads, mno for modules *)
			intval*: LONGINT; fltval*: LONGREAL; (* for modules and constants *)
			mode*: SHORTINT;
			marked*: BOOLEAN;
			name*: ARRAY 32 OF CHAR;
		END ;

		StrDesc* = RECORD
			form*: SHORTINT;
			mno*, ref*: INTEGER;
			n*: LONGINT;
			descr*: INTEGER; (* number of type descriptor for arrays and records; -1 = no descriptor *)
			BaseTyp*: Struct;
			link*, strobj*: Object
		END;
		(* size  field is thrown away because it's value isn't  needed
			at compile time; at runtime sizeof(<typename>) is  sufficient *)

		Item* = RECORD
			mode*: SHORTINT;
			mnolev*: INTEGER;
			intval*:LONGINT; fltval*:LONGREAL; (* for constants *)
			qoffs*:INTEGER;  (* offset in SSeq *)
			typ*: Struct;
			obj*: Object
		END;

(* pOt Cymbol File BNF

CymFile ::= SFtag ModuleAnchor Objects UndPtrs.
 ModuleAnchor ::= 22 key:4 name.
 Objects ::= {Object}.
 Object ::= Con | Typ | Var | XProc | CProc.
	Con ::= 1 (Byte|Bool|Char|SInt|Int|LInt|Set|Real|LReal|String|NilTyp) name:asciiz.
	 Byte ::= int:1.
	 Bool ::= int:1.
	 Char ::= int:1.
	 SInt ::= int:1.
	 Int ::= int:2.
	 LInt ::= int:4.
	 Set ::= int:4
	 Real ::= flt:4.
	 LReal ::= flt:8.
	 String ::= int:1 :asciiz.
	 NilType ::= .
	Typ  ::= Str [2 ref mno name:asciiz].
	 Str ::= [[ModuleAnchor] (Pointer|ProcTyp|Array|DynArr|Record) [(2|3) strno mno name:asciiz]].
		Pointer ::= 8 (ref | Undef) mno.
		ProcTyp ::= Str(ret) Parameters 9 ref mno.
		 Parameters ::=  13 {Str (14|15) ref name:asciiz}.
		Array ::= Str(base) 10 ref mno descr:2 n:4.
		DynArr ::= Str(base) 11 ref mno.
		Record ::= [Str(base)] Fields 12 ref mno descr:2.
		 Fields ::= [16] {(Str 17 ref name)|Fields|18}.
	Var ::= Str 4 ref name.
	XProc ::= Str(ret) Parameters 5 ref name.
	CProc ::= Str(ret) Parameters 7 ref name.
 UndPtrs ::=  {Str(base) 20 ref ref(base)}.
*)

	VAR topScope*: Object;
		undftyp*, bytetyp*, booltyp*, chartyp*, sinttyp*, inttyp*, linttyp*,
		realtyp*, lrltyp*, settyp*, stringtyp*, niltyp*, notyp*: Struct;
		nofGmod*: INTEGER;   (*nof imports*)
		GlbMod*:  ARRAY maxImps OF Object;

		level*: INTEGER;
		wasderef*: Object;
		typchk*: BOOLEAN;

		universe, syslink: Object;
		strno, udpinx: INTEGER;  (*for export*)
		nofExp: SHORTINT;
		SR: Files.Rider;
		undPtr: ARRAY maxUDP OF Struct;

	PROCEDURE Init*;
		VAR imod,iudp: INTEGER;
	BEGIN
		topScope := universe; level := 0; strno := 0; udpinx := 0; nofGmod := 0;
		imod := 0; WHILE imod # maxImps DO GlbMod[imod] := NIL; INC(imod) END;
		iudp := 0; WHILE iudp # maxUDP DO undPtr[iudp] := NIL; INC(iudp) END
	END Init;

	PROCEDURE Close*;
		VAR i: INTEGER;
	BEGIN i := 0; WHILE i # maxImps DO GlbMod[i] := NIL; INC(i) END;
		i := 0; WHILE i # maxUDP DO undPtr[i] := NIL; INC(i) END
	END Close;

	PROCEDURE FindImport*(mod: Object; VAR res: Object);
		VAR obj: Object;
	BEGIN obj := mod.dsc;
		WHILE (obj # NIL) & (obj.name # COCS.name) DO obj := obj.next END ;
		IF (obj # NIL) & (obj.mode = Typ) & (~obj.marked) THEN obj := NIL END ;
		res := obj
	END FindImport;

	PROCEDURE Find*(VAR res: Object; VAR level: INTEGER);
		VAR obj, head: Object;
	BEGIN head := topScope;
		LOOP obj := head.next; (* go up by levels *)
			WHILE (obj # NIL) & (obj.name # COCS.name) DO obj := obj.next END ;
			IF obj # NIL THEN level := SHORT(head.mnolev); EXIT END ;
			head := head.dsc;
			IF head = NIL THEN level := 0; EXIT END
		END;
		res := obj
	END Find;

	PROCEDURE FindObj*(obj: Object; VAR level: INTEGER);
		VAR head, cur: Object;
	BEGIN head := topScope;
		LOOP cur := head.next;
			WHILE (cur # NIL) & (cur # obj) DO cur := cur.next END;
			IF cur # NIL THEN level := SHORT(head.mnolev); EXIT END;
			head := head.dsc;
			IF head = NIL THEN level := 0; EXIT END
		END
	END FindObj;

	PROCEDURE FindField*(typ: Struct; VAR res: Object);
		VAR obj: Object;
	BEGIN (*typ.form = Record*)
		LOOP obj := typ.link;
			WHILE (obj # NIL) & (obj.name # COCS.name) DO obj := obj.next END ;
			IF obj # NIL THEN EXIT END ;
			typ := typ.BaseTyp;
			IF typ = NIL THEN EXIT END
		END ;
		res := obj
	END FindField;

	PROCEDURE Insert*(VAR name: ARRAY OF CHAR; VAR res: Object);
		VAR obj, new: Object;
	BEGIN obj := topScope;
		WHILE (obj.next # NIL) & (obj.next.name # name) DO obj := obj.next END ;
		IF obj.next = NIL THEN NEW(new);
			new.dsc := NIL; new.next := NIL;
			COPY(name, new.name); new.mnolev := ordObj;
			obj.next := new; res := new
		ELSE res := obj.next;
			IF obj.next.mode # Undef THEN COCS.Mark(1) END
		END
	END Insert;

	PROCEDURE Remove*(del: Object);
		VAR obj: Object;
	BEGIN obj := topScope;
		WHILE obj.next # del DO obj := obj.next END ;
		obj.next := del.next
	END Remove;

	PROCEDURE OpenScope*(level: INTEGER; VAR name: ARRAY OF CHAR);
		VAR head: Object;
	BEGIN NEW(head);
		head.mode := Head; head.mnolev := level; COPY(name, head.name);
		head.typ := NIL; head.dsc := topScope; head.next := NIL;
		topScope := head
	END OpenScope;

	PROCEDURE CloseScope*;
	BEGIN topScope := topScope.dsc
	END CloseScope;

	PROCEDURE HasPtr*(typ: Struct): BOOLEAN;
		VAR fld: Object;
	BEGIN
		IF typ.form = Pointer THEN RETURN TRUE
		ELSIF typ.form IN {Array, DynArr} THEN RETURN HasPtr(typ.BaseTyp)
		ELSIF typ.form = Record THEN
			IF (typ.BaseTyp # NIL) & HasPtr(typ.BaseTyp) THEN RETURN TRUE END;
			fld := typ.link;
			WHILE fld # NIL DO
				IF (fld.name = "") OR HasPtr(fld.typ) THEN RETURN TRUE END;
				fld := fld.next
			END
		END;
		RETURN FALSE
	END HasPtr;

	PROCEDURE IsParam*(obj: Object): BOOLEAN;
	BEGIN RETURN (obj # NIL) & (obj.mode <= Ind) & (obj.intval = 1)
	END IsParam;

	PROCEDURE VarMode*(VAR x: Item);
	BEGIN
		IF IsParam(x.obj) THEN
			CASE x.typ.form OF
				Undef .. ProcTyp:
			| Array: x.mode := Ind
			| DynArr:
				IF x.typ.BaseTyp = bytetyp THEN x.mode := Var
				ELSE x.mode := Ind
				END;
				x.intval := 0
			| Record: x.mode := Ind
			END
		END
	END VarMode;

 (*---------------------- import ------------------------*)

	PROCEDURE ReadInt(VAR i: INTEGER);
	BEGIN Files.ReadBytes(SR, i, SIZE(INTEGER))
	END ReadInt;

	PROCEDURE ReadXInt(VAR k: LONGINT);
		VAR i: INTEGER;
	BEGIN Files.ReadBytes(SR, i, SIZE(INTEGER)); k := i
	END ReadXInt;

	PROCEDURE ReadLInt(VAR k: LONGINT);
	BEGIN Files.ReadBytes(SR, k, SIZE(LONGINT))
	END ReadLInt;

	PROCEDURE ReadXReal(VAR lr: LONGREAL);
		VAR r: REAL;
	BEGIN Files.ReadBytes(SR, r, SIZE(REAL)); lr := r
	END ReadXReal;

	PROCEDURE ReadLReal(VAR lr: LONGREAL);
	BEGIN Files.ReadBytes(SR, lr, SIZE(LONGREAL))
	END ReadLReal;

	PROCEDURE ReadId(VAR id: ARRAY OF CHAR);
		VAR i: INTEGER; ch: CHAR;
	BEGIN i := 0;
		REPEAT Files.Read(SR, ch); id[i] := ch; INC(i)
		UNTIL ch = 0X
	END ReadId;

	PROCEDURE Import*(VAR name, self, FileName: ARRAY OF CHAR);
		VAR i, j, m, s, class: INTEGER; k: LONGINT;
				nofLmod, strno, parlev, fldlev: INTEGER;
				obj, ob0: Object;
				typ: Struct;
				ch, ch1, ch2: CHAR;
				si: SHORTINT;
				xval: REAL; yval: LONGREAL;
				SymFile: Files.File;
				modname: ARRAY 32 OF CHAR;
				LocMod:  ARRAY maxMod OF Object;
				struct:  ARRAY maxStr OF Struct;
				lastpar: ARRAY maxParLev OF Object;
				lastfld: ARRAY maxFldLev OF Object;

		PROCEDURE reversedList(p: Object): Object;
			VAR q, r: Object;
		BEGIN q := NIL;
			WHILE p # NIL DO
				r := p.next; p.next := q; q := p; p := r
			END ;
			RETURN q
		END reversedList;

	BEGIN nofLmod := 0; strno := firstStr;
		parlev := -1; fldlev := -1;
		IF FileName = "SYSTEM.Cym" THEN
			Insert(name, obj); obj.mode := Mod; obj.dsc := syslink;
			obj.mnolev := 0; obj.typ := notyp
		ELSE SymFile := Files.Old(FileName);
			IF SymFile # NIL THEN
				Files.Set(SR, SymFile, 0); Files.Read(SR, ch);
				IF ch = SFtag THEN
					struct[Undef] := undftyp; struct[Byte] := bytetyp;
					struct[Bool] := booltyp;  struct[Char] := chartyp;
					struct[SInt] := sinttyp;  struct[Int] := inttyp;
					struct[LInt] := linttyp;  struct[Real] := realtyp;
					struct[LReal] := lrltyp;  struct[Set] := settyp;
					struct[String] := stringtyp; struct[NilTyp] := niltyp; struct[NoTyp] := notyp;
					LOOP (*read next item from symbol file*)
						Files.Read(SR, ch); class := ORD(ch);
						IF SR.eof THEN EXIT END ;
						CASE class OF
							0: COCS.Mark(151)
						| 1..7: (*object*) NEW(obj); m := 0;
							Files.Read(SR, ch); s := ORD(ch); obj.typ := struct[s];
							CASE class OF
								1: obj.mode := Con;
										CASE obj.typ.form OF
											2,4: Files.Read(SR, si); obj.intval := si
										| 1,3: Files.Read(SR, ch); obj.intval := ORD(ch)
										| 5: ReadXInt(obj.intval)
										| 6,9: ReadLInt(obj.intval) (* longint and set *)
										| 7: ReadXReal(obj.fltval)
										| 8: ReadLReal(obj.fltval)
										| 10: Files.Read(SR, si); obj.intval := LONG(LONG(si)); (*String constant is referenced by name only *)
												  REPEAT Files.Read(SR, ch) UNTIL ch = 0X;
										| 11: (*NIL*)
										END
							|2,3: obj.mode := Typ; Files.Read(SR, ch); m := ORD(ch);
										IF obj.typ.strobj = NIL THEN obj.typ.strobj := obj END;
										obj.marked := class = 2
							|4: obj.mode := Var; obj.intval := 0
							|5,6,7: IF class # 7 THEN obj.mode := XProc ELSE obj.mode := CProc END;
										obj.dsc := reversedList(lastpar[parlev]); DEC(parlev)
							END ;
							ReadId(obj.name); ob0 := LocMod[m];
							WHILE (ob0.next # NIL) & (ob0.next.name # obj.name) DO ob0 := ob0.next END ;
							IF ob0.next = NIL THEN ob0.next := obj; obj.next := NIL  (*insert object*)
							ELSIF obj.mode = Typ THEN struct[s] := ob0.next.typ
							END
						| 8..12: (*structure*)
							NEW(typ); typ.strobj := NIL; typ.ref := 0;
							Files.Read(SR, ch); typ.BaseTyp := struct[ORD(ch)];
							Files.Read(SR, ch); typ.mno := SHORT(LocMod[ORD(ch)].mnolev);
							CASE class OF
								 8: typ.form := Pointer; typ.n := 0
							|  9: typ.form := ProcTyp;
										typ.link := reversedList(lastpar[parlev]); DEC(parlev)
							| 10: typ.form := Array; ReadInt(typ.descr); ReadLInt(typ.n)
							| 11: typ.form := DynArr (* DynArr is a reference *)
							| 12: typ.form := Record; typ.n := 0;
										typ.link := reversedList(lastfld[fldlev]); DEC(fldlev);
										IF typ.BaseTyp = notyp THEN typ.BaseTyp := NIL; typ.n := 0
										ELSE typ.n := typ.BaseTyp.n + 1
										END ;
										ob0 := typ.link;
										WHILE ob0 # NIL DO
											ob0.mnolev := SHORT(typ.n);
											ob0 := ob0.next
										END;
										ReadInt(typ.descr)
							END ;
							struct[strno] := typ; INC(strno)
						| 13: (*parameter list start*)
							IF parlev < maxParLev-1 THEN INC(parlev); lastpar[parlev] := NIL
							ELSE COCS.Mark(229)
							END
						| 14, 15: (*parameter*)
							NEW(obj);
							IF class = 14 THEN obj.mode := Var ELSE obj.mode := Ind END;
							obj.intval := 1;
							Files.Read(SR, ch); obj.typ := struct[ORD(ch)];
							ReadId(obj.name);
							obj.dsc := NIL; obj.next := lastpar[parlev]; lastpar[parlev] := obj
						| 16: (*start field list*)
							IF fldlev < maxFldLev-1 THEN INC(fldlev); lastfld[fldlev] := NIL
							ELSE COCS.Mark(229)
							END
						| 17: (*field*)
							NEW(obj); obj.mode := Fld;
							Files.Read(SR, ch); obj.typ := struct[ORD(ch)];
							ReadId(obj.name); obj.marked := TRUE;
							obj.dsc := NIL; obj.next := lastfld[fldlev]; lastfld[fldlev] := obj
						| 18: (*hidden pointer field*) (* to make it possible to apply HasPtr(...) call *)
							NEW(obj); obj.mode := Fld;
							obj.name := ""; obj.typ := notyp; obj.marked := FALSE;
							obj.dsc := NIL; obj.next := lastfld[fldlev]; lastfld[fldlev] := obj
						| 19: (*hidden procedure field*)
							(*nothing*)
						| 20: (*fixup pointer typ*)
							Files.Read(SR, ch); typ := struct[ORD(ch)];
							Files.Read(SR, ch1);
							IF typ.BaseTyp = undftyp THEN typ.BaseTyp := struct[ORD(ch1)] END
						| 21, 23, 24: COCS.Mark(151); EXIT
						| 22: (*module anchor*)
							ReadLInt(k); ReadId(modname);
							IF modname = self THEN COCS.Mark(49) END;
							i := 0;
							WHILE (i < nofGmod) & (modname # GlbMod[i].name) DO INC(i) END ;
							IF i < nofGmod THEN (*module already present*)
								IF k # GlbMod[i].intval THEN COCS.Mark(150) END ;
								obj := GlbMod[i]
							ELSE NEW(obj);
								IF nofGmod < maxImps THEN GlbMod[nofGmod] := obj; INC(nofGmod)
								ELSE COCS.Mark(227)
								END ;
								obj.mode := NotYetExp; COPY(modname, obj.name);
								obj.intval := k; obj.mnolev := nofGmod; obj.next := NIL
							END ;
							IF nofLmod < maxMod THEN LocMod[nofLmod] := obj; INC(nofLmod)
							ELSE COCS.Mark(227)
							END
						END
					END (*LOOP*);
					Insert(name, obj);
					obj.mode := Mod; obj.dsc := LocMod[0].next;
					obj.mnolev := LocMod[0].mnolev; obj.typ := notyp
				ELSE COCS.Mark(151)
				END;
				Files.Close(SymFile)
			ELSE COCS.Mark(152)   (*sym file not found*)
			END
		END
	END Import;

	(*---------------------- export ------------------------*)

	PROCEDURE WriteByte(i: INTEGER);
	BEGIN Files.Write(SR, CHR(i))
	END WriteByte;

	PROCEDURE WriteInt(l: LONGINT);
		VAR i: INTEGER;
	BEGIN i:= SHORT(l); Files.WriteBytes(SR, i, SIZE(INTEGER))
	END WriteInt;

	PROCEDURE WriteLInt(k: LONGINT);
	BEGIN Files.WriteBytes(SR, k, SIZE(LONGINT))
	END WriteLInt;

	PROCEDURE WriteReal(lr: LONGREAL);
		VAR r: REAL;
	BEGIN r := SHORT(lr); Files.WriteBytes(SR, r, SIZE(REAL))
	END WriteReal;

	PROCEDURE WriteLReal(r: LONGREAL);
	BEGIN Files.WriteBytes(SR, r, SIZE(LONGREAL))
	END  WriteLReal;

	PROCEDURE WriteId(VAR name: ARRAY OF CHAR);
		VAR ch: CHAR; i: INTEGER;
	BEGIN i := 0;
		REPEAT ch := name[i]; Files.Write(SR, ch); INC(i)
		UNTIL ch = 0X
	END WriteId;

	PROCEDURE^ OutStr(typ: Struct; visible: BOOLEAN);

	PROCEDURE OutPars(par: Object; visible: BOOLEAN);
	BEGIN 
		IF visible THEN
			WriteByte(13);
			WHILE (par # NIL) & (par.mode <= Ind) & (par.intval = 1) DO
				OutStr(par.typ, visible);
				IF par.mode = Var THEN WriteByte(14) ELSE WriteByte(15) END ;
				WriteByte(par.typ.ref); WriteId(par.name);
				par := par.next
			END
		ELSE
			WHILE (par # NIL) & (par.mode <= Ind) & (par.intval = 1) DO
				OutStr(par.typ, visible); par := par.next
			END
		END
	END OutPars;

	PROCEDURE OutFlds(fld: Object; visible: BOOLEAN);
		VAR m: INTEGER; mod: Object;
	BEGIN
		IF visible THEN 
			WriteByte(16);
			WHILE fld # NIL DO
				IF fld.marked THEN
					OutStr(fld.typ, TRUE);
					WriteByte(17); WriteByte(fld.typ.ref); WriteId(fld.name)
				ELSE
					OutStr(fld.typ, FALSE);
					IF HasPtr(fld.typ) THEN
						WriteByte(18) (* remember: if HasPtr(...) then insert into pointer list *)
					END
				END;
				fld := fld.next
			END
		ELSE WHILE fld # NIL DO OutStr(fld.typ, FALSE); fld := fld.next END
		END
	END OutFlds;

	PROCEDURE OutStr(typ: Struct; visible: BOOLEAN);
		VAR m, em, r: INTEGER; btyp: Struct; mod: Object;
	BEGIN
		IF visible & (typ.ref <= 0) THEN
			m := typ.mno; btyp := typ.BaseTyp;
			IF m > 0 THEN mod := GlbMod[m-1]; em := mod.mode;
				IF (em = NotYetExp) OR (em = PrivExp) THEN
					GlbMod[m-1].mode := nofExp; m := nofExp; INC(nofExp);
					WriteByte(22); WriteLInt(mod.intval); WriteId(mod.name)
				ELSE m := em
				END
			END;
			CASE typ.form OF Undef .. NoTyp:
			| Pointer: WriteByte(8);
				IF btyp.ref > 0 THEN WriteByte(btyp.ref)
				ELSE WriteByte(Undef);
					IF udpinx < maxUDP THEN undPtr[udpinx] := typ; INC(udpinx) ELSE COCS.Mark(224) END
				END ;
				WriteByte(m)
			| ProcTyp: OutStr(btyp, TRUE); OutPars(typ.link, TRUE);
				WriteByte(9); WriteByte(btyp.ref); WriteByte(m)
			| Array: OutStr(btyp, TRUE);
				WriteByte(10); WriteByte(btyp.ref); WriteByte(m);
				WriteInt(typ.descr); WriteLInt(typ.n)
			| DynArr: OutStr(btyp, TRUE);
				WriteByte(11); WriteByte(btyp.ref); WriteByte(m)
			| Record:
				IF btyp = NIL THEN r := NoTyp
				ELSE OutStr(btyp, TRUE); r := btyp.ref
				END ;
				OutFlds(typ.link, TRUE); WriteByte(12); WriteByte(r); WriteByte(m);
				WriteInt(typ.descr)
			END;
			IF typ.strobj # NIL THEN
				IF typ.strobj.marked THEN WriteByte(2) ELSE WriteByte(3) END;
				WriteByte(strno); WriteByte(m); WriteId(typ.strobj.name)
			END ;
			typ.ref := strno; INC(strno);
			IF strno > maxStr THEN COCS.Mark(228) END
		ELSIF ~visible & (typ.ref = 0) THEN
			m := typ.mno; btyp := typ.BaseTyp;
			IF (m > 0) THEN mod := GlbMod[m-1];
				IF mod.mode = NotYetExp THEN mod.mode := PrivExp END
			END;
			CASE typ.form OF Undef .. NoTyp:
			| Pointer:
				IF btyp.ref = 0 THEN
					IF udpinx < maxUDP THEN undPtr[udpinx] := typ; INC(udpinx)
					ELSE COCS.Mark(224)
					END
				END;
			| ProcTyp: OutStr(btyp, FALSE); OutPars(typ.link, FALSE);
			| Array: OutStr(btyp, FALSE);
			| DynArr: OutStr(btyp, FALSE);
			| Record:
				IF btyp # NIL THEN OutStr(btyp, FALSE) END;
				OutFlds(typ.link, FALSE)
			END;
			typ.ref := -1
		END
	END OutStr;

	PROCEDURE OutPtrFixup(typ: Struct; visible: BOOLEAN);
	BEGIN
		OutStr(typ.BaseTyp, visible);
		IF visible THEN 
			WriteByte(20); WriteByte(typ.ref); WriteByte(typ.BaseTyp.ref)
		END
	END OutPtrFixup;

	PROCEDURE OutObjs;
		VAR obj: Object;
			f: INTEGER; xval: REAL; yval: LONGREAL;
	BEGIN obj := topScope.next;
		WHILE obj # NIL DO
			IF obj.marked THEN
				IF obj.mode = Con THEN
					WriteByte(1); f := obj.typ.form; WriteByte(f);
					CASE f OF
						Undef:
					| Byte, Bool, Char, SInt: WriteByte(SHORT(obj.intval))
					| Int: WriteInt(SHORT(obj.intval))
					| LInt, Set: WriteLInt(obj.intval)
					| Real: WriteReal(SHORT(obj.fltval))
					| LReal:  WriteLReal(obj.fltval)
					| String: WriteByte(SHORT(obj.intval MOD 100H)); WriteByte(0)
					| NilTyp:
					END;
					WriteId(obj.name)
				ELSIF obj.mode = Typ THEN
					OutStr(obj.typ, TRUE);
					IF (obj.typ.strobj # obj) & (obj.typ.strobj # NIL) THEN
						WriteByte(2); WriteByte(obj.typ.ref); WriteByte(0); WriteId(obj.name)
					END
				ELSIF obj.mode = Var THEN
					OutStr(obj.typ, TRUE); WriteByte(4);
					WriteByte(obj.typ.ref); WriteId(obj.name)
				ELSIF obj.mode = XProc THEN
					OutStr(obj.typ, TRUE); OutPars(obj.dsc, TRUE); WriteByte(5);
					WriteByte(obj.typ.ref); WriteId(obj.name)
				ELSIF obj.mode = CProc THEN
					OutStr(obj.typ, TRUE); OutPars(obj.dsc, TRUE); WriteByte(7);
					WriteByte(obj.typ.ref); WriteId(obj.name)
				END
			END ;
			obj := obj.next
		END
	END OutObjs;

	PROCEDURE Export*(VAR name, TmpFileName, FileName: ARRAY OF CHAR;
			VAR newSF: BOOLEAN; VAR key: LONGINT);
		VAR i: INTEGER;
			ch, ch0, ch1: CHAR;
			oldkey: LONGINT;
			typ: Struct;
			oldFile, newFile: Files.File;
			oldSR: Files.Rider;
			res: INTEGER;
	BEGIN newFile := Files.New(TmpFileName);
		IF newFile # NIL THEN
			Files.Set(SR, newFile, 0); Files.Write(SR, SFtag); strno := firstStr;
			WriteByte(22); WriteLInt(key); WriteId(name); nofExp := 1;
			OutObjs; i := 0;
			WHILE i < udpinx DO
				typ := undPtr[i]; OutPtrFixup(typ, typ.ref > 0);
				undPtr[i] := NIL; INC(i)
			END ;
			IF ~COCS.scanerr THEN
				oldFile := Files.Old(FileName);
				IF oldFile # NIL THEN (*compare*)
					Files.Set(oldSR, oldFile, 2); Files.ReadBytes(oldSR, oldkey, SIZE(LONGINT)); 
					Files.Set(SR, newFile, 2+SIZE(LONGINT));
					REPEAT Files.Read(oldSR, ch0); Files.Read(SR, ch1)
					UNTIL (ch0 # ch1) OR SR.eof;
					IF oldSR.eof & SR.eof THEN (*equal*) newSF := FALSE;  key := oldkey
					ELSIF ~newSF THEN COCS.Mark(155)
					END;
					Files.Close(oldFile)
				ELSE newSF := TRUE
				END
			ELSE newSF := FALSE
			END;
			IF newSF THEN
				Files.Register(newFile); Files.Delete(FileName, res);
				Files.Rename(TmpFileName, FileName, res)
			ELSE Files.Close(newFile); Files.Delete(TmpFileName, res)
			END;
			IF res > 1 THEN HALT(21H) END
		ELSE HALT(21H)
		END
	END Export;

	(*------------------------ initialization ------------------------*)

	PROCEDURE InitStruct(VAR typ: Struct; f: SHORTINT);
	BEGIN NEW(typ); typ.form := f; typ.ref := f;
	END InitStruct;

	PROCEDURE EnterConst(name: ARRAY OF CHAR; value: INTEGER);
		VAR obj: Object;
	BEGIN Insert(name, obj); obj.mode := Con; obj.typ := booltyp; obj.intval := value;
		obj.mnolev := stdObj
	END EnterConst;

	PROCEDURE EnterTyp(name: ARRAY OF CHAR; form: SHORTINT; VAR res: Struct);
		VAR obj: Object; typ: Struct;
	BEGIN Insert(name, obj);
		NEW(typ); obj.mode := Typ; obj.typ := typ; obj.marked := TRUE;
		obj.mnolev := stdObj;
		typ.form := form; typ.strobj := obj;
		typ.mno := 0; typ.ref := form; res := typ
	END EnterTyp;

	PROCEDURE EnterProc(name: ARRAY OF CHAR; num: INTEGER);
		VAR obj: Object;
	BEGIN Insert(name, obj); obj.mode := SProc; obj.typ := notyp; obj.intval := num;
		obj.mnolev := stdObj
	END EnterProc;

	PROCEDURE EnterSysTyp(name: ARRAY OF CHAR; form: SHORTINT; VAR res: Struct);
		VAR obj: Object; typ: Struct;
	BEGIN Insert(name, obj);
		NEW(typ); obj.mode := Typ; obj.typ := typ; obj.marked := TRUE;
		obj.mnolev := sysObj;
		typ.form := form; typ.strobj := obj;
		typ.mno := 0; typ.ref := form; res := typ
	END EnterSysTyp;

	PROCEDURE EnterSysProc(name: ARRAY OF CHAR; num: INTEGER);
		VAR obj: Object;
	BEGIN Insert(name, obj); obj.mode := SProc; obj.typ := notyp; obj.intval := num;
		obj.mnolev := sysObj
	END EnterSysProc;

	PROCEDURE OpenGlobalScope;
		VAR name: ARRAY 1 OF CHAR;
	BEGIN
		name[0] := 0X;
		OpenScope(0, name)
	END OpenGlobalScope;

BEGIN topScope := NIL;
	InitStruct(undftyp, Undef); InitStruct(notyp, NoTyp);
	InitStruct(stringtyp, String); InitStruct(niltyp, NilTyp);
	OpenGlobalScope;

	(*initialization of module SYSTEM*)
	EnterSysProc("LSH", 22);
	EnterSysProc("ROT", 23);
	EnterSysProc("ADR",  9);
	EnterSysProc("OVFL",15);
	EnterSysProc("GET", 24);
	EnterSysProc("PUT", 25);
	EnterSysProc("BIT", 26);
	EnterSysProc("VAL", 27);
	EnterSysProc("NEW", 28);
	EnterSysProc("MOVE",30);
	EnterSysProc("CC",  2);
	EnterSysTyp("BYTE", Byte, bytetyp);
	syslink := topScope.next;
	universe := topScope; topScope.next := NIL;

	EnterTyp("CHAR", Char, chartyp);
	EnterTyp("SET", Set, settyp);
	EnterTyp("REAL", Real, realtyp);
	EnterTyp("INTEGER", Int, inttyp);
	EnterTyp("LONGINT",  LInt, linttyp);
	EnterTyp("LONGREAL", LReal, lrltyp);
	EnterTyp("SHORTINT", SInt, sinttyp);
	EnterTyp("BOOLEAN", Bool, booltyp);
	EnterProc("INC",   16);
	EnterProc("DEC",   17);
	EnterConst("FALSE", 0);
	EnterConst("TRUE",  1);
	EnterProc("HALT",   0);
	EnterProc("NEW",    1);
	EnterProc("ABS",    3);
	EnterProc("CAP",    4);
	EnterProc("ORD",    5);
	EnterProc("ENTIER", 6);
	EnterProc("SIZE",   7);
	EnterProc("ODD",    8);
	EnterProc("MIN",   10);
	EnterProc("MAX",   11);
	EnterProc("CHR",   12);
	EnterProc("SHORT", 13);
	EnterProc("LONG",  14);
	EnterProc("INCL",  18);
	EnterProc("EXCL",  19);
	EnterProc("LEN",   20);
	EnterProc("ASH",   21);
	EnterProc("COPY",  29);

 (* a better place *)
	NEW(wasderef);
	typchk := TRUE
END COCT.
