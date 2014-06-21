POT=..

#CC = lcc
#CFLAGS = 
CC = gcc
CFLAGS = -O2 -s
CCLD = gcc
CLDFLAGS = -s
CINCLUDES = -I$(POT)/lib -I$(POT)/lib/Core
CLIBS = $(POT)/lib/libpOt.a $(POT)/lib/Core/libpOtCore.a -lm -lc
MODNAME = POT
ENTRY = Compile

OBJ = COCC.o COCD.o COCE.o COCH.o COCJ.o COCN.o COCO.o COCP.o \
  COCQ.o COCS.o COCT.o COCX.o COCY.o POT.o $(MODNAME).$(ENTRY).o 

TARGET = pOt

.c.o:
	$(CC) $(CFLAGS) $(CINCLUDES) -c $<

.SUFFIXES: .c .o .h

all: $(TARGET)

clean:
	-rm -f *.o pOt
 
cleanall:
	-rm -f *.o Coco *.c *.h *.hi *.Cym
 
$(TARGET): ${OBJ} $(POT)/lib/libpOt.a $(POT)/lib/Core/libpOtCore.a
	$(CCLD) -o $(TARGET) $(CLDFLAGS) $(OBJ) $(CLIBS)

$(MODNAME).$(ENTRY).c : $(POT)/lib/Core/main.tpl
	sed -e "s/<ModName>/${MODNAME}/g" -e "s/<Command>/${ENTRY}/g" < $(POT)/lib/Core/main.tpl > ${MODNAME}.${ENTRY}.c

$(MODNAME).$(ENTRY).o: $(MODNAME).$(ENTRY).c POT.h 
# DO NOT DELETE THIS LINE -- make depend depends on it.

COCC.o: COCC.c COCT.h COCD.h COCQ.h COCN.h COCJ.h COCO.h COCH.h COCY.h COCC.hi
COCD.o: COCD.c COCS.h COCT.h COCQ.h COCN.h COCY.h COCO.h COCD.hi
COCE.o: COCE.c COCS.h COCT.h COCQ.h COCN.h COCJ.h COCX.h COCE.hi
COCH.o: COCH.c COCS.h COCT.h COCD.h COCQ.h COCP.h COCH.hi
COCJ.o: COCJ.c COCT.h COCQ.h COCN.h COCJ.hi
COCN.o: COCN.c COCT.h COCQ.h COCN.hi
COCO.o: COCO.c COCS.h COCO.hi
COCP.o: COCP.c COCT.h COCQ.h COCN.h COCP.hi
COCQ.o: COCQ.c COCS.h COCT.h COCO.h COCQ.hi
COCS.o: COCS.c COCS.hi
COCT.o: COCT.c COCS.h COCT.hi
COCX.o: COCX.c COCT.h COCQ.h COCN.h COCJ.h COCX.hi
COCY.o: COCY.c COCT.h COCQ.h COCN.h COCJ.h COCO.h COCY.hi
POT.o: POT.c COCS.h COCO.h COCT.h COCE.h COCH.h COCD.h COCC.h POT.hi
