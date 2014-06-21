POT = ../..

CC=gcc
CFLAGS = -O2
#CC=lcc
#CFLAGS = 
CINCLUDES = -I.
CLIBS =
AR = ar
RANLIB = ranlib
SYSTEM = 

OBJ = pOtRTL.o _Files.o _Reals.o _OS.o
TARGET = libpOtCore.a

.c.o:
	$(CC) $(CFLAGS) $(CINCLUDES) -c $<

all: $(TARGET)

clean:
	-rm -f *.o *.a
 
cleanall:
	-rm -f *.o Coco *.c *.h *.hi *.Cym
 
$(TARGET): $(OBJ)
	$(AR) rcv $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)
	
pOtRTL.o: pOtRTL.c pOtRTL.h
_Files.o: _Files.c pOtRTL.h
_Reals.o: _Reals.c pOtRTL.h
_OS.o: _OS.c pOtRTL.h
