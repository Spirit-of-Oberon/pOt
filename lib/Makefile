POT = ..

#CC = lcc
#CFLAGS = 
CC = gcc
CFLAGS = -O2
CINCLUDES = -I$(POT)/lib/Core -I$(POT)/lib
CLIBS =
AR = ar
RANLIB = ranlib
SYSTEM = 

OBJ = Texts.o Strings.o
TARGET = libpOt.a

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
	
Texts.o: Texts.c Core/pOtRTL.h Core/Files.h Core/Reals.h
Strings.o: Strings.c Core/pOtRTL.h
