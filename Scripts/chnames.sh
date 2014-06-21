#!/bin/sh
from_dir=`pwd`
cd $POT
mv docu Docu
mv scripts Scripts
cd lib
mv core Core
cd Core
cp makefile.unx Makefile
mv  _os.c  _OS.c
mv  os.h  OS.h
mv  os.hi  OS.hi
mv  _files.c  _Files.c
mv  files.h    Files.h
mv  files.hi  Files.hi
mv  _reals.c  _Reals.c
mv  reals.h  Reals.h
mv  reals.hi  Reals.hi
mv  potrtl.c  pOtRTL.c
mv  potrtl.h  pOtRTL.h
cd ..
cp makefile.unx Makefile
mv texts.c  Texts.c
mv texts.h  Texts.h
mv texts.hi Texts.hi
mv  strings.c  Strings.c
mv  strings.h  Strings.h
mv  strings.hi  Strings.hi
cd $POT/src
cp makefile.unx Makefile
mv  cocc.c  COCC.c
mv  cocc.h  COCC.h
mv  cocc.hi  COCC.hi
mv  cocd.c  COCD.c
mv  cocd.h  COCD.h
mv  cocd.hi  COCD.hi
mv  coce.c  COCE.c
mv  coce.h  COCE.h
mv  coce.hi  COCE.hi
mv  coch.c  COCH.c
mv  coch.h  COCH.h
mv  coch.hi  COCH.hi
mv  cocj.c  COCJ.c
mv  cocj.h  COCJ.h
mv  cocj.hi  COCJ.hi
mv  cocn.c  COCN.c
mv  cocn.h  COCN.h
mv  cocn.hi  COCN.hi
mv  coco.c  COCO.c
mv  coco.h  COCO.h
mv  coco.hi  COCO.hi
mv  cocp.c  COCP.c
mv  cocp.h  COCP.h
mv  cocp.hi  COCP.hi
mv  cocq.c  COCQ.c
mv  cocq.h  COCQ.h
mv  cocq.hi  COCQ.hi
mv  cocs.c  COCS.c
mv  cocs.h  COCS.h
mv  cocs.hi  COCS.hi
mv  coct.c  COCT.c
mv  coct.h  COCT.h
mv  coct.hi  COCT.hi
mv  cocx.c  COCX.c
mv  cocx.h  COCX.h
mv  cocx.hi  COCX.hi
mv  cocy.c  COCY.c
mv  cocy.h  COCY.h
mv  cocy.hi  COCY.hi
mv  pot.c  POT.c
mv  pot.h  POT.h
mv  pot.hi  POT.hi
mv  pot_comp.c POT.Compile.c
cd $POT/Docu
mv poterr.txt pOtErr.txt
mv obrept.txt OberonReport.txt
mv potnotes.txt pOtNotes.txt
cd $from_dir
