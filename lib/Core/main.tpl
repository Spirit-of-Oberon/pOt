#include <pOtRTL.h>
#include "<ModName>.h"
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#ifdef __sun__
#define atexit(x) on_exit(x,NULL)
# ifndef L_tmpnam
# define L_tmpnam 1024
# endif
#endif

#ifdef __MSDOS__
# ifdef __TURBOC__
extern unsigned _stklen = 32768;
# endif
#endif

pOt_LONGINT pOt__gc_heapthreshold = 65536;

void catch(signo)
	int signo;
{
  switch(signo) {
    case SIGINT: pOt__halt(__FILE__,__LINE__,22); break;
    case SIGFPE: pOt__halt(__FILE__,__LINE__,14); break;
    default: pOt__halt(__FILE__,__LINE__,16); break;
  }
} 
  
static void rmparfile()
{
  unlink(pOt__parfilename);
  free(pOt__parfilename);
}
  
int main(argc, argv)
	int argc;
	char **argv;
{
  FILE *parfp, *nestedfp;
  int ch;

  parfp = NULL;
  nestedfp = NULL;

  signal(SIGINT, catch);
  signal(SIGFPE, catch);

  if((pOt__parfilename = malloc(L_tmpnam))==NULL) {
    fprintf(stderr, "%s\n", "Not enough memory to start the program.");
    exit(255);
  }
  tmpnam(pOt__parfilename);
  parfp = fopen(pOt__parfilename, "w");
  if(parfp == NULL) {
    fprintf(stderr, "%s %s.\n", "Cannot create ", pOt__parfilename);
    exit(255);
  }
  while(*(++argv)) {
    if((*argv)[0] == '@') {
      nestedfp = fopen((*argv + 1), "r");
      if(nestedfp == NULL) fprintf(stderr, "%s not found.\n", (*argv + 1));
      else {
        while((ch = fgetc(nestedfp)) != EOF) fputc(ch, parfp);
        fputc('\n', parfp);
        fclose(nestedfp);
      }
    } else fprintf(parfp, "%s\n", *argv);
  }
  fclose(parfp);
  atexit(rmparfile);
  
  pOt_<ModName>__body();
  pOt_<Command>_<ModName>();

  exit(0);
  return 0;
}
