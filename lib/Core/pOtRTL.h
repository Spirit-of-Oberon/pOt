#ifndef pOt__pOtRTL__INC
#define pOt__pOtRTL__INC
/* pOt RTL definitions file, DT Wed Jan 26 1994 */

#if defined(__STDC__) && __STDC__ && !defined(__LCC__)
#  define pOt__ANSI_C 1
#else
#  define pOt__ANSI_C 0
#endif

#if pOt__ANSI_C
#  define pOt__ARGS(args) args
#else
#  define pOt__ARGS(args) ()
#endif

#define pOt__MaxStrLen 127
#define pOt__MaxExts   16

#define pOt_NIL     (void *)0 
#define pOt_FALSE   0
#define pOt_TRUE    1

typedef unsigned char pOt_BYTE_SYSTEM;
typedef char          pOt_BOOLEAN;
typedef char          pOt_CHAR;
typedef char          pOt_SHORTINT;
typedef short         pOt_INTEGER;
typedef long          pOt_LONGINT;
typedef unsigned long pOt_SET;
typedef float         pOt_REAL;
typedef double        pOt_LONGREAL;

/* type descriptors */
 /* 0 - rec, 
    1 - arr of basic type, 
    2 - arr of ptr,
    3 -arr of proc, 
    4 - arr of str or arr */
typedef struct pOt__tag_TypDsc {
  pOt_INTEGER mode;
} pOt__TypDsc;

/* type 0 - records */
typedef struct pOt__tag_RecTypDsc {
  pOt_INTEGER mode;
  pOt_LONGINT size;
  pOt_INTEGER extlev, nstr, nptr, npro;
  struct pOt__tag_RecTypDsc *base_td[pOt__MaxExts];
  struct {pOt_LONGINT poffs; pOt__TypDsc *fld_td;} tab[1];
} pOt__RecTypDsc; 

/* type 1 - array of basic types (neither initialization nor GC is required) */
typedef struct pOt__tag_ArrTypDsc {
  pOt_INTEGER mode;
  pOt_LONGINT nofel, elsize; 
} pOt__ArrTypDsc;

/* type 2, 3 - initialized with zeroes, gc works if 2 */
typedef struct pOt__tag_PtrArrTypDsc {
  pOt_INTEGER mode;
  pOt_LONGINT nofel, elsize; 
} pOt__PtrArrTypDsc;

/* type 4 - records are initialized */
typedef struct pOt__tag_StrArrTypDsc {
  pOt_INTEGER mode;
  pOt_LONGINT nofel, elsize; 
  pOt__TypDsc *base_td;
} pOt__StrArrTypDsc;

typedef pOt__ArrTypDsc *pOt__DynArr;

/* byte array */
typedef struct pOt__tag_BytArr {
  pOt_LONGINT len;
  pOt_BYTE_SYSTEM *data;
} pOt__BytArr;

typedef pOt_CHAR *pOt_String;

extern int pOt__gc_enabled;
extern struct pOt__tag_gc_node *pOt__gc_root;
extern char *pOt__parfilename;

void pOt__init_var pOt__ARGS((pOt__TypDsc **rec, pOt__TypDsc *td));
    
/* halt & trap */
void pOt__halt pOt__ARGS((char *filename, unsigned long line, pOt_SHORTINT trapnum));

/* checks */
pOt_LONGINT pOt__inxchk pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT len, pOt_LONGINT li));
void *pOt__nilchk pOt__ARGS((char *filename, unsigned long line, void *ptr));
pOt_REAL     pOt__rngchk_r pOt__ARGS((char *filename, unsigned long line, pOt_LONGREAL lr));
pOt_LONGINT  pOt__rngchk_li pOt__ARGS((char *filename, unsigned long line, pOt_LONGREAL lr));
pOt_INTEGER  pOt__rngchk_i pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT li));
pOt_SHORTINT pOt__rngchk_si pOt__ARGS((char *filename, unsigned long line, pOt_INTEGER i));
pOt_SHORTINT pOt__rngchk_se pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT i));
unsigned char pOt__rngchk_cn pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT li));
pOt__RecTypDsc **pOt__typchk pOt__ARGS((char *filename, unsigned long line, pOt__RecTypDsc**rec, pOt__RecTypDsc *td, pOt_LONGINT extlev));

/* operations */
pOt_LONGINT pOt__addchk pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT x, pOt_LONGINT y, pOt_SHORTINT typ));
pOt_LONGINT pOt__subchk pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT x, pOt_LONGINT y, pOt_SHORTINT typ));
pOt_LONGINT pOt__mulchk pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT x, pOt_LONGINT y, pOt_SHORTINT typ));
pOt_LONGINT pOt__div pOt__ARGS((pOt_LONGINT x, pOt_LONGINT y));
pOt_LONGINT pOt__divchk pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT x, pOt_LONGINT y, pOt_SHORTINT typ));
pOt_LONGINT pOt__mod pOt__ARGS((pOt_LONGINT x, pOt_LONGINT y));
pOt_LONGINT pOt__modchk pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT x, pOt_LONGINT y, pOt_SHORTINT typ));

pOt_BOOLEAN pOt__typtest pOt__ARGS((pOt__RecTypDsc **rec, pOt__RecTypDsc *td, pOt_LONGINT extlev));

/* strings relations */
pOt_BOOLEAN pOt__cmpss pOt__ARGS((pOt_CHAR*,pOt_CHAR*,pOt_INTEGER op));
pOt_BOOLEAN pOt__cmpsc pOt__ARGS((pOt_CHAR*,pOt_CHAR,pOt_INTEGER op));
pOt_BOOLEAN pOt__cmpcs pOt__ARGS((pOt_CHAR,pOt_CHAR*,pOt_INTEGER op));

/* built-in functions */
void pOt__new pOt__ARGS((char *filename, unsigned long line, pOt__TypDsc ***pp, pOt__TypDsc *td)); /* calls init_var */

pOt_LONGINT pOt__abs pOt__ARGS((pOt_LONGINT x));
pOt_LONGREAL pOt__fabs pOt__ARGS((pOt_LONGREAL x));

pOt_CHAR pOt__cap pOt__ARGS((pOt_CHAR c));
pOt_LONGINT pOt__entier pOt__ARGS((pOt_LONGREAL lr));

pOt_LONGINT pOt__ash pOt__ARGS((pOt_LONGINT x, pOt_LONGINT n));
pOt_LONGINT pOt__lsh pOt__ARGS((pOt_LONGINT x, pOt_LONGINT n));
pOt_LONGINT pOt__rot pOt__ARGS((pOt_LONGINT x, pOt_SHORTINT l, pOt_LONGINT n));

void pOt__copy pOt__ARGS((pOt_CHAR *src, pOt_CHAR *dst));
void pOt__copychk pOt__ARGS((char *filename, unsigned long line, pOt_CHAR *src, pOt_CHAR *dst));

void pOt__get pOt__ARGS((pOt_BYTE_SYSTEM *src, pOt_BYTE_SYSTEM *dst,pOt_LONGINT size));
void pOt__put pOt__ARGS((pOt_BYTE_SYSTEM *dst, pOt_BYTE_SYSTEM *src, pOt_LONGINT size));
void pOt__move pOt__ARGS((pOt_BYTE_SYSTEM *src, pOt_BYTE_SYSTEM *dst, pOt_LONGINT size));
void *pOt__alloc pOt__ARGS((char *filename, unsigned long line, pOt_LONGINT size));

/* strings constants */
extern pOt__ArrTypDsc pOt__str_td[pOt__MaxStrLen+1];
pOt__ArrTypDsc **pOt__set_str_td pOt__ARGS((pOt_CHAR *, pOt__ArrTypDsc *));

/* passing as parameters */
pOt__BytArr pOt__make_byte_arr pOt__ARGS((void*,pOt_INTEGER,pOt_LONGINT));
pOt__BytArr pOt__dup_byte_arr pOt__ARGS((char *filename, unsigned long line, void*,pOt_INTEGER));
void pOt__rm_byte_arr pOt__ARGS((pOt__BytArr));

pOt__ArrTypDsc **pOt__dup_arr pOt__ARGS((char *filename, unsigned long line, pOt__ArrTypDsc**));
pOt__RecTypDsc **pOt__dup_rec pOt__ARGS((char *filename, unsigned long line, pOt__RecTypDsc**,pOt__RecTypDsc*));
void pOt__rm_par pOt__ARGS((pOt__TypDsc**));

/* assignment */
void pOt__arr_assign pOt__ARGS((pOt__ArrTypDsc**,pOt__ArrTypDsc**));
void pOt__rec_assign pOt__ARGS((pOt__RecTypDsc**,pOt__RecTypDsc**));
void pOt__varrec_assign pOt__ARGS((char*filename,unsigned long line,pOt__RecTypDsc**,pOt__RecTypDsc**));

/* system dependant keywords */
#define pOt__interrupt

/* the end */
#endif
