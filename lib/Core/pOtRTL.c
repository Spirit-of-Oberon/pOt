/* pOt RTL implementation file, DT Wed Jan 26 1994 */

#include <pOtRTL.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#ifdef __sun__
#define memmove(d,s,size) bcopy(s,d,size)
#endif

#define PtrSize 4

#define MinChar 0x0
#define MaxChar 0x0FF
#define MinBool 0
#define MaxBool 1
#define MinSInt -128
#define MaxSInt 127
#define MinInt -32768
#define MaxInt 32767
#define MinLInt ((pOt_LONGINT)0x80000000)
#define MaxLInt 0x7FFFFFFF

#define MinReal -3.40282347E+38
#define MaxReal 3.40282347E+38
#define MinLReal -1.7976931348623157E+308
#define MaxLReal 1.7976931348623157E+308
#define MinSet 0
#define MaxSet 31

typedef struct pOt__tag_gc_node {
  struct pOt__tag_gc_node *next;
  void *pvar[1];
} pOt__gc_node;

int pOt__gc_enabled = 1;
pOt__gc_node *pOt__gc_root = pOt_NIL;
char *pOt__parfilename = NULL;

extern void pOt__gc pOt__ARGS(());
static void pOt__gc_register pOt__ARGS((void *p, pOt_LONGINT size));

void pOt__init_var(rec,td)
	pOt__TypDsc **rec; pOt__TypDsc *td;
{
  *rec = td;
  switch(td->mode) {
    case 0: /* rec */ {
      pOt_LONGINT i, stop;
      pOt__RecTypDsc *rtd = (pOt__RecTypDsc *)td;

      for(;;) {
        stop = rtd->nstr; i = 0;
        while(i != stop) {
          pOt__init_var((pOt__TypDsc**)((char *)rec + rtd->tab[i].poffs), rtd->tab[i].fld_td);
          i++;
        }
        stop += rtd->nptr + rtd->npro;
        while(i != stop) {
          *(void **)((char *)rec + rtd->tab[i++].poffs) = pOt_NIL;
        }
        if(!(i = rtd->extlev)) break;
        rtd = rtd->base_td[i-1];
      }
    }
    break;
    case 1: /* basic arr */
      /* no initialization required */
    break;
    case 2: /* ptr arr */
    case 3: /* proc arr */
    {
      pOt_LONGINT i;
      pOt__PtrArrTypDsc *atd = (pOt__PtrArrTypDsc *)td;

      i = 0; rec = (pOt__TypDsc**)((char*)rec + sizeof(pOt__PtrArrTypDsc *));
      while(i++ != atd->nofel) {
        *rec = pOt_NIL;
        rec = (pOt__TypDsc**)((char*)rec + atd->elsize);
      }
    }
    break;
    case 4: /* rec arr */ {
      pOt_LONGINT i;
      pOt__StrArrTypDsc *atd = (pOt__StrArrTypDsc *)td;

      i = 0; rec = (pOt__TypDsc**)((char*)rec + sizeof(pOt__StrArrTypDsc *));
      while(i++ != atd->nofel) {
        pOt__init_var(rec, atd->base_td);
        rec = (pOt__TypDsc**)((char*)rec + atd->elsize);
      }
    }
    break;
  }
}

/* halt */
void pOt__halt(filename,line,trapnum)
	char *filename; unsigned long line; pOt_SHORTINT trapnum;
{
  printf("\n%s(%lu):trap %i\n", filename, line, trapnum);
  exit(trapnum);
}

/* checks */
pOt_LONGINT pOt__inxchk(filename,line,len,li)
	char *filename; unsigned long line; pOt_LONGINT len; pOt_LONGINT li;
{
  if((0 > li) || (li >= len)) {
    pOt__halt(filename,line,3);
  }
  return li;
}

void *pOt__nilchk(filename,line,ptr)
	char *filename; unsigned long line; void *ptr;
{
  if(ptr == NULL) pOt__halt(filename,line,5);
  return ptr;
}

pOt_REAL pOt__rngchk_r(filename,line,lr)
	char *filename; unsigned long line; pOt_LONGREAL lr;
{
  if((lr < MinReal) || (MaxReal < lr)) pOt__halt(filename,line,4);
  return (pOt_REAL)lr;
}

pOt_LONGINT  pOt__rngchk_li(filename,line,lr)
	char *filename; unsigned long line; pOt_LONGREAL lr;
{
  pOt_LONGREAL flr;
  flr = floor(lr);
  if((flr < (pOt_LONGREAL)MinLInt) || ((pOt_LONGREAL)MaxLInt < flr)) pOt__halt(filename,line,4);
  return (pOt_LONGINT)flr;
}

pOt_INTEGER  pOt__rngchk_i(filename,line,li)
	char *filename; unsigned long line; pOt_LONGINT li;
{
  if((li < MinInt) || (MaxInt < li)) pOt__halt(filename,line,4);
  return (pOt_INTEGER)li;
}

pOt_SHORTINT pOt__rngchk_si(filename,line,i)
	char *filename; unsigned long line; pOt_INTEGER i;
{
  if((i < MinSInt) || (MaxSInt < i)) pOt__halt(filename,line,4);
  return (pOt_SHORTINT)i;
}

pOt_SHORTINT pOt__rngchk_se(filename,line,i)
	char *filename; unsigned long line; pOt_LONGINT i;
{
  if((i < MinSet) || (MaxSet < i)) pOt__halt(filename,line,4);
  return (pOt_SHORTINT)i;
}

unsigned char pOt__rngchk_cn(filename,line,li)
	char *filename; unsigned long line; pOt_LONGINT li;
{
  if((li < MinChar) || (MaxChar < li)) pOt__halt(filename,line,4);
  return (unsigned char)li;
}

pOt__RecTypDsc **pOt__typchk(filename,line,rec,td,extlev)
	char *filename; unsigned long line; pOt__RecTypDsc**rec; pOt__RecTypDsc *td; pOt_LONGINT extlev;
{
  if((rec != pOt_NIL) && ((((*rec)->extlev > extlev) && ((*rec)->base_td[extlev] == td)) || ((*rec) == td))) return rec;
  pOt__halt(filename,line,18);
}

/* operations */
pOt_LONGINT pOt__div(x,y)
	pOt_LONGINT x; pOt_LONGINT y;
{
  if(x >= 0) return x/y; return -((-x - 1)/y + 1);
}

pOt_LONGINT pOt__addchk(filename,line,x,y,typ)
	char *filename; unsigned long line; pOt_LONGINT x; pOt_LONGINT y; pOt_SHORTINT typ;
{
  x += y;
  switch(typ) {
  case 4: if((MinSInt > x) || (x > MaxSInt)) pOt__halt(filename,line,4); break;
  case 5: if((MinInt > x) || (x > MaxInt)) pOt__halt(filename,line,4); break;
  case 6: break;
  default: pOt__halt(filename,line,16); break;
  }
  return x;
}

pOt_LONGINT pOt__subchk(filename,line,x,y,typ)
	char *filename; unsigned long line; pOt_LONGINT x; pOt_LONGINT y; pOt_SHORTINT typ;
{
  x -= y;
  switch(typ) {
  case 4: if((MinSInt > x) || (x > MaxSInt)) pOt__halt(filename,line,4); break;
  case 5: if((MinInt > x) || (x > MaxInt)) pOt__halt(filename,line,4); break;
  case 6: break;
  default: pOt__halt(filename,line,16); break;
  }
  return x;
}

pOt_LONGINT pOt__mulchk(filename,line,x,y,typ)
	char *filename; unsigned long line; pOt_LONGINT x; pOt_LONGINT y; pOt_SHORTINT typ;
{
  x *= y;
  switch(typ) {
  case 4: if((MinSInt > x) || (x > MaxSInt)) pOt__halt(filename,line,4); break;
  case 5: if((MinInt > x) || (x > MaxInt)) pOt__halt(filename,line,4); break;
  case 6: break;
  default: pOt__halt(filename,line,16); break;
  }
  return x;
}


pOt_LONGINT pOt__divchk(filename,line,x,y,typ)
	char *filename; unsigned long line; pOt_LONGINT x; pOt_LONGINT y; pOt_SHORTINT typ;
{
  if(y == 0) pOt__halt(filename,line,6);
  if(y < 0) pOt__halt(filename,line,7);
  if(x >= 0) return x/y; return -((-x - 1)/y + 1);
}

pOt_LONGINT pOt__mod(x,y)
	pOt_LONGINT x; pOt_LONGINT y;
{
  if(x >= 0) return x%y; return y - 1 - (-x-1)%y;
}

pOt_LONGINT pOt__modchk(filename,line,x,y,typ)
	char *filename; unsigned long line; pOt_LONGINT x; pOt_LONGINT y; pOt_SHORTINT typ;
{
  if(y == 0) pOt__halt(filename,line,6);
  if(y < 0) pOt__halt(filename,line,7);
  if(x >= 0) return x%y; return y - 1 - (-x-1)%y;
}

pOt_BOOLEAN pOt__typtest(rec,td,extlev)
	pOt__RecTypDsc **rec; pOt__RecTypDsc *td; pOt_LONGINT extlev;
{
  if((*rec)->extlev > extlev) return (*rec)->base_td[extlev] == td;
  return (*rec) == td;
}

/* strings relations */
pOt_BOOLEAN pOt__cmpss(s1,s2,op)
	pOt_CHAR *s1;pOt_CHAR *s2;pOt_INTEGER op;
{
  pOt_LONGINT i;
  s1 += sizeof(pOt__ArrTypDsc*); s2 += sizeof(pOt__ArrTypDsc*);
  i = 0; while((s1[i] != '\0') && (s1[i] == s2[i])) i++;
  if(s1[i] == s2[i]) {
    switch(op) {
    case 9: case 12: case 14: return pOt_TRUE;
    case 10: case 11: case 13: return pOt_FALSE;
    }
  }
  else {
    switch(op) {
    case 9: return pOt_FALSE;
    case 10: return pOt_TRUE;
    case 11: case 12: return s1[i] < s2[i];
    case 13: case 14: return s1[i] > s2[i];
    }
  }
}

pOt_BOOLEAN pOt__cmpsc(s1,c2,op)
	pOt_CHAR *s1;pOt_CHAR c2;pOt_INTEGER op;
{
  s1 += sizeof(pOt__ArrTypDsc*);
  if(s1[0] == c2) {
    switch(op) {
    case 9: return s1[1] == '\0';
    case 10: return s1[1] != '\0';
    case 11: return pOt_FALSE;
    case 12: return s1[1] == '\0';
    case 13: return s1[1] > '\0';
    case 14: return s1[1] >= '\0';
    }
  }
  else {
    switch(op) {
    case 9: return pOt_FALSE;
    case 10: return pOt_TRUE;
    case 11: case 12: return s1[0] < c2;
    case 13: case 14: return s1[0] > c2;
    }
  }
}

pOt_BOOLEAN pOt__cmpcs(c1,s2,op)
	pOt_CHAR c1;pOt_CHAR *s2;pOt_INTEGER op;
{
  s2 += sizeof(pOt__ArrTypDsc*);
  if(c1 == s2[0]) {
    switch(op) {
    case 9: return s2[1] == '\0';
    case 10: return s2[1] != '\0';
    case 11: return s2[1] > '\0';
    case 12: return s2[1] >= '\0';
    case 13: return pOt_FALSE;
    case 14: return s2[1] == '\0';
    }
  }
  else {
    switch(op) {
    case 9: return pOt_FALSE;
    case 10: return pOt_TRUE;
    case 11: case 12: return c1 < s2[0];
    case 13: case 14: return c1 > s2[0];
    }
  }
}

/* built-in functions */
void pOt__new(filename,line,pp,td)
	char *filename; unsigned long line; pOt__TypDsc ***pp; pOt__TypDsc *td;
{
  pOt_LONGINT size;
  
  if(td->mode == 0) size = ((pOt__RecTypDsc*)td)->size;
  else size = sizeof(pOt__ArrTypDsc*) + ((pOt__ArrTypDsc*)td)->nofel*((pOt__ArrTypDsc*)td)->elsize;
  *pp = (pOt__TypDsc**)malloc(size);
  if(*pp == NULL) {
    pOt__gc(); *pp = (pOt__TypDsc**)malloc(size);
    if(*pp == NULL) pOt__halt(filename,line,1);
  }
  pOt__init_var(*pp, td);
  pOt__gc_register(*pp, size);
}

pOt_LONGINT pOt__abs(x) pOt_LONGINT x; {if(x < 0) return -x; return x;}
pOt_LONGREAL pOt__fabs(x) pOt_LONGREAL x; {if(x < 0) return -x; return x;}

pOt_CHAR pOt__cap(c) pOt_CHAR c; {return toupper(c);}
pOt_LONGINT pOt__entier(lr) pOt_LONGREAL lr; {return floor(lr);}

pOt_LONGINT pOt__ash(x,n)
	pOt_LONGINT x; pOt_LONGINT n;
{
  if(n>0) {if(x>0) return x << n; return -(-x << n);}
  else {if(x>0) return x >> -n; return -(-x >> -n);}
} 

pOt_LONGINT pOt__lsh(x,n)
	pOt_LONGINT x; pOt_LONGINT n;
{
  if(n>0) return (unsigned long)x << n; return (unsigned long)x >> -n;
}

pOt_LONGINT pOt__rot(x,l,n)
	pOt_LONGINT x; pOt_SHORTINT l; pOt_LONGINT n;
{
  unsigned long a,b;

  if(n > 0) { n %= l; a = (unsigned long)x << n; b = (unsigned long)x >> (l - n);}
  else {n = -n % l; a = (unsigned long)x >> n; b = (unsigned long)x << (l - n);}
  return a | b;
}

void pOt__copy(src,dst)
	pOt_CHAR *src; pOt_CHAR *dst;
{
  src += PtrSize; dst += PtrSize; while(*(dst++) = *(src++));
}

void pOt__copychk(filename,line,src,dst)
	char *filename; unsigned long line; pOt_CHAR *src; pOt_CHAR *dst;
{
  pOt_LONGINT len, i;

  len = (*(pOt__ArrTypDsc**)src)->nofel < (*(pOt__ArrTypDsc**)dst)->nofel?
          (*(pOt__ArrTypDsc**)src)->nofel : (*(pOt__ArrTypDsc**)dst)->nofel;
  src += PtrSize; dst += PtrSize; i = 0;
  for(;;) {
    if((dst[i] = src[i]) == '\0') break;
    if(++i == len) pOt__halt(filename, line, 3);
  }
}

void pOt__get(src,dst,size)
	pOt_BYTE_SYSTEM *src; pOt_BYTE_SYSTEM *dst;pOt_LONGINT size;
{
  memmove((char*)dst,(char*)src,size);
}
  
void pOt__put(dst,src,size)
	pOt_BYTE_SYSTEM *dst; pOt_BYTE_SYSTEM *src; pOt_LONGINT size;
{
  memmove((char*)dst,(char*)src,size);
}

void pOt__move(src,dst,size)
	pOt_BYTE_SYSTEM *src; pOt_BYTE_SYSTEM *dst; pOt_LONGINT size;
{
  memmove((char*)dst,(char*)src,size);
}
  
void *pOt__alloc(filename,line,size)
	char *filename; unsigned long line; pOt_LONGINT size;
{
  void *pp;
  pp = malloc(size);
  if(pp == NULL) {
    pOt__gc(); pp = malloc(size);
    if(pp == NULL) pOt__halt(filename,line,1);
  }  
  return pp;
}

/* strings constants */
pOt__ArrTypDsc pOt__str_td[pOt__MaxStrLen+1] = {
  {1,   1, 1}, {1,   2, 1}, {1,   3, 1}, {1,   4, 1}, {1,   5, 1}, 
  {1,   6, 1}, {1,   7, 1}, {1,   8, 1}, {1,   9, 1}, {1,  10, 1}, 
  {1,  11, 1}, {1,  12, 1}, {1,  13, 1}, {1,  14, 1}, {1,  15, 1}, 
  {1,  16, 1}, {1,  17, 1}, {1,  18, 1}, {1,  19, 1}, {1,  20, 1}, 
  {1,  21, 1}, {1,  22, 1}, {1,  23, 1}, {1,  24, 1}, {1,  25, 1}, 
  {1,  26, 1}, {1,  27, 1}, {1,  28, 1}, {1,  29, 1}, {1,  30, 1}, 
  {1,  31, 1}, {1,  32, 1}, {1,  33, 1}, {1,  34, 1}, {1,  35, 1}, 
  {1,  36, 1}, {1,  37, 1}, {1,  38, 1}, {1,  39, 1}, {1,  40, 1}, 
  {1,  41, 1}, {1,  42, 1}, {1,  43, 1}, {1,  44, 1}, {1,  45, 1},
  {1,  46, 1}, {1,  47, 1}, {1,  48, 1}, {1,  49, 1}, {1,  50, 1}, 
  {1,  51, 1}, {1,  52, 1}, {1,  53, 1}, {1,  54, 1}, {1,  55, 1}, 
  {1,  56, 1}, {1,  57, 1}, {1,  58, 1}, {1,  59, 1}, {1,  60, 1}, 
  {1,  61, 1}, {1,  62, 1}, {1,  63, 1}, {1,  64, 1}, {1,  65, 1}, 
  {1,  66, 1}, {1,  67, 1}, {1,  68, 1}, {1,  69, 1}, {1,  70, 1}, 
  {1,  71, 1}, {1,  72, 1}, {1,  73, 1}, {1,  74, 1}, {1,  75, 1}, 
  {1,  76, 1}, {1,  77, 1}, {1,  78, 1}, {1,  79, 1}, {1,  80, 1}, 
  {1,  81, 1}, {1,  82, 1}, {1,  83, 1}, {1,  84, 1}, {1,  85, 1}, 
  {1,  86, 1}, {1,  87, 1}, {1,  88, 1}, {1,  89, 1}, {1,  90, 1}, 
  {1,  91, 1}, {1,  92, 1}, {1,  93, 1}, {1,  94, 1}, {1,  95, 1}, 
  {1,  96, 1}, {1,  97, 1}, {1,  98, 1}, {1,  99, 1}, {1, 100, 1}, 
  {1, 101, 1}, {1, 102, 1}, {1, 103, 1}, {1, 104, 1}, {1, 105, 1},
  {1, 106, 1}, {1, 107, 1}, {1, 108, 1}, {1, 109, 1}, {1, 110, 1}, 
  {1, 111, 1}, {1, 112, 1}, {1, 113, 1}, {1, 114, 1}, {1, 115, 1}, 
  {1, 116, 1}, {1, 117, 1}, {1, 118, 1}, {1, 119, 1}, {1, 120, 1}, 
  {1, 121, 1}, {1, 122, 1}, {1, 123, 1}, {1, 124, 1}, {1, 125, 1},
  {1, 126, 1}, {1, 127, 1}, {1, 128, 1}
};

pOt__ArrTypDsc **pOt__set_str_td(str,td)
	pOt_CHAR *str; pOt__ArrTypDsc *td;
{
  *(pOt__ArrTypDsc**)str = td;
  return (pOt__ArrTypDsc**)str;
}

/* passing as parameters */
pOt__BytArr pOt__make_byte_arr(var,form,size)
	void *var;pOt_INTEGER form;pOt_LONGINT size;
{ 
  pOt__BytArr ba;
  if(size == 0) {
    switch(form) {
    case 0: return *(pOt__BytArr*)var;
    case 1: 
      ba.len = (*(pOt__ArrTypDsc**)var)->nofel; 
      ba.data = (pOt_BYTE_SYSTEM*)var + sizeof(pOt__ArrTypDsc*);
    break;
    case 2: 
      ba.len = (*(pOt__ArrTypDsc**)var)->nofel*(*(pOt__ArrTypDsc**)var)->elsize;
      ba.data = (pOt_BYTE_SYSTEM*)var + sizeof(pOt__ArrTypDsc*);
    break;
    case 3:
      ba.len = (*(pOt__RecTypDsc**)var)->size - sizeof(pOt__RecTypDsc*);
      ba.data = (pOt_BYTE_SYSTEM*)var + sizeof(pOt__RecTypDsc*);
    break;
    }
  }
  else {
    ba.len = size;
    ba.data = (pOt_BYTE_SYSTEM*)var;
  }
  return ba;
}

pOt__BytArr pOt__dup_byte_arr(filename,line,var,form)
	char *filename; unsigned long line; void*var;pOt_INTEGER form;
{
  pOt__BytArr ba;
  switch(form) {
  case 0: 
    ba.len = ((pOt__BytArr*)var)->len;
    if((ba.data = (pOt_BYTE_SYSTEM*)malloc(ba.len)) == NULL) pOt__halt(filename,line,2);
    memcpy((char*)ba.data, (char*)((pOt__BytArr*)var)->data, ba.len);
  break;
  case 1:
    ba.len = (*(pOt__ArrTypDsc**)var)->nofel;
    if((ba.data = (pOt_BYTE_SYSTEM*)malloc(ba.len)) == NULL) pOt__halt(filename,line,2);
    memcpy((char*)ba.data, (char*)((pOt_BYTE_SYSTEM*)var + sizeof(pOt__ArrTypDsc*)), ba.len);
  break;
  }
  return ba;
}

void pOt__rm_byte_arr(ba) pOt__BytArr ba;
{
  free(ba.data);
}

pOt__ArrTypDsc **pOt__dup_arr(filename,line,var)
	char *filename; unsigned long line; pOt__ArrTypDsc**var;
{
  pOt__ArrTypDsc **arr;
  pOt_LONGINT size;

  size = (*(pOt__ArrTypDsc**)var)->nofel*(*(pOt__ArrTypDsc**)var)->elsize + sizeof(pOt__ArrTypDsc*);
  if((arr = (pOt__ArrTypDsc**)malloc(size)) == NULL) pOt__halt(filename,line,2);
  memcpy((char*)arr, (char*)var, size);

  return arr;
}  

pOt__RecTypDsc **pOt__dup_rec(filename,line,var,td)
	char *filename; unsigned long line; pOt__RecTypDsc**var;pOt__RecTypDsc*td;
{
  pOt__RecTypDsc **rec;

  if((rec = (pOt__RecTypDsc**)malloc(td->size)) == NULL) pOt__halt(filename,line,2);
  memcpy((char*)(rec+1), (char*)(var+1), td->size - sizeof(pOt__RecTypDsc*));
  *rec = td;
  return rec;
}

void pOt__rm_par(var)
	pOt__TypDsc **var;
{
  free(var);
}

/* assignment */
void pOt__arr_assign(dst,src)
	pOt__ArrTypDsc**dst; pOt__ArrTypDsc**src;
{
  memcpy((char*)(dst+1),(char*)(src+1), (*dst)->nofel*(*dst)->elsize);
}

void pOt__rec_assign(dst,src)
	pOt__RecTypDsc**dst;pOt__RecTypDsc**src;
{
  memcpy((char*)(dst+1), (char*)(src+1), (*dst)->size - sizeof(pOt__RecTypDsc*));
}

void pOt__varrec_assign(filename,line,dst,src)
	char*filename; unsigned long line; pOt__RecTypDsc**dst;pOt__RecTypDsc**src;
{
  if((*dst)->extlev > (*src)->extlev) pOt__halt(filename,line,19);
  memcpy((char*)(dst+1), (char*)(src+1), (*dst)->size - sizeof(pOt__RecTypDsc*));
}

/* system dependant keywords */
#define pOt__interrupt 

/* GC */

extern pOt_LONGINT pOt__gc_heapthreshold;

typedef struct pOt__gc_tag_HeapNodeDesc {
  void *chunk;
  struct pOt__gc_tag_HeapNodeDesc *next;
} pOt__gc_HeapNodeDesc;
typedef pOt__gc_HeapNodeDesc *pOt__gc_HeapNode;

static pOt__gc_HeapNode pOt__gc_marked = pOt_NIL, pOt__gc_heap = pOt_NIL;
static long pOt__gc_heapdelta = 0L;

static void pOt__gc_register(p,size)
	void *p; pOt_LONGINT size;
{
  pOt__gc_HeapNode node;
  node = (pOt__gc_HeapNode)malloc(sizeof(pOt__gc_HeapNodeDesc));
  if(node == NULL) {
    pOt__gc();
    node = (pOt__gc_HeapNode)malloc(sizeof(pOt__gc_HeapNodeDesc));
    if(node == NULL) pOt__halt(__FILE__,__LINE__,20);
  }
  node->next = pOt__gc_heap;
  pOt__gc_heap = node;
  node->chunk = p;
  pOt__gc_heapdelta += size;
  
  if(pOt__gc_heapdelta >= pOt__gc_heapthreshold) pOt__gc();
}

static void *pOt__gc_markptr(p)
	void *p;
{
  pOt__gc_HeapNode node, node1;

  node = pOt__gc_heap;
  if(node == pOt_NIL) return pOt_NIL;
  else if(node->chunk == p) {
    pOt__gc_heap = node->next;
    node->next = pOt__gc_marked;
    pOt__gc_marked = node;
  } else {
    while((node->next != pOt_NIL) && (node->next->chunk != p)) node = node->next;
    if(node->next != pOt_NIL) {
      node1 = node->next;
      node->next = node1->next;
      node1->next = pOt__gc_marked;
      pOt__gc_marked = node1;
    } else return pOt_NIL;
  }
  return p;
}

void pOt__gc_markvar(v)
	pOt__TypDsc **v;
{
  void *p; 
  
  switch((*v)->mode) {
  case 0: /* rec */ {
    pOt_LONGINT i, stop;
    pOt__RecTypDsc *rtd = *(pOt__RecTypDsc**)v;
    for(;;) {
      stop = rtd->nstr; i = 0;
      while(i != stop) {
        pOt__gc_markvar((pOt__TypDsc**)((char*)v + rtd->tab[i].poffs));
        i++;
      }
      stop += rtd->nptr;
      while(i != stop) {
        p = *(void**)((char*)v + rtd->tab[i].poffs);
        if((p != pOt_NIL) && (pOt__gc_markptr(p) != pOt_NIL)) {
          pOt__gc_markvar((pOt__TypDsc**)p);
        }
        i++;
      }
      if(!(i = rtd->extlev)) break;
      rtd = rtd->base_td[i-1];
    }
  }
  break;
  case 1:
  break;
  case 2: {
    pOt_LONGINT i;
    pOt__PtrArrTypDsc *atd = *(pOt__PtrArrTypDsc**)v;

    i = 0;  v = (pOt__TypDsc**)((char*)v + sizeof(pOt__PtrArrTypDsc*));
    while(i != atd->nofel) {
      p = *v;
      if((p != pOt_NIL) && (pOt__gc_markptr(p) != pOt_NIL)) {
        pOt__gc_markvar((pOt__TypDsc**)p);
      }
      v = (pOt__TypDsc**)((char*)v + atd->elsize);
      i++;
    }
    
  }
  break;
  case 3:
  break;
  case 4: {
    pOt_LONGINT i;
    pOt__StrArrTypDsc *atd = *(pOt__StrArrTypDsc**)v;

    i = 0;  v = (pOt__TypDsc**)((char*)v + sizeof(pOt__StrArrTypDsc*));
    while(i != atd->nofel) {
      pOt__gc_markvar(v);
      v = (pOt__TypDsc**)((char*)v + atd->elsize);
      i++;
    }
  }
  break;
  default:
    pOt__halt(__FILE__,__LINE__,21);
  break;
  }
}

static void pOt__gc_mark()
{
  pOt__gc_node *frame;
  pOt_LONGINT i;  
  pOt__TypDsc ***pptr, **pstr;

  frame = pOt__gc_root;
  while(frame != pOt_NIL) {
    i = 0;
    for(;;) {
      pstr = (pOt__TypDsc**)frame->pvar[i];
      if(pstr == pOt_NIL) break;
      pOt__gc_markvar(pstr);
      i++;
    }
    frame = frame->next;
    i = 0;
    for(;;) {
      pptr = (pOt__TypDsc***)frame->pvar[i];
      if(pptr == pOt_NIL) break;
      if((*pptr != pOt_NIL) && (pOt__gc_markptr(*pptr)!= pOt_NIL)) {
        pOt__gc_markvar(*pptr);
      }
      i++;
    }
    frame = frame->next;
  }   
} 

static void pOt__gc_sweep()
{
  pOt__gc_HeapNode node;
  node = pOt__gc_heap;
  while(node != pOt_NIL) {
    free(node->chunk); pOt__gc_heap = node->next; 
    free(node); node = pOt__gc_heap;
  }
  pOt__gc_heap = pOt__gc_marked; pOt__gc_marked = pOt_NIL;
  pOt__gc_heapdelta = 0L;
}

void pOt__gc()
{
  if(pOt__gc_enabled) {
    pOt__gc_mark();
    pOt__gc_sweep();
  }
}

/* the end */
