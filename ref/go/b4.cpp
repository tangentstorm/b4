#include<stdio.h>

//-- macros for c++ types ----
#define I int                           // I = any int
#define A int                           // A = specifically an address in vm's ram
#define B int                           // B = int used as boolean
#define C char                          // C = char type
#define U8 char                         // U8= 8 bits
#define V void                          // V = void

//-- c++ syntax shorthand ---
#define $ir I r                         // $ir = declares int return var
#define $r return                       // $r  = returns
#define $rr $r r                        // $rr = returns r
#define $if(p,t,e)if(p)t;else e;        // $if = if/then/else
#define $fri(n)for(I i=0;i<n;i++)       // $fri(n)= for (i=0 to..n.)
#define $slp(s)$fri(s.len)              // $slp(s) = i=0 to len(s)
#define $ith(s)s.buf[i]                 // $ith(s) = s.buf[i]

// args are (mostly) passed on internal stacks,
// so define some macros to streamline the declarations.
#define _op(f) V f(){                   // _op(f) -> f( );
#define iop(f) V f(I i){                // iop(f) -> f(I);
#define sop(f) V f(S s){                // sop(f) -> f(S);
#define aop(f) V f(A a){                // aop(f) -> f(A);
#define lop(f) sop(f) A lb=fnd(s);      // lop(f) -> f(S);


// == fixed length string buffers ============================================

struct sstr{ U8 len; C buf[255];};      // S = small byte-counted string type
typedef struct sstr S;

B seq(S& a, S& b){                      // seq(a,b) = are strings equal?
  $if(a.len != b.len, $r 0,)            //   - they'd have to be same length.
    $slp(a) $if($ith(a)!=$ith(b),$r 0,) //   - and all the chars must match.
  $r 1; }                               //   if still here, they're equal.

V scp(S& a, S& b){                      // scp(a,b) = copy a's data onto b.
  $slp(a) $ith(b)=$ith(a);              //   first the chars.
  b.len=a.len; }                        //   then the length field.


// == stacks ============================================================

// -- index masks. ANDing these with stack pointers and other indices
// make those indices wrap around rather than overflow. this means
// a stack overflow will overwrite the oldest value on that stack,
// but the pointers won't actually escape from the array.

#define mm 0xFFFF                       // memory mask          (cell*64k)
#define dm 0x0FFF                       // data stack mask      (int*4096)
#define am 0x0FFF                       // addr stack mask      (int*4096)
#define vm 0x0FFF                       // local var stack mask (int*4096)
#define sm 0x03FF                       // string table mask (256b*1024=256kb)

// -- registers
A ip=0;                                 // ip  = instruction pointer
A eh=-1;                                // eh  = vm address of error handler.

// -- string buffer table
S ss[sm + 1];                           // ss = table of strings
I sc=0;                                 // sc= string count

// -- ram and stacks.
I m[mm + 1];                            // m = 64-kCell memory area
I d[dm + 1]; I dp=0;                    // d = data stack
I a[am + 1]; I ap=0;                    // a = address stack
I v[vm + 1]; I vp=0;                    // v = local variable stack

// -- c helpers for stack manipulation
//  TODO: use stl stack or make a class since i'm using c++
I $POPA(){$ir=a[ap--];ap&=am;$rr;}      // POPA = get int from addr stack
I $POPD(){$ir=d[dp--];dp&=dm;$rr;}      // POPD = get int from data stack
I $POPV(){$ir=v[vp--];vp&=vm;$rr;}      // POPV = get int from var stack
V $PUTA(I i){a[ap++]=i;ap&=am;}         // PUTA = put i on addr stack
V $PUTD(I i){d[dp++]=i;dp&=dm;}         // PUTD = put i on data stack
V $PUTV(I i){v[vp++]=i;vp&=vm;}         // PUTV = put i on var stack

// helper for string table
I fnd(S& s){ I n=0;                     // fnd(s) = find id of string
  $fri(sc)                              //   linear search of string table
    $if(seq(s,ss[i]), $r i, n++)        //   if found, return index
  n&=sm; scp(s,ss[n]); $r sc=n;}        //   else add string to the table


// == core instruction set ===================================================

// -- instructions ---                  -- stack effects ---
_op(NOP) }                              // nop      -   : do nothing.
iop(LIT) d[dp++]=i; }                   // lit(i)   -i  : puts i on data stack d
_op(PSH) a[ap++]=d[dp--]; }             // psh     i-   : push i from d to a
_op(POP) d[dp++]=a[ap--]; }             // pop      -i  : pop from a onto d
_op(GCH) $PUTD(getchar()); }            // gch      -c  : get char (stdin->d)
_op(PCH) putchar(*(C*)$POPD); }         // pch     c-   : put char (d->stdout)
_op(RET) ip=$POPA(); }                  // ret          : return from subroutine


// == patern matching system =================================================

// registers specific to pattern matching
B ok=1;                                 // ok  = matched so far?
A g0=0, g1=0;                           // g#  = label registers
I gn=0;                                 // gn  = autonumber for label generator
B eot=0;                                // eot = end of text?
A top=-1;                               // top = vm address of start rule

// branching to string labels
lop(brn) ip=lb; }                       // brn    : branch to label
lop(bok) ip=(ok? ip : lb); }            // bok    : branch if ok=1
lop(bno) ip=(ok? lb : ip); }            // bno    : branch if not ok
lop(eno) ip=(ok? lb : eh); }            // eno    : error if not ok

// -- matching instructions
_op(idn) }                              // idn    : read identifier
_op(num) }                              // num    : read number
_op(str) }                              // str    : read 'quoted' string
_op(cll) }                              // cll    : call label
_op(aok) ok=1; }                        // aok    : set ok=1
_op(cps) }                              // cps    : copy string literal to output
_op(cpt) }                              // cpi    : copy last token to output
lop(elb) }                              // lbl    : emit label
_op(out) }                              // out    : newline
lop(adr) top=lb; }                      // adr    : label top rule
_op(end) eot=1; }                       // end    : end of text
#define gn0 $IFE(g0,g0,g0=gn++)
#define gn1 $IFE(g1,g1,g1=gn++)
int main(){ printf("b4:what?\n"); }

