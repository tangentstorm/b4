/* b4.h - b4 virtual machine core */
#ifndef B4_H
#define B4_H

#include <stdint.h>

typedef int32_t val;   /* b4 cell value: signed 32-bit */
typedef uint8_t byte;

/* memory layout constants */
#define MAXCELL  16383
#define MAXBYTE  ((MAXCELL+1)*4-1)
#define STACKSZ  256
/* stacks live at the top of memory, control above data */
#define MINHEAP  256
#define MAXHEAP  (MAXBYTE - 2*STACKSZ*4 - 256)

/* register offsets into mem[] (byte addresses) */
/* named registers: @A=4, @B=8, ... @_=124 */
#define REGA(ch) (4*((ch)-'@'))
/* system registers live at byte offsets $80-$FF */
#define RGO   REGA('\\')   /* 0x70 ^\ main entry point */
#define RLP   REGA('^')    /* 0x78 ^^ linked list pointer */
#define RHP   REGA('_')    /* 0x7C ^_ here pointer */
/* private regs: byte 0x84..0xFF */
#define RIP   (4*0x21)  /* instruction pointer */
#define RDS   (4*0x23)  /* data stack height */
#define RCS   (4*0x26)  /* control stack height */
#define RST   (4*0x28)  /* state flag */
#define RDB   (4*0x29)  /* debug flag */
#define RED   (4*0x31)  /* editor address */
#define RBP   (4*0x32)  /* breakpoint */

/* opcode constants */
#define OP_AD 0x80
#define OP_SB 0x81
#define OP_ML 0x82
#define OP_DV 0x83
#define OP_MD 0x84
#define OP_SH 0x85
#define OP_AN 0x86
#define OP_OR 0x87
#define OP_XR 0x88
#define OP_NT 0x89
#define OP_EQ 0x8A
#define OP_LT 0x8B
#define OP_DU 0x8C
#define OP_SW 0x8D
#define OP_OV 0x8E
#define OP_ZP 0x8F
#define OP_DC 0x90
#define OP_CD 0x91
#define OP_RB 0x92
#define OP_RI 0x93
#define OP_WB 0x94
#define OP_WI 0x95
#define OP_LB 0x96
#define OP_LI 0x97
#define OP_RS 0x98
#define OP_LS 0x99
#define OP_JM 0x9A
#define OP_HP 0x9B
#define OP_H0 0x9C
#define OP_CL 0x9D
#define OP_RT 0x9E
#define OP_NX 0x9F
#define OP_TM 0xBE
#define OP_C0 0xC0
#define OP_C1 0xC1
#define OP_C2 0xF6
#define OP_N1 0xF7
#define OP_C4 0xF8
#define OP_IO 0xFD
#define OP_DB 0xFE
#define OP_HL 0xFF

/* VM state */
extern byte mem[];
extern val  ds[]; /* data stack */
extern val  cs[]; /* control stack */

/* read/write register as 32-bit value */
static inline val  rg(int off) {
  return (val)(mem[off] | (mem[off+1]<<8) | (mem[off+2]<<16) | (mem[off+3]<<24));
}
static inline void srg(int off, val v) {
  mem[off]   = v & 0xFF;
  mem[off+1] = (v>>8)  & 0xFF;
  mem[off+2] = (v>>16) & 0xFF;
  mem[off+3] = (v>>24) & 0xFF;
}

/* convenience accessors for IP, DS height, CS height, etc. */
#define IP    rg(RIP)
#define SIP(v) srg(RIP,(v))
#define DSH   rg(RDS)
#define SDH(v) srg(RDS,(v))
#define CSH   rg(RCS)
#define SCH(v) srg(RCS,(v))
#define ST    rg(RST)
#define SST(v) srg(RST,(v))
#define DBG   rg(RDB)
#define SDB(v) srg(RDB,(v))
#define HERE  rg(RHP)
#define SHERE(v) srg(RHP,(v))

/* data stack operations */
static inline void dput(val v) { ds[DSH] = v; SDH(DSH+1); }
static inline val  dpop(void)  { SDH(DSH-1); return ds[DSH]; }
static inline val  dtos(void)  { return ds[DSH-1]; }
static inline val  dnos(void)  { return ds[DSH-2]; }
static inline void dswp(void)  { val a=dpop(), b=dpop(); dput(a); dput(b); }

/* control stack operations */
static inline void cput(val v) { cs[CSH] = v; SCH(CSH+1); }
static inline val  cpop(void)  { SCH(CSH-1); return cs[CSH]; }
static inline val  ctos(void)  { return cs[CSH-1]; }

/* memory read/write */
static inline val rdval(int a) {
  return (val)((uint32_t)mem[a] | ((uint32_t)mem[a+1]<<8) |
               ((uint32_t)mem[a+2]<<16) | ((uint32_t)mem[a+3]<<24));
}
static inline void wrval(int a, val v) {
  mem[a]   = v & 0xFF;
  mem[a+1] = (v>>8)  & 0xFF;
  mem[a+2] = (v>>16) & 0xFF;
  mem[a+3] = (v>>24) & 0xFF;
}

/* go: set IP with floor at 0x100 */
static inline void go(int a) {
  int v = a < 0x100 ? 0x100 : a;
  SIP(v-1);
}

/* hop: relative jump from signed byte at ip+1 */
static inline void hop(void) {
  go(IP + (int8_t)mem[IP+1]);
}

/* output buffer */
#define OB_MAX 4096
extern char ob[];
extern int ob_len;

/* run a single opcode */
void runop(byte op);

/* step: execute instruction at IP, increment IP, return new IP */
val b4step(void);

/* boot: initialize VM state */
void b4boot(void);

#endif /* B4_H */
