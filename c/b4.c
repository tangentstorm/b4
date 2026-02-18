/* b4.c - b4 virtual machine core implementation */
#include "b4.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* global state */
byte mem[MAXBYTE+1];
val  ds[STACKSZ];
val  cs[STACKSZ];
char ob[OB_MAX];
int  ob_len;

void b4boot(void) {
  memset(mem, 0, sizeof(mem));
  memset(ds, 0, sizeof(ds));
  memset(cs, 0, sizeof(cs));
  SDH(0); SCH(0);
  SIP(MINHEAP);
  SHERE(MINHEAP);
  srg(RED, MINHEAP);
  srg(RBP, MAXCELL);
  ob_len = 0;
}

static void opio(void) {
  char cmd = (char)dpop();
  switch (cmd) {
  case 'e': {  /* emit character to output buffer */
    char ch = (char)dpop();
    if (ch < ' ') ch = ' ';
    if (ob_len < OB_MAX-1) ob[ob_len++] = ch;
  } break;
  case 'o': {  /* output character directly to stdout */
    char ch = (char)dpop();
    putchar(ch);
    fflush(stdout);
  } break;
  default: break;
  }
}

void runop(byte op) {
  val t;
  if (op == 0) { /* nop */ }
  else if (op < 0x20) { /* ^R: invoke register */
    int r = 4 * (op % 32);
    cput(IP+1);
    go(rg(r));
  }
  else if (op < 0x40) { /* @R: read register */
    dput(rg(4 * (op % 32)));
  }
  else if (op < 0x60) { /* !R: write register */
    srg(4 * (op % 32), dpop());
  }
  else if (op < 0x80) { /* +R: stream via register */
    int r = 4 * (op % 32);
    val d = dpop(), v = rg(r);
    dput(v);
    srg(r, v + d);
  }
  else switch (op) {
  case OP_AD: dput(dpop() + dpop()); break;
  case OP_SB: { t=dpop(); dput(dpop()-t); } break;
  case OP_ML: dput(dpop() * dpop()); break;
  case OP_DV: { t=dpop(); dput(dpop()/t); } break;
  case OP_MD: { t=dpop(); dput(dpop()%t); } break;
  case OP_SH: {
    t=dpop(); val x=dpop();
    if (t<0) dput((val)((uint32_t)x >> -t));
    else dput(x << t);
  } break;
  case OP_AN: dput(dpop() & dpop()); break;
  case OP_OR: dput(dpop() | dpop()); break;
  case OP_XR: dput(dpop() ^ dpop()); break;
  case OP_NT: dput(~dpop()); break;
  case OP_EQ: { t=dpop(); dput(dpop()==t ? -1 : 0); } break;
  case OP_LT: { t=dpop(); dput(dpop()<t ? -1 : 0); } break;
  case OP_DU: dput(dtos()); break;
  case OP_SW: dswp(); break;
  case OP_OV: dput(dnos()); break;
  case OP_ZP: dpop(); break;
  case OP_DC: cput(dpop()); break;
  case OP_CD: dput(cpop()); break;
  case OP_RB: dput(mem[dpop()]); break;
  case OP_RI: dput(rdval(dpop())); break;
  case OP_WB: { t=dpop(); mem[t]=(byte)dpop(); } break;
  case OP_WI: { t=dpop(); wrval(t, dpop()); } break;
  case OP_LB: dput(mem[IP+1]); SIP(IP+1); break;
  case OP_LI: dput(rdval(IP+1)); SIP(IP+4); break;
  case OP_RS: { /* read signed byte */
    int a = dpop();
    dput((int8_t)mem[a]);
  } break;
  case OP_LS: { /* load signed byte */
    dput((int8_t)mem[IP+1]);
    SIP(IP+1);
  } break;
  case OP_JM: go(rdval(IP+1)); break;
  case OP_HP: hop(); break;
  case OP_H0: if (dpop()==0) hop(); else SIP(IP+1); break;
  case OP_CL: cput(IP+5); go(rdval(IP+1)); break;
  case OP_RT: {
    val a = cpop();
    if (a) { SIP(a-1); }
    else SST(0);
  } break;
  case OP_NX: {
    if (ctos()>0) { t=cpop(); t--; cput(t); }
    if (ctos()==0) { cpop(); SIP(IP+1); }
    else hop();
  } break;
  case OP_C0: dput(0); break;
  case OP_C1: dput(1); break;
  case OP_C2: dput(2); break;
  case OP_N1: dput(-1); break;
  case OP_C4: dput(4); break;
  case OP_IO: opio(); break;
  case OP_DB: SDB(1); break;
  case OP_HL: exit(0); break;
  default: break; /* unknown op: nop */
  }
}

val b4step(void) {
  runop(mem[IP]);
  SIP(IP+1);
  return IP;
}
