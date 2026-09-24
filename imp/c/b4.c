/* b4.c - b4 virtual machine core implementation */
#define _POSIX_C_SOURCE 200809L
#include "b4.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

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

/* process table for io 'p'/'w'/'r'/'k' */
#define MAX_PROCS 16
static FILE* proc_in[MAX_PROCS];
static FILE* proc_out[MAX_PROCS];
static int proc_active[MAX_PROCS];

/* read null-terminated string from memory */
static void mem_readstr(int addr, char *buf, int maxlen) {
  int i = 0;
  while (i < maxlen-1 && mem[addr+i] != 0) {
    buf[i] = mem[addr+i]; i++;
  }
  buf[i] = 0;
}

static void opio(void) {
  char cmd = (char)dpop();
  char buf[4096];
  int addr, len, handle, i;
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
  case 'i': { /* read line from stdin: (dest-ptr maxlen -- actual-len) */
    len = dpop();
    addr = dpop();
    if (fgets(buf, len < (int)sizeof(buf) ? len+1 : (int)sizeof(buf), stdin)) {
      int slen = strlen(buf);
      while (slen > 0 && (buf[slen-1]=='\n'||buf[slen-1]=='\r')) slen--;
      if (slen > len) slen = len;
      for (i = 0; i < slen; i++) mem[addr+i] = buf[i];
      mem[addr+slen] = 0;
      dput(slen);
    } else {
      dput(-1); /* EOF */
    }
  } break;
  case 'p': { /* spawn process: (cmdline-ptr -- handle|-1) */
    addr = dpop();
    mem_readstr(addr, buf, sizeof(buf));
    /* find free slot */
    handle = -1;
    for (i = 0; i < MAX_PROCS; i++) {
      if (!proc_active[i]) { handle = i; break; }
    }
    if (handle < 0) { dput(-1); break; }
    /* use popen2-like approach: create two pipes */
    int to_child[2], from_child[2];
    if (pipe(to_child) < 0 || pipe(from_child) < 0) { dput(-1); break; }
    int pid = fork();
    if (pid < 0) { dput(-1); break; }
    if (pid == 0) {
      /* child */
      close(to_child[1]); close(from_child[0]);
      dup2(to_child[0], 0);
      dup2(from_child[1], 1);
      close(to_child[0]); close(from_child[1]);
      execl("/bin/sh", "sh", "-c", buf, NULL);
      _exit(1);
    }
    /* parent */
    close(to_child[0]); close(from_child[1]);
    proc_in[handle] = fdopen(to_child[1], "w");
    proc_out[handle] = fdopen(from_child[0], "r");
    proc_active[handle] = 1;
    dput(handle);
  } break;
  case 'w': { /* write line to process: (line-ptr handle --) */
    handle = dpop();
    addr = dpop();
    if (handle >= 0 && handle < MAX_PROCS && proc_active[handle]) {
      mem_readstr(addr, buf, sizeof(buf));
      fprintf(proc_in[handle], "%s\n", buf);
      fflush(proc_in[handle]);
    }
  } break;
  case 'r': { /* read line from process: (dest-ptr handle -- length|-1) */
    handle = dpop();
    addr = dpop();
    if (handle >= 0 && handle < MAX_PROCS && proc_active[handle]) {
      if (fgets(buf, sizeof(buf), proc_out[handle])) {
        len = strlen(buf);
        while (len > 0 && (buf[len-1]=='\n'||buf[len-1]=='\r')) len--;
        for (i = 0; i < len; i++) mem[addr+i] = buf[i];
        mem[addr+len] = 0;
        dput(len);
      } else {
        dput(-1);
      }
    } else {
      dput(-1);
    }
  } break;
  case 'k': { /* kill process: (handle --) */
    handle = dpop();
    if (handle >= 0 && handle < MAX_PROCS && proc_active[handle]) {
      fclose(proc_in[handle]);
      fclose(proc_out[handle]);
      proc_active[handle] = 0;
    }
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
  case OP_HL: SST(0); break;
  default: break; /* unknown op: nop */
  }
}

val b4step(void) {
  runop(mem[IP]);
  SIP(IP+1);
  return IP;
}
