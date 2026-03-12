/* b4i.c - b4 interactive assembler/debugger in C */
#include "b4.h"
#include "b4ops.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* forward declarations */
static void load_b4i_file(const char *path);
static void load_b4a_file(const char *path);

/* --- label dictionary --- */
#define MAX_LABELS 4096
typedef struct { char name[17]; int addr; } label_t;
static label_t labels[MAX_LABELS];
static int nlabels = 0;

/* forward references */
#define MAX_FWDS 256
typedef struct { char name[17]; int at; } fwd_t;
static fwd_t fwds[MAX_FWDS];
static int nfwds = 0;

/* --- helpers --- */

static int is_upper_hex(char c) {
  return (c>='0' && c<='9') || (c>='A' && c<='F');
}

/* try parsing as hex; returns 1 on success */
static int try_hex(const char *s, int *out) {
  const char *p = s;
  int neg = 0;
  if (*p == '-') { neg = 1; p++; }
  if (*p == 0) return 0;
  for (const char *q = p; *q; q++)
    if (!is_upper_hex(*q)) return 0;
  unsigned long v = strtoul(p, NULL, 16);
  *out = neg ? -(int)v : (int)v;
  return 1;
}

static int is_reg_char(char c) {
  return c >= '@' && c <= '_';
}

static int regn(char c) { return toupper(c) - '@'; }
static int rega(char c) { return 4 * regn(c); }

/* format value as hex in b4 style (lowercase, no leading zeros except for 0) */
static void b4fmt(char *buf, val v) {
  if (v < 0) sprintf(buf, "-%X", (unsigned)-v);
  else sprintf(buf, "%X", (unsigned)v);
}

/* print stack */
static void print_stack(const char *pre, val *s, int count) {
  printf("%s[", pre);
  for (int i = 0; i < count; i++) {
    char buf[20];
    if (i > 0) printf(" ");
    b4fmt(buf, s[i]);
    printf("%s", buf);
  }
  printf("]\n");
}

static void show_mem(int addr) {
  for (int i = 0; i < 16; i++) {
    if (i > 0) printf(" ");
    printf("%s", dis(mem[addr+i]));
  }
  printf("\n");
}

static void show_hex_mem(int addr) {
  for (int i = 0; i < 16; i++) {
    if (i > 0) printf(" ");
    if (mem[addr+i] == 0) printf("..");
    else printf("%02X", mem[addr+i]);
  }
  printf("\n");
}

/* find label by name; returns address or -1 */
static int find_label(const char *name) {
  for (int i = nlabels-1; i >= 0; i--)
    if (strcmp(labels[i].name, name) == 0) return labels[i].addr;
  return -1;
}

/* add label */
static void add_label(const char *name, int addr) {
  if (nlabels < MAX_LABELS) {
    strncpy(labels[nlabels].name, name, 16);
    labels[nlabels].name[16] = 0;
    labels[nlabels].addr = addr;
    nlabels++;
    /* resolve forward references */
    int i = 0;
    while (i < nfwds) {
      if (strcmp(fwds[i].name, name) == 0) {
        wrval(fwds[i].at, addr);
        /* remove by shifting */
        memmove(&fwds[i], &fwds[i+1], (nfwds-i-1)*sizeof(fwd_t));
        nfwds--;
      } else i++;
    }
  }
}

static void clear_dict(void) {
  nlabels = 0;
  nfwds = 0;
}

/* find address by name (label or register) */
static int find_addr(const char *name) {
  int a = find_label(name);
  if (a >= 0) return a;
  if (strlen(name) == 1 && is_reg_char(name[0]))
    return rega(name[0]);
  return -1;
}

/* --- b4a opcode lookup --- */
static int lookup_op(const char *tok) {
  /* check register ops first: ^A, @B, !C, +D */
  if (strlen(tok) == 2) {
    char pfx = tok[0], reg = tok[1];
    if (is_reg_char(reg)) {
      byte r = regn(reg);
      switch (pfx) {
      case '^': return r;
      case '@': return r + 0x20;
      case '!': return r + 0x40;
      case '+': return r + 0x60;
      }
    }
    /* opcode mnemonic */
    int op = op_lookup(tok);
    if (op >= 0) return op;
  }
  if (strcmp(tok, "..") == 0) return 0;
  return -1;
}

/* --- tokenizer --- */
/* split line into tokens, handling strings and comments */
#define MAX_TOKS 256
#define MAX_TOK_LEN 256
static char tokbuf[MAX_TOKS][MAX_TOK_LEN];
static int ntoks;

static void tokenize(const char *line) {
  ntoks = 0;
  int i = 0, len = strlen(line);
  while (i < len && ntoks < MAX_TOKS) {
    /* skip whitespace */
    while (i < len && (line[i]==' ' || line[i]=='\t' || line[i]=='\r' || line[i]=='\n')) i++;
    if (i >= len) break;
    /* comment */
    if (line[i] == '#') break;
    /* string literals */
    if (line[i] == '"' || (line[i] == '.' && i+1 < len && line[i+1] == '"')) {
      int start = i;
      if (line[i] == '.') i++; /* skip dot prefix */
      i++; /* skip opening quote */
      while (i < len && line[i] != '"') i++;
      if (i < len) i++; /* skip closing quote */
      int tlen = i - start;
      if (tlen >= MAX_TOK_LEN) tlen = MAX_TOK_LEN - 1;
      memcpy(tokbuf[ntoks], &line[start], tlen);
      tokbuf[ntoks][tlen] = 0;
      ntoks++;
    } else {
      int start = i;
      while (i < len && line[i] != ' ' && line[i] != '\t' &&
             line[i] != '\r' && line[i] != '\n') i++;
      int tlen = i - start;
      if (tlen >= MAX_TOK_LEN) tlen = MAX_TOK_LEN - 1;
      memcpy(tokbuf[ntoks], &line[start], tlen);
      tokbuf[ntoks][tlen] = 0;
      ntoks++;
    }
  }
}

/* --- run wrapped (for ^R calls) --- */
static void run_wrapped(void) {
  SST(1); SDB(0);
  while (ST == 1 && DBG == 0) b4step();
}

static void wrap_call(int addr) {
  cput(IP);
  cput(0);
  go(addr+1);
  run_wrapped();
  if (DBG == 0) SIP(cpop());
}

/* --- assembler state --- */
/* emit byte at HERE, increment HERE */
static void emit(int v) {
  int h = HERE;
  mem[h] = (byte)v;
  SHERE(h+1);
}
static void emitv(val v) {
  int h = HERE;
  wrval(h, v);
  SHERE(h+4);
}
static void emit_call(int addr) {
  emit(OP_CL);
  emitv(addr);
}

/* macro helpers for assembler */
static void hop_slot(byte opcode) {
  emit(opcode);
  dput(HERE);
  emit(0); /* placeholder */
}
static void hop_here(void) {
  int slot = dpop();
  int dist = HERE - slot;
  if (dist < 0 || dist > 126) {
    fprintf(stderr, "hop out of range: %d\n", dist);
    return;
  }
  mem[slot] = (byte)(dist + 1);
}
static void hop_back(void) {
  int dest = dpop();
  int dist = dest - HERE;
  if (dist > 0 || dist < -128) {
    fprintf(stderr, "hop_back out of range: %d\n", dist);
    return;
  }
  emit((byte)(dist + 1));
}

/* compile a token in assembly mode */
static void compile(const char *tok) {
  int v, op;
  if (tok[0] == '#') return; /* comment */

  /* opcode or register op */
  op = lookup_op(tok);
  if (op >= 0) { emit(op); return; }

  /* hex literal */
  if (try_hex(tok, &v)) { emit(v); return; }

  /* u32 literal: $XXXXXXXX */
  if (tok[0] == '$') {
    if (try_hex(tok+1, &v)) { emitv(v); return; }
  }

  /* character literal: 'x */
  if (tok[0] == '\'') {
    if (tok[1] == 0) emit(32); /* space */
    else emit(tok[1]);
    return;
  }

  /* register/label address: `X or `label */
  if (tok[0] == '`') {
    if (strlen(tok)==2 && is_reg_char(tok[1])) {
      emitv(rega(tok[1]));
    } else {
      int a = find_label(tok+1);
      if (a >= 0) emitv(a);
      else fprintf(stderr, "asm: unknown `label: %s\n", tok+1);
    }
    return;
  }

  /* forward reference: >label */
  if (tok[0] == '>') {
    if (nfwds < MAX_FWDS) {
      strncpy(fwds[nfwds].name, tok+1, 16);
      fwds[nfwds].name[16] = 0;
      fwds[nfwds].at = HERE;
      nfwds++;
      emitv(0); /* placeholder */
    }
    return;
  }

  /* macros */
  if (tok[0] == '.' && tok[1] != '"' && tok[1] != '.') {
    switch (tok[1]) {
    case 'i': emit(OP_H0); dput(HERE); emit(0); break;
    case 'e': hop_slot(OP_HP); dswp(); hop_here(); break;
    case 't': hop_here(); break;
    case 'w': dput(HERE); break;
    case 'd': emit(OP_H0); dput(HERE); emit(0); break;
    case 'o': emit(OP_HP); dswp(); hop_back(); hop_here(); break;
    case 'f': emit(OP_DC); dput(HERE); break;
    case 'n': emit(OP_NX); hop_back(); break;
    case '^': { /* linked list */
      int a = HERE;
      emitv(rg(RLP));
      srg(RLP, a);
    } break;
    default:
      fprintf(stderr, "unknown macro: %s\n", tok);
    }
    return;
  }

  /* raw string: "abc" */
  if (tok[0] == '"') {
    int len = strlen(tok);
    for (int i = 1; i < len - 1; i++) emit(tok[i]);
    return;
  }
  /* counted string: ."abc" */
  if (tok[0] == '.' && tok[1] == '"') {
    int len = strlen(tok);
    int slen = len - 3; /* skip ." and closing " */
    emit(slen);
    for (int i = 2; i < len - 1; i++) emit(tok[i]);
    return;
  }

  /* label reference (assembles a cl to the label) */
  int a = find_addr(tok);
  if (a >= 0) { emit_call(a); return; }

  /* @label -> li addr ri */
  if (tok[0] == '@') {
    a = find_addr(tok+1);
    if (a >= 0) { emit(OP_LI); emitv(a); emit(OP_RI); return; }
  }
  /* !label -> li addr wi */
  if (tok[0] == '!') {
    a = find_addr(tok+1);
    if (a >= 0) { emit(OP_LI); emitv(a); emit(OP_WI); return; }
  }

  fprintf(stderr, "asm: unknown token: %s\n", tok);
}

/* --- main b4i line handler --- */
/* returns 1 if should quit */
static int b4i(const char *line) {
  int i, v;
  int done = 0;

  if (strlen(line) == 0) return 0;

  /* if line starts with ':', handle assembly */
  if (line[0] == ':') {
    tokenize(line);
    for (i = 0; i < ntoks; i++) {
      char *tok = tokbuf[i];
      if (tok[0] == '#') break; /* comment */
      if (tok[0] == ':') {
        char *rest = tok + 1;
        if (strlen(rest) == 0) {
          /* bare ":" - continue from HERE */
          continue;
        }
        if (strlen(rest) == 1 && is_reg_char(rest[0])) {
          /* :R - point register at HERE */
          srg(rega(rest[0]), HERE);
        } else if (try_hex(rest, &v)) {
          /* :hex - set HERE */
          SHERE(v);
        } else {
          /* :label - define label */
          add_label(rest, HERE);
          if (strcmp(rest, "main") == 0)
            srg(RGO, HERE);
        }
      } else {
        compile(tok);
      }
    }
    return 0;
  }

  /* immediate mode */
  tokenize(line);
  for (i = 0; i < ntoks; i++) {
    char *tok = tokbuf[i];
    if (tok[0] == '#') break; /* comment */

    /* slash commands */
    if (tok[0] == '/') {
      if (strcmp(tok, "/q") == 0) { done = 1; break; }
      else if (strcmp(tok, "/") == 0 || strcmp(tok, "/s") == 0) b4step();
      else if (strcmp(tok, "/C") == 0) { b4boot(); clear_dict(); }
      else if (strcmp(tok, "/R") == 0) {
        SCH(0); SDH(0); SIP(HERE);
      }
      else if (strcmp(tok, "/g") == 0 || strcmp(tok, "//") == 0) run_wrapped();
      else if (strcmp(tok, "/p") == 0) {
        for (int j = 0; j < nlabels; j++)
          printf("%04X:%s\n", labels[j].addr, labels[j].name);
      }
      else if (strcmp(tok, "/f") == 0) {
        for (int j = 0; j < nfwds; j++)
          printf("%04X>%s\n", fwds[j].at, fwds[j].name);
      }
      else if (strcmp(tok, "/a") == 0) {
        if (i+1 < ntoks) { i++; load_b4a_file(tokbuf[i]); }
        else printf("usage: /a <filename>\n");
      }
      else if (strcmp(tok, "/i") == 0) {
        if (i+1 < ntoks) { i++; load_b4i_file(tokbuf[i]); }
        else printf("usage: /i <filename>\n");
      }
      else {
        int addr;
        if (try_hex(tok + 1, &addr)) SIP(addr);
        else printf("/.no: %s\n", tok);
      }
      continue;
    }
    /* query commands */
    if (tok[0] == '?') {
      if (strcmp(tok, "?d") == 0) print_stack("ds: ", ds, DSH);
      else if (strcmp(tok, "?c") == 0) print_stack("cs: ", cs, CSH);
      else if (strcmp(tok, "?i") == 0) {
        char buf[20]; b4fmt(buf, IP);
        printf("ip: %s\n", buf);
      }
      else if (strlen(tok)==2 && is_reg_char(tok[1])) {
        /* ?R: show register value */
        printf("%08X\n", (unsigned)rg(rega(tok[1])));
      }
      else if (strlen(tok)==3 && tok[1]=='@' && is_reg_char(tok[2])) {
        /* ?@R: show memory at register address */
        int addr = rg(rega(tok[2]));
        printf("%08X ", (unsigned)addr);
        show_mem(addr);
      }
      else {
        /* ?addr or ?label */
        char *s = tok+1;
        int addr;
        size_t slen = strlen(s);
        if (slen > 0 && s[slen-1] == 'x') {
          char tmp[256];
          if (slen >= sizeof(tmp)) slen = sizeof(tmp) - 1;
          memcpy(tmp, s, slen - 1);
          tmp[slen - 1] = 0;
          if (try_hex(tmp, &addr)) { show_hex_mem(addr); continue; }
        }
        if (try_hex(s, &addr)) show_mem(addr);
        else {
          int a = find_label(s);
          if (a >= 0) { printf("%08X ", (unsigned)a); show_mem(a); }
          else printf("invalid address: %s\n", s);
        }
      }
      continue;
    }
    /* character literal */
    if (tok[0] == '\'') {
      if (tok[1] == 0) dput(32);
      else dput(tok[1]);
      continue;
    }
    /* register address */
    if (tok[0] == '`') {
      if (strlen(tok)==2 && is_reg_char(tok[1])) {
        dput(rega(tok[1]));
      } else {
        int a = find_label(tok+1);
        if (a >= 0) dput(a);
        else printf("unknown `word\n");
      }
      continue;
    }
    /* register ops: ^R @R !R +R */
    if (strlen(tok)==2 && is_reg_char(tok[1])) {
      int r = rega(tok[1]);
      switch (tok[0]) {
      case '^': wrap_call(rg(r)); continue;
      case '@': dput(rg(r)); continue;
      case '!': srg(r, dpop()); continue;
      case '+': { val d=dpop(), v2=rg(r); dput(v2); srg(r,v2+d); continue; }
      }
    }
    /* @label !label */
    if (tok[0] == '@') {
      int a = find_addr(tok+1);
      if (a >= 0) { dput(rdval(a)); continue; }
      printf("unknown @word\n"); continue;
    }
    if (tok[0] == '!') {
      int a = find_addr(tok+1);
      if (a >= 0) { wrval(a, dpop()); continue; }
      printf("unknown !word\n"); continue;
    }
    /* hex number */
    if (try_hex(tok, &v)) { dput(v); continue; }
    /* opcode in immediate mode */
    int op = lookup_op(tok);
    if (op >= 0) {
      if (op < 0x20) wrap_call(rg(4*op)); /* ^R: invoke */
      else runop(op);
      continue;
    }
    /* label call */
    int a = find_label(tok);
    if (a >= 0) { wrap_call(a); continue; }
    printf("what does \"%s\" mean?\n", tok);
  }

  /* flush output buffer */
  if (ob_len > 0) {
    ob[ob_len] = 0;
    printf("%s\n", ob);
    ob_len = 0;
  }
  return done;
}

/* load and interpret a .b4i file */
static void load_b4i_file(const char *path) {
  FILE *f = fopen(path, "r");
  char line[4096];
  if (!f) { fprintf(stderr, "file not found: %s\n", path); return; }
  while (fgets(line, sizeof(line), f)) {
    int len = strlen(line);
    while (len > 0 && (line[len-1]=='\n' || line[len-1]=='\r'))
      line[--len] = 0;
    if (b4i(line)) break;
  }
  fclose(f);
}

/* load and assemble a .b4a file (assembly mode: each line starts with :) */
static void load_b4a_file(const char *path) {
  FILE *f = fopen(path, "r");
  char line[4096];
  if (!f) { fprintf(stderr, "file not found: %s\n", path); return; }
  while (fgets(line, sizeof(line), f)) {
    int len = strlen(line);
    while (len > 0 && (line[len-1]=='\n' || line[len-1]=='\r'))
      line[--len] = 0;
    if (strlen(line) > 0) b4i(line);
  }
  fclose(f);
}

int main(int argc, char **argv) {
  char line[4096];
  int run_main = 0;
  b4boot();
  init_optbl();
  /* process command-line arguments */
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-a") == 0 && i+1 < argc) {
      load_b4a_file(argv[++i]);
    } else if (strcmp(argv[i], "-i") == 0 && i+1 < argc) {
      load_b4i_file(argv[++i]);
    } else if (strcmp(argv[i], "-x") == 0) {
      run_main = 1;
    } else if (strcmp(argv[i], "-q") == 0) {
      return 0;
    }
  }
  if (run_main) {
    /* run 'main' label or ^\ register */
    int a = find_label("main");
    if (a >= 0) wrap_call(a);
    else if (rg(RGO) != 0) wrap_call(rg(RGO));
    /* flush output */
    if (ob_len > 0) { ob[ob_len]=0; printf("%s\n",ob); ob_len=0; }
    return 0;
  }
  /* interactive REPL */
  while (fgets(line, sizeof(line), stdin)) {
    int len = strlen(line);
    while (len > 0 && (line[len-1]=='\n' || line[len-1]=='\r'))
      line[--len] = 0;
    if (b4i(line)) break;
  }
  return 0;
}
