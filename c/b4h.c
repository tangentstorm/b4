/* b4h.c - hex-only debugger REPL in C */
#include "b4.h"
#include "b4ops.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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
  fflush(stdout);
}

static void show_mem(int addr) {
  for (int i = 0; i < 16; i++) {
    if (i > 0) printf(" ");
    printf("%s", dis(mem[addr+i]));
  }
  printf("\n");
  fflush(stdout);
}

static void show_hex_mem(int addr) {
  for (int i = 0; i < 16; i++) {
    if (i > 0) printf(" ");
    printf("%02X", mem[addr+i]);
  }
  printf("\n");
  fflush(stdout);
}

static void drain_output_hex(void) {
  int i;
  int count = ob_len;
  printf("%02X\n", count & 0xFF);
  for (i = 0; i < count; i++) {
    if (i > 0 && (i % 16) == 0) printf("\n");
    else if (i > 0) printf(" ");
    printf("%02X", (unsigned char)ob[i]);
  }
  if (count > 0) printf("\n");
  fflush(stdout);
  ob_len = 0;
}

/* --- tokenizer --- */
/* split line into tokens; # starts a comment */
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

static int is_reg_char(char c) {
  return c >= '@' && c <= '_';
}

static int rega(char c) {
  return 4 * (toupper((unsigned char)c) - '@');
}

static int parse_reg(const char *tok, int prefix_len, int *out) {
  if ((int)strlen(tok) != prefix_len + 1) return 0;
  if (!is_reg_char(tok[prefix_len])) return 0;
  *out = rega(tok[prefix_len]);
  return 1;
}

/* --- raw hex assembly --- */
static int assemble_hex(const char *line) {
  int addr;
  tokenize(line);
  if (ntoks == 0) return 0;
  if (tokbuf[0][0] != ':') return 0;
  if (tokbuf[0][1] == '\0') {
    addr = HERE;  /* bare ':' means continue at HERE */
  } else if (!try_hex(tokbuf[0] + 1, &addr)) {
    printf("invalid address: %s\n", tokbuf[0] + 1);
    return 0;
  }
  for (int i = 1; i < ntoks; i++) {
    int v;
    if (strcmp(tokbuf[i], "..") == 0) {
      mem[addr++] = 0;
      continue;
    }
    if (!try_hex(tokbuf[i], &v) || v < 0 || v > 0xFF) {
      printf("invalid byte: %s\n", tokbuf[i]);
      return 0;
    }
    mem[addr++] = (byte)v;
  }
  SHERE(addr);
  return 1;
}

/* --- main b4h line handler --- */
/* returns 1 if should quit */
static int b4h(const char *line) {
  int i;
  int done = 0;

  if (strlen(line) == 0) return 0;

  if (line[0] == ':') {
    assemble_hex(line);
    return 0;
  }

  tokenize(line);
  for (i = 0; i < ntoks; i++) {
    char *tok = tokbuf[i];

    /* slash commands */
    if (tok[0] == '/') {
      if (strcmp(tok, "/q") == 0) { done = 1; break; }
      else if (strcmp(tok, "/ox?") == 0) drain_output_hex();
      else if (strcmp(tok, "/") == 0 || strcmp(tok, "/s") == 0) b4step();
      else if (strcmp(tok, "/C") == 0) b4boot();
      else if (strcmp(tok, "/R") == 0) {
        SCH(0); SDH(0); SIP(MINHEAP);
      }
      else if (strcmp(tok, "/g") == 0 || strcmp(tok, "//") == 0) run_wrapped();
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
        fflush(stdout);
      }
      else if (strlen(tok)==2 && is_reg_char(tok[1])) {
        /* ?R: show register value */
        printf("%08X\n", (unsigned)rg(rega(tok[1])));
        fflush(stdout);
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
        else printf("invalid address: %s\n", s);
      }
      continue;
    }

    /* optional raw register helpers retained for debugger use */
    if (tok[0] == '^') {
      int r;
      if (parse_reg(tok, 1, &r)) { wrap_call(rg(r)); continue; }
    }
    if (tok[0] == '@') {
      int r;
      if (parse_reg(tok, 1, &r)) { dput(rg(r)); continue; }
    }
    if (tok[0] == '!') {
      int r;
      if (parse_reg(tok, 1, &r)) { srg(r, dpop()); continue; }
    }
    if (tok[0] == '+') {
      int r;
      if (parse_reg(tok, 1, &r)) {
        val d = dpop();
        val v = rg(r);
        dput(v);
        srg(r, v + d);
        continue;
      }
    }
    printf("what does \"%s\" mean?\n", tok);
    fflush(stdout);
  }

  return done;
}

int main(int argc, char **argv) {
  char line[4096];
  b4boot();
  init_optbl();

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-q") == 0) {
      return 0;
    } else {
      fprintf(stderr, "usage: b4h [-q]\n");
      return 1;
    }
  }

  while (fgets(line, sizeof(line), stdin)) {
    int len = strlen(line);
    while (len > 0 && (line[len-1]=='\n' || line[len-1]=='\r'))
      line[--len] = 0;
    if (b4h(line)) break;
  }
  return 0;
}
