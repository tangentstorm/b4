/* b4ops.h - opcode name table for disassembly */
#ifndef B4OPS_H
#define B4OPS_H

#include <stddef.h>
#include <stdio.h>

static const char *optbl[256];

static void init_optbl(void) {
  int i;
  for (i = 0; i < 256; i++) optbl[i] = NULL;
  optbl[0x80] = "ad"; optbl[0x81] = "sb"; optbl[0x82] = "ml";
  optbl[0x83] = "dv"; optbl[0x84] = "md"; optbl[0x85] = "sh";
  optbl[0x86] = "an"; optbl[0x87] = "or"; optbl[0x88] = "xr";
  optbl[0x89] = "nt"; optbl[0x8A] = "eq"; optbl[0x8B] = "lt";
  optbl[0x8C] = "du"; optbl[0x8D] = "sw"; optbl[0x8E] = "ov";
  optbl[0x8F] = "zp"; optbl[0x90] = "dc"; optbl[0x91] = "cd";
  optbl[0x92] = "rb"; optbl[0x93] = "ri"; optbl[0x94] = "wb";
  optbl[0x95] = "wi"; optbl[0x96] = "lb"; optbl[0x97] = "li";
  optbl[0x98] = "rs"; optbl[0x99] = "ls";
  optbl[0x9A] = "jm"; optbl[0x9B] = "hp"; optbl[0x9C] = "h0";
  optbl[0x9D] = "cl"; optbl[0x9E] = "rt"; optbl[0x9F] = "nx";
  optbl[0xBE] = "tm";
  optbl[0xC0] = "c0"; optbl[0xC1] = "c1";
  optbl[0xF6] = "c2"; optbl[0xF7] = "n1"; optbl[0xF8] = "c4";
  optbl[0xFD] = "io"; optbl[0xFE] = "db"; optbl[0xFF] = "hl";
}

/* opcode lookup by mnemonic -> byte value; -1 if not found */
static __attribute__((unused)) int op_lookup(const char *s) {
  int i;
  for (i = 0; i < 256; i++)
    if (optbl[i] && optbl[i][0]==s[0] && optbl[i][1]==s[1]) return i;
  /* special: ".." -> 0 */
  if (s[0]=='.' && s[1]=='.') return 0;
  return -1;
}

/* disassemble a byte to a 2-char string */
static const char *dis(byte b) {
  static char buf[4];
  if (b == 0) return "..";
  if (b >= 0x01 && b <= 0x1F) {
    buf[0] = '^'; buf[1] = '@' + b; buf[2] = 0; return buf;
  }
  if (b >= 0x20 && b <= 0x3F) {
    buf[0] = '@'; buf[1] = '@' + (b-0x20); buf[2] = 0; return buf;
  }
  if (b >= 0x40 && b <= 0x5F) {
    buf[0] = '!'; buf[1] = '@' + (b-0x40); buf[2] = 0; return buf;
  }
  if (b >= 0x60 && b <= 0x7F) {
    buf[0] = '+'; buf[1] = '@' + (b-0x60); buf[2] = 0; return buf;
  }
  if (optbl[b]) return optbl[b];
  sprintf(buf, "%02X", b);
  return buf;
}

#endif /* B4OPS_H */
