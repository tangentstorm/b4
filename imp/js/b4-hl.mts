// b4 syntax highlighter (standalone, no rendering dependencies)

const OPCODES = new Set('ad sb ml dv md sh an or xr nt eq lt du sw ov zp dc cd rb ri wb wi lb li rs ls jm hp h0 cl rt nx c0 c1 c2 n1 c4 wl ds hx tm io db hl'.split(' '))

export type B4Token = { text: string, kind: string }

export const B4_COLORS: Record<string, number> = {
  comment:  0x6A9955,
  string:   0xCE9178,
  label:    0xDCDCAA,
  regop:    0x569CD6,
  macro:    0xFF8C00,
  preproc:  0xC586C0,
  number:   0xB5CEA8,
  opcode:   0x4EC9B0,
  plain:    0xCCCCCC,
  ws:       0xCCCCCC,
}

export function b4TokenKind(tok: string): string {
  const t = tok[0]
  if (t === '#') return 'comment'
  if (t === '"' || (t === '.' && tok[1] === '"')) return 'string'
  if (t === ':') return 'label'
  if (/^[@!+^][A-Z\[\\\]^_]$/.test(tok)) return 'regop'
  if (t === '.' && tok.length === 2) return 'macro'
  if (t === '%') return 'preproc'
  if (t === '?' || t === '$' || t === '\\') return 'regop'
  if (t === '>') return 'label'
  if (t === "'") return 'string'
  if (OPCODES.has(tok)) return 'opcode'
  if (/^-?[0-9A-F]+$/.test(tok)) return 'number'
  return 'plain'
}

export function b4Tokenize(line: string): B4Token[] {
  if (!line) return []
  // split off comment before tokenizing
  const ci = line.indexOf('#')
  const code = ci >= 0 ? line.slice(0, ci) : line
  const comment = ci >= 0 ? line.slice(ci) : ''
  const parts = code.split(/(\s+)/)
  const result: B4Token[] = []
  for (const part of parts) {
    if (!part) continue
    if (/^\s+$/.test(part)) result.push({ text: part, kind: 'ws' })
    else result.push({ text: part, kind: b4TokenKind(part) })}
  if (comment) result.push({ text: comment, kind: 'comment' })
  return result
}

export function b4Esc(s: string): string {
  return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;')}

export function b4HlLine(text: string): string {
  return b4Tokenize(text).map(t => {
    if (t.kind === 'ws') return t.text
    const c = B4_COLORS[t.kind] ?? B4_COLORS.plain
    return `<span style="color:#${c.toString(16).padStart(6,'0')}">${b4Esc(t.text)}</span>`
  }).join('')}
