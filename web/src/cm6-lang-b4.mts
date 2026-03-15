// lang-b4a: B4a language support for CodeMirror 6
// Translated from ref/b4/etc/b4-vscode/syntaxes/b4a.tmLanguage.json
import {StreamLanguage, type StreamParser} from '@codemirror/language'
import {LanguageSupport} from '@codemirror/language'

const OPCODES = new Set('ad sb ml dv md sh an or xr nt eq lt du sw ov zp dc cd rb ri wb wi lb li rs ls jm hp h0 cl rt nx c0 c1 c2 n1 c4 wl ds hx tm io db hl'.split(' '))

interface B4aState { inString: boolean; inMacroString: boolean }

const b4aParser: StreamParser<B4aState> = {
  startState(): B4aState { return { inString: false, inMacroString: false } },

  token(stream, state): string | null {
    // Inside a macro string (." ... ")
    if (state.inMacroString) {
      if (stream.eat('"' as any)) { state.inMacroString = false; return 'string' }
      stream.next()
      return 'string'
    }

    // Inside a regular string
    if (state.inString) {
      if (stream.eat('"' as any)) { state.inString = false; return 'string' }
      stream.next()
      return 'string'
    }

    // Comments: # to end of line
    if (stream.eat('#' as any)) {
      stream.skipToEnd()
      return 'lineComment'
    }

    // Macro string: ." ... "
    if (stream.match(/^\.\"/)) {
      state.inMacroString = true
      return 'string'
    }

    // Regular string: " ... "
    if (stream.eat('"' as any)) {
      state.inString = true
      return 'string'
    }

    // Char literal: 'x
    if (stream.match(/^'[a-zA-Z0-9 ]/)) {
      return 'character'
    }

    // Backtick constants: `name
    if (stream.match(/^`[a-zA-Z0-9_]+/)) {
      return 'atom'
    }

    // Escape sequences: \x
    if (stream.match(/^\\[a-zA-Z0-9]/)) {
      return 'escape'
    }

    // Register ops: @A !B +C ^D
    if (stream.match(/^[@!+^][A-Z]/)) {
      return 'operatorKeyword'
    }

    // Function names: :something
    if (stream.match(/^:[^ ]+/)) {
      return 'labelName'
    }

    // Dot commands (not ." which is handled above): .x
    if (stream.match(/^\.[^#\s]/)) {
      return 'processingInstruction'
    }

    // Preprocessor: %...
    if (stream.match(/^%[^#]*/)) {
      return 'meta'
    }

    // Words: consume whole word, then classify as opcode, hex, or plain
    const word = stream.match(/^[a-zA-Z0-9_]+/)
    if (word) {
      const w = word[0]
      if (OPCODES.has(w)) return 'keyword'
      if (/^[A-F0-9]{2,}$/.test(w)) return 'number'
      return null
    }

    // Skip whitespace and other characters
    stream.next()
    return null
  }
}

const b4aLang = StreamLanguage.define(b4aParser)

/** Returns a LanguageSupport instance for the B4a language */
export function b4a(): LanguageSupport {
  return new LanguageSupport(b4aLang)
}
