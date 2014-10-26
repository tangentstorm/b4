/**
 * Assembler for Retro's ngaro virtual machine (javascript edition)
 *
 * http://retroforth.org/docs/The_Ngaro_Virtual_Machine.html
 *
 * Copyright (c) 2013 Michal J Wallace
 *
 * Permission to use, copy, modify, and/or distribute this software for
 * any purpose with or without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all copies.
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *
 * Terminology:
 * ----------------------
 * "TOS" is the [T]op [O]f [S]tack
 * "NOS" is [N]ext [O]n [S]tack
 *
 * Stack Effect comments:
 * ----------------------
 *  syntax: ( <data before> ⇢ <data after> | <addr before> ⇢ <addr after> )
 *
 * where <data> is the main stack and <addr> is the return address stack.
 * If the address stack is unaffected, it is not show.
 *
 * Both stacks contains only numbers, but numbers can be treated as::
 *
 * a = address (inside the vm)     n = number
 * f = flag (boolean)              p = port
 *
 * Example : ( n a ⇢ f )  means before: TOS=a NOS=n  after: TOS = f
 *
 * Only the items actually used are shown. There may be many other
 * items on the stack below the indicated values.
 *
 */


/**
 * The Ngaro Instruction set.
 *
 * The names are shortened from the official names in the Ngaro
 * documentation, and the symbols used in retro have all been
 * replaced with words.
 *
 * The goal is that B4 assembly language should look more like
 * assembly language than like forth.
 *
 * Ops that end in ":" are jump opcodes, and must be followed
 * by a label name.
 *
 * As a convenience, all decimal numbers are treated as literals,
 * so there is never any need to use the LIT: mnemonic. (Instead
 * of "LIT: 5 PUSH" you can just say "5 PUSH".
 *
 */
var ops = {

  'NOP'    :  0, // (  ⇢   ) "no op" : do nothing
  '<LIT:>' :  1, // (  ⇢ n ) "literal" : put number on the data stack
  'DUP'    :  2, // ( n ⇢ n ) "duplicate" : copy the TOS
  'DROP'   :  3, // ( n ⇢ ) "drop" : remove the TOS
  'SWAP'   :  4, // (  ⇢ b a ) "swap" : transpose TOS and NOS
  'PUSH'   :  5, // ( n ⇢ | ⇢ n ) "push" : move TOS to the return stack
  'POP'    :  6, // ( ⇢ n | n ⇢ ) "pop" : place top item on return stack in TOS
  'NXT:'   :  7, // [TOS > 1]:( n ⇢ n ) [else]:( n ⇢ ) "next" : if tos >= 1, then DEC and JUMP, else DROP ("for" loop)
  'JMP:'   :  8, // ( ⇢ ) "jump" : jump to the address in the next cell
  'RET'    :  9, // ( ⇢ | a ⇢ ) "return" : jump to previously stored address
  'JLT:'   : 10, // ( x y ⇢ ) "jump if less than" : jump to address in next cell if x < y
  'JGT:'   : 11, // ( x y ⇢ ) "jump if greater than" : jump to address in next cell if x > y
  'JNE:'   : 12, // ( x y ⇢ ) "jump if not equal" : jump to address in next cell if x ≠ y
  'JEQ:'   : 13, // ( x y ⇢ ) "jump if equal" : jump to address in next cell if x = y
  'GET'    : 14, // ( a ⇢ n ) "get" : fetch the value stored at address a
  'PUT'    : 15, // ( n a ⇢ ) "put" : store value n at address a
  'ADD'    : 16, // ( n n ⇢ n ) "add" : replace TOS and NOS with their sum (x + y)
  'SUB'    : 17, // ( x y ⇢ n ) "add" : replace TOS and NOS with their difference (x - y)
  'MUL'    : 18, // ( n n ⇢ n ) "mul" : replace TOS and NOS with their product (x * y)
  'DVM'    : 19, // ( x y ⇢ r q ) "dvm" : divide TOS by NOS, leaving remainder and quotient (ex: 7 5 ⇢ 2 1 )
  'AND'    : 20, // ( n n ⇢ n ) "bitwise and"
  'OR'     : 21, // ( n n ⇢ n ) "bitwise inclusive or"
  'XOR'    : 22, // ( n n ⇢ n ) "bitwise exclusive or"
  'SHL'    : 23, // ( n b ⇢ n ) "shift left"  : shift n left by b bits (ex: 1 4 ⇢ 16 )
  'SHR'    : 24, // ( n b ⇢ n ) "shift right" : shift n right by b bits (ex: 65536 12 ⇢ 16 )
  'ZRET'   : 25, // [TOS ≠ 0]:( n ⇢ n ) [TOS = 0]:( n -- | a -- ) "return if zero"
  'INC'    : 26, // ( n ⇢ n ) "increment" : add 1 to TOS
  'DEC'    : 27, // ( n ⇢ n ) "decrement" : subtract 1 from TOS
  'IN'     : 28, // ( p ⇢ n ) "in"  : read the value currently in port p
  'OUT'    : 29, // ( n p ⇢ ) "out" : write value n to port p
  'WAIT'   : 30  // ( varies ) trigger IO after writing to a port with OUT
};

/**
 * Assemble instructions into machine code.
 *
 * @param {string} src - the assembly language source code to compile.
 * @return {Array} - Array of integers representing the compiled code.
 *
 * See test suite below for some example code.
 *
 */
function assemble( src ) {
  var phase, here, lines, words, word, ln, wd, res = [], labels = {};
  for (phase = 1; phase <= 2; ++phase) {
    here = 0;
    lines = src.split("\n");
    for (ln = 0; ln < lines.length; ++ln) {
      words = lines[ln].split(/\s/);
      for (wd=0; wd<words.length; ++wd) {
        word = words[wd];
        if (word[0] == "#") break;
        if (word[0] == ":") labels[ word.substr( 1 )] = here;
        else {
          here++;
          if (/^-?\d+$/.exec( word )) {  // match int literal
            here++; // because of the implicit "LIT" opcode
            if (phase==2) res.push(ops['<LIT:>'], parseInt(word, 10));
          }
          else if (phase == 1) {} // phase 1 only for labels
          else if (word in labels) res.push( labels[ word ]);
          else if (word in ops) res.push( ops[ word ]);
          else throw "Unresolved symbol '" + word + "' on line " + ln + ".";
        }
      }
    }
  }
  return res;
}

//---- test suite ----------------------------------------------

/**
 * Compare two arrays item by item to determine whether the contents are the same.
 * @param a - an array
 * @param b - an array
 * @returns {boolean} true if the items in the two arrays are equal
 */
function same(a, b) {
  for (var i=0; i<a.length; i++) if (a[i] != b[i]) return false;
  return true;
}

function assertSame(a,b, msg) {
  if (!same(a,b)) console.log("failed test <" + msg + "> : " + a + " != " + b);
}

function test_assembler() {

  assertSame(assemble( ':zero zero' ), [ 0 ],
    "normal label use");

  assertSame(assemble( 'one :one'   ), [ 1 ],
    "forward label use"); // the ++ comes first

  assertSame(assemble( ':zero zero 2 :three three' ), [ 0, 1, 2, 3 ],
    "literals should increment label counter by 2" );

}

if (console.clear !== undefined) console.clear(); // only in firebug
console.log("running tests.");
test_assembler();
console.log("done.");
