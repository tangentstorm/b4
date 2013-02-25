// b4a.js : assembler for ngaro virtual machine, javascript version
// http://retroforth.org/docs/The_Ngaro_Virtual_Machine.html

var ops = {
    "NOP"   :  0,  "<lit>" :  1,  "DUP"   :  2,   "DROP"  :  3,
    "SWAP"  :  4,  "PUSH"  :  5,  "POP"   :  6,   "LOOP"  :  7,
    "JMP:"  :  8,  "RET"   :  9,  "JLT:"  : 10,   "JGT:"  : 11,
    "JNE:"  : 12,  "JMP:"  : 13,  "GET"   : 14,   "PUT"   : 15,
    "ADD"   : 16,  "SUB"   : 17,  "MUL"   : 18,   "DVM"   : 19,
    "AND"   : 20,  "OR"    : 21,  "XOR"   : 22,   "SHL"   : 23,
    "SHR"   : 24,  "ZRT"   : 25,  "INC"   : 26,   "DEC"   : 27,
    "IN"    : 28,  "OUT"   : 29,  "WAIT"  : 30,
};

function assemble( src ) {
    var phase, here, lines, words, word, ln, wd, res = [], labels = {};
    for (phase = 1; phase<=2; phase++) {
        here = 0;
        lines = src.split("\n");
        for (ln = 0; ln < lines.length; ++ln) {
            words = lines[ln].split(/\s/)
            for (wd=0; wd<words.length; ++wd) {
                word = words[wd];
                if (word[0] == "#") break;
                if (word[0] == ":") labels[ word.substr( 1 )] = here;
                else {
                    here++;
                    if (/^-?\d+$/.exec( word )) {  // match int literal
                        here++; // because 
                        if (phase==2) res.push(ops['<lit>'], parseInt(word));
                    }
                    else if (phase == 1) continue // phase 1 is only to gather labels
                    else if (word in labels) res.push( labels[ word ]);
                    else if (word in ops) res.push( ops[ word ]);
                    else throw "Unresolved symbol '" + word + "' on line " + ln + ".";
                }
            }
        }
    }
    return res;
}

function same(a, b) {
    for (var i=0; i<a.length; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

function assertEqual(a,b, msg) {
    if (! same(a,b)) {
        console.log( "failed test <" + msg + "> : " + a + " != " + b );
    }
}

function test_assembler() {
    assertEqual(assemble( ':zero zero' ), [ 0 ], "normal label use");
    assertEqual(assemble( 'one :one'   ), [ 1 ], "forward label use"); // the ++ comes first
    assertEqual(assemble( ':zero zero 2 :three three' ), [ 0, 1, 2, 3 ],
           "literals should increment label counter by 2" );
}

console.clear(); // only in firebug
test_assembler();
console.log("done");
