// b4 cheat sheet

#set page(
  width: 8.5in,
  height: 11in,
  margin: (x: 0.35in, y: 0.3in),
  footer: align(center)[
    #set text(size: 6.5pt, style: "italic")
    Implementations: J · Pascal · Lil · JavaScript · GDScript
  ]
)

#set text(font: "DejaVu Sans", size: 8pt)
#let mono = text.with(font: "DejaVu Sans Mono")
#let code = text.with(font: "DejaVu Sans Mono", size: 7.5pt)
#let smallcode = text.with(font: "DejaVu Sans Mono", size: 6.5pt)
#let heading-font = text.with(weight: "bold", size: 9pt)

// Title
#align(center)[
  #text(size: 20pt, weight: "bold", tracking: 0.05em)[B4 VM]
  #v(0em)
  #text(size: 10pt, style: "italic")[a tiny forth-like virtual machine]
  #v(0em)
  #text(size: 9pt)[b4a assembly language reference]
  #v(0.1em)
  #code(size: 8pt)[github.com/tangentstorm/b4]
]

#v(0.2em)
#line(length: 100%, stroke: 0.75pt)
#v(0.15em)

// Helper for opcode entries
#let op(name, desc) = {
  box(width: 100%)[
    #grid(
      columns: (2.2em, 1fr),
      gutter: 0.3em,
      code(weight: "bold")[#name],
      text(size: 7.5pt, fill: luma(60))[#desc]
    )
  ]
}

#let smallop(name, desc) = {
  box(width: 100%)[
    #grid(
      columns: (2.5em, 1fr),
      gutter: 0.2em,
      smallcode(weight: "bold")[#name],
      text(size: 6.5pt)[#desc]
    )
  ]
}

#let section(title) = {
  v(0.3em)
  box(
    width: 100%,
    stroke: (bottom: 0.5pt + luma(180)),
    inset: (bottom: 2pt),
  )[#heading-font[#title]]
  v(0.15em)
}

// Shaded info box
#let infobox(content) = {
  block(
    fill: luma(245),
    stroke: 0.5pt + luma(200),
    inset: 5pt,
    radius: 2pt,
    width: 100%,
  )[#content]
}

// Top section: 3 columns - opcodes, syntax+stack, example
#grid(
  columns: (0.38fr, 0.27fr, 0.38fr),
  gutter: 1em,
  // Left: Opcode table
  [
    #text(size: 8pt, weight: "bold")[Opcode Table]
    #v(0.1em)
    #block(
      stroke: 0.5pt + luma(120),
      inset: 2pt,
      radius: 2pt,
    )[
      #set text(font: "DejaVu Sans Mono", size: 5.5pt)
      #table(
        columns: 17,
        stroke: none,
        inset: 1.8pt,
        align: center,
        fill: (x, y) => {
          let priv = y >= 3 and y <= 8 and x >= 1
          let resRow = (y == 11 or y == 12 or y == 14 or y == 15) and x >= 1
          let resC = y == 13 and x >= 3
          let resF1 = y == 16 and x >= 1 and x <= 6
          let resF2 = y == 16 and x >= 10 and x <= 13
          if priv { luma(235) } else if resRow or resC or resF1 or resF2 { luma(210) }
        },
        [], [+0], [+1], [+2], [+3], [+4], [+5], [+6], [+7], [+8], [+9], [+A], [+B], [+C], [+D], [+E], [+F],
        [\$0], [..], [\^A], [\^B], [\^C], [\^D], [\^E], [\^F], [\^G], [\^H], [\^I], [\^J], [\^K], [\^L], [\^M], [\^N], [\^O],
        [\$1], [\^P], [\^Q], [\^R], [\^S], [\^T], [\^U], [\^V], [\^W], [\^X], [\^Y], [\^Z], [\^\[], [\^\\], [\^\]], [\^\^], [\^\_],
        [\$2], [\@\@], [\@A], [\@B], [\@C], [\@D], [\@E], [\@F], [\@G], [\@H], [\@I], [\@J], [\@K], [\@L], [\@M], [\@N], [\@O],
        [\$3], [\@P], [\@Q], [\@R], [\@S], [\@T], [\@U], [\@V], [\@W], [\@X], [\@Y], [\@Z], [\@\[], [\@\\], [\@\]], [\@\^], [\@\_],
        [\$4], [!\@], [!A], [!B], [!C], [!D], [!E], [!F], [!G], [!H], [!I], [!J], [!K], [!L], [!M], [!N], [!O],
        [\$5], [!P], [!Q], [!R], [!S], [!T], [!U], [!V], [!W], [!X], [!Y], [!Z], [!\[], [!\\], [!\]], [!\^], [!\_],
        [\$6], [+\@], [+A], [+B], [+C], [+D], [+E], [+F], [+G], [+H], [+I], [+J], [+K], [+L], [+M], [+N], [+O],
        [\$7], [+P], [+Q], [+R], [+S], [+T], [+U], [+V], [+W], [+X], [+Y], [+Z], [+\[], [+\\], [+\]], [+\^], [+\_],
        [\$8], [ad], [sb], [ml], [dv], [md], [sh], [an], [or], [xr], [nt], [eq], [lt], [du], [sw], [ov], [zp],
        [\$9], [dc], [cd], [rb], [ri], [wb], [wi], [lb], [li], [rs], [ls], [jm], [hp], [h0], [cl], [rt], [nx],
        [\$A], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--],
        [\$B], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--],
        [\$C], [c0], [c1], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--],
        [\$D], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--],
        [\$E], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--], [--],
        [\$F], [--], [--], [--], [--], [--], [--], [c2], [n1], [c4], [--], [--], [--], [--], [io], [db], [hl],
      )
    ]
  ],
  // Middle: Syntax + Stack Notation
  [
    #text(size: 8pt, weight: "bold")[Syntax]
    #v(0.1em)
    #infobox[
      #set text(size: 6.5pt)
      #grid(
        columns: (2.8em, 1fr),
        row-gutter: 0.35em,
        column-gutter: 0.3em,
        smallcode[FF], [hex byte],
        smallcode['c], [ASCII char],
        smallcode[:foo], [define word],
        smallcode[:X], [set register X],
        smallcode[foo], [call word],
        smallcode[\^X], [call register X],
        smallcode[\#], [line comment],
      )
    ]
    #v(0.3em)
    #text(size: 8pt, weight: "bold")[Stack Notation]
    #v(0.1em)
    #infobox[
      #set text(size: 6.5pt)
      #smallcode[(before — after)] \
      #smallcode[x] = nos, #smallcode[y] = tos \
      #smallcode[ds] = data stack \
      #smallcode[cs] = control stack \
      #smallcode[a] = addr, #smallcode[n] = num, #smallcode[b] = bool
    ]
  ],
  // Right: Full example
  [
    #text(size: 8pt, weight: "bold")[Example: Nested Loops]
    #v(0.1em)
    #infobox[
      #set text(font: "DejaVu Sans Mono", size: 5.5pt)
      #let cmt(s) = text(fill: luma(100))[\##s]
      #cmt[ set \^E to emit a character] \
      :E lb 'e io rt \
      \
      #cmt[ set \^I to fetch loop counter] \
      :I cd cd du dc sw dc rt \
      \
      :loop \
      #h(0.5em) du .f #box(width: 4.5em)[] #cmt[ outer loop] \
      #h(1em) du \^I sb c1 ad #box(width: 1em)[] #cmt[ inner len] \
      #h(1em) .f lb '. \^E .n #box(width: 0.9em)[] #cmt[ print dots] \
      #h(1em) lb '| \^E #box(width: 3em)[] #cmt[ print pipe] \
      #h(0.5em) .n zp rt \
      \
      5 loop \
      \
      #cmt[ Output:] \
      #raw(". | .. | ... | .... | ..... |")
    ]
  ]
)

#v(0.15em)

// Main content in 4 columns
#columns(4, gutter: 0.7em)[

#section[Registers #raw("@.._")]
#text(size: 6.5pt)[32 registers: #code[\@] through #code[\_] and #code[A]–#code[Z]]
#v(0.1em)
#op("^X", "invoke register X as function")
#op("@X", "fetch from register X  ( — n)")
#op("!X", "store to register X  (n — )")
#op("+X", "add m, return old X (m — n)")

#section[Constants]
#op("c0", "push 0  ( — 0)")
#op("c1", "push 1  ( — 1)")
#op("c2", "push 2  ( — 2)")
#op("n1", "push -1  ( — -1)")
#op("c4", "push 4  ( — 4)")

#section[Literals]
#op("lb", "load unsigned byte  ( — n)")
#op("li", "load int32 little-endian  ( — n)")
#op("ls", "load signed byte  ( — n)")

#section[Arithmetic]
#op("ad", "add  (xy — x+y)")
#op("sb", "subtract  (xy — x-y)")
#op("ml", "multiply  (xy — x*y)")
#op("dv", "divide  (xy — x/y)")
#op("md", "modulo  (xy — x mod y)")
#op("sh", "shift left  (xy — x<<y)")

#section[Logic]
#op("an", "bitwise AND  (xy — r)")
#op("or", "bitwise OR  (xy — r)")
#op("xr", "bitwise XOR  (xy — r)")
#op("nt", "bitwise NOT  (x — ~x)")

#colbreak()

#section[Comparison]
#op("eq", "equal?  (xy — b)")
#op("lt", "less than?  (xy — b)")
#text(size: 6pt, style: "italic")[  b = -1 if true, else 0]

#section[Stack]
#op("du", "duplicate  (x — xx)")
#op("sw", "swap  (xy — yx)")
#op("ov", "over  (xy — xyx)")
#op("zp", "drop  (x — )")
#op("dc", "data→ctrl  (ds:x — cs:x)")
#op("cd", "ctrl→data  (cs:x — ds:x)")

#section[Memory]
#op("rb", "read unsigned byte  (a — n)")
#op("rs", "read signed byte  (a — n)")
#op("ri", "read int32  (a — n)")
#op("wb", "write byte  (x a — )")
#op("wi", "write int32  (x a — )")

#section[Control Flow]
#op("jm", "jump to addr in next 4 bytes")
#op("hp", "hop: add signed byte to ip")
#op("h0", "hop if zero  (x — )")
#op("cl", "call: push ip, jump to addr")
#op("rt", "return: pop cs to ip")
#op("nx", "next: dec cs, hop if > 0")
#op("hl", "halt vm")
#op("db", "trigger debugger")

#colbreak()

#section[I/O]
#text(size: 6pt, style: "italic")[#code[io] op with cmd byte taken from stack]
#v(0.1em)
#text(size: 6.5pt, weight: "bold")[Universal]
#smallop("'e", "emit char  (c — )")
#v(0.1em)
#text(size: 6.5pt, weight: "bold")[Pascal only]
#smallop("'a", "get arg v  (i — ptr)")
#smallop("'A", "get arg count  ( — n)")
#smallop("'d", "delete file  (ptr — )")
#smallop("'E", "get env var  (ptr — ptr)")
#smallop("'i", "read stdin  (ptr len — n)")
#smallop("'k", "kill process  (h — )")
#smallop("'m", "load file  (addr ptr — )")
#smallop("'o", "output to stdout  (c — )")
#smallop("'p", "spawn process  (ptr — h)")
#smallop("'r", "read from child  (ptr h — n)")
#smallop("'s", "save to file  (addr len ptr — )")
#smallop("'S", "sleep  (ms — )")
#smallop("'x", "file exists?  (ptr — b)")
#smallop("'z", "file size  (ptr — n)")

#section[Assembler Macros]
#op(".f", "for: begin loop")
#op(".n", "next: loop or break")
#op(".i", "if: skip if tos = 0")
#op(".e", "else")
#op(".t", "then: end if/else")
#op(".w", "while: begin loop")
#op(".d", "do: begin body")
#op(".o", "od: end while")
#op(".[", "begin quotation")
#op(".]", "end quotation")
#op(".^", "linked list ptr")
#op(".\"", "length-prefixed string")

#colbreak()

#section[b4i Interactive]
#text(size: 6.5pt, weight: "bold")[Inspect]
#smallop("?d", "data stack")
#smallop("?c", "control stack")
#smallop("?i", "instruction pointer")
#smallop("?R", "register R (e.g. ?H)")
#smallop("?100", "dump 16 bytes at addr")

#v(0.15em)
#text(size: 6.5pt, weight: "bold")[Control]
#smallop("%q", "quit")
#smallop("%s", "step one instruction")
#smallop("%g", "go (run until halt)")
#smallop("%C", "clear/reboot VM")
#smallop("%R", "reset VM")

#v(0.15em)
#text(size: 6.5pt, weight: "bold")[Files]
#smallop("\\a", "assemble .b4a file")
#smallop("\\i", "interpret .b4i script")
#smallop("\\d", "change directory")
#smallop("\\p", "print dictionary")

#v(0.15em)
#text(size: 6.5pt, weight: "bold")[Calculator Mode]
#text(size: 6pt)[
  #smallcode[FF] → push hex \
  #smallcode['a] → push ASCII \
  #smallcode[ad sw] → execute op \
  #smallcode[\^R] → invoke register
]

]
