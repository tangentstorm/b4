# B4vm: Pascal Edition

This directory contains a simple implementation of the B4 environment
that works with the [free pascal compiler](https://www.freepascal.org/).


## features

 + stack based virtual machine (ub4.pas)
 + assembler (ub4a.pas)
 + visual debugger (b4.pas)
 + test suite (testb4a.pas)
 + a small imperative testing framework (tptester.pas)


## turbo pascal support

This code was originally written in turbo pascal 5.5 for MS-DOS (which you can
[download for free](http://edn.embarcadero.com/article/20803) and run
either on an old dos/windows box or inside [dosbox](http://www.dosbox.com/)).

I don't know if this code still works with turbo pascal, but free pascal is
a much more capable compiler, and *also* runs on MS-DOS, as well as about a
zillion other operating systems and architectures... So if you're looking
for retrocomputing support, fpc is probably the way to go.
