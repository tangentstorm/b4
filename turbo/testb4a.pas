{$mode tp}
program testb4a;
uses tptester, ub4, ub4asm, crt;

{$F+}{ enable FAR calls to pass functions as parameters across units. }

procedure b4tests(testnum : word);
  begin
    case testnum of

      00: begin test('registers');
            assert(ram[ip] = ub4.minheap,
            'Code should start at ub4.minheap to avoid opcode collisions.')
          end;

      else tptester.stop
    end;
  end;

procedure b4setup;
  begin
    ub4.boot
  end;

{$F-}

begin
  clrscr;
  tptester.onsetup(b4setup);
  tptester.runtests(b4tests);
end.
