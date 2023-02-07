program ja;
uses empty, stacks, cw, kvm, li;

  type
    TTriplePart	= (sub, rel, obj);

  var
    state : boolean = true;
    i	  : integer;

  procedure bye;          begin halt(0) end;
  procedure compiler_on;  begin state := true;  end;
  procedure compiler_off; begin state := false; end;
  procedure show_words;
  begin
    for i := numwds-1 downto 1 do
      write(words[ i ].word, ' ');
    writeln(words[ 0 ].word);
  end;
  procedure echo_which; begin write(words[which].word, ' ') end;
  procedure cw_which; begin cwrite(words[which].word) end;

begin
  empty.createop( '/*',  @echo_which   );
  empty.createop( 'bye',    @bye          );
  empty.createop( 'clear',  @clrscr       );
  empty.createop( 'words',  @show_words   );
  empty.createop( 'package',  @echo_which   );
  empty.createop( 'import',  @echo_which   );
  empty.createop( 'public',  @echo_which   );
  empty.createop( '@Override',  @echo_which   );
  empty.createop( '{',  @echo_which   ); empty.createop( '}',  @echo_which );
  empty.createop( '(',  @echo_which   ); empty.createop( ')',  @echo_which );
  empty.createop( '[',  @compiler_off ); empty.createop( ']',  @compiler_on );
  empty.createop( 'void',  @echo_which   );
  empty.createop( 'for',  @echo_which   );
  empty.createop( 'return',  @echo_which   );
  empty.createop( ';',  @echo_which   );
  empty.createop( '=',  @echo_which   );
  empty.createop( '<',  @echo_which   );
  empty.createop( 'true',  @echo_which   );
  empty.createop( '==',  @echo_which   );
  empty.createop( '//',  @echo_which   );
  empty.createop( '&&',  @echo_which   );
  empty.mainloop;
end.
