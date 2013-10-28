{$IFDEF NESTUNITS }
unit ng.input; implementation   { input stack }
{$ENDIF}


{ include pushes a new file onto the input stack }
{ we use a stack rather than a queue so that one file can import another }
procedure TNgaroVM.include( path : string );
  begin
    if not( sysutils.fileexists( path )) then begin
      writeln( '<< error: file "', path ,'" not found. press enter. >>' );
      readln;
    end else begin
      writeln( '<<include: "', path ,'">>' );

      { add to the input stack }
      setlength( self.inputs, length( self.inputs ) + 1);
      self.input := @self.inputs[ length( self.inputs ) - 1 ];

      { open the file }
      system.assign( self.input^, path );
      reset( self.input^ );

      { make sure we're reading from the file, rather than keyboard }
      self.devices[ 1 ] := @self.handle_input;
    end
  end; { vm.include }


{ next_input either switches to the next file or to the keyboard }
procedure TNgaroVM.next_input;
  var len : int32;
  begin
    len := length( self.inputs );
    if len > 0 then begin
      close( self.input^ );
      dec( len );
    end;
    setlength( self.inputs, len );
    if len = 0 then self.devices[ 1 ] := @self.handle_keyboard
    else self.input := @self.inputs[ len - 1 ];
  end;

{$IFDEF NESTUNITS }
end.
{$ENDIF}
