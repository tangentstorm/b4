{$IFDEF NESTUNITS }
unit ng.files; implementation  { file i/o }
{$ENDIF}

  var
    { parallel arrays -- file handles and 'isopen?' flags }
    files : array of file of byte;
    flags : array of boolean;
    error : integer;

  { explicit range check }
  function valid_handle( handle :  int32 ): boolean;
  begin
    result := ( handle > 0 ) and ( handle < length( flags ))
  end; { valid_handle }

procedure file_open( const mode : int32; const path : string; var handle : int32 );
  var i : byte = 0;
begin {$I-}
  { reuse closed handles if possible  }
  handle := -1;  
  while ( handle = -1 ) and ( i < length( files )) do begin
    if not flags[ i ] then handle := i;
    inc( i );
  end;
  { otherwise, make a new one }
  if handle = -1 then begin
    setlength( files, length( files ) + 1 );
    setlength( flags, length( files ));
    handle := length( files ) - 1;
  end;
  error := 0;
  assign( files[ handle ], path );
  case mode of
    0 : { read }
	if fileexists( path ) then reset( files[ handle ])
	else error := -1;
    1 : { write }
	rewrite( files[ handle ]);
    2 : { append }
	begin
	  rewrite( files[ handle ]);
	  seek( files[ handle ], filesize( files[ handle ]))
	end;
    3 : { modify }
	if fileexists( path ) then rewrite( files[ handle ])
	else error := -1;
  end;
  if error <> 0 then error := ioresult;
  flags[ handle ] := error = 0;
end; {$I+}

procedure file_close( const handle : int32 );
begin
  if valid_handle( handle ) and flags[ handle ] then begin
    close( files[ handle ]);
    flags[ handle ] := false;
  end
end;

procedure file_read( const handle : int32; var result : int32 );
  var b : byte;
begin
  result := 0;
  if valid_handle( handle ) and ( flags[ handle ])
  then begin
    if eof( files[ handle ]) then file_close( handle )
    else begin
      read( files[ handle ], b );
      result := b;
    end
  end
end;

procedure file_write(const handle : int32; const ch : char; var error : int32 );
begin
  if not valid_handle( handle ) then error := -1
  else if not flags[ handle ] then error := -2
  else begin
    {$i-} write( files[ handle ], ord( ch )); {$i+}
    error := ioresult + 1; { success = 0 in pascal, 1 in ngaro spec }
  end;
end;

procedure file_getpos( const handle : int32; var result : int32 );
begin
  if valid_handle( handle ) then 
    result := filepos( files[ handle ])
  else result := -1;
end;

procedure file_setpos( const handle, pos : int32; var result : int32 );
begin
  result := 0;
  if valid_handle( handle ) then begin
    {$i-} seek( files[ handle ], pos ); {$i+}
    result := ioresult;
  end
end;

procedure file_size( const handle : int32; var result : int32 );
begin
  if valid_handle( handle ) then 
    result := filesize( files[ handle ])
  else result := -1;
end;

procedure file_delete( const path : string; var result : int32 );
begin
  result := 0; // failure in retro
  if sysutils.deletefile( path ) then result := -1; // success
end;

{$IFDEF NESTUNITS }
end.
{$ENDIF}
