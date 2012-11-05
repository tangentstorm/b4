{  string formatting commands by michal j wallace [ circa 1996 :) ... taken from github.com/tangentstorm/silverware ]  }

 function clength( s : string ) : byte;
  var
   i,c : byte;
  begin
   c := 0;
   i := 1;
   while i <= length(s) do
    if
     s[ i ] = '|'
    then
     case s[i+1] of
      '@' : inc( i, 5 );
      '#' : inc( i, 4 );
      else inc( i, 2 );
     end
    else
     begin
      inc( c );
      inc( i );
     end;
   clength := c;
  end;

 function cstrip( s : string ) : string;
  var
   i : byte;
   c : string;
  begin
   c := '';
   i := 1;
   while i <= length(s) do
    if
     s[ i ] = '|'
    then
     case s[i+1] of
      '@' : inc( i, 5 );
      '#' : inc( i, 4 );
      else inc( i, 2 );
     end
    else
     begin
      c := c + s[i];
      inc( i );
     end;
   cstrip := c;
  end;


 function normaltext( s : string ) : string;
   var
    c : byte;
    t : string;
   begin
    t := '';
    for c := 1 to length( s ) do
     if
      s[c] = '|'
     then
      t := t + '|' + s[c]
     else
      t := t + s[c];
     normaltext := t;
   end;

 function strtrunc( s : string; len : byte ) : string;
  begin
   if ord( s[ 0 ] ) > len then s[ 0 ] := chr( len );
   strtrunc := s;
  end;

 function upstr( s : string ) : string;
  var
   count : byte;
  begin
   for count := 1 to length( s ) do
    s[ count ] := upcase( s[ count ] );
   upstr := s;
  end;

 function dncase( ch: char ) :char;
  begin
   if
    ( 'A' <= ch ) and ( ch <= 'Z' )
   then
    dncase := chr( ord( ch ) + 32 )
   else
    dncase := ch;
  end;

 function dnstr( s : string ) : string;
  var
   count : Byte;
  begin
   for count := 1 to length( s ) do
    s[ count ] := dncase( s[ count ] );
   dnstr := s;
   end;


 function chntimes( c : char; n : byte ) : string;
  var
   i : byte;
   s : string;
  begin
   s := '';
   if n <> 0 then for i := 1 to n do s := s + c;
   chntimes := s;
  end;

 function flushrt( s : string; n : byte; ch : char ) : string;
  begin
   if
    clength( s ) < n
   then
    insert( chntimes( ch, n-clength(s)), s, 1 );
   flushrt := s;
  end;


  
  function pad( s : string; len : byte; ch : char ) : string;
  begin
  if length( s ) > len then s := strtrunc( s, len );
   while length( s ) < len do s := s + ch;
   result := s;
  end;

  function padstr( s : string; len : byte; ch : char ) : string;
begin
  {
   if length( s ) > len then s := strtrunc( s, len );
   while length( s ) < len do s := s + ch;
   padstr := s;}
  result := pad( s, len, ch );
  end;


 function unpadstr( s : string; ch : char ) : string;
  begin
   while s[ length( s ) ] = ch do dec( byte( s[ 0 ] ));
   unpadstr := s;
  end;

 function cpadstr( s : string; len : byte; ch : char ) : string;
  begin
   if clength( s ) > len then s := strtrunc( s, len );
   while clength( s ) < len do s := s + ch;
   cpadstr := s;
  end;

