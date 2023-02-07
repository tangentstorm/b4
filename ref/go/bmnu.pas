program bmnu;
uses crt;

const kmaxcount = 10;
type menu = object
   count : byte;
   choices : array[ 0 .. kmaxcount ] of string;
   constructor open( filename : string );
   procedure show;
end;


const tab = #9;
procedure hline;
var i : integer;
begin
   crt.textcolor( crt.blue );
   for i := 1 to crt.windmaxx do write( '-' );
   crt.normvideo;
end;


constructor menu.open( filename : string );
var f : text; s : string;
begin
   assign( f, filename );
   reset( f );
   
   self.count := 0;
   while ( self.count < kmaxcount ) and not eof( f ) do
   begin
      readln( f, s );
      if s <> '' then
      begin
         self.choices[ self.count ] := s;
         inc( self.count );
      end;
   end
end;


{ a stack for holding terminal data }
var vt_stack : array[ 0..255 ] of byte;
var vt_count: byte = 0;
procedure vt_push( v : byte );
begin
   vt_stack[ vt_count ] := v;
   inc( vt_count );
end;
function vt_pop : byte;
begin
   dec( vt_count );
   vt_pop := vt_stack[ vt_count ];
end;
procedure pushc; begin vt_push( crt.textattr ); end;
procedure popc; begin crt.textattr := vt_pop;  end;
procedure pushxy; begin vt_push( crt.wherex ); vt_push( crt.wherey ) end;
procedure popxy; var y:byte; begin y := vt_pop; crt.gotoxy( vt_pop, y ) end;


procedure menu.show;
var
   uc : char = #0; { user choice }
   i  : byte;
begin
   
   crt.clrscr;
   writeln( 'bmnu' );
   hline;
   for i := 0 to self.count - 1 do
   begin
      if odd( i ) then
         crt.textcolor( crt.cyan )
      else
         crt.textcolor( crt.magenta );
      write( ' ', i );
      pushc; crt.textcolor( crt.blue ); write( ' : ' ); popc;
      writeln( self.choices[ i ]);
   end;
   hline;
   
   repeat
      pushxy;
      crt.textcolor( crt.yellow );
      write( '> ' );
      crt.normvideo;
      uc := readkey;
      popxy;
   until uc in [ '0'..'9' ];
   
   i := ord( uc ) - ord( '0' );
   write( 'you picked: ' );
   crt.textcolor( crt.red );
   write( self.choices[ i ]);
   crt.normvideo;
   writeln( '.' );
   hline;
   crt.normvideo;
end;     

var m : menu;
begin
   m.open( 'bmnu.pas' );
   m.show;
end.
