{ these were ported from ngaro.js, except file/io and enhanced text }

{ -- port 0 ------------------------------------------------- }

{ Nothing to do: port 0 isn't connected to a device.
  It's just used to signal that one side or the other
  has data to transfer. }

{ -- port 1 ------------------------------------------------- }

function handle_keyboard( msg :  integer ) : integer;
begin
  { direct input }
  if direct_input then
  begin
    result := lastKey;
    lastKey := 0;
  end else begin
    lastKey := tib.charCodeAt( 0 );
    tib := tib.substr( 1, tib.length - 1 );
    lastKey := 0;
  end
end;

{ -- port 2 : simple text output ---------------------------- }

function handle_write( msg : integer ) : integer;
var x : integer;
begin
  if msg = 1 then
    kvm.clrscr
  else begin
    x := data.pop;
    if x < 0 then clrscr else write( chr( x ));
  end;
  result := 0;
end;

{ -- port 3 : video refresh --------------------------------- }

function handle_mouse( msg : integer ) : integer;
begin
  result := 0;
  case msg of
    1 : data.push2( mx, my );
    2 : data.push( mb );
    else
      result := -1;
  end;
end;

{ -- port 4 : file i/o -------------------------------------- }

  type
    ngfile = record
	       handle : file of byte; 
	       assigned, opened, closed	: boolean;
	     end;			
  var
    files   : array of ngfile;

function handle_file_io( msg : integer ) : integer;
  const r = 0; w = 1; a = 2; m = 3;
  procedure nexthandle;
  begin
    
  end;
begin
  result := 0;
  case msg of
    +1 : saveimage;
    -1 : begin { open :: name -> mode -> handle }
	   { } 
	 end;
    -2 : begin { read :: handle -> flag }
	   
	 end;
    -3 : begin { write :: char -> handle -> flag }
	   
	 end;
    -4 : begin { close :: handle -> flag }
	   { 0 on successful close }
	 end;
    -5 : begin { fpos :: handle -> offset }
	 end;
    -6 : begin { seek :: offset -> handle -> flag }
	 end;
    -7 : begin { size :: handle -> size }
	   
	 end;
    -8 : begin { delete :: filename -> flag }
	   { -1 if deleted, else 0 }
	 end
    else
end;

{ -- port 5 : vm query -------------------------------------- }

function handle_vmquery( msg: integer ) : integer;
begin
  case msg of
    -1	: result := IMAGE_SIZE;
    -2	: result := FB_EXISTS;
    -3	: result := FB_WIDTH;
    -4	: result := FB_HEIGHT;
    -5	: result := data.sp;
    -6	: result := addr.sp;
    -7	: result := -1;
    -8	: begin
	   {
	     var foo = new Date;
	     var unixtime_ms = foo.getTime();
	     var unixtime = parseInt(unixtime_ms / 1000);
	     result : = unixtime;
	     }
	 end;
    -9	: result := 0;
    -11	: result := TERM_WIDTH;
    -12	: result := TERM_HEIGHT;
    else
      result := -1
  end
end;


{ -- port 6 : graphic canvas -------------------------------- }

function handle_canvas( msg: integer ) : integer;
  var x, y, h, w : integer;
begin
  result := 0;
  case msg of
    1 : rxCanvasSetColor(data.pop);
    2 : begin
	  data.pop2( y, x );
	  fb.fillRect(x, y, 2, 2);
	end;
    3 : begin
	  data.pop4( w, h, y, x );
	  fb.strokeRect(x, y, w, h);
	end;
    4 : begin
	  data.pop4( w, h, y, x );
	  fb.fillRect(x, y, w, h);
	end;
    5 : begin
	  data.pop3( h, y, x );
	  fb.fillRect(x, y, 2, h);
	end;
    6 : begin
	  data.pop3( w, y, x );
	  fb.fillRect(x, y, w, 2);
	end;
    7 : begin
	  data.pop3( w, y, x );
	  fb.beginPath;
	  fb.arc(x, y, w, 0, Math.PI*2, true);
	  fb.closePath();
	  fb.stroke();
	end;
    8 : begin
	  data.pop3( w, y, x );
	  fb.beginPath;
	  fb.arc(x, y, w, 0, Math.PI*2, true);
	  fb.closePath;
	  fb.fill;
	end
    else
      result := -1;
  end
end;
  
{ -- port 7 : mouse ----------------------------------------- }
  
function handle_mouse( msg : integer ) : integer;
begin
  result := 0;
  case msg of
    1 : data.push2( mx, my );
    2 : data.push( mb );
    else
      result := -1;
  end;
end;

{ -- port 8 : enhanced terminal ----------------------------- }

function handle_eterm( msg : integer ) : integer;
begin
  result := 0;
  case msg of
    1 : begin { * msg 1 : ( rc- ) row col }
        end;
    2 : begin { * msg 2 : ( n-  ) fg      }
        end;
    3 : begin { * msg 3 : ( n-  ) bg      }
        end;
    else
      result := -1;
  end;
end;

{ -- the port map ------------------------------------------- }

  portHandlers[1] = handle_keyboard;
  portHandlers[2] = handle_write;
  portHandlers[3] = handle_refresh;
  portHandlers[4] = handle_fileio;
  portHandlers[5] = handle_vmquery;
  portHandlers[6] = handle_canvas;
  portHandlers[7] = handle_mouse;
  portHandlers[8] = handle_eterm;
