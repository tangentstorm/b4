

// virtual machine data:
int ip = 10;
byte[][] ram = new byte[16][256];
byte[] addr  = new byte[16];
byte[] data  = new byte[16];
byte page = 0;
byte pages = 16;

boolean paused = false;


// presentation:

int cs = 24; // cell size
int page_x = 64;
int page_y = 64;
int tabs_x = page_x;
int tabs_y = page_y - cs - 16;
int data_x = page_x - cs - 16;
int data_y = page_y;
int addr_x = page_x + 16 * cs + 16;
int addr_y = page_y;

int baseline = 16;
int indent = 4;

int textColor;
int zeroColor; // changes dynamically
 
int gray = 200;
int black = 0;
int white = 0xff;
int silver = 0xcc;

void drawCell( int x, int y, byte val ) {
  rect( x, y, cs, cs );
  fill( val == 0 ? zeroColor : textColor );
  text( hex( val ) , x + indent, y + baseline);
}

void drawRing( byte[] ring ) {
  for (int i = 0; i < ring.length; ++i) {
    fill( white );
    drawCell( 0, i * cs, ring[ i ]); 
  }
}

void drawTabs() {
  for (byte i = 0; i < pages; i++) {
    fill( i == page ? silver : white );
    zeroColor = 0;
    drawCell( i * cs, 0, i );
  }
}

void drawPage( byte p ) {
  int w = 16;
  for (int y = 0; y < w; y++)
    for (int x = 0; x < w; x++) {
      int a = y * w + x; // address
      byte v = ram[ p ][ a ];  // value 
      if (a == ip) { fill(0xe0); } else { fill(color(0xff)); }
      drawCell( cs * x, cs * y, v );
    }
}
  
void origin( int x, int y ) {
   pushMatrix();
  translate( x, y );
}

void setup() {
  size(800, 600);
  background(#ffffff); stroke(gray);
  textSize(12);
}

void draw() {
  textColor = 0; zeroColor = 0; stroke(white);
  origin( tabs_x, tabs_y ); drawTabs();       popMatrix();
  
  zeroColor = gray; stroke( silver );
  origin( page_x, page_y ); drawPage( page ); popMatrix();
  origin( data_x, data_y ); drawRing( data ); popMatrix();
  origin( addr_x, addr_y ); drawRing( addr ); popMatrix();
  if (!paused && ++ip > 0xff) {
    ip = 0; page++;
    if (page == pages) page = 0;
  }
}


void mouseClicked() {
  paused = !paused;
} 

