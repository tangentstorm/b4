
// these are the standard vga colors
final color[] palette = {
  color(  0,   0,   0), // black
  color(170,   0,   0), // red 
  color(  0, 170,   0), // green
  color(170,  85,   0), // brown
  color(  0,   0, 170), // blue
  color(170,   0, 170), // magenta
  color(  0, 170, 170), // cyan
  color(170, 170, 170), // gray
  color( 85,  85,  85), // dark gray
  color(255,  85,  85), // light red
  color( 85, 255,  85), // light green
  color(255, 255,  85), // yellow
  color( 85,  85, 255), // light blue
  color(255,  85, 255), // light magenta
  color( 85, 255, 255), // light cyan
  color(255, 255, 255)};// white



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
int page_y = 200;
int tabs_x = page_x;
int tabs_y = page_y - cs - 8;
int data_x = page_x - cs - 16;
int data_y = page_y;
int addr_x = page_x + 16 * cs + 16;
int addr_y = page_y;


int gw = 2;  // gutter width (gets multiplied by 2)
int bh = 48; // button height
int bw = 48; // button width
int bx = addr_x + 48;
int by = page_y;

int baseline1 = 12;
int baseline2 = 22;
int indent = 6;

int textColor;
int zeroColor; // changes dynamically
 
int gray = 0xB4;
int black = 0;
int white = 0xff;
int silver = 0xe8;

final int pixel = 8;
final int graphicsPage = 0x0f;
final int gx = bx;
final int gy = 8;

void drawCell( int x, int y, byte val ) {
  rect( x, y, cs, cs );
  fill( val == 0 ? zeroColor : textColor );
  text( hex( val ) , x + indent, y + baseline1);
  text( hex( val ) , x + indent, y + baseline2);
}

void drawRing( byte[] ring ) {
  for (int i = 0; i < ring.length; ++i) {
    fill( black );
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
      if (a == ip) { fill(palette[4]); } else { fill(color(black)); }
      drawCell( cs * x, cs * y, v );
    }
}

void drawButtons( ) {
  for (int y = 0; y < 4; y++) for (int x = 0; x < 4; x++ ) {
    rect( x * bw + gw, y * bh + gw, bh - gw, bw - gw );
  }
}
  
void origin( int x, int y ) {
   pushMatrix();
  translate( x, y );
}

void setup() {
  size(800, 600);
  background(#000000); stroke(palette[8]);
  textSize(10);
}


void drawGraphic() {
  int px, py; byte y, x; int pix2 = pixel * 2;
  for ( y = 0, py = 0; y < 0x0c; y++, py += pix2) 
  for ( x = 0, px = 0; x < 0x0f; x++, px += pix2) {
    noStroke();
    int qpix = ram[ graphicsPage ][ y * 0x0f + x ];

    // upper left
    fill( palette[ 0 ]);
    rect( px,         py,         pixel, pixel ); 

    // upper right
    fill( palette[ 1 ]);
    rect( px + pixel, py,         pixel, pixel ); 

    // lower left
    fill( palette[ 2 ]);
    rect( px,         py + pixel, pixel, pixel ); 

    // lower right
    fill( palette[ 4 ]);
    rect( px + pixel, py + pixel, pixel, pixel ); 
  } 

}

void draw() {
  textColor = 0; zeroColor = 0; stroke(white);
  origin( tabs_x, tabs_y ); drawTabs();       popMatrix();
  
  zeroColor = gray; stroke( silver );
  origin( page_x, page_y ); drawPage( page ); popMatrix();
  origin( data_x, data_y ); drawRing( data ); popMatrix();
  origin( addr_x, addr_y ); drawRing( addr ); popMatrix();

  origin( bx, by ); drawButtons( ); popMatrix();

  origin( gx, gy ); drawGraphic( ); popMatrix();

  if (!paused && ++ip > 0xff) {
    ip = 0; page++;
    if (page == pages) page = 0;
  }
}


void mouseClicked() {
  paused = !paused;
} 

