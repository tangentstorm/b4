function loadImage()
{
  var lit = vm.LIT, wait=vm.WAIT, out=vm.OUT; // from ngaro.js

  image = [ 

      // set color to red:
      // : 4 1 6 out wait ;
      lit, 4, lit, 1, lit, 6, out, wait,

      // draw a pixel
      // : 320 200 2 6 out wait ;
      lit, 320, lit, 200, lit, 2, lit, 6, out, wait,

      // write 0 to port 3 to force video update. 
      lit, 0, lit, 3, out, // wait, // no wait necessary

      // infinite loop
      vm.JUMP, 0
  ];
 
  // change the jump target to the vm.JUMP instruction
  // so the code I'm trying to debug doesn't loop :)
  image[ image.length - 1 ] = image.length - 2;

  ip = 0;
}
