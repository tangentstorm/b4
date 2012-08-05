/**********************************************************************
 * Ngaro Virtual Machine
 * Copyright (C) 2008 - 2011, Charles Childers
 **********************************************************************/


/**********************************************************************
 * Some constants useful to us for dealing with the VM settings.
 **********************************************************************/
  var   IMAGE_SIZE    = 256000;         /* Amount of simulated RAM    */
  var   DATA_DEPTH    =    128;         /* Depth of data stack        */
  var   ADDRESS_DEPTH =   1000;         /* Depth of the stacks        */
  var   TERM_WIDTH    =     79;         /* Width of virtual terminal  */
  var   TERM_HEIGHT   =     18;         /* Height of virtual terminal */
  var   FB_WIDTH      =    640;         /* Canvas Width               */
  var   FB_HEIGHT     =    480;         /* Canvas Height              */
  var   FB_EXISTS     =     -1;         /* Is Canvas Present?         */



/**********************************************************************
 * Stack object. Provides an easy way to create and work with LIFO
 * stacks.
 **********************************************************************/
function Stack(size)
{
  this.sp    = 0;
  this.data  = new Array(size);
  this.push  = function(n) { this.sp++; this.data[this.sp] = n; }
  this.pop   = function()  { return this.data[this.sp--]; }
  this.depth = function()  { return this.sp; }
  this.tos   = function()  { return this.data[this.sp]; }
  this.nos   = function()  { return this.data[this.sp - 1]; }
  this.dup   = function()  { this.push(this.tos()); }
  this.drop  = function()  { this.sp--; }
  this.swap  = function()  { var a = this.nos();
                             this.data[this.sp - 1] = this.tos();
                             this.data[this.sp] = a;
                           }
  this.inc   = function()  { this.data[this.sp]++; }
  this.dec   = function()  { this.data[this.sp]--; }
  this.reset = function()  { this.sp = 0; }
}


/**********************************************************************
 * Symbolic constants for each instruction.
 **********************************************************************/
function Opcodes()
{
  this.NOP = 0;       this.LIT = 1;         this.DUP = 2;
  this.DROP = 3;      this.SWAP = 4;        this.PUSH = 5;
  this.POP = 6;       this.LOOP = 7;        this.JUMP = 8;
  this.RETURN = 9;    this.GT_JUMP = 10;    this.LT_JUMP = 11;
  this.NE_JUMP = 12;  this.EQ_JUMP = 13;    this.FETCH = 14;
  this.STORE = 15;    this.ADD = 16;        this.SUB = 17;
  this.MUL = 18;      this.DIVMOD = 19;     this.AND = 20;
  this.OR = 21;       this.XOR = 22;        this.SHL = 23;
  this.SHR = 24;      this.ZERO_EXIT = 25;  this.INC = 26;
  this.DEC = 27;      this.IN = 28;         this.OUT = 29;
  this.WAIT = 30;
}



/**********************************************************************
 * Internal registers, flags, and variables
 **********************************************************************/
var ip = 0;
var data    = new Stack(DATA_DEPTH);
var address = new Stack(ADDRESS_DEPTH);
var ports   = new Array(64);
var portHandlers = new Array(64);

var image   = new Array(IMAGE_SIZE);
var vm = new Opcodes();


var instructions = new Array(vm.WAIT);


/**********************************************************************
 * Control (and fine tuning) of the VM Processing
 *
 * Specifically, these routines will use form elements to alter the
 * number of instructions processed per cycle.
 *
 * This implementation has a timer, by default called every 75 ms. If
 * the prior run has not completed, it exits. Otherwise, it processes
 * a specific number of instructions (5000 by default) and then returns.
 *
 * A variable 'frequency' is used to set the number of times each second
 * that the processor runs. A second variable 'cycles' controls the
 * number of instructions processed per cycle.
 *
 * You can start or suspend execution with rxStartVM() and rxStopVM().
 * The state can be toggled via rxToggleVM().
 **********************************************************************/
var interval;
var frequency = 75;
var cycles    = 5000;
var run = 0;

function rxStartVM()
{
  interval = setInterval("rxProcessImage()", frequency);
  run = 1;
  tib += "  ";
}

function rxStopVM()
{
  clearInterval(interval);
  interval = null;
  run = 0;
}

function rxToggleVM()
{
  if (run == 0)
  {
    rxStartVM();
    document.getElementById("vmtoggle").value = "pause vm";
  }
  else
  {
    rxStopVM();
    document.getElementById("vmtoggle").value = "resume vm";
  }
}

function rxSetInterval()
{
  rxStopVM();
  frequency = document.getElementById('frequency').value;
  try
  {
    localStorage.setItem("rxFrequency", frequency);
  }
  catch (e)
  {
    alert("Sorry, but we couldn't save the frequency settings for later use.");
  }
  rxStartVM();
}

function rxSetCyclesPerInterval()
{
  rxStopVM();
  cycles = document.getElementById('cycles').value;
  try
  {
    localStorage.setItem("rxCycles", cycles);
  }
  catch (e)
  {
    alert("Sorry, but we couldn't save the cycle settings for later use.");
  }
  rxStartVM();
}

function rxPrepareVM()
{
  ip  = 0;
  ports[0] = 0;
  width = 0;
  var i = 0;
  while (i < 64)
  {
    ports[i] = 0;
    i++;
  }
  data.reset();
  address.reset();

  if (localStorage.getItem("rxFrequency") === null)
  {
    frequency = 75;
  }
  else
  {
    frequency = localStorage['rxFrequency'];
    document.getElementById('frequency').value = frequency;
  }
  if (localStorage.getItem("rxCycles") === null)
  {
    cycles = 5000;
  }
  else
  {
    cycles = localStorage['rxCycles'];
    document.getElementById('cycles').value = cycles;
  }
}



/**********************************************************************
 * Keyboard Handling
 *
 * We have two approaches to handling the keyboard. The default is to
 * store a string containing input read from a text box, and extract
 * the characters from this. This is the "buffered" input model. The
 * alternate (and original method) uses a custom keyboard handler to
 * catch keystrokes and stores the most recent one in a variable.
 *
 * There are merits to both approaches. We use the buffered model as
 * the default as it works on more devices (tablets and phones without
 * a physical keyboard won't work with the non-buffered input).
 **********************************************************************/
var inputMethod = 1;
var lastKey = " ";
var tib = "";

function rxReadKeyboard(e)
{
  var uni = e.keyCode ? e.keyCode : e.charCode;
  lastKey = uni;
  if (uni == 8)
    return false;
}

function rxToggleInputMethod()
{
  if (inputMethod == 0)
  {
    document.onkeypress = null;
    inputMethod = 1;
  }
  else
  {
    document.onkeypress = rxReadKeyboard;
    inputMethod = 0;
  }
}

function rxProcessInput()
{
  tib = tib + document.getElementById('tib').value + "  ";
  document.getElementById('tib').value = "";
}



/**********************************************************************
 * Console Output
 *
 * We use a <div> with an id of "output" as the simulated terminal
 * window. The output is cached in a variable (rxOutputCache) and is
 * updated periodically.
 *
 * Output is wrapped to TERM_WIDTH. The width is tracked by a variable
 * (width), and is reset after each wrapping.
 **********************************************************************/
var rxOutputCache;

function rxDisplayCharacter(c)
{
  fbText.save();
  var lineHeight = 16;
  fbText.strokeStyle = "white";
  if (c != 10)
  {
    fbText.drawString(String.fromCharCode(c), Terminus12, fb_x, fb_y)
    fb_x += 8;
    if (fb_x >= FB_WIDTH)
    {
      fb_x = 0;
      fb_y += lineHeight;
    }
  }
  else
  {
    fb_x = 0;
    fb_y += lineHeight;
  }
  if (c < 0)
  {
    rxClearCanvas();
    fb_x = 0;
    fb_y = lineHeight;
  }
  if (fb_y >= (FB_HEIGHT - lineHeight))
  {
    rxOutputCache = fbText.getImageData(0, lineHeight, FB_WIDTH, FB_HEIGHT);
    fbText.clearRect(0, 0, FB_WIDTH, FB_HEIGHT);
    fbText.putImageData(rxOutputCache, 0, 0);
    fb_x = 0;
    fb_y -= lineHeight;
  }
  fbText.restore();
}



/**********************************************************************
 * Image Management
 *
 * Functions for loading a saved image, saving the image, and restoring
 * to a clean image are here.
 **********************************************************************/
function rxLoadImage()
{
  rxClearCanvas();
  rxStopVM();
  ip = 0;
  tib = "";
  try
  {
    image = localStorage['retroImage'].split(';').map(function(x){return parseInt(x)});
  }
  catch (e)
  {
    alert("Sorry, but we couldn't find a saved image.");
  }
  data.reset();
  address.reset();
  rxStartVM();
}

function rxSaveImage()
{
  rxStopVM();
  try
  {
    localStorage.setItem("retroImage", image.join(";"));
  }
  catch (e)
  {
    alert("Sorry, but we couldn't save your image.");
  }
  rxStartVM();
}

function rxLoadCleanImage()
{
  rxClearCanvas();
  rxStopVM();
  loadImage();
  ip = 0;
  tib = "";
  data.reset();
  address.reset();
  rxStartVM();
}



/**********************************************************************
 * Simulated Device Handlers
 *
 * We have a function for each I/O port, and a generalized dispatch
 * loop.
 *
 * Due to technical constraints, the keyboard input is handled by the
 * dispatch loop, and port 3 for display updating is handled by the
 * rxProcessImage() function.
 *
 * See "The Ngaro Language" for a description of each port.
 *
 * Canvas and Mouse devices are handled via a handler in canvas.js
 * currently.
 **********************************************************************/
portHandlers[2] = function()
{
  rxDisplayCharacter(data.pop());
  ports[2] = 0;
}

portHandlers[4] = function()
{
  ports[4] = 0;
  saveImage();
}

portHandlers[5] = function()
{
  if (ports[5] == -1)
    ports[5] = IMAGE_SIZE;
  if (ports[5] == -2)
    ports[5] = FB_EXISTS;
  if (ports[5] == -3)
    ports[5] = FB_WIDTH;
  if (ports[5] == -4)
    ports[5] = FB_HEIGHT;
  if (ports[5] == -5)
    ports[5] = data.depth();
  if (ports[5] == -6)
    ports[5] = address.depth();
  if (ports[5] == -7)
    ports[5] = -1;
  if (ports[5] == -8)
  {
    var foo = new Date;
    var unixtime_ms = foo.getTime();
    var unixtime = parseInt(unixtime_ms / 1000);
    ports[5] = unixtime;
  }
  if (ports[5] == -9)
    ports[5] = 0;
  if (ports[5] == -11)
    ports[5] = TERM_WIDTH;
  if (ports[5] == -12)
    ports[5] = TERM_HEIGHT;
}

function handleDevices()
{
  if (ports[0] != 0)
    return;

  ports[0] = 1;

  /* Input */
  if (ports[1] == 1 && inputMethod == 0)
  {
    ports[1] = lastKey;
    lastKey = 0;
    return;
  }
  if (ports[1] == 1 && inputMethod == 1)
  {
    ports[1] = tib.charCodeAt(0);
    tib = tib.substr(1, tib.length - 1);
    lastKey = 0;
    return;
  }

  if (ports[2] == 1)
  {
    if (typeof portHandlers[2] == 'function')
      portHandlers[2]();
    return;
  }

  for (var a = 4; a < 64; a++)
  {
    if (ports[a] != 0)
    {
      if (typeof portHandlers[a] == 'function')
      {
        portHandlers[a]();
        return;
      }
    }
  }
}



/**********************************************************************
 * The Opcode Processor
 *
 * This is the heart of Ngaro. It handles carrying out the operations of
 * each of the instructions.
 *
 * See "The Ngaro Virtual Machine" for details on the behavior of each
 * instruction.
 **********************************************************************/
instructions[vm.NOP] = function() { }

instructions[vm.LIT] = function()
{
  ip++;
  data.push(image[ip]);
}

instructions[vm.DUP] = function()
{
  data.dup();
}

instructions[vm.DROP] = function()
{
  data.drop();
}

instructions[vm.SWAP] = function()
{
  data.swap();
}

instructions[vm.PUSH] = function()
{
  address.push(data.pop());
}

instructions[vm.POP] = function()
{
  data.push(address.pop())
}

instructions[vm.LOOP] = function()
{
  data.dec();
  if (data.tos() != 0)
  {
    ip++;
    ip = image[ip] - 1;
  }
  else
  {
    ip++;
    data.drop();
  }
}

instructions[vm.JUMP] = function()
{
  ip++;
  ip = image[ip] - 1;
  if (image[ip + 1] == 0) ip++;
  if (image[ip + 1] == 0) ip++;
}

instructions[vm.RETURN] = function()
{
  ip = address.pop();
  if (image[ip + 1] == 0) ip++;
  if (image[ip + 1] == 0) ip++;
}

instructions[vm.GT_JUMP] = function()
{
  ip++;
  if (data.nos() > data.tos())
    ip = image[ip] - 1;
  data.drop();
  data.drop();
}

instructions[vm.LT_JUMP] = function()
{
  ip++;
  if (data.nos() < data.tos())
    ip = image[ip] - 1;
  data.drop();
  data.drop();
}

instructions[vm.NE_JUMP] = function()
{
  ip++;
  if (data.nos() != data.tos())
    ip = image[ip] - 1;
  data.drop();
  data.drop();
}

instructions[vm.EQ_JUMP] = function()
{
  ip++;
  if (data.nos() == data.tos())
    ip = image[ip] - 1;
  data.drop();
  data.drop();
}

instructions[vm.FETCH] = function()
{
  x = data.pop();
  data.push(image[x]);
}

instructions[vm.STORE] = function()
{
  image[data.tos()] = data.nos();
  data.drop();
  data.drop();
}

instructions[vm.ADD] = function()
{
  var x = data.pop();
  var y = data.pop();
  data.push(x + y);
}

instructions[vm.SUB] = function()
{
  var x = data.pop();
  var y = data.pop();
  data.push(y - x);
}

instructions[vm.MUL] = function()
{
  var x = data.pop();
  var y = data.pop();
  data.push(y * x);
}

instructions[vm.DIVMOD] = function()
{
  var b = data.pop();
  var a = data.pop();
  if (b == 0)
  {
    ip = 0;
    data.sp = 0;
    address.sp = 0;
  }
  else
  {
    var x = Math.abs(b);
    var y = Math.abs(a);
    var q = Math.floor(y / x);
    var r = y % x;
    if (a < 0 && b < 0)
      r = r * -1;
    if (a > 0 && b < 0)
      q = q * -1;
    if (a < 0 && b > 0)
    {
      r = r * -1;
      q = q * -1;
    }
    data.push(r);
    data.push(q);
  }
}

instructions[vm.AND] = function()
{
  var x = data.pop();
  var y = data.pop();
  data.push(x & y);
}

instructions[vm.OR] = function()
{
  var x = data.pop();
  var y = data.pop();
  data.push(x | y);
}

instructions[vm.XOR] = function()
{
  var x = data.pop();
  var y = data.pop();
  data.push(x ^ y);
}

instructions[vm.SHL] = function()
{
  var x = data.pop();
  var y = data.pop();
  data.push(y << x);
}

instructions[vm.SHR] = function()
{
  var x = data.pop();
  var y = data.pop();
  data.push(y >>= x);
}

instructions[vm.ZERO_EXIT] = function()
{
  if (data.tos() == 0)
  {
    data.drop();
    ip = address.pop();
  }
}

instructions[vm.INC] = function()
{
  data.inc();
}

instructions[vm.DEC] = function()
{
  data.dec();
}

instructions[vm.IN] = function()
{
  var x = data.pop();
  data.push(ports[x]);
  ports[x] = 0;
}

instructions[vm.OUT] = function()
{
  var x = data.pop();
  var y = data.pop();
  ports[x] = y;
}

instructions[vm.WAIT] = function()
{
  handleDevices();
}

function processOpcode()
{
  var op = image[ip];
  if (op <= vm.WAIT)
  {
    instructions[op]();
  }
  else
  {
    address.push(ip);
    ip = op - 1;
    if (image[ip + 1] == 0) ip++;
    if (image[ip + 1] == 0) ip++;
  }
  ip++;
  checkStack();
}

function checkStack()
{
  var depth  = data.depth();
  var adepth = address.depth();
  var flag = 0;
  if (depth < 0 || adepth < 0)
  {
    flag = -1;
  }
  if (depth > DATA_DEPTH || adepth > DATA_DEPTH)
  {
    flag = -1;
  }

  if (flag == -1)
  {
    ip = 0;
    data.sp = 0;
    address.sp = 0;
  }
}

function rxProcessImage()
{
  if (run == 0)
    return;

  run = 0;

  if (inputMethod == 0)
    for (var a = cycles; a > 0; a--)
      processOpcode();
  else
    for (var a = cycles; a > 0 && tib != ""; a--)
      processOpcode();

  run = 1;
}



/**********************************************************************
 * Mouse / Touch Support
 **********************************************************************/
var mx, my, mb;

function moveMouse(e)
{
  if (e.offsetX)
  {
    mx = e.offsetX;
    my = e.offsetY;
  }
  else if (e.layerX)
  {
    mx = e.layerX;
    my = e.layerY;
  }
  return true;
}

function setButton(e)
{
  mb = 1;
  return true;
}

function releaseButton(e)
{
  mb = 0;
  return true;
}

document.onmousedown = setButton;
document.onmouseup = releaseButton;
document.onmousemove = moveMouse;
document.touchstart = setButton;
document.touchend = releaseButton;

portHandlers[7] = function()
{
  if (ports[7] == 1)
  {
    data.push(mx);
    data.push(my);
    ports[7] = 0;
  }
  if (ports[7] == 2)
  {
    data.push(mb);
    ports[7] = 0;
  }
}



/**********************************************************************
 * Canvas Support
 **********************************************************************/
var fb, fbraw, fbText, fbTextRaw, fb_x, fb_y;

function rxClearCanvas()
{
  fb.clearRect(0, 0, FB_WIDTH, FB_HEIGHT);
  fbText.clearRect(0, 0, FB_WIDTH, FB_HEIGHT);
  fb_x = 0;
  fb_y = 0;
}

function rxPrepareCanvas(t, d)
{
  fbTextRaw = document.getElementById(t);
  fbText = fbTextRaw.getContext("2d");

  fbraw = document.getElementById(d);
  fb = fbraw.getContext("2d");
  rxClearCanvas();
  TERM_WIDTH  = Math.floor(FB_WIDTH / 8);
  TERM_HEIGHT = Math.floor(FB_HEIGHT / 16);
}

function rxCanvasSetColor(c)
{
  if (c == 0)
    fb.fillStyle = "black";
  if (c == 1)
    fb.fillStyle = "darkblue";
  if (c == 2)
    fb.fillStyle = "darkgreen";
  if (c == 3)
    fb.fillStyle = "darkcyan";
  if (c == 4)
    fb.fillStyle = "darkred";
  if (c == 5)
    fb.fillStyle = "purple";
  if (c == 6)
    fb.fillStyle = "brown";
  if (c == 7)
    fb.fillStyle = "darkgray";
  if (c == 8)
    fb.fillStyle = "gray";
  if (c == 9)
    fb.fillStyle = "blue";
  if (c == 10)
    fb.fillStyle = "green";
  if (c == 11)
    fb.fillStyle = "cyan";
  if (c == 12)
    fb.fillStyle = "red";
  if (c == 13)
    fb.fillStyle = "magenta";
  if (c == 14)
    fb.fillStyle = "yellow";
  if (c == 15)
    fb.fillStyle = "white";
  if (c < 0 || c > 15)
    fb.fillStyle = "black";
}

portHandlers[6] = function()
{
  if (ports[6] == 1)
  {
    rxCanvasSetColor(data.pop());
    ports[6] = 0;
  }
  if (ports[6] == 2)
  {
    var x, y;
    y = data.pop();
    x = data.pop();
    fb.fillRect(x, y, 2, 2);
    ports[6] = 0;
  }
  if (ports[6] == 3)
  {
    var x, y, h, w;
    w = data.pop();
    h = data.pop();
    y = data.pop();
    x = data.pop();
    fb.strokeRect(x, y, w, h);
    ports[6] = 0;
  }
  if (ports[6] == 4)
  {
    var x, y, h, w;
    w = data.pop();
    h = data.pop();
    y = data.pop();
    x = data.pop();
    fb.fillRect(x, y, w, h);
    ports[6] = 0;
  }
  if (ports[6] == 5)
  {
    var x, y, h;
    h = data.pop();
    y = data.pop();
    x = data.pop();
    fb.fillRect(x, y, 2, h);
    ports[6] = 0;
  }
  if (ports[6] == 6)
  {
    var x, y, w;
    w = data.pop();
    y = data.pop();
    x = data.pop();
    fb.fillRect(x, y, w, 2);
    ports[6] = 0;
  }
  if (ports[6] == 7)
  {
    var x, y, w;
    w = data.pop();
    y = data.pop();
    x = data.pop();
    fb.beginPath();
    fb.arc(x, y, w, 0, Math.PI*2, true);
    fb.closePath();
    fb.stroke();
    ports[6] = 0;
  }
  if (ports[6] == 8)
  {
    var x, y, w;
    w = data.pop();
    y = data.pop();
    x = data.pop();
    fb.beginPath();
    fb.arc(x, y, w, 0, Math.PI*2, true);
    fb.closePath();
    fb.fill();
    ports[6] = 0;
  }
}



/**********************************************************************
 * Save and/or Run A Project
 **********************************************************************/
function rxSaveProject()
{
  var project = "rx_project";
  try
  {
    localStorage.setItem(project, document.getElementById('project').value);
  }
  catch (e)
  {
    alert("Sorry, but we couldn't save your project.");
  }
}

function rxLoadSavedProject()
{
  var project = "rx_project";
  if (localStorage.getItem(project) === null)
  {
    document.getElementById('project').value = "";
  }
  else
  {
    document.getElementById('project').value = localStorage[project];
  }
}

function rxRunProject()
{
  tib += document.getElementById('project').value + "  ";
}

function rxNewProject()
{
  document.getElementById('project').value = "";
}


/**********************************************************************
 * Misc. Other Routines
 **********************************************************************/
function toggleVisibilityOf(id)
{
  var e = document.getElementById(id);
  if (e.style.display == 'block')
    e.style.display = 'none';
  else
    e.style.display = 'block';
}

