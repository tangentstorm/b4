let b4 = (new function() {
  let EOF='\0',
  d=[], a=[],                             // data and auxiliary/return stack
  defs=[],core=[],scope=[[]],             // dictionary
  base=10,                                // numbers
  cp=-1,ch='\x01',ib=[],wd='',            // lexer state
  compiling=false,state=[],target=[];     // compiler state

  function def(k,v) {
    let res=defs.length; defs.push(v); scope[0].push([k,res]); return res }

  function tos() { return d.length ? d[d.length-1]:null }

  function nextch() {
    cp++;
    while (ib.length && (cp>=ib[0].length)) {cp=0; ib.shift()}
    ch = (ib.length ? ib[0][cp] : EOF)
    return ch }

  function word() {
    var res=[];
    while (ch <= ' ') if (ch===EOF) { return false } else nextch();
      while (ch > ' ') { res.push(ch); nextch() }
      wd = res.join('');
      return true }

  function send(x) { ib.push(x) }

  function findwd() {
    var found = false;
    for (var i=scope.length-1; i>=0 && !found; i--){
      for (var dict=scope[i], j=dict.length-1; j>=0 && !found; j--) {
        if (dict[j][0]===wd) { found=true; d.push(dict[j][1]) }}}
    return found }

  function lift(n) { return function(){ d.push(n) }}

  function number() {
    var i = parseInt(wd,base);
    if (isNaN(i)) {return false} else {d.push(i); return true }}

  function error(msg){ console.log('error:',msg); throw msg }

  function run(x) {
    send(x); var op, args=[], res='.';
    while (word()) {
      op = findwd() ? defs[d.pop()]
         : number() ? lift(d.pop())
         : ()=> error(wd+'?');
      if (compiling) { target.push(op) }
      else {
        args=[]; for (var i=0;i<op.length;i++) args.push(d.pop())
        res = op.apply(this, args)
        if(res!==undefined) d.push(res)}
      console.log(`wd: ${wd} args: ${args}  res: ${res} -> ${d}`) }}

  var coreOps=[
    ['+', (x,y)=> x+y ],
    ['-', (x,y)=> x-y ],
    ['*', (x,y)=> x*y ],
    ['base', ()=> d.push(base) ]
    ['compiling?', ()=> compiling.length ]]

  scope.push(core)
  for (var i=0; i<coreOps.length;++i) def.apply(this, coreOps[i])
  return { run, d }});

document.addEventListener("DOMContentLoaded", ()=>{
  b4.run('1 2 + 3 *');
  log = x=> { document.body.innerHTML+=`<p>${x}</p>` }
  log(b4.d.pop()) });
