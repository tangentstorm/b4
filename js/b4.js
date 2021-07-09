let b4 = (new function() {
  let EOF='\0',
  d=[], a=[],                             // data and auxiliary/return stack
  defs=[],core=[],scope=[[]],             // dictionary
  base=10,                                // numbers
  cp=-1,ch='\x01',ib=[],wd='',            // lexer state
  compiling=0,state=[],target=[];         // compiler state

  //function log(x) { document.body.innerHTML += `<p>${x}</p>` }
  function log(x) { console.log(x) }

  function def(k,v) {
    let res=defs.length; defs.push(v); scope[0].push([k,res]); return res }

  function nx() {
    while (ib.length && (++cp>=ib[0].length)) {cp=0; ib.shift()}
    return ch = (ib.length ? ib[0][cp] : EOF) }

  function word() { let res=[];
    while (ch <= ' ') if (ch===EOF) return 0; else nx()
    while (ch > ' ') { res.push(ch); nx() }
    wd = res.join(''); return 1 }

  function findwd() { let found = 0;
    for (var i=scope.length-1; i>=0 && !found; i--){
      for (var dict=scope[i], j=dict.length-1; j>=0 && !found; j--) {
        if (dict[j][0]===wd) { found=1; d.push(dict[j][1]) }}}
    return found }

  function number() { let i = parseInt(wd,base);
    return isNaN(i) ? 0 : d.push(i), 1 }

  function err(msg){ console.error(msg); throw msg }

  function run(x) { ib.push(x); var op, args=[], res='.', trace=[]
    while (word()) {
      op = findwd() ? defs[d.pop()]
         : number() ? (n=>()=>n)(d.pop())
         : ()=> err(wd+'?');
      if (compiling) { target.push(op) }
      else {
        args=[]; for (var i=0;i<op.length;i++) args.push(d.pop())
        res = op.apply(this, args)
        if(res!==undefined) d.push(res)}
      trace.push({ wd, op:op.toString(), args:JSON.stringify(args),
                   res, d:JSON.stringify(d) })}
    console.table(trace) }

  var coreOps=[
    ['+', (x,y)=> x+y ],    ['-', (x,y)=> x-y ],
    ['*', (x,y)=> x*y ],    ['/', (x,y)=> x/y ],
    ['base', ()=> base ]
    ['compiling?', ()=>compiling ]]

  scope.push(core)
  for (var i=0; i<coreOps.length;++i) def.apply(this, coreOps[i])
  return { run, d }});

document.addEventListener("DOMContentLoaded", ()=>{
  b4.run('2 1 + 3 *');
  log = x=> { document.body.innerHTML+=`<p>${x}</p>` }
  log(b4.d.pop()) });
