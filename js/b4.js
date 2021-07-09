var b4 = (new function() { var EOF='\0',self = {

  d:[], a:[],                             // data and auxiliary/return stack
  defs:[],core:[],scope:[],               // dictionary
  base:10,                                // numbers
  cp:-1, ch:'\x01',ibuf:[],wd:'',         // lexer state
  compiling:false,state:[],target:[],     // compiler state
  def : function (k,v){
    var res=self.defs.length; self.defs.push(v); self.scope[0].push([k,res]); return res },

  tos : function(){return d.length ? d[d.length-1]:null},

  nextch : function(){
    this.cp++;
    while (this.ibuf.length && (this.cp>=this.ibuf[0].length)) {this.cp=0; this.ibuf.shift();}
    this.ch = (this.ibuf.length ? this.ibuf[0][this.cp] : EOF);
    return this.ch },

  word : function(){
    var res=[];
    while (this.ch <= ' ') if (this.ch===EOF) { return false } else this.nextch();
      while (this.ch > ' ') { res.push(this.ch); this.nextch() }
      this.wd = res.join('');
      return true },

  send : function(s){ self.ibuf.push(s) },

  findwd : function() {
      var found = false;
      for (var i=self.scope.length-1; i>=0 && !found; i--){
          for (var dict=self.scope[i], j=dict.length-1; j>=0 && !found; j--) {
              if (dict[j][0]===this.wd) { found=true; self.d.push(dict[j][1]) }}}
      return found },

  lift : function(n) { return function(){ self.d.push(n) }},

  number : function() {
      var i = parseInt(this.wd,this.base);
      if (isNaN(i)) {return false} else {self.d.push(i); return true }},

  error : function(msg){ console.log('error:',msg); throw msg },

  run : function(s){
      var op;
      this.send(s);
      while (this.word()) {
          op = this.findwd() ? this.defs[self.d.pop()]
             : this.number() ? this.lift(self.d.pop())
             : function() { self.error(self.wd+'?') };
          if (this.compiling) { self.target.push(op) } else op.call(self);
          console.log('wd: ', self.wd, '->', self.d); }
      return this;}
  };// end self

  var coreOps=[
      ['+', function (){self.d.push(self.d.pop()+self.d.pop())}],
      ['-', function(){self.d.push(self.d.pop()-self.d.pop())}],
      ['*', function (){self.d.push(self.d.pop()*self.d.pop())}],
      ['base', function(){self.d.push(self.base)}],
      ['compiling?', function(){self.d.push(self.compiling.length)}]];

  self.scope.push(self.core);
  for (var i=0; i<coreOps.length;++i) self.def.apply(self, coreOps[i]);
  return self;
});

document.addEventListener("DOMContentLoaded", ()=>{
  b4.run('1 2 + 3 *');
  log = x=> { document.body.innerHTML+=`<p>${x}</p>` }
  log(b4.d.pop()) });
