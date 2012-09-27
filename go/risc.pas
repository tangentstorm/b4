{$mode objfpc}
program risc;
{ mjw 2012-07-14 | direct port of RISC.mod for oberon by N.Wirth }

const
  MemSize = 1024;    (* in words *)
  _mov = 0; _and = 1; _ior = 2; _xor = 3; _lsl = 4; _asr = 5;
  _add = 8; _sub = 9; _mul = 10; _div = 11; _cmp=12;


var
   IR   : LONGINT;  (* instruction register *)
   PC   : LONGINT;  (* program counter *)
   N, Z : BOOLEAN;  (* condition regosters *)
   R    : array [ 0 .. 15 ] of LONGINT;


procedure Execute( var M: array of LONGINT );
  var pq, arga, argb, op, im : LONGINT;     (* instruction fields *)
  var adr, A, B, C     : LONGINT;
begin
  PC := 0;
  repeat                              (* interpretation cycle *)
    IR := M[ PC ];
    INC( PC );
    pq := IR div $40000000 mod 4;    (* insr. class *)
    arga :=  IR div $1000000 mod $10;
    argb :=  IR div $100000 mod $10;
    op := IR div $10000 mod $10;
    im := IR mod $10000;
    
    case pq of
      
      0, 1 :                         (* register instructions *)
        begin
          B := R[ argb ];
          
          if pq = 0 then C := R[ IR mod $10 ]
          else if ODD( IR div $10000000 ) then C := im + $0FFFF0000
          else C := im;
          
          case op of
            _mov : A := C;
            _and : A := B * C;
            _ior : A := B + C;
            _xor : A := B xor C;
            _LSL : A := B shl C;
            _asr : A := B shr C;
            // TODO: IF ODD( IR div 20000000H ) THEN A := SYSTEM.ROT( B, -C ) ELSE A := ASH( B, -C ) END
            _add : A := B + C;
            _sub, _cmp : A := B - C;
            _mul : A := B * C;
            _div : A := B div C;
          else
            writeln( 'invalid opcode' );
          end;
          
          if op <> _cmp then R[ arga ] := A;
          
          N := A < 0; 
          Z := A = 0;
          
        end;
      
      2 : (* memory instructions *)
        begin
          adr := ( R[ argb ] + IR ) mod $100000 div 4;
          if adr < MemSize then
            begin
              if ODD( IR div $20000000 ) then
                M[ adr ] := R[ arga ] 
              else
                R[ arga ] := M[ adr ];
            end
          else (* I/O *)
            if ODD( IR div $20000000 ) then
              begin
                if adr = $3FFF2 then
                  write( R[ arga ] )
                else if adr = $3FFF3 then
                  writeln( );
              end
            else 
              if adr = $3FFF2 then
                read( R[ arga ] );
              {TODO: else if adr = $3FFF3 then
                if S._class <> Texts.Int then R[ arga ] := 1
                else R[ arga ] := 0;}
        end;
      
      3 : (* branch instructions *)
        if (((arga= 0 ) and N )
            or (( arga= 1 ) and Z )
            or (( arga= 5 ) and N )
            or (( arga= 6 ) and ( N or Z ))
            or (  arga= 7 ) 
            or (( arga= 8 ) and not N )
            or (( arga= 9 ) and not Z )
            or (( arga= 13 ) and not N )
            or (( arga= 14 ) and not ( N or Z )))
        then
          begin
            if Odd( IR div $10000000 ) then R[ 15 ] := PC * 4;
            if Odd( IR div $20000000 ) then 
              PC := ( PC + ( IR mod $1000000 )) mod $40000
	    else 
              PC := R[ IR mod $10 ] div 4
          end;
    end;
  until PC = 0;
end;

begin
end.
