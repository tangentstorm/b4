cocurrent 'kbd'

NB. terminal mode keyboard event driver
NB. supports windows and linux.


NB. unix-style terminals.
NB. jconsole automatically recovers from raw mode
NB. when it issues the prompt, so calling u_raw is safe.
NB. -------------------

NB. smoutput a.{~>  u_rkey u_raw''
NB.

with =: {{ v '' [ raw 1  while. 1-:v rkey'' do.  u''  end.  raw 0 }}


NB. windows terminal:
NB. -----------------

ReadConsoleInput =: 'kernel32 ReadConsoleInputW b x *s s *s' & cd
'FOCUS KEY MENU MOUSE SIZE' =.  16b10 16b01 16b08 16b02 16b04
RBS =: 10 NB. read buffer size

w_rkey =: {{ for.i.y do.
  'r h b n nr' =. ReadConsoleInput stdi;(*#~RBS);1;(,0)
  select. 0 { b
    case. FOCUS do. echo 'FOCUS ',":b
    case. KEY do.
      dn=. 2{b
      'rc kc sc ch' =. 4{.4}.b
      ch =. u:ch
      mod =. '.x'{~_32{.!.0#: 65536 65536 #. _2{.b
      smoutput 'dn:';dn;'rc:';rc;'kc:';kc;'ch:';ch;'mod:';mod
    case. MENU do.  echo 'MENU: ',":b
    case. MOUSE do. echo 'MOUSE: ',":b
    case. SIZE do. echo 'SIZE: ',":b
    case. do. 'UNKNOWN EVENT!' throw.
  end.
end. }}

init =: {{
  if. IFWIN do.
    stdi =: >{. 'kernel32 GetStdHandle *c s' cd <_10
    NB. TODO: jshowconsole_j_
    NB. echo 'r h m' =. 'kernel32 GetConsoleMode b x *x' cd stdi;,0
    raw  =: ]
    rkey =: w_rkey
  elseif. IFUNIX do.
    u_raw1 =: '' [ [: 2!:0 'stty raw -echo' []
    u_raw0 =: ][ [: 2!:0 'stty -raw echo' []
    u_libc =: >0{ (LF cut 2!:0) ::(a:"_) 'locate libc.so'
    raw  =: {{ if. y do. u_raw1'' else. u_raw0'' end. }}
    rkey =: (u_libc,' getchar l') & cd
  else.
    'kvm only supports windows and unix platforms.'
  end. >a: }}
