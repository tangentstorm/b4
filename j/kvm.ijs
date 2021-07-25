NB.
NB. kvm: keyboard/video/mouse* for terminal apps in j
NB.
NB. * but no mouse stuff yet
NB. ----------------------------------------------------
cocurrent 'kvm'
coinsert 'vt' [ require 'vt.ijs'

ticks=: 0 NB. milliseconds between calling v
break=: 0

NB. with_kbd: run v on keypress, u every 'ticks' milliseconds.
NB. until i get keyp'' test working)
with_kbd =: {{
  u'' [ raw 1 [ y
  while. -. break_kvm_ do.
    if. keyp'' do. v rkey''
    else. sleep ticks end.
    u''
  end. raw 0 [ curs 1 }}


onkey =: {{
  select. c =. 1 26 27 31 126 127 I. k =. {.>y
  case. 0 do. vnm =. 'k_nul'
  case. 1 do. vnm =. 'kc_',a.{~97+<:k  NB. ascii ctrl+letter ^C-> kc_c
  case. 2 do. vnm =. 'k_esc'
    NB. check for immediate second key
    NB. TODO: this should be table driven, since different
    NB. terminals have different encodings.
    if. keyp'' do.
      if. 91 = k2=.>rkey'' do.
        select. k3=.{.>rkey''
        case.  65 do. vnm =. 'k_arup'
        case.  66 do. vnm =. 'k_ardn'
        case.  67 do. vnm =. 'k_arrt'
        case.  68 do. vnm =. 'k_arlf'
        case. do.
          if. 49 54 ({.@] < [)*.([ < {:@]) k do.
            select. kn =. 0". (k3{a.),wfc'~'
            case. 1 do. vnm =. 'k_home'   case. 2 do. vnm =. 'k_ins'
            case. 3 do. vnm=. 'k_del'    case. 4 do. vnm=. 'k_end'
            case. 5 do. vnm=. 'k_pgup'   case. 6 do. vnm=. 'k_pgdn'
            case. 7 do. vnm=. 'k_pgdn'
            case. 11 do. vnm=. 'k_f1'  NB. shift-f1 is same as f11
            case. 12 do. vnm=. 'k_f2'
            case. 13 do. vnm=. 'k_f3'  case. 25 do. vnm =. 'k_sf3'
            case. 14 do. vnm=. 'k_f4'  case. 26 do. vnm =. 'k_sf4'
            case. 15 do. vnm=. 'k_f5'  case. 28 do. vnm =. 'k_sf5'
            case. 16 do. NB. ?!?!      case. 27 do. ??
            case. 17 do. vnm=. 'k_f6'  case. 29 do. vnm =. 'k_sf6'
            case. 18 do. vnm=. 'k_f7'  case. 31 do. vnm =. 'k_sf7'
            case. 19 do. vnm=. 'k_f8'  case. 32 do. vnm =. 'k_sf8'
            case. 20 do. vnm=. 'k_f9'  case. 33 do. vnm =. 'k_sf9'
            case. 21 do. vnm=. 'k_f10'
            case. 22 do. NB. !?
            case. 23 do. vnm=. 'k_f11'  NB. same as shift-f1
            case. 24 do. vnm=. 'k_f12'  NB. same as shift-f2
            end.
          end.
        end.
        NB. TODO: esc x = alt x
      else.
        NB. echo 'unexpected key after esc:', ":k2
      end.
    end.
  NB. case. 3 do. NB. TODO ^\, ^], ^^, ^_ (FS,GS,RS,US)
  case. 4 do. vnm =. 'k_',a.{~k
  case. 5 do. vnm =. 'k_bksp'
  case.   do. vnm =. 'kx_',hfd k
    NB. hex code catchall (k=255)-> kc_ff
    NB. ^? = KDEL, other alt chars
  end.
  NB. ask for more keys unless break=1
  if. 3=4!:0<vnm do. (vnm~) a.{~>y
  elseif. (c=4)*.3=4!:0<'k_asc' do. k_asc k{a.
  elseif. 3=4!:0<'k_any' do. 1[k_any a.{~>y
  elseif. k e. 3 0 do. break_kvm_ =: 1
  end.
  sp =: putc@' '
  NB.reset @ ceol (puts ":>coname'') sp puts ": vnm [ sp puts ":y [ echo'' NB.goxy 0 9
  0 0$0}}

loop =: {{ u with_kbd onkey break_kvm_ =: 0 [ cocurrent y }}
