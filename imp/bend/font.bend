import Base

# 8x8 glyphs, LSB is the leftmost pixel.

def row_of_32(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      0
    case 3:
      0
    case 4:
      0
    case 5:
      0
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_33(+row: U32) -> U32:
  match row:
    case 0:
      24
    case 1:
      60
    case 2:
      60
    case 3:
      24
    case 4:
      24
    case 5:
      0
    case 6:
      24
    case 7:
      0
    case _:
      0

def row_of_34(+row: U32) -> U32:
  match row:
    case 0:
      54
    case 1:
      54
    case 2:
      0
    case 3:
      0
    case 4:
      0
    case 5:
      0
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_35(+row: U32) -> U32:
  match row:
    case 0:
      54
    case 1:
      54
    case 2:
      127
    case 3:
      54
    case 4:
      127
    case 5:
      54
    case 6:
      54
    case 7:
      0
    case _:
      0

def row_of_36(+row: U32) -> U32:
  match row:
    case 0:
      12
    case 1:
      62
    case 2:
      3
    case 3:
      30
    case 4:
      48
    case 5:
      31
    case 6:
      12
    case 7:
      0
    case _:
      0

def row_of_37(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      99
    case 2:
      51
    case 3:
      24
    case 4:
      12
    case 5:
      102
    case 6:
      99
    case 7:
      0
    case _:
      0

def row_of_38(+row: U32) -> U32:
  match row:
    case 0:
      28
    case 1:
      54
    case 2:
      28
    case 3:
      110
    case 4:
      59
    case 5:
      51
    case 6:
      110
    case 7:
      0
    case _:
      0

def row_of_39(+row: U32) -> U32:
  match row:
    case 0:
      6
    case 1:
      6
    case 2:
      3
    case 3:
      0
    case 4:
      0
    case 5:
      0
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_40(+row: U32) -> U32:
  match row:
    case 0:
      24
    case 1:
      12
    case 2:
      6
    case 3:
      6
    case 4:
      6
    case 5:
      12
    case 6:
      24
    case 7:
      0
    case _:
      0

def row_of_41(+row: U32) -> U32:
  match row:
    case 0:
      6
    case 1:
      12
    case 2:
      24
    case 3:
      24
    case 4:
      24
    case 5:
      12
    case 6:
      6
    case 7:
      0
    case _:
      0

def row_of_42(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      102
    case 2:
      60
    case 3:
      255
    case 4:
      60
    case 5:
      102
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_43(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      12
    case 2:
      12
    case 3:
      63
    case 4:
      12
    case 5:
      12
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_44(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      0
    case 3:
      0
    case 4:
      0
    case 5:
      12
    case 6:
      12
    case 7:
      6
    case _:
      0

def row_of_45(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      0
    case 3:
      63
    case 4:
      0
    case 5:
      0
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_46(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      0
    case 3:
      0
    case 4:
      0
    case 5:
      12
    case 6:
      12
    case 7:
      0
    case _:
      0

def row_of_47(+row: U32) -> U32:
  match row:
    case 0:
      96
    case 1:
      48
    case 2:
      24
    case 3:
      12
    case 4:
      6
    case 5:
      3
    case 6:
      1
    case 7:
      0
    case _:
      0

def row_of_48(+row: U32) -> U32:
  match row:
    case 0:
      62
    case 1:
      99
    case 2:
      115
    case 3:
      123
    case 4:
      111
    case 5:
      103
    case 6:
      62
    case 7:
      0
    case _:
      0

def row_of_49(+row: U32) -> U32:
  match row:
    case 0:
      12
    case 1:
      14
    case 2:
      12
    case 3:
      12
    case 4:
      12
    case 5:
      12
    case 6:
      63
    case 7:
      0
    case _:
      0

def row_of_50(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      51
    case 2:
      48
    case 3:
      28
    case 4:
      6
    case 5:
      51
    case 6:
      63
    case 7:
      0
    case _:
      0

def row_of_51(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      51
    case 2:
      48
    case 3:
      28
    case 4:
      48
    case 5:
      51
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_52(+row: U32) -> U32:
  match row:
    case 0:
      56
    case 1:
      60
    case 2:
      54
    case 3:
      51
    case 4:
      127
    case 5:
      48
    case 6:
      120
    case 7:
      0
    case _:
      0

def row_of_53(+row: U32) -> U32:
  match row:
    case 0:
      63
    case 1:
      3
    case 2:
      31
    case 3:
      48
    case 4:
      48
    case 5:
      51
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_54(+row: U32) -> U32:
  match row:
    case 0:
      28
    case 1:
      6
    case 2:
      3
    case 3:
      31
    case 4:
      51
    case 5:
      51
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_55(+row: U32) -> U32:
  match row:
    case 0:
      63
    case 1:
      51
    case 2:
      48
    case 3:
      24
    case 4:
      12
    case 5:
      12
    case 6:
      12
    case 7:
      0
    case _:
      0

def row_of_56(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      51
    case 2:
      51
    case 3:
      30
    case 4:
      51
    case 5:
      51
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_57(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      51
    case 2:
      51
    case 3:
      62
    case 4:
      48
    case 5:
      24
    case 6:
      14
    case 7:
      0
    case _:
      0

def row_of_58(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      12
    case 2:
      12
    case 3:
      0
    case 4:
      0
    case 5:
      12
    case 6:
      12
    case 7:
      0
    case _:
      0

def row_of_59(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      12
    case 2:
      12
    case 3:
      0
    case 4:
      0
    case 5:
      12
    case 6:
      12
    case 7:
      6
    case _:
      0

def row_of_60(+row: U32) -> U32:
  match row:
    case 0:
      24
    case 1:
      12
    case 2:
      6
    case 3:
      3
    case 4:
      6
    case 5:
      12
    case 6:
      24
    case 7:
      0
    case _:
      0

def row_of_61(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      63
    case 3:
      0
    case 4:
      0
    case 5:
      63
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_62(+row: U32) -> U32:
  match row:
    case 0:
      6
    case 1:
      12
    case 2:
      24
    case 3:
      48
    case 4:
      24
    case 5:
      12
    case 6:
      6
    case 7:
      0
    case _:
      0

def row_of_63(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      51
    case 2:
      48
    case 3:
      24
    case 4:
      12
    case 5:
      0
    case 6:
      12
    case 7:
      0
    case _:
      0

def row_of_64(+row: U32) -> U32:
  match row:
    case 0:
      62
    case 1:
      99
    case 2:
      123
    case 3:
      123
    case 4:
      123
    case 5:
      3
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_65(+row: U32) -> U32:
  match row:
    case 0:
      12
    case 1:
      30
    case 2:
      51
    case 3:
      51
    case 4:
      63
    case 5:
      51
    case 6:
      51
    case 7:
      0
    case _:
      0

def row_of_66(+row: U32) -> U32:
  match row:
    case 0:
      63
    case 1:
      102
    case 2:
      102
    case 3:
      62
    case 4:
      102
    case 5:
      102
    case 6:
      63
    case 7:
      0
    case _:
      0

def row_of_67(+row: U32) -> U32:
  match row:
    case 0:
      60
    case 1:
      102
    case 2:
      3
    case 3:
      3
    case 4:
      3
    case 5:
      102
    case 6:
      60
    case 7:
      0
    case _:
      0

def row_of_68(+row: U32) -> U32:
  match row:
    case 0:
      31
    case 1:
      54
    case 2:
      102
    case 3:
      102
    case 4:
      102
    case 5:
      54
    case 6:
      31
    case 7:
      0
    case _:
      0

def row_of_69(+row: U32) -> U32:
  match row:
    case 0:
      127
    case 1:
      70
    case 2:
      22
    case 3:
      30
    case 4:
      22
    case 5:
      70
    case 6:
      127
    case 7:
      0
    case _:
      0

def row_of_70(+row: U32) -> U32:
  match row:
    case 0:
      127
    case 1:
      70
    case 2:
      22
    case 3:
      30
    case 4:
      22
    case 5:
      6
    case 6:
      15
    case 7:
      0
    case _:
      0

def row_of_71(+row: U32) -> U32:
  match row:
    case 0:
      60
    case 1:
      102
    case 2:
      3
    case 3:
      3
    case 4:
      115
    case 5:
      102
    case 6:
      124
    case 7:
      0
    case _:
      0

def row_of_72(+row: U32) -> U32:
  match row:
    case 0:
      51
    case 1:
      51
    case 2:
      51
    case 3:
      63
    case 4:
      51
    case 5:
      51
    case 6:
      51
    case 7:
      0
    case _:
      0

def row_of_73(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      12
    case 2:
      12
    case 3:
      12
    case 4:
      12
    case 5:
      12
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_74(+row: U32) -> U32:
  match row:
    case 0:
      120
    case 1:
      48
    case 2:
      48
    case 3:
      48
    case 4:
      51
    case 5:
      51
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_75(+row: U32) -> U32:
  match row:
    case 0:
      103
    case 1:
      102
    case 2:
      54
    case 3:
      30
    case 4:
      54
    case 5:
      102
    case 6:
      103
    case 7:
      0
    case _:
      0

def row_of_76(+row: U32) -> U32:
  match row:
    case 0:
      15
    case 1:
      6
    case 2:
      6
    case 3:
      6
    case 4:
      70
    case 5:
      102
    case 6:
      127
    case 7:
      0
    case _:
      0

def row_of_77(+row: U32) -> U32:
  match row:
    case 0:
      99
    case 1:
      119
    case 2:
      127
    case 3:
      127
    case 4:
      107
    case 5:
      99
    case 6:
      99
    case 7:
      0
    case _:
      0

def row_of_78(+row: U32) -> U32:
  match row:
    case 0:
      99
    case 1:
      103
    case 2:
      111
    case 3:
      123
    case 4:
      115
    case 5:
      99
    case 6:
      99
    case 7:
      0
    case _:
      0

def row_of_79(+row: U32) -> U32:
  match row:
    case 0:
      28
    case 1:
      54
    case 2:
      99
    case 3:
      99
    case 4:
      99
    case 5:
      54
    case 6:
      28
    case 7:
      0
    case _:
      0

def row_of_80(+row: U32) -> U32:
  match row:
    case 0:
      63
    case 1:
      102
    case 2:
      102
    case 3:
      62
    case 4:
      6
    case 5:
      6
    case 6:
      15
    case 7:
      0
    case _:
      0

def row_of_81(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      51
    case 2:
      51
    case 3:
      51
    case 4:
      59
    case 5:
      30
    case 6:
      56
    case 7:
      0
    case _:
      0

def row_of_82(+row: U32) -> U32:
  match row:
    case 0:
      63
    case 1:
      102
    case 2:
      102
    case 3:
      62
    case 4:
      54
    case 5:
      102
    case 6:
      103
    case 7:
      0
    case _:
      0

def row_of_83(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      51
    case 2:
      7
    case 3:
      14
    case 4:
      56
    case 5:
      51
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_84(+row: U32) -> U32:
  match row:
    case 0:
      63
    case 1:
      45
    case 2:
      12
    case 3:
      12
    case 4:
      12
    case 5:
      12
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_85(+row: U32) -> U32:
  match row:
    case 0:
      51
    case 1:
      51
    case 2:
      51
    case 3:
      51
    case 4:
      51
    case 5:
      51
    case 6:
      63
    case 7:
      0
    case _:
      0

def row_of_86(+row: U32) -> U32:
  match row:
    case 0:
      51
    case 1:
      51
    case 2:
      51
    case 3:
      51
    case 4:
      51
    case 5:
      30
    case 6:
      12
    case 7:
      0
    case _:
      0

def row_of_87(+row: U32) -> U32:
  match row:
    case 0:
      99
    case 1:
      99
    case 2:
      99
    case 3:
      107
    case 4:
      127
    case 5:
      119
    case 6:
      99
    case 7:
      0
    case _:
      0

def row_of_88(+row: U32) -> U32:
  match row:
    case 0:
      99
    case 1:
      99
    case 2:
      54
    case 3:
      28
    case 4:
      28
    case 5:
      54
    case 6:
      99
    case 7:
      0
    case _:
      0

def row_of_89(+row: U32) -> U32:
  match row:
    case 0:
      51
    case 1:
      51
    case 2:
      51
    case 3:
      30
    case 4:
      12
    case 5:
      12
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_90(+row: U32) -> U32:
  match row:
    case 0:
      127
    case 1:
      99
    case 2:
      49
    case 3:
      24
    case 4:
      76
    case 5:
      102
    case 6:
      127
    case 7:
      0
    case _:
      0

def row_of_91(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      6
    case 2:
      6
    case 3:
      6
    case 4:
      6
    case 5:
      6
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_92(+row: U32) -> U32:
  match row:
    case 0:
      3
    case 1:
      6
    case 2:
      12
    case 3:
      24
    case 4:
      48
    case 5:
      96
    case 6:
      64
    case 7:
      0
    case _:
      0

def row_of_93(+row: U32) -> U32:
  match row:
    case 0:
      30
    case 1:
      24
    case 2:
      24
    case 3:
      24
    case 4:
      24
    case 5:
      24
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_94(+row: U32) -> U32:
  match row:
    case 0:
      8
    case 1:
      28
    case 2:
      54
    case 3:
      99
    case 4:
      0
    case 5:
      0
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_95(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      0
    case 3:
      0
    case 4:
      0
    case 5:
      0
    case 6:
      0
    case 7:
      255
    case _:
      0

def row_of_96(+row: U32) -> U32:
  match row:
    case 0:
      12
    case 1:
      12
    case 2:
      24
    case 3:
      0
    case 4:
      0
    case 5:
      0
    case 6:
      0
    case 7:
      0
    case _:
      0

def row_of_97(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      30
    case 3:
      48
    case 4:
      62
    case 5:
      51
    case 6:
      110
    case 7:
      0
    case _:
      0

def row_of_98(+row: U32) -> U32:
  match row:
    case 0:
      7
    case 1:
      6
    case 2:
      6
    case 3:
      62
    case 4:
      102
    case 5:
      102
    case 6:
      59
    case 7:
      0
    case _:
      0

def row_of_99(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      30
    case 3:
      51
    case 4:
      3
    case 5:
      51
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_100(+row: U32) -> U32:
  match row:
    case 0:
      56
    case 1:
      48
    case 2:
      48
    case 3:
      62
    case 4:
      51
    case 5:
      51
    case 6:
      110
    case 7:
      0
    case _:
      0

def row_of_101(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      30
    case 3:
      51
    case 4:
      63
    case 5:
      3
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_102(+row: U32) -> U32:
  match row:
    case 0:
      28
    case 1:
      54
    case 2:
      6
    case 3:
      15
    case 4:
      6
    case 5:
      6
    case 6:
      15
    case 7:
      0
    case _:
      0

def row_of_103(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      110
    case 3:
      51
    case 4:
      51
    case 5:
      62
    case 6:
      48
    case 7:
      31
    case _:
      0

def row_of_104(+row: U32) -> U32:
  match row:
    case 0:
      7
    case 1:
      6
    case 2:
      54
    case 3:
      110
    case 4:
      102
    case 5:
      102
    case 6:
      103
    case 7:
      0
    case _:
      0

def row_of_105(+row: U32) -> U32:
  match row:
    case 0:
      12
    case 1:
      0
    case 2:
      14
    case 3:
      12
    case 4:
      12
    case 5:
      12
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_106(+row: U32) -> U32:
  match row:
    case 0:
      48
    case 1:
      0
    case 2:
      48
    case 3:
      48
    case 4:
      48
    case 5:
      51
    case 6:
      51
    case 7:
      30
    case _:
      0

def row_of_107(+row: U32) -> U32:
  match row:
    case 0:
      7
    case 1:
      6
    case 2:
      102
    case 3:
      54
    case 4:
      30
    case 5:
      54
    case 6:
      103
    case 7:
      0
    case _:
      0

def row_of_108(+row: U32) -> U32:
  match row:
    case 0:
      14
    case 1:
      12
    case 2:
      12
    case 3:
      12
    case 4:
      12
    case 5:
      12
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_109(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      51
    case 3:
      127
    case 4:
      127
    case 5:
      107
    case 6:
      99
    case 7:
      0
    case _:
      0

def row_of_110(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      31
    case 3:
      51
    case 4:
      51
    case 5:
      51
    case 6:
      51
    case 7:
      0
    case _:
      0

def row_of_111(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      30
    case 3:
      51
    case 4:
      51
    case 5:
      51
    case 6:
      30
    case 7:
      0
    case _:
      0

def row_of_112(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      59
    case 3:
      102
    case 4:
      102
    case 5:
      62
    case 6:
      6
    case 7:
      15
    case _:
      0

def row_of_113(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      110
    case 3:
      51
    case 4:
      51
    case 5:
      62
    case 6:
      48
    case 7:
      120
    case _:
      0

def row_of_114(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      59
    case 3:
      110
    case 4:
      102
    case 5:
      6
    case 6:
      15
    case 7:
      0
    case _:
      0

def row_of_115(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      62
    case 3:
      3
    case 4:
      30
    case 5:
      48
    case 6:
      31
    case 7:
      0
    case _:
      0

def row_of_116(+row: U32) -> U32:
  match row:
    case 0:
      8
    case 1:
      12
    case 2:
      62
    case 3:
      12
    case 4:
      12
    case 5:
      44
    case 6:
      24
    case 7:
      0
    case _:
      0

def row_of_117(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      51
    case 3:
      51
    case 4:
      51
    case 5:
      51
    case 6:
      110
    case 7:
      0
    case _:
      0

def row_of_118(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      51
    case 3:
      51
    case 4:
      51
    case 5:
      30
    case 6:
      12
    case 7:
      0
    case _:
      0

def row_of_119(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      99
    case 3:
      107
    case 4:
      127
    case 5:
      127
    case 6:
      54
    case 7:
      0
    case _:
      0

def row_of_120(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      99
    case 3:
      54
    case 4:
      28
    case 5:
      54
    case 6:
      99
    case 7:
      0
    case _:
      0

def row_of_121(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      51
    case 3:
      51
    case 4:
      51
    case 5:
      62
    case 6:
      48
    case 7:
      31
    case _:
      0

def row_of_122(+row: U32) -> U32:
  match row:
    case 0:
      0
    case 1:
      0
    case 2:
      63
    case 3:
      25
    case 4:
      12
    case 5:
      38
    case 6:
      63
    case 7:
      0
    case _:
      0

def row_of_123(+row: U32) -> U32:
  match row:
    case 0:
      56
    case 1:
      12
    case 2:
      12
    case 3:
      7
    case 4:
      12
    case 5:
      12
    case 6:
      56
    case 7:
      0
    case _:
      0

def row_of_124(+row: U32) -> U32:
  match row:
    case 0:
      24
    case 1:
      24
    case 2:
      24
    case 3:
      0
    case 4:
      24
    case 5:
      24
    case 6:
      24
    case 7:
      0
    case _:
      0

def row_of_125(+row: U32) -> U32:
  match row:
    case 0:
      7
    case 1:
      12
    case 2:
      12
    case 3:
      56
    case 4:
      12
    case 5:
      12
    case 6:
      7
    case 7:
      0
    case _:
      0

def row_of_126(+row: U32) -> U32:
  match row:
    case 0:
      110
    case 1:
      59
    case 2:
      0
    case 3:
      0
    case 4:
      0
    case 5:
      0
    case 6:
      0
    case 7:
      0
    case _:
      0

def glyph(+ch: U32, +row: U32) -> U32:
  match ch:
    case 32:
      row_of_32(row)
    case 33:
      row_of_33(row)
    case 34:
      row_of_34(row)
    case 35:
      row_of_35(row)
    case 36:
      row_of_36(row)
    case 37:
      row_of_37(row)
    case 38:
      row_of_38(row)
    case 39:
      row_of_39(row)
    case 40:
      row_of_40(row)
    case 41:
      row_of_41(row)
    case 42:
      row_of_42(row)
    case 43:
      row_of_43(row)
    case 44:
      row_of_44(row)
    case 45:
      row_of_45(row)
    case 46:
      row_of_46(row)
    case 47:
      row_of_47(row)
    case 48:
      row_of_48(row)
    case 49:
      row_of_49(row)
    case 50:
      row_of_50(row)
    case 51:
      row_of_51(row)
    case 52:
      row_of_52(row)
    case 53:
      row_of_53(row)
    case 54:
      row_of_54(row)
    case 55:
      row_of_55(row)
    case 56:
      row_of_56(row)
    case 57:
      row_of_57(row)
    case 58:
      row_of_58(row)
    case 59:
      row_of_59(row)
    case 60:
      row_of_60(row)
    case 61:
      row_of_61(row)
    case 62:
      row_of_62(row)
    case 63:
      row_of_63(row)
    case 64:
      row_of_64(row)
    case 65:
      row_of_65(row)
    case 66:
      row_of_66(row)
    case 67:
      row_of_67(row)
    case 68:
      row_of_68(row)
    case 69:
      row_of_69(row)
    case 70:
      row_of_70(row)
    case 71:
      row_of_71(row)
    case 72:
      row_of_72(row)
    case 73:
      row_of_73(row)
    case 74:
      row_of_74(row)
    case 75:
      row_of_75(row)
    case 76:
      row_of_76(row)
    case 77:
      row_of_77(row)
    case 78:
      row_of_78(row)
    case 79:
      row_of_79(row)
    case 80:
      row_of_80(row)
    case 81:
      row_of_81(row)
    case 82:
      row_of_82(row)
    case 83:
      row_of_83(row)
    case 84:
      row_of_84(row)
    case 85:
      row_of_85(row)
    case 86:
      row_of_86(row)
    case 87:
      row_of_87(row)
    case 88:
      row_of_88(row)
    case 89:
      row_of_89(row)
    case 90:
      row_of_90(row)
    case 91:
      row_of_91(row)
    case 92:
      row_of_92(row)
    case 93:
      row_of_93(row)
    case 94:
      row_of_94(row)
    case 95:
      row_of_95(row)
    case 96:
      row_of_96(row)
    case 97:
      row_of_97(row)
    case 98:
      row_of_98(row)
    case 99:
      row_of_99(row)
    case 100:
      row_of_100(row)
    case 101:
      row_of_101(row)
    case 102:
      row_of_102(row)
    case 103:
      row_of_103(row)
    case 104:
      row_of_104(row)
    case 105:
      row_of_105(row)
    case 106:
      row_of_106(row)
    case 107:
      row_of_107(row)
    case 108:
      row_of_108(row)
    case 109:
      row_of_109(row)
    case 110:
      row_of_110(row)
    case 111:
      row_of_111(row)
    case 112:
      row_of_112(row)
    case 113:
      row_of_113(row)
    case 114:
      row_of_114(row)
    case 115:
      row_of_115(row)
    case 116:
      row_of_116(row)
    case 117:
      row_of_117(row)
    case 118:
      row_of_118(row)
    case 119:
      row_of_119(row)
    case 120:
      row_of_120(row)
    case 121:
      row_of_121(row)
    case 122:
      row_of_122(row)
    case 123:
      row_of_123(row)
    case 124:
      row_of_124(row)
    case 125:
      row_of_125(row)
    case 126:
      row_of_126(row)
    case _:
      0
