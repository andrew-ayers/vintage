DIM code1%(76), sbuffer%(1000)
'
code1$ = ""
code1$ = code1$ + "1E5589E58B460C508B460A508B46108ED88B760E8B04B103D3E8508B5EFE"
code1$ = code1$ + "01C3895EFE8B4402508B5EFC01C3895EFC83C60489760E89E58B46188ED8"
code1$ = code1$ + "8B76168A04468976163A461074208B5E1C8EDB8B7612B106D3E689F3B102"
code1$ = code1$ + "D3E601DE8B5E1401DE8B5E1A01DE88048B4614408946148B460639461475"
code1$ = code1$ + "BE8B46142B46028946148B4612408946128B460439461275A6585858585D"
code1$ = code1$ + "1FCA0E00"
'
DEF SEG = VARSEG(code1%(0))
'
FOR i% = 0 TO 153
  d% = VAL("&h" + MID$(code1$, i% * 2 + 1, 2))
  POKE VARPTR(code1%(0)) + i%, d%
NEXT i%
'
DEF SEG
'
SCREEN 13
'
LINE (5, 5)-(10, 10), 15, BF
GET (0, 0)-(15, 15), sbuffer%(0)
'
CLS
'
dbufferseg% = &HA000: dbufferptr% = 0: icol% = 0: cnt% = 0
'
DEF SEG = VARSEG(code1%(0))
'
t = TIMER
DO
  xpos% = INT(RND * 300)
  ypos% = INT(RND * 170)
  '
  CALL ABSOLUTE(BYVAL dbufferseg%, BYVAL dbufferptr%, BYVAL VARSEG(sbuffer%(0)), BYVAL VARPTR(sbuffer%(0)), BYVAL xpos%, BYVAL ypos%, BYVAL icol%, VARPTR(code1%(0)))
  cnt% = cnt% + 1
LOOP UNTIL cnt% = 1000
PRINT TIMER - t
DEF SEG

