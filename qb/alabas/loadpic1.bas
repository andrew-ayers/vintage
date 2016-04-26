SCREEN 13
'
DEF SEG = &HA000
BLOAD "a:\pic1.pic"
OUT &H3C8, 16
FOR a = 0 TO 47
     OUT &H3C9, PEEK(a + 64000)
NEXT
'
DEF SEG
'
FOR yy% = 0 TO 199
  FOR xx% = 0 TO 319
    col% = POINT(xx%, yy%) + 16
    PSET (xx%, yy%), col%
  NEXT xx%
NEXT yy%
'
DIM a$(64)
'
FOR yy% = 38 TO 101
  FOR xx% = 140 TO 203
    a$(yy% - 38) = a$(yy% - 38) + CHR$(65 + POINT(xx%, yy%))
  NEXT xx%
NEXT yy%
'
OUT &H3C7, 16
'
FOR a = 0 TO 15
  r% = INP(&H3C9)
  g% = INP(&H3C9)
  b% = INP(&H3C9)
  a$(64) = a$(64) + CHR$(65 + r%) + CHR$(65 + g%) + CHR$(65 + b%)
NEXT a
'
OPEN "a:\pic1.bas" FOR OUTPUT AS #1
'
FOR tt% = 0 TO 64
  IF tt% = 0 THEN
    PRINT #1, "SCREEN 13"
    PRINT #1, "'"
    PRINT #1, "DIM a$(64)"
    PRINT #1, "'"
    PRINT #1, "' Picture DATA first"
    PRINT #1, "'"
  END IF
  IF tt% = 64 THEN
    PRINT #1, "'"
    PRINT #1, "' Palette DATA Next"
    PRINT #1, "'"
  END IF
  PRINT #1, "a$(" + STR$(tt%) + ")=" + CHR$(34) + a$(tt%) + CHR$(34)
NEXT tt%
'
PRINT #1, "'"
PRINT #1, "' Tricky code to read picture data"
PRINT #1, "'"
PRINT #1, "FOR yy% = 0 to 63"
PRINT #1, "  FOR xx% = 0 to 63"
PRINT #1, "    col% = ASC(MID$(a$(yy%),xx% + 1,1)) - 64"
PRINT #1, "    PSET(xx%,yy%), col%"
PRINT #1, "  NEXT xx%"
PRINT #1, "NEXT yy%"
PRINT #1, "'"
PRINT #1, "' Tricky code to read palette data"
PRINT #1, "'"
PRINT #1, "OUT &H3C8, 16 ' Set to start at slot 16"
PRINT #1, "'"
PRINT #1, "FOR a% = 0 to 15"
PRINT #1, "  r% = ASC(MID$(a$(64),a%*3+1,1)) - 65"
PRINT #1, "  g% = ASC(MID$(a$(64),a%*3+2,1)) - 65"
PRINT #1, "  b% = ASC(MID$(a$(64),a%*3+3,1)) - 65"
PRINT #1, "  OUT &H3C9, r%"
PRINT #1, "  OUT &H3C9, g%"
PRINT #1, "  OUT &H3C9, b%"
PRINT #1, "NEXT a%"
'
CLOSE #1

