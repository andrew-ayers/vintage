'
' Description : Mode 13 VGA Cloud Plasma!
' Written by  : Andrew L. Ayers
' Date        : 07/24/96
'
' Now here's yet another for the masses! This creates cload plasma, which
' is also known as fractal plasma. This routine is pretty damn fast
' already, but if you can speed it up, go for it! Play with it some.
' As always, if you use the routine in your own program or demo, please
' mention my name. Thanks, and have phun!

DECLARE SUB SetPal (start.slot%, end.slot%)
DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB PLASMA (XE%, YE%, SCALE%)
DECLARE SUB DRAW.PLASMA (XS%, YS%, XE%, YE%, REDRAW%, SCALE%)
'
DIM oldr%(255), oldg%(255), oldb%(255)
'
SCREEN 13
'
' Save old palette, change to black to
' hide build process
'
FOR t% = 0 TO 255
  CALL ReadRGB(oldr%(t%), oldg%(t%), oldb%(t%), t%)
  CALL WriteRGB(0, 0, 0, t%)
NEXT t%

'
RANDOMIZE TIMER
'
CALL PLASMA(512, 256, 4)

LOCATE 2, 11: PRINT "Cloud Plasma Effect!"
LOCATE 23, 12: PRINT "By Andrew L. Ayers"

'
' Create a custom 256 color palette
'
CALL WriteRGB(0, 0, 0, 1)
CALL WriteRGB(63, 63, 0, 31)
CALL WriteRGB(0, 0, 63, 63)
CALL WriteRGB(0, 63, 63, 95)
CALL WriteRGB(63, 0, 0, 127)
CALL WriteRGB(0, 63, 0, 159)
CALL WriteRGB(63, 0, 63, 191)
CALL WriteRGB(63, 63, 63, 223)
CALL WriteRGB(0, 0, 0, 255)
'
CALL SetPal(1, 31)
CALL SetPal(31, 63)
CALL SetPal(63, 95)
CALL SetPal(95, 127)
CALL SetPal(127, 159)
CALL SetPal(159, 191)
CALL SetPal(191, 223)
CALL SetPal(223, 255)

'
' Rotate palette - this isn't how you would do it
' for speed (for more speed, inline the read/write code
' to eliminate subroutine calling overhead), but it
' does show how to do it. Notice the "sparklies" along
' the right hand edge. These are only apparent if your
' computer is fast enough. I believe this has to do with
' the registers being updated faster than the video card
' can keep up with or sothing like that. If anyone knows
' how to fix this, go for it!
'
DO
  '
  CALL ReadRGB(ored%, ogrn%, oblu%, 1)     ' Read in slot 1.
  '
  FOR t% = 1 TO 254
    CALL ReadRGB(red%, grn%, blu%, t% + 1) ' Read slots 2-255, then
    CALL WriteRGB(red%, grn%, blu%, t%)    ' shift to slots 1-254.
  NEXT t%
  '
  CALL WriteRGB(ored%, ogrn%, oblu%, 255)  ' Write what was in slot 1 to
                                           ' slot 255.
  FOR dlay% = 1 TO 15000: NEXT dlay%        ' This may need adjusting
LOOP UNTIL INKEY$ <> ""
'
CLS

'
' Reset original RGB values
'
FOR t% = 0 TO 255
  CALL WriteRGB(oldr%(t%), oldg%(t%), oldb%(t%), t%)
NEXT t%

SUB DRAW.PLASMA (XS%, YS%, XE%, YE%, REDRAW%, SCALE%)
  '
  STATIC ITER%
  '
  IF REDRAW% THEN ITER% = 1: REDRAW% = 0
  IF ITER% = 1 THEN
    ITER% = 0
    LINE (XS%, YS%)-(XS% + SCALE% - 1, YS% + SCALE% - 1), INT(RND * 63) + 1, BF
    LINE (XE%, YS%)-(XE% + SCALE% - 1, YS% + SCALE% - 1), INT(RND * 63) + 1, BF
    LINE (XS%, YE%)-(XS% + SCALE% - 1, YE% + SCALE% - 1), INT(RND * 63) + 1, BF
    LINE (XE%, YE%)-(XE% + SCALE% - 1, YE% + SCALE% - 1), INT(RND * 63) + 1, BF
  END IF
  '
  SIZE% = (XE% - XS%) / 2
  IF SIZE% < SCALE% THEN EXIT SUB
  '
  SIZE% = SIZE% + (INT(RND * 8) - 4)
  '
  X1% = XS% + (XE% - XS%) / 2
  Y1% = YS% + (YE% - YS%) / 2
  '
  C1% = POINT(XS%, YS%)' UL
  C2% = POINT(XE%, YS%)' UR
  C3% = POINT(XS%, YE%)' LL
  C4% = POINT(XE%, YE%)' LR
  '
  C5% = (C1% + C2%) / 2 ' UL+UR
  C6% = (C1% + C3%) / 2 ' UL+LL
  C7% = (C2% + C4%) / 2 ' UR+LR
  C8% = (C3% + C4%) / 2 ' LL+LR
  C9% = (C5% + C6% + C7% + C8%) / 4 ' MID
  '
  C5% = C5% + INT(RND * SIZE%) - (SIZE% / 2)
  C6% = C6% + INT(RND * SIZE%) - (SIZE% / 2)
  C7% = C7% + INT(RND * SIZE%) - (SIZE% / 2)
  C8% = C8% + INT(RND * SIZE%) - (SIZE% / 2)
  C9% = C9% + INT(RND * SIZE%) - (SIZE% / 2)
  '
  IF C5% < 1 THEN C5% = 1
  IF C6% < 1 THEN C6% = 1
  IF C7% < 1 THEN C7% = 1
  IF C8% < 1 THEN C8% = 1
  IF C9% < 1 THEN C9% = 1
  '
  IF C5% > 63 THEN C5% = 63
  IF C6% > 63 THEN C6% = 63
  IF C7% > 63 THEN C7% = 63
  IF C8% > 63 THEN C8% = 63
  IF C9% > 63 THEN C9% = 63
  '
  IF XS% = 0 OR YS% = 0 THEN
    LINE (XS%, YS%)-(XS% + SCALE% - 1, YS% + SCALE% - 1), C5%, BF' TM
  END IF
  IF XS% = 0 OR Y1% = 0 THEN
    LINE (XS%, Y1%)-(XS% + SCALE% - 1, Y1% + SCALE% - 1), C6%, BF' LM
  END IF
  '
  IF XE% < 320 AND Y1% < 200 THEN
    LINE (XE%, Y1%)-(XE% + SCALE% - 1, Y1% + SCALE% - 1), C7%, BF' RM
  END IF
  IF X1% < 320 AND YE% < 200 THEN
    LINE (X1%, YE%)-(X1% + SCALE% - 1, YE% + SCALE% - 1), C8%, BF' BM
  END IF
  IF X1% < 320 AND Y1% < 200 THEN
    LINE (X1%, Y1%)-(X1% + SCALE% - 1, Y1% + SCALE% - 1), C9%, BF' MID
  END IF
  '
  CALL DRAW.PLASMA(XS%, YS%, X1%, Y1%, REDRAW%, SCALE%)
  CALL DRAW.PLASMA(X1%, YS%, XE%, Y1%, REDRAW%, SCALE%)
  CALL DRAW.PLASMA(XS%, Y1%, X1%, YE%, REDRAW%, SCALE%)
  CALL DRAW.PLASMA(X1%, Y1%, XE%, YE%, REDRAW%, SCALE%)
  '
END SUB

SUB PLASMA (XE%, YE%, SCALE%)
  '
  CALL DRAW.PLASMA(0, 0, XE%, YE%, 1, SCALE%)
  LINE (0, 0)-(XE% + SCALE% - 1, YS% + SCALE% - 1), 0, BF
  LINE (0, 0)-(XS% + SCALE% - 1, YE% + SCALE% - 1), 0, BF
  '
END SUB

SUB ReadRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C7, slot% ' Read RGB values from slot
  '
  red% = INP(&H3C9)
  grn% = INP(&H3C9)
  blu% = INP(&H3C9)
  '
END SUB

SUB SetPal (start.slot%, end.slot%)
  '
  num.slots% = end.slot% - start.slot%
  '
  CALL ReadRGB(sr%, sg%, sb%, start.slot%)
  CALL ReadRGB(er%, eg%, eb%, end.slot%)
  '
  rr% = ABS(er% - sr%): rg% = ABS(eg% - sg%): rb% = ABS(eb% - sb%)
  rs% = SGN(er% - sr%): gs% = SGN(eg% - sg%): bs% = SGN(eb% - sb%)
  '
  stepr = (rr% / num.slots%) * rs%
  stepg = (rg% / num.slots%) * gs%
  stepb = (rb% / num.slots%) * bs%
  '
  r = sr%: g = sg%: b = sb%
  wr% = r: wg% = g: wb% = b
  '
  FOR t% = start.slot% TO end.slot%
    '
    CALL WriteRGB(wr%, wg%, wb%, t%)
    '
    r = r + stepr: wr% = r
    g = g + stepg: wg% = g
    b = b + stepb: wb% = b
    '
  NEXT t%
  '
END SUB

SUB WriteRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C8, slot% ' Write RGB values to slot
  '
  OUT &H3C9, red%
  OUT &H3C9, grn%
  OUT &H3C9, blu%
  '
END SUB

