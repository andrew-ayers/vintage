DECLARE SUB SetBox (x1%, y1%, x2%, y2%, chr%, col%)
DECLARE SUB clr (col%)
DECLARE SUB SetBlock (x1%, y1%, x2%, y2%, chr%, col%)
SCREEN 0: WIDTH 80
'
DEF SEG = &HB800
'
CALL clr(0)
'
xp% = 0: yp% = 0: col% = 0: chr% = 219
x1% = -1: y1% = -1
'
BLOAD "test.scr", 0
'
DO
  pcol% = PEEK((xp% * 2) + (yp% * 160) + 1)
  pchr% = PEEK((xp% * 2) + (yp% * 160))
  '
  POKE (xp% * 2) + (yp% * 160) + 1, col%
  POKE (xp% * 2) + (yp% * 160), chr%
  '
  DO: a$ = INKEY$: LOOP UNTIL a$ <> ""
  '
  POKE (xp% * 2) + (yp% * 160) + 1, pcol%
  POKE (xp% * 2) + (yp% * 160), pchr%
  '
  ' Function Keys
  '
  '   1 = Clear the screen to the current color
  '   2 = Clear a block of the screen to the current character/color
  '   3 = Set a box on the screen to the current character/color
  '
  IF a$ = CHR$(0) + CHR$(&H3B) THEN CALL clr(col%)
  IF a$ = CHR$(0) + CHR$(&H3C) THEN
    IF x1% = -1 AND y1% = -1 THEN
      x1% = xp%: y1% = yp%
      POKE (xp% * 2) + (yp% * 160) + 1, col%
      POKE (xp% * 2) + (yp% * 160), chr%
    ELSE
      IF x1% <> -1 AND y1% <> -1 THEN
        x2% = xp%: y2% = yp%
        CALL SetBlock(x1%, y1%, x2%, y2%, chr%, col%)
        x1% = -1: y1% = -1
      END IF
    END IF
  END IF
  '
  IF a$ = CHR$(0) + CHR$(&H3D) THEN
    IF x1% = -1 AND y1% = -1 THEN
      x1% = xp%: y1% = yp%
      POKE (xp% * 2) + (yp% * 160) + 1, col%
      POKE (xp% * 2) + (yp% * 160), chr%
    ELSE
      IF x1% <> -1 AND y1% <> -1 THEN
        x2% = xp%: y2% = yp%
        CALL SetBox(x1%, y1%, x2%, y2%, chr%, col%)
        x1% = -1: y1% = -1
      END IF
    END IF
  END IF
  '
  IF a$ = CHR$(0) + CHR$(&H3E) THEN
    ps% = xp%
    DO
      DO: k$ = INKEY$: LOOP UNTIL k$ <> ""
      SELECT CASE ASC(k$)
        CASE 13
          EXIT DO
        CASE 8
          ps% = ps% - 1: IF ps% < 0 THEN ps% = 0
        CASE ELSE
          POKE (ps% * 2) + (yp% * 160) + 1, col%
          POKE (ps% * 2) + (yp% * 160), ASC(k$)
          ps% = ps% + 1: IF ps% > 79 THEN ps% = 79
      END SELECT
    LOOP
  END IF
  '
  ' Directional Keys
  '
  IF a$ = CHR$(0) + CHR$(&H4D) THEN xp% = xp% + 1: IF xp% > 79 THEN xp% = 79
  IF a$ = CHR$(0) + CHR$(&H4B) THEN xp% = xp% - 1: IF xp% < 0 THEN xp% = 0
  IF a$ = CHR$(0) + CHR$(&H48) THEN yp% = yp% - 1: IF yp% < 0 THEN yp% = 0
  IF a$ = CHR$(0) + CHR$(&H50) THEN yp% = yp% + 1: IF yp% > 24 THEN yp% = 24
  '
  ' Home/End Keys
  '
  IF a$ = CHR$(0) + CHR$(&H47) THEN xp% = 0: yp% = 0
  IF a$ = CHR$(0) + CHR$(&H4F) THEN xp% = 79: yp% = 24
  '
  ' Color Control (Ins/Del)
  '
  IF a$ = CHR$(0) + CHR$(&H52) THEN col% = col% + 1: IF col% > 255 THEN col% = 0
  IF a$ = CHR$(0) + CHR$(&H53) THEN col% = col% - 1: IF col% < 0 THEN col% = 255
  '
  ' Character Control (PgUp/PgDn)
  '
  IF a$ = CHR$(0) + CHR$(&H49) THEN chr% = chr% + 1: IF chr% > 255 THEN chr% = 0
  IF a$ = CHR$(0) + CHR$(&H51) THEN chr% = chr% - 1: IF chr% < 0 THEN chr% = 255
  '
  IF a$ = CHR$(32) THEN
    '
    POKE (xp% * 2) + (yp% * 160) + 1, col%
    POKE (xp% * 2) + (yp% * 160), chr%
    '
  END IF
  '
  IF a$ = CHR$(27) THEN EXIT DO
LOOP
'
BSAVE "test.scr", 0, 4000
'
DEF SEG

SUB clr (col%)
  '
  FOR t% = 0 TO 3999 STEP 2: POKE t%, 32: POKE t% + 1, col%: NEXT
  '
END SUB

SUB SetBlock (x1%, y1%, x2%, y2%, chr%, col%)
  '
  FOR y% = y1% TO y2%
    FOR x% = x1% TO x2%
      POKE (x% * 2) + (y% * 160) + 1, col%
      POKE (x% * 2) + (y% * 160), chr%
    NEXT
  NEXT
  '
END SUB

SUB SetBox (x1%, y1%, x2%, y2%, chr%, col%)
  '
  FOR y% = y1% TO y2%
    FOR x% = x1% TO x2%
      IF (y% = y1% OR y% = y2%) OR (x% = x1% OR x% = x2%) THEN
        POKE (x% * 2) + (y% * 160) + 1, col%
        POKE (x% * 2) + (y% * 160), chr%
      END IF
    NEXT
  NEXT
  '
END SUB

