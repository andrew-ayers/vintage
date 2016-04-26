'
' Description : BigScroll! - Another VGA Mode 13 Scrolling Routine
' Written by  : Andrew L. Ayers
' Date        : 08/15/96
'
' This uses my FastScroll! routine, as well as another routine
' to do a LARGE text scroller. Check it out!
'
' You may use this routine in any manner you like, as long
' as you give credit in an appropriate manner. Have phun!
'
DECLARE SUB FastScroll (XSpeed%, YSpeed%)
'
DEFINT A-Z
'
DIM a1%(32 * 64 * 11), a2%(32 * 64 * 11), a3%(32 * 64 * 11)
'
SCREEN 13
'
COLOR 7: LOCATE 10, 6: PRINT "Please wait...Building font": COLOR 0
'
FOR t% = 15 TO 255: PALETTE t%, 0: NEXT
'
A$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ!?.-"
'
FOR t% = 0 TO 30
  GOSUB DrawLetter
  GOSUB GetLetter
NEXT
'
SCREEN 0: CLS 0: SCREEN 13
'
B$ = "BIG SCROLL! BY ANDREW AYERS - HOW DO YOU LIKE IT?...      "
'
done% = 0: DO
  FOR tt% = 1 TO LEN(B$)
    t% = INSTR(A$, MID$(B$, tt%, 1)) - 1
    GOSUB PutLetter
    FOR x% = 0 TO 7
      CALL FastScroll(-8, 0)
      LINE (311, 0)-(319, 199), 0, BF
      IF INKEY$ <> "" THEN done% = 1: EXIT FOR
    NEXT
    IF done% THEN EXIT FOR
  NEXT
LOOP UNTIL done%
'
STOP
'
DrawLetter:
  '
  LINE (0, 0)-(8, 8), 0, BF
  LINE (0, 100)-(319, 199), 0, BF
  '
  COLOR 255: LOCATE 1, 1: PRINT MID$(A$, t% + 1, 1)
  '
  SCALE% = 8
  '
  FOR y% = 0 TO (SCALE - 1)
    FOR x% = 1 TO 1 * SCALE
      IF POINT(x% - 1, y%) = 255 THEN
        LINE (x% * SCALE%, 100 + y% * SCALE%)-(x% * SCALE% + SCALE%, 100 + y% * SCALE% + SCALE%), 15, BF
      END IF
    NEXT
  NEXT
  '
  FOR y% = 0 TO SCALE% * SCALE%
    C% = (16 * ABS(y% < 31)) + INT(y% / 2)
    FOR x% = 0 TO SCALE% * SCALE%
      IF POINT(x%, 100 + y%) THEN
        PSET (x%, 100 + y%), C%
      END IF
    NEXT
  NEXT
  '
  RETURN

GetLetter:
  '
  IF t% >= 0 AND t% < 10 THEN GET (0, 100)-(64, 160), a1%(t% * 32 * 64)
  IF t% >= 10 AND t% < 20 THEN GET (0, 100)-(64, 160), a2%((t% - 10) * 32 * 64)
  IF t% >= 20 AND t% < 30 THEN GET (0, 100)-(64, 160), a3%((t% - 20) * 32 * 64)
  '
  RETURN

PutLetter:
  '
  IF t% >= 0 AND t% < 10 THEN PUT (255, 68), a1%(t% * 32 * 64), PSET
  IF t% >= 10 AND t% < 20 THEN PUT (255, 68), a2%((t% - 10) * 32 * 64), PSET
  IF t% >= 20 AND t% < 30 THEN PUT (255, 68), a3%((t% - 20) * 32 * 64), PSET
  '
  RETURN

SUB FastScroll (XSpeed%, YSpeed%)
  '
  DIM Temp%(502)
  '
  XStep% = 40: YStep% = 25
  '
  IF XSpeed% < 0 OR YSpeed% < 0 THEN
    FOR y% = 0 TO 199 STEP YStep%
      FOR x% = 0 TO 319 STEP XStep%
        IF (XSpeed% <> 0 AND x% = 0) OR (YSpeed% <> 0 AND y% = 0) THEN
          GET (x% - XSpeed%, y% - YSpeed%)-(x% + XStep% - 1, y% + YStep% - 1), Temp%
          PUT (x%, y%), Temp%, PSET
        ELSE
          GET (x%, y%)-(x% + XStep% - 1, y% + YStep% - 1), Temp%
          PUT (x% + XSpeed%, y% + YSpeed%), Temp%, PSET
        END IF
      NEXT x%
    NEXT y%
  ELSE
    FOR y% = 199 TO 0 STEP -YStep%
      FOR x% = 319 TO 0 STEP -XStep%
        IF (XSpeed% <> 0 AND x% = 319) OR (YSpeed% <> 0 AND y% = 199) THEN
          GET (x% - (XStep% - 1), y% - (YStep% - 1))-(x% - XSpeed%, y% - YSpeed%), Temp%
          PUT (x% - (XStep% - 1) + XSpeed%, y% - (YStep% - 1) + YSpeed%), Temp%, PSET
        ELSE
          GET (x% - (XStep% - 1), y% - (YStep% - 1))-(x%, y%), Temp%
          PUT (x% - (XStep% - 1) + XSpeed%, y% - (YStep% - 1) + YSpeed%), Temp%, PSET
        END IF
      NEXT x%
    NEXT y%
  END IF
  '
END SUB

