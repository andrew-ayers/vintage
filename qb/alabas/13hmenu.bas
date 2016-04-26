DIM temp%(7000)
'
SCREEN 13
'
FOR t% = 1 TO 100
  LINE (INT(RND * 320), INT(RND * 200))-(INT(RND * 320), INT(RND * 200)), INT(RND * 16), BF
NEXT
'
menu$ = "  File    Edit    View    Run           "
'
LOCATE 1, 1: PRINT menu$
'
FOR y% = 0 TO 7
  FOR x% = 0 TO (LEN(menu$) * 8) - 1
    IF POINT(x%, y%) THEN
      PSET (x%, y%), 0
    ELSE
      PSET (x%, y%), 15
    END IF
  NEXT
NEXT
'
xpos% = 0
'
DO
  '
  FOR y% = 0 TO 7
    FOR x% = xpos% * 8 TO ((xpos% + 8) * 8) - 1
      IF POINT(x%, y%) THEN
        PSET (x%, y%), 2
      ELSE
        PSET (x%, y%), 15
      END IF
    NEXT
  NEXT
  '
  IF toggle THEN GOSUB drawmenu
  '
  oxpos% = xpos%
  '
  DO: a$ = INKEY$: LOOP UNTIL a$ <> ""
  '
  SELECT CASE a$
    CASE "."
      xpos% = xpos% + 8: IF xpos% > 24 THEN xpos% = 0
    CASE ","
      xpos% = xpos% - 8: IF xpos% < 0 THEN xpos% = 24
    CASE CHR$(13)
      toggle = NOT (toggle)
      IF toggle THEN GOSUB drawmenu
      IF NOT (toggle) THEN GOSUB erasemenu
  END SELECT
  '
  FOR y% = 0 TO 7
    FOR x% = oxpos% * 8 TO ((oxpos% + 8) * 8) - 1
      IF POINT(x%, y%) = 2 THEN
        PSET (x%, y%), 15
      ELSE
        PSET (x%, y%), 0
      END IF
    NEXT
  NEXT
  '
  IF toggle THEN GOSUB erasemenu
  '
LOOP

drawmenu:
  GET (xpos% * 8, 8)-((xpos% + 16) * 8 - 1, 8 + (10 * 8)), temp%
  LINE (xpos% * 8, 8)-((xpos% + 16) * 8 - 1, 8 + (10 * 8)), 15, BF
  LINE (xpos% * 8 + 3, 8 + 3)-((xpos% + 16) * 8 - 4, 8 + (10 * 8) - 4), 0, B
  LINE (xpos% * 8 + 1, 8 + 1)-((xpos% + 16) * 8 - 2, 8 + (10 * 8) - 2), 0, B
RETURN

erasemenu:
  PUT (oxpos% * 8, 8), temp%, PSET
RETURN

