DECLARE SUB dlin (x1%, y1%, x2%, y2%, colr%)
'
SCREEN 13
'
DO
  x1% = INT(RND * 320)
  x2% = INT(RND * 320)
  y1% = INT(RND * 200)
  y2% = INT(RND * 200)
  colr% = INT(RND * 16)
  '
  CALL dlin(x1%, y1%, x2%, y2%, colr%)
LOOP UNTIL INKEY$ <> ""

SUB dlin (x1%, y1%, x2%, y2%, colr%)
  '
  h% = x1%: v% = y1%
  '
  xdiff% = x2% - x1%
  ydiff% = y2% - y1%
  '
  IF xdiff% < 0 THEN
    xdiff% = -xdiff%
    xunit% = -1
  ELSE
    xunit% = 1
  END IF
  '
  IF ydiff% < 0 THEN
    ydiff% = -ydiff%
    yunit% = -1
  ELSE
    yunit% = 1
  END IF
  '
  errorterm% = 0
  '
  IF xdiff% > ydiff% THEN
    length% = xdiff% + 1
    FOR i% = 1 TO length%
      PSET (h%, v%), colr%
      h% = h% + xunit%
      errorterm% = errorterm% + ydiff%
      IF errorterm% > xdiff% THEN
        v% = v% + yunit%
        errorterm% = errorterm% - xdiff%
      END IF
    NEXT i%
  ELSE
    length% = ydiff% + 1
    FOR i% = 1 TO length%
      PSET (h%, v%), colr%
      v% = v% + yunit%
      errorterm% = errorterm% + xdiff%
      IF errorterm% > 0 THEN
        h% = h% + xunit%
        errorterm% = errorterm% - ydiff%
      END IF
    NEXT i%
  END IF
  '
END SUB

