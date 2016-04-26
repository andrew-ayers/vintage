DECLARE SUB flat.base.tri (x1%, y1%, x2%, y2%, x3%, y3%, colr%)
'
SCREEN 13
'
t = TIMER
cnt% = 0: DO
  x1% = INT(RND * 320): y1% = INT(RND * 200)
  x2% = INT(RND * 320): y2% = INT(RND * 200)
  x3% = INT(RND * 320): y3% = y2%
  colr% = INT(RND * 16)
  CALL flat.base.tri(x1%, y1%, x2%, y2%, x3%, y3%, colr%)
  cnt% = cnt% + 1
LOOP UNTIL cnt% = 50
PRINT TIMER - t

SUB flat.base.tri (x1%, y1%, x2%, y2%, x3%, y3%, colr%)
  '
  IF y2% <> y3% THEN
    SWAP x1%, x2%
    SWAP y1%, y2%
    IF y2% <> y3% THEN
      SWAP x1%, x3%
      SWAP y1%, y3%
    END IF
  END IF
  '
  px% = x1%: py% = y2%
  pxdist% = px% - x3%
  pydist% = py% - y1%
  IF pydist% = 0 THEN
    LINE (x2%, y2%)-(x3%, y3%), colr%
    EXIT SUB
  END IF
  '
  error.term% = 0
  '
  xdiff% = x2% - x1%: ydiff% = y2% - y1%
  xstep% = 1: ystep% = 1
  '
  IF x1% >= x2% THEN xstep% = -1: xdiff% = -xdiff%
  IF y1% >= y2% THEN ystep% = -1: ydiff% = -ydiff%
  '
  xend% = ABS(xdiff%): yend% = ABS(ydiff%)
  '
  IF xdiff% > ydiff% THEN
    '
    y% = y1%: x% = x1%
    '
    FOR xx% = 0 TO xend%
      ratio = (y1% - y%) / pydist%
      xxx% = x1% + INT(pxdist% * ratio)
      LINE (x%, y%)-(xxx%, y%), colr%
      x% = x% + xstep%
      error.term% = error.term% + ydiff%
      IF error.term% >= xdiff% THEN
        y% = y% + ystep%
        error.term% = error.term% - xdiff%
      END IF
    NEXT
    '
  ELSE
    '
    y% = y1%: x% = x1%
    '
    FOR yy% = 0 TO yend%
      ratio = (y1% - y%) / pydist%
      xxx% = x1% + INT(pxdist% * ratio)
      LINE (x%, y%)-(xxx%, y%), colr%
      y% = y% + ystep%
      error.term% = error.term% + xdiff%
      IF error.term% >= ydiff% THEN
        x% = x% + xstep%
        error.term% = error.term% - ydiff%
      END IF
    NEXT
    '
  END IF
  '
END SUB

