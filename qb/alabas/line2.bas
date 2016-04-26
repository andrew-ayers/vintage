DECLARE SUB line.draw (dsegment%, doffset%, x1%, y1%, x2%, y2%, colr%)
'
SCREEN 13
'
x1% = 160: y1% = 100: colr% = 0
'
DO
  FOR t% = 0 TO 319
    CALL line.draw(&HA000, 0, x1%, y1%, t%, 0, colr%)
    colr% = colr% + 1: IF colr% > 255 THEN colr% = 0
  NEXT t%
  '
  FOR t% = 0 TO 199
    CALL line.draw(&HA000, 0, x1%, y1%, 319, t%, colr%)
    colr% = colr% + 1: IF colr% > 255 THEN colr% = 0
  NEXT t%
  '
  FOR t% = 319 TO 0 STEP -1
    CALL line.draw(&HA000, 0, x1%, y1%, t%, 199, colr%)
    colr% = colr% + 1: IF colr% > 255 THEN colr% = 0
  NEXT t%
  '
  FOR t% = 199 TO 0 STEP -1
    CALL line.draw(&HA000, 0, x1%, y1%, 0, t%, colr%)
    colr% = colr% + 1: IF colr% > 255 THEN colr% = 0
  NEXT t%
LOOP

SUB line.draw (dsegment%, doffset%, x1%, y1%, x2%, y2%, colr%)
  '
  DEF SEG = dsegment%
  '
  error.term% = 0
  '
  xdiff% = x2% - x1%: ydiff% = y2% - y1%
  xstep% = 1: ystep% = 320
  '
  IF x1% >= x2% THEN xstep% = -1: xdiff% = -xdiff%
  IF y1% >= y2% THEN ystep% = -320: ydiff% = -ydiff%
  '
  xend% = ABS(xdiff%) - 1: yend% = ABS(ydiff%) - 1
  '
  tt& = doffset% + (y1% * 320) + x1%
  '
  IF xdiff% > ydiff% THEN
    '
    FOR xx% = 0 TO xend%
      POKE tt&, colr%
      tt& = tt& + xstep%
      error.term% = error.term% + ydiff%
      IF error.term% >= xdiff% THEN
        tt& = tt& + ystep%
        error.term% = error.term% - xdiff%
      END IF
    NEXT
    '
  ELSE
    '
    FOR yy% = 0 TO yend%
      POKE tt&, colr%
      tt& = tt& + ystep%
      error.term% = error.term% + xdiff%
      IF error.term% >= ydiff% THEN
        tt& = tt& + xstep%
        error.term% = error.term% - ydiff%
      END IF
    NEXT
    '
  END IF
  '
  DEF SEG
  '
END SUB

