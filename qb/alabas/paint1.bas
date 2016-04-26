DECLARE SUB BlastPaint (xp%, yp%, pc%, bc%)
CLEAR , , 16384
'
SCREEN 13
'
CIRCLE (159, 99), 90, 15
LINE (0, 0)-(200, 190), 15
'
CALL BlastPaint(159, 99, 15, 15)

SUB BlastPaint (xp%, yp%, pc%, bc%)
  '
  IF yp% < 0 OR yp% > 199 THEN EXIT SUB
  '
  IF POINT(xp%, yp%) = bc% THEN EXIT SUB
  '
  offset% = 0
  '
  DO
    IF xp% + offset% > 319 THEN EXIT DO
    IF POINT(xp% + offset%, yp%) = bc% THEN EXIT DO
    '
    PSET (xp% + offset%, yp%), pc%
    offset% = offset% + 1
  LOOP
  '
  offset% = -1
  '
  DO
    IF xp% + offset% < 0 THEN EXIT DO
    IF POINT(xp% + offset%, yp%) = bc% THEN EXIT DO
    '
    PSET (xp% + offset%, yp%), pc%
    offset% = offset% - 1
  LOOP
  '
  CALL BlastPaint(xp%, yp% - 1, pc%, bc%)
  CALL BlastPaint(xp%, yp% + 1, pc%, bc%)
  '
END SUB

SUB BlastPaint2 (xp%, yp%, pc%, bc%)
  '
  DO
    IF POINT(xp%, yp%) = bc% OR xp% > 319 THEN EXIT DO
    PSET (xp%, yp%), pc%
    xp% = xp% + 1
  LOOP
  '
  yp% = yp% + 1
  '
  DO
    xp% = xp% - 1
  LOOP UNTIL POINT(xp%, yp%) <> bc%
  '
  CALL BlastPaint2(xp%, yp%, pc%, bc%)
  '
END SUB

