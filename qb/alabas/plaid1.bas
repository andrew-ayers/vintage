DIM x%(500), y%(500), dx%(500), dy%(500), c%(500)
'
SCREEN 13
'
MAXX% = 319
MAXY% = 199
'
FOR t% = 0 TO 500
  '
  x%(t%) = INT(RND * MAXX%)
  y%(t%) = INT(RND * MAXY%)
  '
  DO: dx%(t%) = INT(RND * 3) - 1: LOOP UNTIL dx%(t%) <> 0
  DO: dy%(t%) = INT(RND * 3) - 1: LOOP UNTIL dy%(t%) <> 0
  '
  c%(t%) = INT(RND * 256)
  '
NEXT
'
DO
  FOR t% = 0 TO 500
    '
    'PSET (x%(t%), y%(t%)), 0
    '
    x%(t%) = x%(t%) + dx%(t%)
    y%(t%) = y%(t%) + dy%(t%)
    '
    IF x%(t%) > MAXX% OR x%(t%) < MINX% THEN dx%(t%) = -dx%(t%)
    IF y%(t%) > MAXY% OR y%(t%) < MINY% THEN dy%(t%) = -dy%(t%)
    '
    tick% = tick% + 1
    '
    IF tick% > 5 THEN
      c%(t%) = c%(t%) + 1: IF c%(t%) > 255 THEN c%(t%) = 0
      tick% = 0
    END IF
    '
    PSET (x%(t%), y%(t%)), c%(t%)
    '
  NEXT
LOOP UNTIL INKEY$ <> ""
'
SCREEN 0: WIDTH 80: CLS


