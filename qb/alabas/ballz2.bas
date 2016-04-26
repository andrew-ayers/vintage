DIM a%(110), b%(110)
DIM bx%(20), by%(20), dx%(20), dy%(20)
'
RANDOMIZE TIMER
'
SCREEN 9, , 1, 0
'
' Create "Sphere"
'
o = 0
FOR t% = 8 TO 0 STEP -1
  IF t% > 6 THEN c% = 8
  IF t% > 1 AND t% <= 4 THEN c% = 7
  IF t% <= 1 THEN c% = 15
  CIRCLE (8 + o, 8 + o), t%, c%
  PAINT (8 + o, 8 + o), c%
  o = o - .38
NEXT t%
'
GET (0, 0)-(16, 16), a%
'
' Create Mask
'
LINE (0, 0)-(16, 16), 255, BF
CIRCLE (8, 8), 8, 0: PAINT (8, 8), 0
'
GET (0, 0)-(16, 16), b%
'
' Initialize Ballz
'
FOR t = 0 TO 9
  bx%(t) = INT(RND * 200) + 50: by%(t) = INT(RND * 100) + 50
  ox%(t) = bx%(t): oy%(t) = by%(t)
  DO
    dx%(t) = INT(RND * 11) - 5: dy%(t) = INT(RND * 11) - 5
  LOOP UNTIL dx%(t) <> 0 AND dy%(t) <> 0
NEXT t
'
' Move Ballz
'
lx% = 10: ly% = 10
ux% = 280: uy% = 160
'
DO
  CLS
  t% = 0: DO
    bx%(t%) = bx%(t%) + dx%(t%): by%(t%) = by%(t%) + dy%(t%)
    IF bx%(t%) > ux% OR bx%(t%) < lx% THEN dx%(t%) = -dx%(t%)
    IF by%(t%) > uy% OR by%(t%) < ly% THEN dy%(t%) = -dy%(t%)
    PUT (bx%(t%), by%(t%)), b%, AND: PUT (bx%(t%), by%(t%)), a%, OR
    t% = t% + 1
  LOOP UNTIL t% = 9
  PCOPY 1, 0
LOOP

