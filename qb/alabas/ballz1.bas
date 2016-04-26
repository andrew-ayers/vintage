DIM a%(500), b%(500)
DIM bx%(40), by%(40), dx%(40), dy%(40)
'
RANDOMIZE TIMER
'
SCREEN 13
'
' Create "Sphere"
'
o = 0
FOR t% = 14 TO 0 STEP -1
  c% = 31 - t%
  CIRCLE (15 + o, 15 + o), t%, c%
  PAINT (15 + o, 15 + o), c%
  o = o - .4
NEXT t%
'
GET (0, 0)-(30, 30), a%
'
' Create Mask
'
LINE (0, 0)-(30, 30), 255, BF
CIRCLE (15, 15), 14, 0: PAINT (15, 15), 0
'
GET (0, 0)-(30, 30), b%
'
' Initialize Ballz
'
FOR t = 0 TO 39
  bx%(t) = INT(RND * 200) + 50: by%(t) = INT(RND * 100) + 50
  DO
    dx%(t) = INT(RND * 11) - 5: dy%(t) = INT(RND * 11) - 5
  LOOP UNTIL dx%(t) <> 0 AND dy%(t) <> 0
NEXT t
'
' Move Ballz
'
numbalz% = 10: tick% = 0: show% = 1
'
LOCATE 12, 12: PRINT "I have ballz, man!"
t = TIMER
DO: LOOP UNTIL TIMER - t >= 5
'
DO
  WAIT &H3DA, 8              ' Wait for vertical retrace
  IF show% THEN LOCATE 12, 12: PRINT "This is" + STR$(numbalz%) + " ballz!"
  FOR t% = 0 TO numbalz% - 1
    PUT (bx%(t%), by%(t%)), b%, AND
    bx%(t%) = bx%(t%) + dx%(t%): by%(t%) = by%(t%) + dy%(t%)
    IF bx%(t%) > 280 OR bx%(t%) < 10 THEN dx%(t%) = -dx%(t%)
    IF by%(t%) > 160 OR by%(t%) < 10 THEN dy%(t%) = -dy%(t%)
    PUT (bx%(t%), by%(t%)), b%, AND: PUT (bx%(t%), by%(t%)), a%, OR
  NEXT t%
  tick% = tick% + 1: IF tick% = 100 THEN tick% = 0: numbalz% = numbalz% + 10
  IF numbalz% > 40 THEN numbalz% = 40: show% = 0
LOOP UNTIL INKEY$ <> ""

