SCREEN 13
'
DIM px%(299), py%(299), pz%(299), nx%(299), ny%(299)
'
vd% = 100
'
FOR t% = 0 TO 299
  px%(t%) = (INT(RND * 300) - 150) * vd%: py%(t%) = (INT(RND * 300) - 150) * vd%: pz%(t%) = INT(RND * 300) + 1
NEXT
'
DO
  FOR t% = 0 TO 80
    PSET (nx%(t%), ny%(t%)), 0
    nx%(t%) = 160 + px%(t%) / pz%(t%)
    ny%(t%) = 100 + py%(t%) / pz%(t%)
    PSET (nx%(t%), ny%(t%)), 15
    pz%(t%) = pz%(t%) - 15
    px%(t%) = px%(t%) - 0
    py%(t%) = py%(t%) - 0
    IF pz%(t%) <= 0 THEN pz%(t%) = 300
    IF px%(t%) <= -15000 THEN px%(t%) = 15000
    IF py%(t%) <= -15000 THEN py%(t%) = 15000
  NEXT
LOOP UNTIL INKEY$ <> ""



