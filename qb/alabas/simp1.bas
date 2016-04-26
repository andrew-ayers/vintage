DIM sx%(130, 3), sy%(130, 3)
DIM vx%(7), vy%(7)
'
FOR t% = 0 TO 3
  FOR tt% = 0 TO 130
    sx%(tt%, t%) = INT(RND * 320)
    sy%(tt%, t%) = INT(RND * 200)
  NEXT tt%
NEXT t%
'
dir% = 0
'
vx%(0) = 0: vy%(0) = 1
vx%(1) = -1: vy%(1) = 1
vx%(2) = -1: vy%(2) = 0
vx%(3) = -1: vy%(3) = -1
vx%(4) = 0: vy%(4) = -1
vx%(5) = 1: vy%(5) = -1
vx%(6) = 1: vy%(6) = 0
vx%(7) = 1: vy%(7) = 1
'
SCREEN 13
'
DO
  FOR t% = 0 TO 3
    FOR tt% = 0 TO 40
      PSET (sx%(tt%, t%), sy%(tt%, t%)), 0
      '
      sx%(tt%, t%) = sx%(tt%, t%) + (vx%(dir%) * (t% + 1) * th%)
      sy%(tt%, t%) = sy%(tt%, t%) + (vy%(dir%) * (t% + 1) * th%)
      '
      IF sx%(tt%, t%) > 319 THEN sx%(tt%, t%) = sx%(tt%, t%) - 320
      IF sx%(tt%, t%) < 0 THEN sx%(tt%, t%) = 320 + sx%(tt%, t%)
      IF sy%(tt%, t%) > 199 THEN sy%(tt%, t%) = sy%(tt%, t%) - 200
      IF sy%(tt%, t%) < 0 THEN sy%(tt%, t%) = 200 + sy%(tt%, t%)
      '
      PSET (sx%(tt%, t%), sy%(tt%, t%)), 15 + ((t% + 1) * 4)
    NEXT tt%
  NEXT t%
  '
  DRAW "c0bm160,100;ta" + STR$(-dir% * 45) + "bm+0,-10;f5u3d3l10u3d3e5"
  '
  a$ = INKEY$
  IF a$ = "." THEN dir% = dir% + 1: IF dir% > 7 THEN dir% = 0
  IF a$ = "," THEN dir% = dir% - 1: IF dir% < 0 THEN dir% = 7
  IF a$ = "a" THEN th% = th% + 1: IF th% > 4 THEN th% = 4
  IF a$ = "z" THEN th% = th% - 1: IF th% < 0 THEN th% = 0
  '
  DRAW "c4bm160,100;ta" + STR$(-dir% * 45) + "bm+0,-10;f5u3d3l10u3d3e5"
LOOP


