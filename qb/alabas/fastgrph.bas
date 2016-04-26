SCREEN 12
'
' Set Random values
'
DIM ar%(319), oar%(319)
'
FOR t% = 0 TO 319
  ar%(t%) = 240
NEXT t%
'
' Time for phun!
'
DO
  LINE (636, 0)-(639, 479), 0, BF
  '
  FOR tt% = 0 TO 318
    t% = tt% * 2
    LINE (t%, oar%(tt%))-(t% + 2, oar%(tt% + 1)), 0
    LINE (t%, ar%(tt%))-(t% + 2, ar%(tt% + 1)), 15
    oar%(tt%) = ar%(tt%)
    ar%(tt%) = ar%(tt% + 1)
  NEXT tt%
  '
  ' Set new value
  '
  ar%(319) = 240 + (SIN(valu) * 50)
  '
  valu = valu + (RND - .5)
  IF valu < -.5 THEN valu = -.5
  IF valu > .5 THEN valu = .5
  '
  LINE (0, 240)-(639, 240), 4
  '
LOOP

