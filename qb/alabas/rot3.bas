DECLARE SUB setpal (begin!, num.slots!, begtrip$, endtrip$, B&())
DIM B&(64), PX%(30), PY%(30), R(30), NX%(30), NY%(30)
'
SCREEN 13
'
CALL setpal(1, 16, "000000", "630063", B&())
CALL setpal(17, 16, "630063", "630000", B&())
CALL setpal(33, 16, "630000", "000063", B&())
CALL setpal(49, 16, "000063", "006300", B&())
'
PX%(1) = -50: PX%(2) = -50: PX%(3) = 50: PX%(4) = 50
PY%(1) = 50: PY%(2) = -50: PY%(3) = -50: PY%(4) = 50
R(1) = 0: R(2) = 0: R(3) = 0: R(4) = 0
'
done% = 0: size = 4: COL% = 1
'
DO
  '
  ' CALCULATE POSITION OF NEW IMAGE
  '
  FOR t% = 1 TO 4
    '
    LX% = PX%(t%) * SIN(R(t%)) + PY%(t%) * COS(R(t%))
    LY% = PX%(t%) * COS(R(t%)) - PY%(t%) * SIN(R(t%))
    '
    NX%(t%) = 160 + LX% * size
    NY%(t%) = 100 + LY% * size
    '
    R(t%) = R(t%) + .1
    IF R(t%) > 6.282 THEN R(t%) = 0
    '
  NEXT t%
  size = size - .05: IF size <= .1 THEN done% = 1
  '
  COL% = COL% + 1: IF COL% > 63 THEN COL% = 1
  '
  ' DRAW NEW IMAGE
  '
  LINE (NX%(1), NY%(1))-(NX%(1), NY%(1)), COL%
  FOR t% = 2 TO 4
    LINE -(NX%(t%), NY%(t%)), COL%
  NEXT t%
  LINE -(NX%(1), NY%(1)), COL%
  PAINT (160, 100), COL%
  '
LOOP UNTIL done%
'
DO
  C = B&(1)
  FOR t = 1 TO 62
    B&(t) = B&(t + 1)
    PALETTE t, B&(t)
  NEXT t
  B&(63) = C
  PALETTE 63, B&(63)
LOOP UNTIL INKEY$ <> ""

SUB setpal (begin, num.slots, begtrip$, endtrip$, B&())
  '
  sr = VAL(LEFT$(begtrip$, 2))
  sg = VAL(MID$(begtrip$, 3, 2))
  sb = VAL(RIGHT$(begtrip$, 2))
  '
  er = VAL(LEFT$(endtrip$, 2))
  eg = VAL(MID$(endtrip$, 3, 2))
  eb = VAL(RIGHT$(endtrip$, 2))
  '
  rr = ABS(er - sr): rg = ABS(eg - sg): rb = ABS(eb - sb)
  rs = SGN(er - sr): gs = SGN(eg - sg): bs = SGN(eb - sb)
  '
  stpr = INT(rr / num.slots) * rs
  stpg = INT(rg / num.slots) * gs
  stpb = INT(rb / num.slots) * bs
  '
  R = sr: g = sg: B = sb
  '
  FOR t = begin TO begin + num.slots - 1
    '
    COL = R + 65536 * B + (256 * g)
    '
    PALETTE t, COL
    '
    B&(t) = COL
    '
    R = R + stpr
    g = g + stpg
    B = B + stpb
    '
  NEXT t
  '
END SUB

