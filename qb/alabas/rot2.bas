SCREEN 7, , 1, 0
'
DIM PX(30), PY(30), R(30), NX(30), NY(30)
'
PX(1) = -50: PX(2) = -50: PX(3) = 50: PX(4) = 50
PY(1) = 50: PY(2) = -50: PY(3) = -50: PY(4) = 50
R(1) = 0: R(2) = 0: R(3) = 0: R(4) = 0
'
DO
  ' ERASE LAST IMAGE
  '
  LINE (NX(1), NY(1))-(NX(1), NY(1)), 0
  FOR T = 2 TO 4
    LINE -(NX(T), NY(T)), 0
  NEXT T
  LINE -(NX(1), NY(1)), 0
  '
  ' CALCULATE POSITION OF NEW IMAGE
  '
  FOR T = 1 TO 4
    '
    LX = PX(T) * SIN(R(T)) + PY(T) * COS(R(T))
    LY = PX(T) * COS(R(T)) - PY(T) * SIN(R(T))
    '
    NX(T) = 160 + LX
    NY(T) = 100 + LY
    '
    R(T) = R(T) + .1
    IF R(T) > 6.282 THEN R(T) = 0
    '
  NEXT T
  '
  ' DRAW NEW IMAGE
  '
  LINE (NX(1), NY(1))-(NX(1), NY(1)), 15
  FOR T = 2 TO 4
    LINE -(NX(T), NY(T)), 15
  NEXT T
  LINE -(NX(1), NY(1)), 15
  '
  ' SHOW IMAGE ON SCREEN
  '
  PCOPY 1, 0
  '
LOOP



