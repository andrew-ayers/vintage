'
' OH, YEAH! THIS PROGRAM DOES REALTIME WIREFRAME 3D MODELING IN THE
' X/Z PLANE (IE., ONLY YAW ROTATION, AND XYZ TRANSLATION ALLOWED)...
' WHY NO ROLL OR PITCH? BECAUSE THIS PROGRAM WAS DESIGNED FOR 3D STYLE
' WALKTHROUGHS, NO NEED FOR ROLL/PITCH... IF YOU WANT THAT, SEE
' PERSP2.BAS...
'
SCREEN 7, , 1, 0
'
DIM PX(10), PY(10), PZ(10), YAW(10), NX(10), NY(10)
'
VD = 250: DCOR = 30
'
PX(1) = -50: PX(2) = -50: PX(3) = 50: PX(4) = 50
PY(1) = 50: PY(2) = 50: PY(3) = 50: PY(4) = 50
PZ(1) = 50: PZ(2) = -50: PZ(3) = -50: PZ(4) = 50
'
FOR T = 1 TO 4
  PX(T + 4) = PX(T)
  PY(T + 4) = PY(T) - 100
  PZ(T + 4) = PZ(T)
NEXT T
'
FOR T = 1 TO 8
  YAW(T) = 0
NEXT T
'
DO
  '
  ' ERASE LAST IMAGE
  '
  LINE (NX(1), NY(1))-(NX(1), NY(1)), 0
  FOR T = 2 TO 4
    LINE -(NX(T), NY(T)), 0
  NEXT T
  LINE -(NX(1), NY(1)), 0
  '
  LINE (NX(5), NY(5))-(NX(5), NY(5)), 0
  FOR T = 6 TO 8
    LINE -(NX(T), NY(T)), 0
  NEXT T
  LINE -(NX(5), NY(5)), 0
  '
  FOR T = 1 TO 4
    LINE (NX(T), NY(T))-(NX(T + 4), NY(T + 4)), 0
  NEXT T
  '
  ' CALCULATE POSITION OF NEW IMAGE
  '
  FOR T = 1 TO 8
    '
    ' TRANSLATE, THEN ROTATE
    '
    TX = PX(T)
    TY = PY(T)
    TZ = PZ(T)
    '
    ' ROTATE
    '
    RX = TX * SIN(YAW(T)) + TZ * COS(YAW(T))
    RY = TY
    RZ = TX * COS(YAW(T)) - TZ * SIN(YAW(T))
    '
    ' TRANSLATE
    '
    'RX = RX + 50
    'RZ = RZ + 50
    '
    NX(T) = 160 + ((VD * RX) / (RZ + VD))
    NY(T) = 100 + ((VD * RY) / (RZ + VD + DCOR))
    '
    YAW(T) = YAW(T) + .05
    IF YAW(T) > 6.2831 THEN YAW(T) = 0
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
  LINE (NX(5), NY(5))-(NX(5), NY(5)), 15
  FOR T = 6 TO 8
    LINE -(NX(T), NY(T)), 15
  NEXT T
  LINE -(NX(5), NY(5)), 15
  '
  FOR T = 1 TO 4
    LINE (NX(T), NY(T))-(NX(T + 4), NY(T + 4)), 15
  NEXT T
  '
  ' SHOW IMAGE ON SCREEN
  '
  PCOPY 1, 0
  '
LOOP



