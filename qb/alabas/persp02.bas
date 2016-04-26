'
' OH, YEAH! THIS PROGRAM DOES REALTIME WIREFRAME 3D MODELING!
' X,Y, AND Z PLANE TRANSLATION AND ROTATION ARE FULLY SUPPORTED!
' USES TABLES TO SPEED UP SIN/COS CALCS. NEEDS TO BE INTEGERIZED
' FOR FASTEST CALCULATIONS YET...
' NEXT STOP, A WIRE FRAME WALKTHROUGH!
'
SCREEN 7, , 1, 0
'
DIM PX(10), PY(10), PZ(10), YAW(10), PIT(10), ROL(10), NX(10), NY(10)
DIM STAB(359), CTAB(359)
'
VD = 250: DCOR = 30
'
PX(1) = 50: PX(2) = 50: PX(3) = -50: PX(4) = -50
PY(1) = 50: PY(2) = 50: PY(3) = 50: PY(4) = 50
PZ(1) = 50: PZ(2) = -50: PZ(3) = -50: PZ(4) = 50
'
FOR T = 4 TO 1 STEP -1
  PX(T + 4) = PX(5 - T)
  PY(T + 4) = PY(5 - T) - 100
  PZ(T + 4) = PZ(5 - T)
NEXT T
'
FOR T = 1 TO 8
  YAW(T) = 0
  PIT(T) = 0
  ROL(T) = 0
NEXT T
'
FOR T = 0 TO 359
  STAB(T) = SIN((6.282 / 360) * T)
  CTAB(T) = COS((6.282 / 360) * T)
NEXT T
'
DO
  '
  ' ERASE LAST IMAGE
  '
  LINE (NX(1), NY(1))-(NX(2), NY(2)), 0
  LINE -(NX(3), NY(3)), 0
  LINE -(NX(4), NY(4)), 0
  LINE -(NX(1), NY(1)), 0
  '
  LINE (NX(5), NY(5))-(NX(6), NY(6)), 0
  LINE -(NX(7), NY(7)), 0
  LINE -(NX(8), NY(8)), 0
  LINE -(NX(5), NY(5)), 0
  '
  LINE (NX(1), NY(1))-(NX(8), NY(8)), 0
  LINE (NX(2), NY(2))-(NX(7), NY(7)), 0
  LINE (NX(3), NY(3))-(NX(6), NY(6)), 0
  LINE (NX(4), NY(4))-(NX(5), NY(5)), 0
  '
  ' CALCULATE POSITION OF NEW IMAGE
  '
  FOR T = 1 TO 8
    '
    ' TRANSLATE, THEN ROTATE
    '
    TX = PX(T)
    TY = -PY(T)
    TZ = PZ(T)
    '
    ' ROTATE (PIT)
    '
    SX = TY * STAB(PIT(T)) + TX * CTAB(PIT(T))
    SY = TY * CTAB(PIT(T)) - TX * STAB(PIT(T))
    SZ = TZ
    '
    ' ROTATE (ROL)
    '
    QX = SX
    QY = SZ * STAB(ROL(T)) + SY * CTAB(ROL(T))
    QZ = SZ * CTAB(ROL(T)) - SY * STAB(ROL(T))
    '
    ' ROTATE (YAW)
    '
    RX = QZ * STAB(YAW(T)) + QX * CTAB(YAW(T))
    RY = QY
    RZ = QZ * CTAB(YAW(T)) - QX * STAB(YAW(T))
    '
    ' ROTATE, THEN TRANSLATE
    '
    'RX = RX + 50
    'RZ = RZ + 50
    '
    NX(T) = 160 + ((VD * RX) / (RZ + VD))
    NY(T) = 100 + ((VD * RY) / (RZ + VD + DCOR))
    '
    PIT(T) = PIT(T) + 3
    IF PIT(T) > 359 THEN PIT(T) = 0
    '
    YAW(T) = YAW(T) + 2
    IF YAW(T) > 359 THEN YAW(T) = 0
    '
    ROL(T) = ROL(T) + 4
    IF ROL(T) > 359 THEN ROL(T) = 0
    '
  NEXT T
  '
  ' DRAW NEW IMAGE
  '
  LINE (NX(1), NY(1))-(NX(2), NY(2)), 15
  LINE -(NX(3), NY(3)), 15
  LINE -(NX(4), NY(4)), 15
  LINE -(NX(1), NY(1)), 15
  '
  LINE (NX(5), NY(5))-(NX(6), NY(6)), 15
  LINE -(NX(7), NY(7)), 15
  LINE -(NX(8), NY(8)), 15
  LINE -(NX(5), NY(5)), 15
  '
  LINE (NX(1), NY(1))-(NX(8), NY(8)), 15
  LINE (NX(2), NY(2))-(NX(7), NY(7)), 15
  LINE (NX(3), NY(3))-(NX(6), NY(6)), 15
  LINE (NX(4), NY(4))-(NX(5), NY(5)), 15
  '
  ' SHOW IMAGE ON SCREEN
  '
  PCOPY 1, 0
  '
LOOP



