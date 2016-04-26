'
' OH, YEAH! THIS PROGRAM DOES REALTIME WIREFRAME 3D MODELING!
' X,Y, AND Z PLANE TRANSLATION AND ROTATION ARE FULLY SUPPORTED!
' USES TABLES TO SPEED UP SIN/COS CALCS. NOW USES AN ANALGLYPHIC
' 3-D. GET OUT YOUR RED/BLUE GLASSES, FOLKS!
'
SCREEN 7, , 1, 0
'
DIM PX(10), PY(10), PZ(10), YAW(10), PIT(10), ROL(10), NX(10, 1), NY(10, 1)
DIM STAB(359), CTAB(359)
'
' THE FOLLOWING ARE LEFT/RIGHT COLORS AND
' INTEROCULAR SPACING (IN PIXELS). ADJUST IO FOR
' INDIVIDUAL USE.
'
RT = 4: LT = 1: IO = 35
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
  FOR TT = 0 TO 1
    '
    ' ERASE LAST IMAGE
    '
    LINE (NX(1, TT), NY(1, TT))-(NX(2, TT), NY(2, TT)), 0
    LINE -(NX(3, TT), NY(3, TT)), 0
    LINE -(NX(4, TT), NY(4, TT)), 0
    LINE -(NX(1, TT), NY(1, TT)), 0
    '
    LINE (NX(5, TT), NY(5, TT))-(NX(6, TT), NY(6, TT)), 0
    LINE -(NX(7, TT), NY(7, TT)), 0
    LINE -(NX(8, TT), NY(8, TT)), 0
    LINE -(NX(5, TT), NY(5, TT)), 0
    '
    LINE (NX(1, TT), NY(1, TT))-(NX(8, TT), NY(8, TT)), 0
    LINE (NX(2, TT), NY(2, TT))-(NX(7, TT), NY(7, TT)), 0
    LINE (NX(3, TT), NY(3, TT))-(NX(6, TT), NY(6, TT)), 0
    LINE (NX(4, TT), NY(4, TT))-(NX(5, TT), NY(5, TT)), 0
    '
  NEXT TT
  '
  FOR TT = 0 TO 1
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
      RX = RX + (IO * TT) - (IO / 2)
      'RZ = RZ + 50
      '
      NX(T, TT) = 160 + ((VD * RX) / (RZ + VD))
      NY(T, TT) = 100 + ((VD * RY) / (RZ + VD + DCOR))
      '
      PIT(T) = PIT(T) + 1
      IF PIT(T) > 359 THEN PIT(T) = 0
      '
      YAW(T) = YAW(T) + 1
      IF YAW(T) > 359 THEN YAW(T) = 0
      '
      ROL(T) = ROL(T) + 1
      IF ROL(T) > 359 THEN ROL(T) = 0
      '
    NEXT T
    '
    ' DRAW NEW IMAGE
    '
    IF TT = 1 THEN CL = RT ELSE CL = LT ' DETERMINE LEFT OR RIGHT CUBE
    '
    LINE (NX(1, TT), NY(1, TT))-(NX(2, TT), NY(2, TT)), CL
    LINE -(NX(3, TT), NY(3, TT)), CL
    LINE -(NX(4, TT), NY(4, TT)), CL
    LINE -(NX(1, TT), NY(1, TT)), CL
    '
    LINE (NX(5, TT), NY(5, TT))-(NX(6, TT), NY(6, TT)), CL
    LINE -(NX(7, TT), NY(7, TT)), CL
    LINE -(NX(8, TT), NY(8, TT)), CL
    LINE -(NX(5, TT), NY(5, TT)), CL
    '
    LINE (NX(1, TT), NY(1, TT))-(NX(8, TT), NY(8, TT)), CL
    LINE (NX(2, TT), NY(2, TT))-(NX(7, TT), NY(7, TT)), CL
    LINE (NX(3, TT), NY(3, TT))-(NX(6, TT), NY(6, TT)), CL
    LINE (NX(4, TT), NY(4, TT))-(NX(5, TT), NY(5, TT)), CL
    '
  NEXT TT
  '
  ' SHOW IMAGE ON SCREEN
  '
  PCOPY 1, 0
  '
LOOP



