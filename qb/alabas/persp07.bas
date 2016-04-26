'
' OH, YEAH! THIS PROGRAM DOES REALTIME SOLID B/W 3D MODELING IN THE
' X/Z PLANE, AND THE CUBE SPINS! (IE., ONLY YAW ROTATION, AND XYZ
' TRANSLATION ALLOWED)... WHY NO ROLL OR PITCH? BECAUSE THIS PROGRAM
' WAS DESIGNED FOR 3D STYLE WALKTHROUGHS, NO NEED FOR ROLL/PITCH.
'
SCREEN 7, , 1, 0
'
DIM PX(10), PY(10), PZ(10), YAW(10), YAW2(10), NX(10), NY(10), NZ(10)
'
MX = 0: MY = 0: MZ = 200: HED = 0
VD = 250: DCOR = 25
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
  YAW2(T) = 0
NEXT T
'
DO
  A$ = INKEY$
  SELECT CASE A$
    CASE "."
      HED = HED + .05
      IF HED > 6.28318 THEN HED = 0
    CASE ","
      HED = HED - .05
      IF HED < 0 THEN HED = 6.28318
    CASE "Z"
      MX = MX + SIN(HED) * 5
      MZ = MZ + COS(HED) * 5
    CASE "A"
      MX = MX - SIN(HED) * 5
      MZ = MZ - COS(HED) * 5
  END SELECT
  '
  ' ERASE LAST IMAGE
  '
  ' SIDE 1
  '
  IF NX(1) < NX(2) THEN
    LINE (NX(1), NY(1))-(NX(2), NY(2)), 0
    LINE -(NX(6), NY(6)), 0
    LINE -(NX(5), NY(5)), 0
    LINE -(NX(1), NY(1)), 0
  END IF
  '
  ' SIDE 2
  '
  IF NX(2) < NX(3) THEN
    LINE (NX(2), NY(2))-(NX(3), NY(3)), 0
    LINE -(NX(7), NY(7)), 0
    LINE -(NX(6), NY(6)), 0
    LINE -(NX(2), NY(2)), 0
  END IF
  '
  ' SIDE 3
  '
  IF NX(3) < NX(4) THEN
    LINE (NX(3), NY(3))-(NX(4), NY(4)), 0
    LINE -(NX(8), NY(8)), 0
    LINE -(NX(7), NY(7)), 0
    LINE -(NX(3), NY(3)), 0
  END IF
  '
  ' SIDE 4
  '
  IF NX(4) < NX(1) THEN
    LINE (NX(4), NY(4))-(NX(1), NY(1)), 0
    LINE -(NX(5), NY(5)), 0
    LINE -(NX(8), NY(8)), 0
    LINE -(NX(4), NY(4)), 0
  END IF
  '
  ' CALCULATE POSITION OF NEW IMAGE
  '
  FOR T = 1 TO 8
    '
    ' ROTATE CUBE FIRST
    '
    YAW2(T) = YAW2(T) + .1
    IF YAW2(T) > 6.28318 THEN YAW2(T) = 0
    '
    TX = PX(T)
    TY = PY(T)
    TZ = PZ(T)
    '
    RX = TX * COS(YAW2(T)) + TZ * SIN(YAW2(T))
    RY = -TY
    RZ = -(TX * SIN(YAW2(T)) - TZ * COS(YAW2(T)))
    '
    ' THEN TRANSLATE CUBE, THEN ROTATE (VIEWPOINT)
    '
    TX = RX + MX
    TY = RY + MY
    TZ = RZ + MZ
    '
    ' ROTATE (VIEWPOINT)
    '
    RX = TX * COS(YAW(T)) + TZ * SIN(YAW(T))
    RY = -TY
    RZ = -(TX * SIN(YAW(T)) - TZ * COS(YAW(T)))
    '
    NX(T) = 160 + ((VD * RX) / (RZ + .00001))
    NY(T) = 100 + ((VD * RY) / (RZ + .00001 + DCOR))
    NZ(T) = RZ
    '
    YAW(T) = -HED
    '
  NEXT T
  '
  ' DRAW NEW IMAGE
  '
  ' SIDE 1
  '
  IF NX(1) < NX(2) THEN
    LINE (NX(1), NY(1))-(NX(2), NY(2)), 15
    LINE -(NX(6), NY(6)), 15
    LINE -(NX(5), NY(5)), 15
    LINE -(NX(1), NY(1)), 15
  END IF
  '
  ' SIDE 2
  '
  IF NX(2) < NX(3) THEN
    LINE (NX(2), NY(2))-(NX(3), NY(3)), 15
    LINE -(NX(7), NY(7)), 15
    LINE -(NX(6), NY(6)), 15
    LINE -(NX(2), NY(2)), 15
  END IF
  '
  ' SIDE 3
  '
  IF NX(3) < NX(4) THEN
    LINE (NX(3), NY(3))-(NX(4), NY(4)), 15
    LINE -(NX(8), NY(8)), 15
    LINE -(NX(7), NY(7)), 15
    LINE -(NX(3), NY(3)), 15
  END IF
  '
  ' SIDE 4
  '
  IF NX(4) < NX(1) THEN
    LINE (NX(4), NY(4))-(NX(1), NY(1)), 15
    LINE -(NX(5), NY(5)), 15
    LINE -(NX(8), NY(8)), 15
    LINE -(NX(4), NY(4)), 15
  END IF
  '
  ' SHOW IMAGE ON SCREEN
  '
  PCOPY 1, 0
  '
LOOP



