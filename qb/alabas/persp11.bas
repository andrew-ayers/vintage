'
' OH, YEAH! THIS PROGRAM DOES REALTIME SOLID 3D MODELING!
' X,Y, AND Z PLANE TRANSLATION AND ROTATION ARE FULLY SUPPORTED!
' USES TABLES TO SPEED UP SINE/COSINE CALCULATIONS.
'
SCREEN 7, , 1, 0
'
DIM OBJECT(9, 9, 4, 2)
'
' OBJECTS DEFINED AS FOLLOWS:
'   (#OBJECTS,#PLANES PER OBJECT,#POINTS PER PLANE, XYZ TRIPLE)
'
DIM DPLANE2D(4, 1) ' SCREEN PLANE COORDINATES
'
' DPLANE2D DEFINED AS FOLLOWS:
'   (#POINTS PER PLANE, XY DOUBLE)
'
DIM DPLANE3D(4, 2) ' 3D PLANE COORDINATES
'
' DPLANE3D DEFINED AS FOLLOWS:
'   (#POINTS PER PLANE, XYZ TRIPLE)
'
DIM PLANECOL(9)
'
DIM STAB(359), CTAB(359) ' SINE/COSINE TABLES
'
D = 1200: MX = 0: MY = 0: MZ = -850
YAW = 0: PIT = 0: ROL = 0
'
' COMPUTE SINE/COSINE TABLES
'
FOR T = 0 TO 359
  STAB(T) = SIN((6.282 / 360) * T)
  CTAB(T) = COS((6.282 / 360) * T)
NEXT T
'
' BUILD CUBE IN OBJECT ARRAY
'
' PLANE 0
OBJECT(0, 0, 0, 0) = -30: OBJECT(0, 0, 0, 1) = 30: OBJECT(0, 0, 0, 2) = -30
OBJECT(0, 0, 1, 0) = -30: OBJECT(0, 0, 1, 1) = -30: OBJECT(0, 0, 1, 2) = -30
OBJECT(0, 0, 2, 0) = 30: OBJECT(0, 0, 2, 1) = -30: OBJECT(0, 0, 2, 2) = -30
OBJECT(0, 0, 3, 0) = 30: OBJECT(0, 0, 3, 1) = 30: OBJECT(0, 0, 3, 2) = -30
OBJECT(0, 0, 4, 0) = 0: OBJECT(0, 0, 4, 1) = 0: OBJECT(0, 0, 4, 2) = -30
'
' PLANE 1
OBJECT(0, 1, 0, 0) = 30: OBJECT(0, 1, 0, 1) = 30: OBJECT(0, 1, 0, 2) = -30
OBJECT(0, 1, 1, 0) = 30: OBJECT(0, 1, 1, 1) = -30: OBJECT(0, 1, 1, 2) = -30
OBJECT(0, 1, 2, 0) = 30: OBJECT(0, 1, 2, 1) = -30: OBJECT(0, 1, 2, 2) = 30
OBJECT(0, 1, 3, 0) = 30: OBJECT(0, 1, 3, 1) = 30: OBJECT(0, 1, 3, 2) = 30
OBJECT(0, 1, 4, 0) = 30: OBJECT(0, 1, 4, 1) = 0: OBJECT(0, 1, 4, 2) = 0
'
' PLANE 2
OBJECT(0, 2, 0, 0) = 30: OBJECT(0, 2, 0, 1) = 30: OBJECT(0, 2, 0, 2) = 30
OBJECT(0, 2, 1, 0) = 30: OBJECT(0, 2, 1, 1) = -30: OBJECT(0, 2, 1, 2) = 30
OBJECT(0, 2, 2, 0) = -30: OBJECT(0, 2, 2, 1) = -30: OBJECT(0, 2, 2, 2) = 30
OBJECT(0, 2, 3, 0) = -30: OBJECT(0, 2, 3, 1) = 30: OBJECT(0, 2, 3, 2) = 30
OBJECT(0, 2, 4, 0) = 0: OBJECT(0, 2, 4, 1) = 0: OBJECT(0, 2, 4, 2) = 30
'
' PLANE 3
OBJECT(0, 3, 0, 0) = -30: OBJECT(0, 3, 0, 1) = 30: OBJECT(0, 3, 0, 2) = 30
OBJECT(0, 3, 1, 0) = -30: OBJECT(0, 3, 1, 1) = -30: OBJECT(0, 3, 1, 2) = 30
OBJECT(0, 3, 2, 0) = -30: OBJECT(0, 3, 2, 1) = -30: OBJECT(0, 3, 2, 2) = -30
OBJECT(0, 3, 3, 0) = -30: OBJECT(0, 3, 3, 1) = 30: OBJECT(0, 3, 3, 2) = -30
OBJECT(0, 3, 4, 0) = -30: OBJECT(0, 3, 4, 1) = 0: OBJECT(0, 3, 4, 2) = 0
'
' PLANE 4
OBJECT(0, 4, 0, 0) = -30: OBJECT(0, 4, 0, 1) = -30: OBJECT(0, 4, 0, 2) = -30
OBJECT(0, 4, 1, 0) = -30: OBJECT(0, 4, 1, 1) = -30: OBJECT(0, 4, 1, 2) = 30
OBJECT(0, 4, 2, 0) = 30: OBJECT(0, 4, 2, 1) = -30: OBJECT(0, 4, 2, 2) = 30
OBJECT(0, 4, 3, 0) = 30: OBJECT(0, 4, 3, 1) = -30: OBJECT(0, 4, 3, 2) = -30
OBJECT(0, 4, 4, 0) = 0: OBJECT(0, 4, 4, 1) = -30: OBJECT(0, 4, 4, 2) = 0
'
' PLANE 5
OBJECT(0, 5, 0, 0) = -30: OBJECT(0, 5, 0, 1) = 30: OBJECT(0, 5, 0, 2) = -30
OBJECT(0, 5, 1, 0) = 30: OBJECT(0, 5, 1, 1) = 30: OBJECT(0, 5, 1, 2) = -30
OBJECT(0, 5, 2, 0) = 30: OBJECT(0, 5, 2, 1) = 30: OBJECT(0, 5, 2, 2) = 30
OBJECT(0, 5, 3, 0) = -30: OBJECT(0, 5, 3, 1) = 30: OBJECT(0, 5, 3, 2) = 30
OBJECT(0, 5, 4, 0) = 0: OBJECT(0, 5, 4, 1) = 30: OBJECT(0, 5, 4, 2) = 0
'
' SET UP PLANE COLORS ON CUBE
'
PLANECOL(0) = 1
PLANECOL(1) = 2
PLANECOL(2) = 3
PLANECOL(3) = 4
PLANECOL(4) = 5
PLANECOL(5) = 6
'
DO
  '
  ' ERASE LAST IMAGE
  '
  CLS
  '
  ' CALCULATE POSITION OF NEW IMAGE
  '
  FOR OB = 0 TO 0 ' UP TO 9 OBJECTS
    FOR PL = 0 TO 5 ' CONSISTING OF UP TO 9 PLANES
      '
      FOR PN = 0 TO 4 ' EACH PLANE WITH UP TO 4 POINTS (#5 TO PAINT)
        '
        ' TRANSLATE, THEN ROTATE
        '
        TX = OBJECT(OB, PL, PN, 0)
        TY = OBJECT(OB, PL, PN, 1)
        TZ = OBJECT(OB, PL, PN, 2)
        '
        ' ROTATE (PIT)
        '
        SX = TX
        SY = TZ * STAB(PIT) + TY * CTAB(PIT)
        SZ = TZ * CTAB(PIT) - TY * STAB(PIT)
        '
        ' ROTATE (ROL)
        '
        QX = SY * STAB(ROL) + SX * CTAB(ROL)
        QY = SY * CTAB(ROL) - SX * STAB(ROL)
        QZ = SZ
        '
        ' ROTATE (YAW)
        '
        RX = QZ * STAB(YAW) - QX * CTAB(YAW)
        RY = QY
        RZ = QZ * CTAB(YAW) + QX * STAB(YAW)
        '
        ' ROTATE, THEN TRANSLATE
        '
        RX = RX + MX
        RY = RY + MY
        RZ = RZ + MZ
        '
        DPLANE3D(PN, 0) = RX: DPLANE3D(PN, 1) = RY: DPLANE3D(PN, 2) = RZ
        '
        DPLANE2D(PN, 0) = 159 + (D * RX / RZ)
        DPLANE2D(PN, 1) = 99 + (D * RY / RZ)
        '
      NEXT PN
      '
      ' CHECK TO SEE IF PLANE IS VISIBLE
      '
      X1 = DPLANE3D(0, 0): Y1 = DPLANE3D(0, 1): Z1 = DPLANE3D(0, 2)
      X2 = DPLANE3D(1, 0): Y2 = DPLANE3D(1, 1): Z2 = DPLANE3D(1, 2)
      X3 = DPLANE3D(2, 0): Y3 = DPLANE3D(2, 1): Z3 = DPLANE3D(2, 2)
      '
      T1 = -X1 * (Y2 * Z3 - Y3 * Z2)
      T2 = X2 * (Y3 * Z1 - Y1 * Z3)
      T3 = X3 * (Y1 * Z2 - Y2 * Z1)
      '
      VISIBLE = T1 - T2 - T3
      '
      IF VISIBLE > 0 THEN
        '
        ' DRAW PLANE
        '
        LINE (DPLANE2D(0, 0), DPLANE2D(0, 1))-(DPLANE2D(1, 0), DPLANE2D(1, 1)), 15
        LINE -(DPLANE2D(2, 0), DPLANE2D(2, 1)), 15
        LINE -(DPLANE2D(3, 0), DPLANE2D(3, 1)), 15
        LINE -(DPLANE2D(0, 0), DPLANE2D(0, 1)), 15
        PAINT (DPLANE2D(4, 0), DPLANE2D(4, 1)), PLANECOL(PL), 15
        '
      END IF
      '
    NEXT PL
    '
    ' ROTATE OBJECT
    '
    PIT = PIT + 3
    IF PIT > 359 THEN PIT = 0
    '
    YAW = YAW + 2
    IF YAW > 359 THEN YAW = 0
    '
    ROL = ROL + 4
    IF ROL > 359 THEN ROL = 0
    '
  NEXT OB
  '
  ' SHOW IMAGE ON SCREEN
  '
  PCOPY 1, 0
  '
LOOP



