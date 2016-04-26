'
' OH, YEAH! THIS PROGRAM DOES REALTIME WIREFRAME 3D MODELING!
' X,Y, AND Z PLANE TRANSLATION AND ROTATION ARE FULLY SUPPORTED!
' USES TABLES TO SPEED UP SIN/COS CALCS. NEEDS TO BE INTEGERIZED
' FOR FASTEST CALCULATIONS YET...
'
' THIS PROGRAM IS THE SAME AS #8, BUT IT USES A TOTALLY NEW WAY TO DEFINE
' OBJECTS IN A 3D VIEW.
'
SCREEN 7, , 1, 0
'
DIM OBJECT(9, 9, 3, 2)
'
' OBJECTS DEFINED AS FOLLOWS:
'   (#OBJECTS,#PLANES PER OBJECT,#POINTS PER PLANE, XYZ TRIPLE)
'
DIM DPLANE(3, 1) ' DISPLAYED PLANE COORDINATES
'
' DPLANE DEFINED AS FOLLOWS:
'   (#POINTS PER PLANE, XY DOUBLE)
'
DIM STAB(359), CTAB(359) ' SINE/COSINE TABLES
'
D = 1200: MX = 0: MY = 0: MZ = -650
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
'
' PLANE 1
OBJECT(0, 1, 0, 0) = 30: OBJECT(0, 1, 0, 1) = 30: OBJECT(0, 1, 0, 2) = -30
OBJECT(0, 1, 1, 0) = 30: OBJECT(0, 1, 1, 1) = -30: OBJECT(0, 1, 1, 2) = -30
OBJECT(0, 1, 2, 0) = 30: OBJECT(0, 1, 2, 1) = -30: OBJECT(0, 1, 2, 2) = 30
OBJECT(0, 1, 3, 0) = 30: OBJECT(0, 1, 3, 1) = 30: OBJECT(0, 1, 3, 2) = 30
'
' PLANE 2
OBJECT(0, 2, 0, 0) = 30: OBJECT(0, 2, 0, 1) = 30: OBJECT(0, 2, 0, 2) = 30
OBJECT(0, 2, 1, 0) = 30: OBJECT(0, 2, 1, 1) = -30: OBJECT(0, 2, 1, 2) = 30
OBJECT(0, 2, 2, 0) = -30: OBJECT(0, 2, 2, 1) = -30: OBJECT(0, 2, 2, 2) = 30
OBJECT(0, 2, 3, 0) = -30: OBJECT(0, 2, 3, 1) = 30: OBJECT(0, 2, 3, 2) = 30
'
' PLANE 3
OBJECT(0, 3, 0, 0) = -30: OBJECT(0, 3, 0, 1) = 30: OBJECT(0, 3, 0, 2) = 30
OBJECT(0, 3, 1, 0) = -30: OBJECT(0, 3, 1, 1) = -30: OBJECT(0, 3, 1, 2) = 30
OBJECT(0, 3, 2, 0) = -30: OBJECT(0, 3, 2, 1) = -30: OBJECT(0, 3, 2, 2) = -30
OBJECT(0, 3, 3, 0) = -30: OBJECT(0, 3, 3, 1) = 30: OBJECT(0, 3, 3, 2) = -30
'
' PLANE 4
OBJECT(0, 4, 0, 0) = -30: OBJECT(0, 4, 0, 1) = -30: OBJECT(0, 4, 0, 2) = -30
OBJECT(0, 4, 1, 0) = -30: OBJECT(0, 4, 1, 1) = -30: OBJECT(0, 4, 1, 2) = 30
OBJECT(0, 4, 2, 0) = 30: OBJECT(0, 4, 2, 1) = -30: OBJECT(0, 4, 2, 2) = 30
OBJECT(0, 4, 3, 0) = 30: OBJECT(0, 4, 3, 1) = -30: OBJECT(0, 4, 3, 2) = -30
'
' PLANE 5
OBJECT(0, 5, 0, 0) = -30: OBJECT(0, 5, 0, 1) = 30: OBJECT(0, 5, 0, 2) = -30
OBJECT(0, 5, 1, 0) = 30: OBJECT(0, 5, 1, 1) = 30: OBJECT(0, 5, 1, 2) = -30
OBJECT(0, 5, 2, 0) = 30: OBJECT(0, 5, 2, 1) = 30: OBJECT(0, 5, 2, 2) = 30
OBJECT(0, 5, 3, 0) = -30: OBJECT(0, 5, 3, 1) = 30: OBJECT(0, 5, 3, 2) = 30
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
      FOR PN = 0 TO 3 ' EACH PLANE WITH UP TO 3 POINTS
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
        DPLANE(PN, 0) = 159 + (D * RX / RZ)
        DPLANE(PN, 1) = 99 + (D * RY / RZ)
        '
      NEXT PN
      '
      ' DRAW PLANE
      '
      LINE (DPLANE(0, 0), DPLANE(0, 1))-(DPLANE(1, 0), DPLANE(1, 1)), 15
      LINE -(DPLANE(2, 0), DPLANE(2, 1)), 15
      LINE -(DPLANE(3, 0), DPLANE(3, 1)), 15
      LINE -(DPLANE(0, 0), DPLANE(0, 1)), 15
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



