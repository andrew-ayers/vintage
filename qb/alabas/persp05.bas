'
' OH, YEAH! THIS PROGRAM DOES REALTIME WIREFRAME 3D MODELING!
' X,Y, AND Z PLANE TRANSLATION AND ROTATION ARE FULLY SUPPORTED!
' USES TABLES TO SPEED UP SIN/COS CALCS. NEEDS TO BE INTEGERIZED
' FOR FASTEST CALCULATIONS YET...
' NEXT STOP, A WIRE FRAME WALKTHROUGH!
'
SCREEN 11
'
DIM STAB(359), CTAB(359)
'
VD = 250: DCOR = 30
YAW = 30: PIT = 330: ROL = 0
'
FOR T = 0 TO 359
  STAB(T) = SIN((6.282 / 360) * T)
  CTAB(T) = COS((6.282 / 360) * T)
NEXT T
'
FOR Z = 250 TO -250 STEP -10
  FOR X = -250 TO 250 STEP 10
    Y = (SIN(X * .06) * 10) + (COS(Z * .06) * 10)
    '
    ' TRANSLATE, THEN ROTATE
    '
    TX = X
    TY = Y
    TZ = Z
    '
    ' ROTATE (PIT)
    '
    SX = TY * STAB(PIT) + TX * CTAB(PIT)
    SY = TY * CTAB(PIT) - TX * STAB(PIT)
    SZ = TZ
    '
    ' ROTATE (ROL)
    '
    QX = SX
    QY = SZ * STAB(ROL) + SY * CTAB(ROL)
    QZ = SZ * CTAB(ROL) - SY * STAB(ROL)
    '
    ' ROTATE (YAW)
    '
    RX = QZ * STAB(YAW) + QX * CTAB(YAW)
    RY = QY
    RZ = QZ * CTAB(YAW) - QX * STAB(YAW)
    '
    ' ROTATE, THEN TRANSLATE
    '
    NX = 320 + ((VD * RX) / (RZ + VD))
    NY = 240 + ((VD * RY) / (RZ + VD + DCOR))
    '
    'IF X = -250 THEN LINE (NX, NY)-(NX, NY), 15
    'LINE -(NX, NY), 15
    PSET (NX, NY), 15
    '
  NEXT X
NEXT Z

