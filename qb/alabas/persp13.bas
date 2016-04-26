DECLARE SUB DrawHline (fromx%, tox%, yy%, col%)
DECLARE SUB DrawTriangle (x1%, y1%, x2%, y2%, x3%, y3%, col%)
'
' OH, YEAH! THIS PROGRAM DOES REALTIME SOLID 3D MODELING!
' X,Y, AND Z PLANE TRANSLATION AND ROTATION ARE FULLY SUPPORTED!
' USES TABLES TO SPEED UP SINE/COSINE CALCULATIONS.
'
' SAME AS #12, BUT USES TRIANGLE DRAWING ROUTINES FOR LESS PAINTING
' GLITCHES, AS WELL AS BETTER SPEED
'
SCREEN 7, , 1, 0
'
DIM OBJECT(9, 9, 4, 2) AS LONG
'
' OBJECTS DEFINED AS FOLLOWS:
'   (#OBJECTS,#PLANES PER OBJECT,#POINTS PER PLANE, XYZ TRIPLE)
'
DIM DPLANE2D(4, 1) AS LONG ' SCREEN PLANE COORDINATES
'
' DPLANE2D DEFINED AS FOLLOWS:
'   (#POINTS PER PLANE, XY DOUBLE)
'
DIM DPLANE3D(4, 2) AS LONG' 3D PLANE COORDINATES
'
' DPLANE3D DEFINED AS FOLLOWS:
'   (#POINTS PER PLANE, XYZ TRIPLE)
'
DIM PLANECOL(9)             AS INTEGER
'
DIM STAB(359), CTAB(359) ' SINE/COSINE TABLES
'
D& = 1200: MX& = 0: MY& = 0: MZ& = -950
'
' COMPUTE SINE/COSINE TABLES
'
FOR t& = 0 TO 359
  STAB(t&) = SIN((6.282 / 360) * t&)
  CTAB(t&) = COS((6.282 / 360) * t&)
NEXT
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
PLANECOL(0) = 3
PLANECOL(1) = 4
PLANECOL(2) = 5
PLANECOL(3) = 6
PLANECOL(4) = 7
PLANECOL(5) = 8
'
DO
  '
  ' ERASE LAST IMAGE
  '
  'CLS
  LINE (0, 0)-(319, 99), 1, BF
  LINE (0, 100)-(319, 199), 2, BF
  '
  ' CALCULATE POSITION OF NEW IMAGE
  '
  FOR OB& = 0 TO 0 ' UP TO 9 OBJECTS
    SP = STAB(PIT(OB&)): CP = CTAB(PIT(OB&))
    SY = STAB(YAW(OB&)): CY = CTAB(YAW(OB&))
    SR = STAB(ROL(OB&)): CR = CTAB(ROL(OB&))
    FOR PL& = 0 TO 5 ' CONSISTING OF UP TO 9 PLANES
      '
      FOR PN& = 0 TO 3 ' EACH PLANE WITH UP TO 4 POINTS (#5 TO PAINT)
        '
        ' TRANSLATE, THEN ROTATE
        '
        TX& = OBJECT(OB&, PL&, PN&, 0)
        TY& = OBJECT(OB&, PL&, PN&, 1)
        TZ& = OBJECT(OB&, PL&, PN&, 2)
        '
        RX& = (TZ& * CP - TY& * SP) * SY - ((TZ& * SP + TY& * CP) * SR + TX& * CR) * CY
        RY& = (TZ& * SP + TY& * CP) * CR - TX& * SR
        RZ& = (TZ& * CP - TY& * SP) * CY + ((TZ& * SP + TY& * CP) * SR + TX& * CR) * SY
        '
        ' ROTATE, THEN TRANSLATE
        '
        RX& = RX& + MX&
        RY& = RY& + MY&
        RZ& = RZ& + MZ&
        '
        DPLANE3D(PN&, 0) = RX&: DPLANE3D(PN&, 1) = RY&: DPLANE3D(PN&, 2) = RZ&
        '
        DPLANE2D(PN&, 0) = 159 + (D& * RX& / RZ&)
        DPLANE2D(PN&, 1) = 99 + (D& * RY& / RZ&)
        '
      NEXT
      '
      ' CHECK TO SEE IF PLANE IS VISIBLE
      '
      x1& = DPLANE3D(0, 0): y1& = DPLANE3D(0, 1): Z1& = DPLANE3D(0, 2)
      x2& = DPLANE3D(1, 0): y2& = DPLANE3D(1, 1): Z2& = DPLANE3D(1, 2)
      x3& = DPLANE3D(2, 0): y3& = DPLANE3D(2, 1): Z3& = DPLANE3D(2, 2)
      '
      T1& = -x1& * (y2& * Z3& - y3& * Z2&)
      T2& = x2& * (y3& * Z1& - y1& * Z3&)
      T3& = x3& * (y1& * Z2& - y2& * Z1&)
      '
      VISIBLE& = T1& - T2& - T3&
      '
      IF VISIBLE& > 0 THEN
        '
        ' DRAW PLANE
        '
        xx1% = DPLANE2D(0, 0): yy1% = DPLANE2D(0, 1)
        xx2% = DPLANE2D(1, 0): yy2% = DPLANE2D(1, 1)
        xx3% = DPLANE2D(2, 0): yy3% = DPLANE2D(2, 1)
        col% = PLANECOL(PL&)
        CALL DrawTriangle(xx1%, yy1%, xx2%, yy2%, xx3%, yy3%, col%)
        '
        xx1% = DPLANE2D(0, 0): yy1% = DPLANE2D(0, 1)
        xx3% = DPLANE2D(2, 0): yy3% = DPLANE2D(2, 1)
        xx4% = DPLANE2D(3, 0): yy4% = DPLANE2D(3, 1)
        CALL DrawTriangle(xx1%, yy1%, xx3%, yy3%, xx4%, yy4%, col%)
        '
      END IF
      '
    NEXT
    '
    ' ROTATE OBJECT
    '
    PIT(OB&) = PIT(OB&) + 5
    IF PIT(OB&) > 359 THEN PIT(OB&) = 0
    '
    YAW(OB&) = YAW(OB&) + 7
    IF YAW(OB&) > 359 THEN YAW(OB&) = 0
    '
    ROL(OB&) = ROL(OB&) + 9
    IF ROL(OB&) > 359 THEN ROL(OB&) = 0
    '
  NEXT
  '
  ' Calculate Frames per Second
  '
  frames% = frames% + 1
  '
  IF oldtime$ <> TIME$ THEN
    fps% = frames%
    frames% = 1
    oldtime$ = TIME$
  END IF
  '
  COLOR 15, 1: LOCATE 1, 1: PRINT "FPS :"; fps%
  '
  ' Show Image on Screen
  '
  PCOPY 1, 0
  '
LOOP UNTIL INKEY$ <> ""
'
WIDTH 80: SCREEN 0: CLS

SUB DrawHline (fromx%, tox%, yy%, col%)
  '
  'DEF SEG = &HA000
  '
  'IF fromx% > tox% THEN SWAP fromx%, tox%
  '
  'yyy& = yy%
  'sloc& = yyy& * 320 + fromx%
  'eloc& = sloc& + (tox% - fromx%)
  '
  'FOR t& = sloc& TO eloc&
  '  POKE t&, col%
  'NEXT
  '
  'DEF SEG
  LINE (fromx%, yy%)-(tox%, yy%), col%
  '
END SUB

SUB DrawTriangle (x1%, y1%, x2%, y2%, x3%, y3%, col%)
  '
  DO
    sflag% = 0
    IF y1% > y2% THEN
      sflag% = 1
      SWAP y1%, y2%
      SWAP x1%, x2%
    END IF
    IF y2% > y3% THEN
      sflag% = 1
      SWAP y2%, y3%
      SWAP x2%, x3%
    END IF
  LOOP UNTIL sflag% = 0
  '
  IF y2% = y3% THEN
    '
    ' Draw a flat bottomed triangle
    '
    ydiff1% = y2% - y1%
    ydiff2% = y3% - y1%
    '
    IF ydiff1% <> 0 THEN
      slope1! = (x2% - x1%) / ydiff1%
    ELSE
      slope1! = 0
    END IF
    '
    IF ydiff2% <> 0 THEN
      slope2! = (x3% - x1%) / ydiff2%
    ELSE
      slope2! = 0
    END IF
    '
    sx! = x1%: ex! = x1%
    '
    FOR y% = y1% TO y2%
      CALL DrawHline(CINT(sx!), CINT(ex!), y%, col%)
      sx! = sx! + slope1!
      ex! = ex! + slope2!
    NEXT
    '
    EXIT SUB
    '
  ELSE
    IF y1% = y2% THEN
      '
      ' Draw a flat topped triangle
      '
      ydiff1% = y3% - y1%
      ydiff2% = y3% - y2%
      '
      IF ydiff1% <> 0 THEN
        slope1! = (x3% - x1%) / ydiff1%
      ELSE
        slope1! = 0
      END IF
      '
      IF ydiff2% <> 0 THEN
        slope2! = (x3% - x2%) / ydiff2%
      ELSE
        slope2! = 0
      END IF
      '
      sx! = x1%: ex! = x2%
      '
      FOR y% = y1% TO y3%
        CALL DrawHline(CINT(sx!), CINT(ex!), y%, col%)
        sx! = sx! + slope1!
        ex! = ex! + slope2!
      NEXT
      '
      x1% = sx!: x2% = ex!
      '
      EXIT SUB
      '
    ELSE
      '
      ' Draw a general purpose triangle
      '
      '
      ' First draw the flat bottom portion (top half)
      '
      ydiff1% = y2% - y1%
      ydiff2% = y3% - y1%
      '
      IF ydiff1% <> 0 THEN
        slope1! = (x2% - x1%) / ydiff1%
      ELSE
        slope1! = 0
      END IF
      '
      IF ydiff2% <> 0 THEN
        slope2! = (x3% - x1%) / ydiff2%
      ELSE
        slope2! = 0
      END IF
      '
      sx! = x1%: ex! = x1%
      '
      FOR y% = y1% TO y2%
        CALL DrawHline(CINT(sx!), CINT(ex!), y%, col%)
        sx! = sx! + slope1!
        ex! = ex! + slope2!
      NEXT
      '
      ' Then draw the flat topped portion (bottom half)
      '
      x1% = x2%
      x2% = ex!
      y1% = y2%
      '
      ydiff1% = y3% - y1%
      ydiff2% = y3% - y2%
      '
      IF ydiff1% <> 0 THEN
        slope1! = (x3% - x1%) / ydiff1%
      ELSE
        slope1! = 0
      END IF
      '
      IF ydiff2% <> 0 THEN
        slope2! = (x3% - x2%) / ydiff2%
      ELSE
        slope2! = 0
      END IF
      '
      sx! = x1%: ex! = x2%
      '
      FOR y% = y1% TO y3%
        CALL DrawHline(CINT(sx!), CINT(ex!), y%, col%)
        sx! = sx! + slope1!
        ex! = ex! + slope2!
      NEXT
      '
      x1% = sx!: x2% = ex!
      '
    END IF
  END IF
  '
END SUB

