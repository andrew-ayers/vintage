DECLARE SUB TRIANGLE (X1!, Y1!, X2!, Y2!, X3!, Y3!, COL!)
DECLARE SUB FLATBOT (X0!, Y0!, SLOPE1!, SLOPE2!, Y1!, LASTX!, COL!)
DECLARE SUB FLATTOP (X0!, X1!, Y0!, SLOPE1!, SLOPE2!, Y2!, COL!)
'
MX = 159: MY = 99
MAXX = 80: MAXZ = 80
STP = 13: SCALE = 1
DEGREES = 45
COMPFLAG = 1
'
GOSUB DEGTORAD
'
GOSUB SETSCREEN
'
IF COMPFLAG = 1 THEN GOSUB SETCOMPASS
'
GOSUB PLOT
'
IF COMPFLAG = 2 THEN GOSUB SETCOMPASS
'
END

SETSCREEN:
  '
  SCREEN 7
  '
  RETURN

DEGTORAD:
  '
  RADIAN = (3.14159 * 2) / 360
  DEGREES2 = 270 + (DEGREES / 2)
  ANGLE = (DEGREES + (DEGREES / 2)) * RADIAN
  ANGLE2 = DEGREES2 * RADIAN
  '
  RETURN

FUNCT:
  '
  'FY = SIN(FX * .1) * COS(FZ * .1) * 10
  'FY = FX / (FZ + (FZ = 0))
  FY = SIN(SQR((FX * FX * .01) + (FZ * FZ * .01))) * 10
  'FY = FY * (FX > -40 AND FX < 40) * (FZ > -40 AND FZ < 40)
  '
  RETURN

SETCOMPASS:
  '
  LINE (MX, 0)-(MX, MY), 8, , &H8888
  PX = 300: PY = 0: PZ = 0
  GOSUB GETCOORDS
  LINE -(X, Y), 8, , &H8888
  PX = 0: PY = 0: PZ = 300
  GOSUB GETCOORDS
  LINE (MX, MY)-(X, Y), 8, , &H8888
  '
  RETURN

PLOT:
  '
  FOR LZ = -MAXZ TO MAXZ - STP STEP STP
    FOR LX = -MAXX TO MAXX - STP STEP STP
      FX = LX: FZ = LZ
      GOSUB FUNCT
      PX = FX: PY = FY: PZ = FZ
      GOSUB GETCOORDS
      X1 = X: Y1 = Y
      '
      FX = LX + STP: FZ = LZ
      GOSUB FUNCT
      PX = FX: PY = FY: PZ = FZ
      GOSUB GETCOORDS
      X2 = X: Y2 = Y
      '
      FX = LX + STP: FZ = LZ + STP
      GOSUB FUNCT
      PX = FX: PY = FY: PZ = FZ
      GOSUB GETCOORDS
      X3 = X: Y3 = Y
      '
      FX = LX: FZ = LZ + STP
      GOSUB FUNCT
      PX = FX: PY = FY: PZ = FZ
      GOSUB GETCOORDS
      X4 = X: Y4 = Y
      '
      IF INT(LZ / 2) = LZ / 2 THEN
        IF INT(LX / 2) = LX / 2 THEN
          REAL = 4
        ELSE
          REAL = 15
        END IF
      ELSE
        IF INT(LX / 2) = LX / 2 THEN
          REAL = 15
        ELSE
          REAL = 4
        END IF
      END IF
      '
      GOSUB DRAWPLANE
      '
    NEXT LX
  NEXT LZ
  '
  RETURN

GETCOORDS:
  '
  TX = PX * SCALE: TY = PY * SCALE
  '
  X = MX + ((SIN(-ANGLE) * PZ) + (SIN(-ANGLE2) * PX)) * SCALE
  Y = MY + ((COS(-ANGLE) * PZ) + (COS(-ANGLE2) * PX)) * SCALE
  '
  Y = Y - TY
  '
  RETURN

DRAWPLANE:
  '
  CALL TRIANGLE(X1, Y1, X2, Y2, X3, Y3, REAL)
  CALL TRIANGLE(X1, Y1, X3, Y3, X4, Y4, REAL)
  '
  RETURN

SUB FLATBOT (X0, Y0, SLOPE1, SLOPE2, Y1, LASTX, COL)
  '
  SX = X0: EX = X0
  '
  FOR Y = Y0 TO Y1
    LINE (SX, Y)-(EX, Y), COL
    SX = SX + SLOPE1
    EX = EX + SLOPE2
  NEXT Y
  LASTX = EX
  '
END SUB

SUB FLATTOP (X0, X1, Y0, SLOPE1, SLOPE2, Y2, COL)
  '
  FOR Y = Y0 TO Y2
    LINE (X0, Y)-(X1, Y), COL
    X0 = X0 + SLOPE1
    X1 = X1 + SLOPE2
  NEXT Y
  '
END SUB

SUB TRIANGLE (X1, Y1, X2, Y2, X3, Y3, COL)
  '
  DIM X(2), Y(2)
  '
  X(0) = X1: Y(0) = Y1
  X(1) = X2: Y(1) = Y2
  X(2) = X3: Y(2) = Y3
  '
  DO
    SFLAG = 0
    FOR T = 0 TO 1
      IF Y(T) > Y(T + 1) THEN
        SFLAG = 1
        TEMP = Y(T)
        Y(T) = Y(T + 1)
        Y(T + 1) = TEMP
        TEMP = X(T)
        X(T) = X(T + 1)
        X(T + 1) = TEMP
      END IF
    NEXT T
  LOOP UNTIL SFLAG = 0
  '
  IF Y(1) = Y(2) THEN
    '
    ' FLAT BOTTOM
    '
    YDIFF1 = Y(1) - Y(0)
    YDIFF2 = Y(2) - Y(0)
    '
    IF YDIFF1 <> 0 THEN
      SLOPE1 = (X(1) - X(0)) / YDIFF1
    ELSE
      SLOPE1 = 0
    END IF
    '
    IF YDIFF2 <> 0 THEN
      SLOPE2 = (X(2) - X(0)) / YDIFF2
    ELSE
      SLOPE2 = 0
    END IF
    '
    CALL FLATBOT(X(0), Y(0), SLOPE1, SLOPE2, Y(1), LASTX, COL)
    '
  ELSE
    IF Y(0) = Y(1) THEN
      '
      ' FLAT TOP
      '
      YDIFF1 = Y(2) - Y(0)
      YDIFF2 = Y(2) - Y(1)
      '
      IF YDIFF1 <> 0 THEN
        SLOPE1 = (X(2) - X(0)) / YDIFF1
      ELSE
        SLOPE1 = 0
      END IF
      '
      IF YDIFF2 <> 0 THEN
        SLOPE2 = (X(2) - X(1)) / YDIFF2
      ELSE
        SLOPE2 = 0
      END IF
      '
      CALL FLATTOP(X(0), X(1), Y(0), SLOPE1, SLOPE2, Y(2), COL)
      '
    ELSE
      '
      ' GENERAL
      '
      '
      ' FIRST, THE FLAT BOTTOM
      '
      YDIFF1 = Y(1) - Y(0)
      YDIFF2 = Y(2) - Y(0)
      '
      IF YDIFF1 <> 0 THEN
        SLOPE1 = (X(1) - X(0)) / YDIFF1
      ELSE
        SLOPE1 = 0
      END IF
      '
      IF YDIFF2 <> 0 THEN
        SLOPE2 = (X(2) - X(0)) / YDIFF2
      ELSE
        SLOPE2 = 0
      END IF
      '
      CALL FLATBOT(X(0), Y(0), SLOPE1, SLOPE2, Y(1), LASTX, COL)
      '
      ' NOW, THE FLAT TOP
      '
      X(0) = X(1)
      X(1) = LASTX
      Y(0) = Y(1)
      '
      ' FLAT TOP
      '
      YDIFF1 = Y(2) - Y(0)
      YDIFF2 = Y(2) - Y(1)
      '
      IF YDIFF1 <> 0 THEN
        SLOPE1 = (X(2) - X(0)) / YDIFF1
      ELSE
        SLOPE1 = 0
      END IF
      '
      IF YDIFF2 <> 0 THEN
        SLOPE2 = (X(2) - X(1)) / YDIFF2
      ELSE
        SLOPE2 = 0
      END IF
      '
      CALL FLATTOP(X(0), X(1), Y(0), SLOPE1, SLOPE2, Y(2), COL)
      '
    END IF
  END IF
  '
END SUB

