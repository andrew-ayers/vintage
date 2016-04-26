'
' DEMOSTRATES ROUTINE TO DRAW SOLID TRIANGLES ON SCREEN
' SHOULD BE ABLE TO SPEED UP BY USING BRESENHAM ALGORITHM
' INSTEAD OF STANDARD SLOPE EQUATION...
'
DECLARE SUB FLATBOT (X0!, Y0!, SLOPE1!, SLOPE2!, Y1!, LASTX!, COL!)
DECLARE SUB FLATTOP (X0!, X1!, Y0!, SLOPE1!, SLOPE2!, Y2!, COL!)
DECLARE SUB TRIANGLE (X1!, Y1!, X2!, Y2!, X3!, Y3!, COL!)
'
SCREEN 13
'
DO
  X1 = INT(RND * 320)
  X2 = INT(RND * 320)
  X3 = INT(RND * 320)
  Y1 = INT(RND * 200)
  Y2 = INT(RND * 200)
  Y3 = INT(RND * 200)
  COL = INT(RND * 255) + 1
  '
  CALL TRIANGLE(X1, Y1, X2, Y2, X3, Y3, COL)
  '
LOOP

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

