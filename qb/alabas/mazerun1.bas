DECLARE SUB Generate.Maze (Maxx%, Maxy%, maze%())
DECLARE SUB Draw.Back ()
DECLARE SUB Get.Map.Sq (map%(), px%, py%, dir%, flag%)
DECLARE SUB Draw.View (map%(), px%, py%, dir%)
DECLARE SUB Draw.Sides (dir%, flag%, n%)
DECLARE SUB Draw.Front (dir%, flag1%, flag2%, n%)
'
SCREEN 7, , 1, 0
'
' BUILD MAP ARRAY
'
DIM maze%(100, 100)
'
CALL Generate.Maze(15, 15, maze%())
'
' SET INITIAL PLAYER POSITION
'
px% = 1: py% = 1
IF maze%(px% + 1, py%) = 1 THEN dir% = 1
IF maze%(px%, py% + 1) = 1 THEN dir% = 2
repaint% = 1
'
' MAIN LOOP LOGIC
'
DO
  IF repaint% THEN
    '
    ' REDRAW VIEW IF NEEDED
    '
    CLS
    CALL Draw.View(maze%(), px%, py%, dir%)
    PCOPY 1, 0
    repaint% = 0
  END IF
  '
  ' GET PLAYER INPUT
  '
  A$ = INKEY$
  '
  ' CHECK FOR DIRECTION CHANGE
  '
  IF A$ = "." THEN
    dir% = dir% + 1: IF dir% > 3 THEN dir% = 0
    repaint% = 1
  END IF
  '
  IF A$ = "," THEN
    dir% = dir% - 1: IF dir% < 0 THEN dir% = 3
    repaint% = 1
  END IF
  '
  ' BASED ON DIRECTION, SET MOVEMENT VARIABLES
  '
  IF dir% = 0 THEN sx% = 0: sy% = -1 ' NORTH
  IF dir% = 1 THEN sx% = 1: sy% = 0  ' EAST
  IF dir% = 2 THEN sx% = 0: sy% = 1  ' SOUTH
  IF dir% = 3 THEN sx% = -1: sy% = 0' WEST
  '
  ' MOVE FORWARD
  '
  IF A$ = "A" THEN
    px% = px% + sx%: py% = py% + sy%
  END IF
  '
  ' MOVE BACKWARD
  '
  IF A$ = "Z" THEN
    px% = px% - sx%: py% = py% - sy%
  END IF
  '
  ' CHECK FOR WALLS
  '
  err.flag% = 0
  '
  IF maze%(px%, py%) = 0 THEN
    err.flag% = 1
  END IF
  '
  IF err.flag% AND A$ = "A" THEN
    px% = px% - sx%: py% = py% - sy%
  END IF
  '
  IF err.flag% AND A$ = "Z" THEN
    px% = px% + sx%: py% = py% + sy%
  END IF
  '
  IF err.flag% = 0 THEN repaint% = 1
  '
LOOP UNTIL A$ = "Q"
'
END

SUB Draw.Back
   '
   LINE (0, 0)-(319, 99), 3, BF
   LINE (0, 100)-(319, 199), 2, BF
   '
END SUB

SUB Draw.Front (dir%, flag1%, flag2%, n%)
  '
  IF n% = 1 THEN sx% = 35: sy% = 20: psx% = 7
  IF n% = 2 THEN sx% = 90: sy% = 60: psx% = 35
  IF n% = 3 THEN sx% = 120: sy% = 80: psx% = 90
  IF n% = 4 THEN sx% = 135: sy% = 90: psx% = 120
  IF n% = 5 THEN sx% = 145: sy% = 97: psx% = 135
  '
  IF flag1% = 1 OR flag1% = 3 OR flag1% = 5 OR flag1% = 7 THEN
    IF flag2% <> 1 AND flag2% <> 3 AND flag2% <> 5 AND flag2% <> 7 THEN
      ' DRAW LEFT FRONT
      LINE (psx%, sy%)-(sx%, 199 - sy%), 7, BF
      LINE (psx%, sy%)-(sx%, 199 - sy%), 15, B
    END IF
  END IF
  '
  IF flag1% = 2 OR flag1% = 3 OR flag1% = 6 OR flag1% = 7 THEN
    IF flag2% <> 2 AND flag2% <> 3 AND flag2% <> 6 AND flag2% <> 7 THEN
      ' DRAW MIDDLE FRONT
      LINE (sx%, sy%)-(319 - sx%, 199 - sy%), 7, BF
      LINE (sx%, sy%)-(319 - sx%, 199 - sy%), 15, B
    END IF
  END IF
  '
  IF flag1% = 4 OR flag1% = 5 OR flag1% = 6 OR flag1% = 7 THEN
    IF flag2% <> 4 AND flag2% <> 5 AND flag2% <> 6 AND flag2% <> 7 THEN
      ' DRAW RIGHT FRONT
      LINE (319 - psx%, sy%)-(319 - sx%, 199 - sy%), 7, BF
      LINE (319 - psx%, sy%)-(319 - sx%, 199 - sy%), 15, B
    END IF
  END IF
  '
END SUB

SUB Draw.Sides (dir%, flag%, n%)
  '
  IF n% = 0 THEN sx% = 35: sy% = 20: psx% = 7: psy% = 0
  IF n% = 1 THEN sx% = 90: sy% = 60: psx% = 35: psy% = 20
  IF n% = 2 THEN sx% = 120: sy% = 80: psx% = 90: psy% = 60
  IF n% = 3 THEN sx% = 135: sy% = 90: psx% = 120: psy% = 80
  IF n% = 4 THEN sx% = 145: sy% = 97: psx% = 135: psy% = 90
  '
  IF flag% = 1 OR flag% = 5 THEN
    ' DRAW LEFT SIDE
    LINE (psx%, psy%)-(sx%, sy%), 15
    LINE -(sx%, 199 - sy%), 15
    LINE -(psx%, 199 - psy%), 15
    LINE -(psx%, psy%), 15
    PAINT (psx% + 5, 100), 7, 15
  END IF
  '
  IF flag% = 4 OR flag% = 5 THEN
    ' DRAW RIGHT SIDE
    LINE (319 - psx%, psy%)-(319 - sx%, sy%), 15
    LINE -(319 - sx%, 199 - sy%), 15
    LINE -(319 - psx%, 199 - psy%), 15
    LINE -(319 - psx%, psy%), 15
    PAINT (319 - psx% - 5, 100), 7, 15
  END IF
  '
END SUB

SUB Draw.View (map%(), px%, py%, dir%)
  '
  CALL Draw.Back
  '
  COLOR 15, 3: LOCATE 2, 3: PRINT px%; ","; py%
  '
  n% = 0
  '
  CALL Get.Map.Sq(map%(), px%, py%, dir%, flag%)
  '
  CALL Draw.Sides(dir%, flag%, n%)
  '
  ppx% = px%: ppy% = py%
  '
  DO
    '
    n% = n% + 1
    '
    IF dir% = 0 THEN sx% = 0: sy% = -1
    IF dir% = 1 THEN sx% = 1: sy% = 0
    IF dir% = 2 THEN sx% = 0: sy% = 1
    IF dir% = 3 THEN sx% = -1: sy% = 0
    '
    ppx% = ppx% + sx%: ppy% = ppy% + sy%
    '
    CALL Get.Map.Sq(map%(), ppx%, ppy%, dir%, flag1%)
    CALL Draw.Sides(dir%, flag1%, n%)
    '
    CALL Get.Map.Sq(map%(), ppx% - sx%, ppy% - sy%, dir%, flag2%)
    CALL Draw.Front(dir%, flag1%, flag2%, n%)
    '
  LOOP UNTIL (flag1% = 2 OR flag1% = 3 OR flag1% = 6 OR flag1% = 7 OR n% = 4)
  '
  LINE (0, 0)-(7, 199), 0, BF
  LINE (312, 0)-(319, 199), 0, BF
  '
  CIRCLE (295, 17), 10, 15
  '
  IF dir% = 0 THEN LINE (295, 17)-(295, 9), 15
  IF dir% = 1 THEN LINE (295, 17)-(305, 17), 15
  IF dir% = 2 THEN LINE (295, 17)-(295, 25), 15
  IF dir% = 3 THEN LINE (295, 17)-(285, 17), 15
  '
END SUB

SUB Generate.Maze (Maxx%, Maxy%, maze%())
  '
  NumF% = (Maxx% * Maxy% * 2): n% = 0
  '
  DIM map%(Maxx%, Maxy%), fx%(NumF%), fy%(NumF%)
  '
  FOR y% = 1 TO Maxy%
    FOR x% = 1 TO Maxx%
      map%(x%, y%) = 0
    NEXT
  NEXT
  '
  RANDOMIZE TIMER
  '
  sx% = INT(RND * Maxx%) + 1
  sy% = INT(RND * Maxy%) + 1
  map%(sx%, sy%) = 100
  '
  done% = 0: x% = sx%: y% = sy%
  '
  DO
    '
    ' Add Frontier Cell to Spanning Tree
    '
    IF x% + 1 <= Maxx% THEN
      IF map%(x% + 1, y%) = 0 THEN
        n% = n% + 1
        fx%(n%) = x% + 1: fy%(n%) = y%
        map%(x% + 1, y%) = -1
      END IF
    END IF
    '
    IF y% + 1 <= Maxy% THEN
      IF map%(x%, y% + 1) = 0 THEN
        n% = n% + 1
        fx%(n%) = x%: fy%(n%) = y% + 1
        map%(x%, y% + 1) = -1
      END IF
    END IF
    '
    IF x% - 1 >= 1 THEN
      IF map%(x% - 1, y%) = 0 THEN
        n% = n% + 1
        fx%(n%) = x% - 1: fy%(n%) = y%
        map%(x% - 1, y%) = -1
      END IF
    END IF
    '
    IF y% - 1 >= 1 THEN
      IF map%(x%, y% - 1) = 0 THEN
        n% = n% + 1
        fx%(n%) = x%: fy%(n%) = y% - 1
        map%(x%, y% - 1) = -1
      END IF
    END IF
    '
    ' Get Frontier Cell
    '
    k% = INT(RND * n%) + 1
    x% = fx%(k%): y% = fy%(k%)
    fx%(k%) = fx%(n%): fy%(k%) = fy%(n%)
    n% = n% - 1: IF n% = 0 THEN done% = 1 ' No more frontier cells, end.
    '
    ' Connect Frontier to a Cell in the Spanning Tree
    '
    dne% = 0
    '
    DO
      dir% = INT(RND * 4)
      '
      IF dir% = 0 AND x% + 1 <= Maxx% THEN
        IF map%(x% + 1, y%) > 0 THEN dne% = 1
      END IF
      '
      IF dir% = 1 AND y% + 1 <= Maxy% THEN
        IF map%(x%, y% + 1) > 0 THEN dne% = 1
      END IF
      '
      IF dir% = 2 AND x% - 1 >= 1 THEN
        IF map%(x% - 1, y%) > 0 THEN dne% = 1
      END IF
      '
      IF dir% = 3 AND y% - 1 >= 1 THEN
        IF map%(x%, y% - 1) > 0 THEN dne% = 1
      END IF
      '
    LOOP UNTIL dne%
    '
    map%(x%, y%) = 0
    '
    SELECT CASE dir%
      CASE 0
        map%(x%, y%) = map%(x%, y%) + 1
        map%(x% + 1, y%) = map%(x% + 1, y%) + 4
      CASE 1
        map%(x%, y%) = map%(x%, y%) + 2
        map%(x%, y% + 1) = map%(x%, y% + 1) + 8
      CASE 2
        map%(x%, y%) = map%(x%, y%) + 4
        map%(x% - 1, y%) = map%(x% - 1, y%) + 1
      CASE 3
        map%(x%, y%) = map%(x%, y%) + 8
        map%(x%, y% - 1) = map%(x%, y% - 1) + 2
    END SELECT
    '
  LOOP UNTIL done%
  '
  map%(sx%, sy%) = map%(sx%, sy%) - 100
  '
  ' Build the maze into the maze array
  '
  FOR y% = 0 TO (Maxy% - 1) * 2
    FOR x% = 0 TO (Maxx% - 1) * 2
      maze%(x%, y%) = 0
    NEXT
  NEXT
  '
  ' Put in "open" cells
  '
  FOR y% = 0 TO (Maxy% - 1) * 2 STEP 2
    FOR x% = 0 TO (Maxx% - 1) * 2 STEP 2
      maze%(x% + 1, y% + 1) = 1
    NEXT
  NEXT
  '
  ' Connect cells based on open walls between them, as defined in
  ' the generated map%() array.
  '
  my% = 0
  FOR y% = 0 TO (Maxy% - 1) * 2 STEP 2
    my% = my% + 1
    mx% = 0
    FOR x% = 0 TO (Maxx% - 1) * 2 STEP 2
      mx% = mx% + 1
      IF map%(mx%, my%) >= 8 THEN maze%(x% + 1, y%) = 1: map%(mx%, my%) = map%(mx%, my%) - 8
      IF map%(mx%, my%) >= 4 THEN maze%(x%, y% + 1) = 1: map%(mx%, my%) = map%(mx%, my%) - 4
      IF map%(mx%, my%) >= 2 THEN maze%(x% + 1, y% + 2) = 1: map%(mx%, my%) = map%(mx%, my%) - 2
      IF map%(mx%, my%) >= 1 THEN maze%(x% + 2, y% + 1) = 1: map%(mx%, my%) = map%(mx%, my%) - 1
    NEXT
  NEXT
  '
  ' Set up start and end cells
  '
  maze%(1, 1) = 2 ' Start
  maze%(((Maxx% - 1) * 2) + 1, ((Maxy% - 1) * 2) + 1) = 3 ' End
  '
END SUB

SUB Get.Map.Sq (map%(), px%, py%, dir%, flag%)
  '
  flag% = 0
  '
  IF dir% = 0 THEN sx% = -1: sy% = 0
  IF dir% = 1 THEN sx% = 0: sy% = -1
  IF dir% = 2 THEN sx% = 1: sy% = 0
  IF dir% = 3 THEN sx% = 0: sy% = 1
  '
  IF (px% + sx% >= 0 AND px% + sx% <= 100) AND (py% + sy% >= 0 AND py% + sy% <= 100) THEN
    IF map%(px% + sx%, py% + sy%) = 0 THEN flag% = flag% + 1
  END IF
  '
  IF (px% >= 0 AND px% <= 100) AND (py% >= 0 AND py% <= 100) THEN
    IF map%(px%, py%) = 0 THEN flag% = flag% + 2
  END IF
  '
  IF (px% - sx% >= 0 AND px% - sx% <= 100) AND (py% - sy% >= 0 AND py% - sy% <= 100) THEN
    IF map%(px% - sx%, py% - sy%) = 0 THEN flag% = flag% + 4
  END IF
  '
END SUB

