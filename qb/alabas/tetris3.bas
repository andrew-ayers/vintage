DECLARE SUB ResetAll ()
DECLARE SUB UpdateScore (Amount!)
DECLARE SUB PlayerDead ()
DECLARE SUB SetupScreen ()
'
COMMON SHARED Score, Speed, Table()
'
' TBrick defined as follows:
'
'       TBrick(Shape #, Rotation #)
'
'       Member data defined as follows:
'
'               Shape - Variable length string which holds shape descriptor
'               Wid   - Integer variable to hold the shape descriptor width
'               Hgt   - Integer variable to hold the shape descriptor height
'
TYPE TBrick
  Shape AS STRING * 16
  Wid AS INTEGER
  Hgt AS INTEGER
END TYPE
'
DIM TBrick(6, 3), MaxRot(6)
DIM Table(79, 23)
'
'***********************************************
'* Define all brick shapes and rotations first *
'***********************************************
'
' Shape 1
'
TBrick.Shape$(0, 0) = "1111"
TBrick.Wid(0, 0) = 2
TBrick.Hgt(0, 0) = 2
MaxRot(0) = 0
'
' Shape 2
'
TBrick.Shape$(1, 0) = "2222": TBrick.Shape$(1, 1) = "2222"
TBrick.Wid(1, 0) = 1:        TBrick.Wid(1, 1) = 4
TBrick.Hgt(1, 0) = 4:        TBrick.Hgt(1, 1) = 1
MaxRot(1) = 1
'
' Shape 3
'
TBrick.Shape$(2, 0) = "303303": TBrick.Shape$(2, 1) = "033330"
TBrick.Wid(2, 0) = 2:          TBrick.Wid(2, 1) = 3
TBrick.Hgt(2, 0) = 3:          TBrick.Hgt(2, 1) = 2
MaxRot(2) = 1
'
' Shape 4
'
TBrick.Shape$(3, 0) = "044440": TBrick.Shape$(3, 1) = "440044"
TBrick.Wid(3, 0) = 2:          TBrick.Wid(3, 1) = 3
TBrick.Hgt(3, 0) = 3:          TBrick.Hgt(3, 1) = 2
MaxRot(3) = 1
'
' Shape 5
'
TBrick.Shape$(4, 0) = "050555": TBrick.Shape$(4, 1) = "505550": TBrick.Shape$(4, 2) = "555050": TBrick.Shape$(4, 3) = "055505"
TBrick.Wid(4, 0) = 3:          TBrick.Wid(4, 1) = 2:          TBrick.Wid(4, 2) = 3:          TBrick.Wid(4, 3) = 2
TBrick.Hgt(4, 0) = 2:          TBrick.Hgt(4, 1) = 3:          TBrick.Hgt(4, 2) = 2:          TBrick.Hgt(4, 3) = 3
MaxRot(4) = 3
'
' Shape 6
'
TBrick.Shape$(5, 0) = "606066": TBrick.Shape$(5, 1) = "666600": TBrick.Shape$(5, 2) = "660606": TBrick.Shape$(5, 3) = "006666"
TBrick.Wid(5, 0) = 2:          TBrick.Wid(5, 1) = 3:          TBrick.Wid(5, 2) = 2:          TBrick.Wid(5, 3) = 3
TBrick.Hgt(5, 0) = 3:          TBrick.Hgt(5, 1) = 2:          TBrick.Hgt(5, 2) = 3:          TBrick.Hgt(5, 3) = 2
MaxRot(5) = 3
'
' Shape 7
'
TBrick.Shape$(6, 0) = "070777": TBrick.Shape$(6, 1) = "700777": TBrick.Shape$(6, 2) = "777070": TBrick.Shape$(6, 3) = "777007"
TBrick.Wid(6, 0) = 2:          TBrick.Wid(6, 1) = 3:          TBrick.Wid(6, 2) = 2:          TBrick.Wid(6, 3) = 3
TBrick.Hgt(6, 0) = 3:          TBrick.Hgt(6, 1) = 2:          TBrick.Hgt(6, 2) = 3:          TBrick.Hgt(6, 3) = 2
MaxRot(6) = 3
'
'****************************
'* Now begin main game loop *
'****************************
'
CALL SetupScreen
'
DO
  '
  ' Pick a new shape to drop
  '
  HPos = 19: VPos = 0: Rot = 0
  OldH = 19: OldV = 0: OlsRot = 0
  ShapeNum = INT(RND * 7)
  '
  DO
    '
    IF Speed > 1 THEN
      FOR Delay = 1 TO Speed
        a$ = INKEY$
        IF a$ = "," THEN
          HPos = HPos - 1
          GOSUB CollisionCheck
          IF Hit = 1 OR HPos < 13 THEN
            HPos = HPos + 1
          ELSE
            GOSUB EraseShape
            GOSUB DrawShape
          END IF
        END IF
        '
        IF a$ = "." THEN
          HPos = HPos + 1
          GOSUB CollisionCheck
          IF Hit = 1 OR HPos + TBrick.Wid(ShapeNum, Rot) > 28 THEN
            HPos = HPos - 1
          ELSE
            GOSUB EraseShape
            GOSUB DrawShape
          END IF
        END IF
        '
        IF a$ = "A" OR a$ = "a" THEN
          Rot = Rot + 1
          IF Rot > MaxRot(ShapeNum) THEN Rot = 0
          GOSUB CollisionCheck
          IF Hit = 1 THEN
            Rot = Rot - 1: IF Rot < 0 THEN Rot = MaxRot(ShapeNum)
          ELSE
            GOSUB EraseShape
            GOSUB DrawShape
          END IF
        END IF
        '
        IF a$ = "Z" OR a$ = "z" THEN
          Speed = 1: EXIT FOR
        END IF
      NEXT Delay
    END IF
    '
    VPos = VPos + 1
    GOSUB CollisionCheck
    IF Hit THEN
      VPos = VPos - 1
    ELSE
      GOSUB EraseShape
      GOSUB DrawShape
    END IF
    '
  LOOP UNTIL Hit
  '
  IF VPos > 1 THEN
    CALL UpdateScore(10)
    '
    ' Shape has moved down as far as possible, so put it into table
    '
    FOR y = 0 TO TBrick.Hgt(ShapeNum, Rot) - 1
      FOR x = 0 TO TBrick.Wid(ShapeNum, Rot) - 1
        Col = VAL(MID$(TBrick.Shape$(ShapeNum, Rot), y * TBrick.Wid(ShapeNum, Rot) + x + 1, 1))
        IF Col <> 0 THEN
          Table(HPos + x, VPos + y) = Col
        END IF
      NEXT x
    NEXT y
    '
    ' Check table for lines to remove
    '
    Bonus = 0
    FOR yy = 22 TO 1 STEP -1
      flag = 1
      FOR xx = 13 TO 27
        IF Table(xx, yy) = 0 THEN flag = 0: EXIT FOR
      NEXT xx
      IF flag = 1 THEN
        '
        ' Yeah! We completed a line, shift everything above down
        '
        FOR yy2 = yy - 1 TO 1 STEP -1
          FOR xx2 = 13 TO 27
            Table(xx2, yy2 + 1) = Table(xx2, yy2)
            LOCATE yy2 + 2, xx2 * 2, 0
            Col = Table(xx2, yy2 + 1)
            IF Col = 0 THEN
              PRINT "  "
            ELSE
              COLOR Col
              PRINT "ÛÛ"
            END IF
          NEXT xx2
        NEXT yy2
        yy = yy + 1
        Bonus = Bonus + 1: IF Bonus = 4 THEN EXIT FOR
        CALL UpdateScore(100)
      END IF
    NEXT yy
    IF Bonus > 1 THEN CALL UpdateScore(Bonus * 250)
  ELSE
    CALL PlayerDead
    CALL ResetAll
    CALL SetupScreen
  END IF
LOOP

EraseShape:
  '
  ' Erase Shape
  '
  FOR y = 0 TO TBrick.Hgt(ShapeNum, OldRot) - 1
    FOR x = 0 TO TBrick.Wid(ShapeNum, OldRot) - 1
      LOCATE OldV + y + 1, (OldH + x) * 2, 0
      Col = VAL(MID$(TBrick.Shape$(ShapeNum, OldRot), y * TBrick.Wid(ShapeNum, OldRot) + x + 1, 1))
      IF Col <> 0 THEN PRINT "  "
    NEXT x
  NEXT y
  '
RETURN

DrawShape:
  '
  ' Draw Shape
  '
  FOR y = 0 TO TBrick.Hgt(ShapeNum, Rot) - 1
    FOR x = 0 TO TBrick.Wid(ShapeNum, Rot) - 1
      LOCATE VPos + y + 1, (HPos + x) * 2, 0
      Col = VAL(MID$(TBrick.Shape$(ShapeNum, Rot), y * TBrick.Wid(ShapeNum, Rot) + x + 1, 1))
      IF Col <> 0 THEN
        COLOR Col
        PRINT "ÛÛ"
      END IF
    NEXT x
  NEXT y
  '
  OldH = HPos: OldV = VPos: OldRot = Rot
  '
RETURN

CollisionCheck:  
  '
  Hit = 0
  '
  ' Do collision checks
  '
  IF VPos + TBrick.Hgt(ShapeNum, Rot) = 23 THEN
    Hit = 1
  ELSE
    IF HPos + TBrick.Wid(ShapeNum, Rot) > 28 THEN Hit = 1
    FOR y = 0 TO TBrick.Hgt(ShapeNum, Rot) - 1
      FOR x = 0 TO TBrick.Wid(ShapeNum, Rot) - 1
        IF VAL(MID$(TBrick.Shape$(ShapeNum, Rot), y * TBrick.Wid(ShapeNum, Rot) + x + 1, 1)) <> 0 THEN
          IF Table(HPos + x, VPos + y) <> 0 THEN Hit = 1
        END IF
      NEXT x
    NEXT y
  END IF
  '
RETURN

SUB PlayerDead
  '
  COLOR 15: LOCATE 11, 18: PRINT "ษออออออออออออออออออออออออออออออออออออออออออป"
  COLOR 15: LOCATE 12, 18: PRINT "บ You Lose! - Enter [RETURN] to play again บ"
  COLOR 15: LOCATE 13, 18: PRINT "ศออออออออออออออออออออออออออออออออออออออออออผ"
  '
  DO: LOOP WHILE INKEY$ = ""
  '
END SUB

SUB ResetAll
  '
  FOR y = 0 TO 23
    FOR x = 0 TO 79
      Table(x, y) = 0
    NEXT x
  NEXT y
  '
  Score = 0
  '
END SUB

SUB SetupScreen
  '
  ' Prepare Screen
  '
  SCREEN 0: WIDTH 80: CLS : COLOR 15
  '
  ' Draw Sides
  '
  FOR t = 1 TO 23
    LOCATE t, 12 * 2, 0: PRINT "ÛÛ"
    LOCATE t, 28 * 2, 0: PRINT "ÛÛ"
  NEXT t
  '
  ' Draw Bottom
  '
  FOR t = 12 TO 28
    LOCATE 23, t * 2, 0: PRINT "ÛÛ"
  NEXT t
  '
  CALL UpdateScore(0)
  '
END SUB

SUB UpdateScore (Amount)
  '
  Score = Score + Amount
  '
  IF Score < 1000 THEN Speed = 1500
  IF Score >= 1000 AND Score < 2000 THEN Speed = 1400
  IF Score >= 2000 AND Score < 3000 THEN Speed = 1300
  IF Score >= 3000 AND Score < 4000 THEN Speed = 1200
  IF Score >= 4000 AND Score < 5000 THEN Speed = 1100
  IF Score >= 5000 AND Score < 6000 THEN Speed = 1000
  IF Score >= 6000 AND Score < 7000 THEN Speed = 900
  IF Score >= 7000 AND Score < 8000 THEN Speed = 800
  IF Score >= 8000 AND Score < 9000 THEN Speed = 700
  IF Score >= 9000 AND Score < 10000 THEN Speed = 600
  IF Score >= 10000 AND Score < 11000 THEN Speed = 500
  IF Score >= 11000 AND Score < 12000 THEN Speed = 400
  IF Score >= 12000 AND Score < 13000 THEN Speed = 300
  IF Score >= 13000 AND Score < 14000 THEN Speed = 200
  IF Score >= 14000 AND Score < 15000 THEN Speed = 100
  '
  COLOR 15: LOCATE 1, 1, 0: PRINT "Score :" + STR$(Score)
  COLOR 15: LOCATE 3, 1, 0: PRINT "Level :" + STR$(16 - (Speed / 100))
  '
END SUB

