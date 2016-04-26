DECLARE SUB PsychoPrint (x%, y%, strg$, fclr%, bclr%, range1%, range2%, factor%, special%)
DECLARE SUB DrawBox (x1%, y1%, x2%, y2%)
DECLARE SUB SteelPrint (x%, y%, text$)
DECLARE SUB ResetAll ()
DECLARE SUB UpdateScore (Amount&)
DECLARE SUB PlayerDead ()
DECLARE SUB SetupScreen ()
'
COMMON SHARED Score&, Speed%, Table%()
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
DIM TBrick(6, 3), MaxRot%(6)
DIM Table%(79, 23)
'
'***********************************************
'* Define all brick shapes and rotations first *
'***********************************************
'
' Shape 1
'
TBrick.Shape$(0, 0) = "7777"
TBrick.Wid(0, 0) = 2
TBrick.Hgt(0, 0) = 2
MaxRot%(0) = 0
'
' Shape 2
'
TBrick.Shape$(1, 0) = "9999": TBrick.Shape$(1, 1) = "9999"
TBrick.Wid(1, 0) = 1:        TBrick.Wid(1, 1) = 4
TBrick.Hgt(1, 0) = 4:        TBrick.Hgt(1, 1) = 1
MaxRot%(1) = 1
'
' Shape 3
'
TBrick.Shape$(2, 0) = "A0AA0A": TBrick.Shape$(2, 1) = "0AAAA0"
TBrick.Wid(2, 0) = 2:          TBrick.Wid(2, 1) = 3
TBrick.Hgt(2, 0) = 3:          TBrick.Hgt(2, 1) = 2
MaxRot%(2) = 1
'
' Shape 4
'
TBrick.Shape$(3, 0) = "0BBBB0": TBrick.Shape$(3, 1) = "BB00BB"
TBrick.Wid(3, 0) = 2:          TBrick.Wid(3, 1) = 3
TBrick.Hgt(3, 0) = 3:          TBrick.Hgt(3, 1) = 2
MaxRot%(3) = 1
'
' Shape 5
'
TBrick.Shape$(4, 0) = "0C0CCC": TBrick.Shape$(4, 1) = "C0CCC0": TBrick.Shape$(4, 2) = "CCC0C0": TBrick.Shape$(4, 3) = "0CCC0C"
TBrick.Wid(4, 0) = 3:          TBrick.Wid(4, 1) = 2:          TBrick.Wid(4, 2) = 3:          TBrick.Wid(4, 3) = 2
TBrick.Hgt(4, 0) = 2:          TBrick.Hgt(4, 1) = 3:          TBrick.Hgt(4, 2) = 2:          TBrick.Hgt(4, 3) = 3
MaxRot%(4) = 3
'
' Shape 6
'
TBrick.Shape$(5, 0) = "D0D0DD": TBrick.Shape$(5, 1) = "DDDD00": TBrick.Shape$(5, 2) = "DD0D0D": TBrick.Shape$(5, 3) = "00DDDD"
TBrick.Wid(5, 0) = 2:          TBrick.Wid(5, 1) = 3:          TBrick.Wid(5, 2) = 2:          TBrick.Wid(5, 3) = 3
TBrick.Hgt(5, 0) = 3:          TBrick.Hgt(5, 1) = 2:          TBrick.Hgt(5, 2) = 3:          TBrick.Hgt(5, 3) = 2
MaxRot%(5) = 3
'
' Shape 7
'
TBrick.Shape$(6, 0) = "0E0EEE": TBrick.Shape$(6, 1) = "E00EEE": TBrick.Shape$(6, 2) = "EEE0E0": TBrick.Shape$(6, 3) = "EEE00E"
TBrick.Wid(6, 0) = 2:          TBrick.Wid(6, 1) = 3:          TBrick.Wid(6, 2) = 2:          TBrick.Wid(6, 3) = 3
TBrick.Hgt(6, 0) = 3:          TBrick.Hgt(6, 1) = 2:          TBrick.Hgt(6, 2) = 3:          TBrick.Hgt(6, 3) = 2
MaxRot%(6) = 3
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
  HPos% = 7: VPos% = 0: Rot% = 0
  OldH% = 7: OldV% = 0: OlsRot% = 0
  ShapeNum% = INT(RND * 7)
  '
  DO
    '
    IF Speed% > 1 THEN
      FOR Delay% = 1 TO Speed%
        a$ = INKEY$
        IF a$ = "," THEN
          HPos% = HPos% - 1
          GOSUB CollisionCheck
          IF Hit% = 1 OR HPos% < 1 THEN
            HPos% = HPos% + 1
          ELSE
            GOSUB EraseShape
            GOSUB DrawShape
          END IF
        END IF
        '
        IF a$ = "." THEN
          HPos% = HPos% + 1
          GOSUB CollisionCheck
          IF Hit% = 1 OR HPos% + TBrick.Wid(ShapeNum%, Rot%) > 28 THEN
            HPos% = HPos% - 1
          ELSE
            GOSUB EraseShape
            GOSUB DrawShape
          END IF
        END IF
        '
        IF a$ = "A" OR a$ = "a" THEN
          Rot% = Rot% + 1
          IF Rot% > MaxRot%(ShapeNum%) THEN Rot% = 0
          GOSUB CollisionCheck
          IF Hit% = 1 THEN
            Rot% = Rot% - 1: IF Rot% < 0 THEN Rot% = MaxRot%(ShapeNum%)
          ELSE
            GOSUB EraseShape
            GOSUB DrawShape
          END IF
        END IF
        '
        IF a$ = "Z" OR a$ = "z" THEN
          Speed% = 1: EXIT FOR
        END IF
      NEXT
    END IF
    '
    VPos% = VPos% + 1
    GOSUB CollisionCheck
    IF Hit% THEN
      VPos% = VPos% - 1
    ELSE
      GOSUB EraseShape
      GOSUB DrawShape
    END IF
    '
  LOOP UNTIL Hit%
  '
  IF VPos% > 1 THEN
    CALL UpdateScore(10)
    '
    ' Shape has moved down as far as possible, so put it into table
    '
    FOR y% = 0 TO TBrick.Hgt(ShapeNum%, Rot%) - 1
      FOR x% = 0 TO TBrick.Wid(ShapeNum%, Rot%) - 1
        Col% = VAL("&H" + MID$(TBrick.Shape$(ShapeNum%, Rot%), y% * TBrick.Wid(ShapeNum%, Rot%) + x% + 1, 1))
        IF Col% <> 0 THEN
          Table%(HPos% + x%, VPos% + y%) = Col%
        END IF
      NEXT
    NEXT
    '
    ' Check table for lines to remove
    '
    Bonus% = 0
    FOR yy% = 22 TO 1 STEP -1
      flag% = 1
      FOR xx% = 1 TO 15
        IF Table%(xx%, yy%) = 0 THEN flag% = 0: EXIT FOR
      NEXT
      IF flag% = 1 THEN
        '
        ' Yeah! We completed a line, shift everything above down
        '
        FOR yy2% = yy% - 1 TO 1 STEP -1
          FOR xx2% = 1 TO 15
            Table%(xx2%, yy2% + 1) = Table%(xx2%, yy2%)
            Col% = Table%(xx2%, yy2% + 1)
            LINE (xx2% * 8, (yy2% + 2) * 8)-(xx2% * 8 + 7, (yy2% + 2) * 8 + 7), Col%, BF
          NEXT
        NEXT
        yy% = yy% + 1
        Bonus% = Bonus% + 1: IF Bonus% = 4 THEN EXIT FOR
        CALL UpdateScore(100)
      END IF
    NEXT yy%
    IF Bonus% > 1 THEN CALL UpdateScore(Bonus% * 250)
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
  FOR y% = 0 TO TBrick.Hgt(ShapeNum%, OldRot%) - 1
    FOR x% = 0 TO TBrick.Wid(ShapeNum%, OldRot%) - 1
      Col% = VAL("&H" + MID$(TBrick.Shape$(ShapeNum%, OldRot%), y% * TBrick.Wid(ShapeNum%, OldRot%) + x% + 1, 1))
      IF Col% <> 0 THEN
        LINE ((OldH% + x%) * 8, (OldV% + y% + 1) * 8)-((OldH% + x%) * 8 + 7, (OldV% + y% + 1) * 8 + 7), 0, BF
      END IF
    NEXT
  NEXT
  '
RETURN

DrawShape:
  '
  ' Draw Shape
  '
  FOR y% = 0 TO TBrick.Hgt(ShapeNum%, Rot%) - 1
    FOR x% = 0 TO TBrick.Wid(ShapeNum%, Rot%) - 1
      Col% = VAL("&H" + MID$(TBrick.Shape$(ShapeNum%, Rot%), y% * TBrick.Wid(ShapeNum%, Rot%) + x% + 1, 1))
      IF Col% <> 0 THEN
        LINE ((HPos% + x%) * 8, (VPos% + y% + 1) * 8)-((HPos% + x%) * 8 + 7, (VPos% + y% + 1) * 8 + 7), Col%, BF
      END IF
    NEXT
  NEXT
  '
  OldH% = HPos%: OldV% = VPos%: OldRot% = Rot%
  '
RETURN

CollisionCheck:
  '
  Hit% = 0
  '
  ' Do collision checks
  '
  IF VPos% + TBrick.Hgt(ShapeNum%, Rot%) = 23 THEN
    Hit% = 1
  ELSE
    IF HPos% + TBrick.Wid(ShapeNum%, Rot%) > 16 THEN Hit% = 1
    FOR y% = 0 TO TBrick.Hgt(ShapeNum%, Rot%) - 1
      FOR x% = 0 TO TBrick.Wid(ShapeNum%, Rot%) - 1
        IF VAL("&H" + MID$(TBrick.Shape$(ShapeNum%, Rot%), y% * TBrick.Wid(ShapeNum%, Rot%) + x% + 1, 1)) <> 0 THEN
          IF Table%(HPos% + x%, VPos% + y%) <> 0 THEN Hit% = 1
        END IF
      NEXT
    NEXT
  END IF
  '
RETURN

SUB DrawBox (x1%, y1%, x2%, y2%)
  '
  FOR t% = 0 TO 6
    LINE (x1% * 8 + t%, y1% * 8 + t%)-(x2% * 8 - t%, y2% * 8 - t%), 30 - (t% * 2), B
  NEXT t%
  '
  LINE (x1% * 8 + t%, y1% * 8 + t%)-(x2% * 8 - t%, y2% * 8 - t%), 0, BF
  '
END SUB

SUB PlayerDead
  '
  CALL DrawBox(5, 9, 35, 14)
  '
  CALL SteelPrint(17, 11, "Game Over")
  CALL SteelPrint(7, 13, "Enter [RETURN] to play again")
  '
  DO
    '
    special% = 1
    '
    CALL PsychoPrint(17, 11, "Game Over", 3, 0, 0, 15, 4, special%)
    '
    t! = TIMER: DO: LOOP UNTIL TIMER - t! > .1
    '
  LOOP UNTIL special% = 999 OR INKEY$ = CHR$(13)
  '
END SUB

SUB PsychoPrint (x%, y%, strg$, fclr%, bclr%, range1%, range2%, factor%, special%)
  '
  STATIC FirstTime AS INTEGER
  STATIC colr AS INTEGER
  '
  IF strg$ = "" THEN FirstTime% = 0: EXIT SUB
  '
  xpos% = x% * 8 - 8: ypos% = y% * 8 - 8
  xend% = xpos% + (LEN(strg$) * 8): yend% = ypos% + 8
  '
  IF FirstTime% = 0 THEN
    COLOR 255: LOCATE y%, x%: PRINT strg$: FirstTime% = 1
    COLOR 15
    colr% = fclr%
    FOR y% = ypos% TO yend%
      FOR x% = xpos% TO xend%
        IF POINT(x%, y%) <> 255 THEN
          PSET (x%, y%), bclr%
        ELSE
          PSET (x%, y%), fclr%
        END IF
      NEXT x%
    NEXT y%
  END IF
  '
  '***********************************************************
  '
  flag% = 999
  '
  FOR y% = ypos% TO yend%
    FOR x% = xpos% TO xend%
      IF POINT(x%, y%) <> bclr% THEN
        flag% = 0
        PSET (x%, y%), colr%
        '
        SELECT CASE special%
          CASE 3 ' Regular Fade
            IF INT(RND * 2) = 1 THEN
              colr% = bclr%
            ELSE
              colr% = fclr%
            END IF
          CASE 4 ' Psycho Snow
            colr% = INT(RND * factor%)
            IF colr% = bclr% THEN colr% = colr% + 1
          CASE 5 ' Psycho Snow Fade
            colr% = INT(RND * factor%)
        END SELECT
        '
      END IF
      '
      SELECT CASE special%
        CASE 1 ' Psycho Cycle
          colr% = colr% + factor%
          IF colr% = bclr% THEN colr% = colr% + 1
          IF colr% >= range2% THEN colr% = range1%
          IF colr% = bclr% THEN colr% = colr% + 1
        CASE 2 ' Psycho Fade
          colr% = colr% + 1
          IF colr% > range2% THEN colr% = range1%
      END SELECT
      '
    NEXT x%
    '
    SELECT CASE special%
      CASE 6 ' Psycho Rainbow
        colr% = colr% + factor%
        IF colr% = bclr% THEN colr% = colr% + 1
        IF colr% >= range2% THEN colr% = range1%
        IF colr% = bclr% THEN colr% = colr% + 1
      CASE 7 ' Psycho Rainbow Fade
        colr% = colr% + 1
        IF colr% > range2% THEN colr% = range1%
      CASE 8 ' Regular Line Fade
        IF INT(RND * 2) = 1 THEN
          colr% = bclr%
        ELSE
          colr% = fclr%
        END IF
      CASE 9 ' Psycho Line Snow
        colr% = INT(RND * factor%)
        IF colr% = bclr% THEN colr% = colr% + 1
      CASE 10 ' Psycho Line Snow Fade
        colr% = INT(RND * factor%)
    END SELECT
    '
  NEXT y%
  '
  'FOR dlay = 1 TO 10000: NEXT dlay' Adjust this to your computer
  '
  special% = flag%
  '
END SUB

SUB ResetAll
  '
  FOR y% = 0 TO 23
    FOR x% = 0 TO 79
      Table%(x%, y%) = 0
    NEXT
  NEXT
  '
  Score& = 0
  '
END SUB

SUB SetupScreen
  '
  ' Prepare Screen
  '
  SCREEN 13: CLS 0: RANDOMIZE TIMER
  '
  ' Draw Sides
  '
  FOR x% = 0 TO 7
    LINE (0 * 8 + x%, 8)-(0 * 8 + x%, 23 * 8 + 7), 16 + (x% * 2)
    LINE (16 * 8 + x%, 8)-(16 * 8 + x%, 23 * 8 + 7), 30 - (x% * 2)
  NEXT
  '
  ' Draw Bottom
  '
  FOR y% = 0 TO 7
    LINE (0 * 8 + (8 - y%), 23 * 8 + y%)-(16 * 8 + 7 - (8 - y%), 23 * 8 + y%), 30 - (y% * 2)
  NEXT y%
  '
  'FOR t% = 0 TO 6
  '  LINE (18 * 8 + t%, 1 * 8 + t%)-(39 * 8 - t%, 6 * 8 - t%), 30 - (t% * 2), B
  'NEXT t%
  CALL DrawBox(18, 1, 39, 6)
  '
  CALL UpdateScore(0)
  '
END SUB

SUB SteelPrint (x%, y%, text$)
  '
  starty% = (y% * 8) - 4
  endy% = (y% * 8) - 9
  startx% = ((x% - 1) * 8)
  endx% = ((x% - 1) * 8) + (LEN(text$) * 8)
  colr% = 32
  '
  COLOR 255: LOCATE y%, x%: PRINT text$
  '
  FOR y1% = starty% TO endy% STEP -1
    y2% = (starty% - 1) + ((starty% - 1) - y1%)
    FOR x% = startx% TO endx%
      IF POINT(x%, y1%) THEN PSET (x%, y1%), colr%
      IF POINT(x%, y2%) THEN PSET (x%, y2%), colr%
    NEXT x%
    colr% = colr% - 2
  NEXT y1%
  '
END SUB

SUB UpdateScore (Amount&)
  '
  Score& = Score& + Amount&
  '
  IF Score& < 1000 THEN Speed% = 1500
  IF Score& >= 1000 AND Score& < 2000 THEN Speed% = 1400
  IF Score& >= 2000 AND Score& < 3000 THEN Speed% = 1300
  IF Score& >= 3000 AND Score& < 4000 THEN Speed% = 1200
  IF Score& >= 4000 AND Score& < 5000 THEN Speed% = 1100
  IF Score& >= 5000 AND Score& < 6000 THEN Speed% = 1000
  IF Score& >= 6000 AND Score& < 7000 THEN Speed% = 900
  IF Score& >= 7000 AND Score& < 8000 THEN Speed% = 800
  IF Score& >= 8000 AND Score& < 9000 THEN Speed% = 700
  IF Score& >= 9000 AND Score& < 10000 THEN Speed% = 600
  IF Score& >= 10000 AND Score& < 11000 THEN Speed% = 500
  IF Score& >= 11000 AND Score& < 12000 THEN Speed% = 400
  IF Score& >= 12000 AND Score& < 13000 THEN Speed% = 300
  IF Score& >= 13000 AND Score& < 14000 THEN Speed% = 200
  IF Score& >= 14000 AND Score& < 15000 THEN Speed% = 100
  '
  CALL SteelPrint(21, 3, "Score :" + STR$(Score&))
  CALL SteelPrint(21, 5, "Level :" + STR$(16 - (Speed% / 100)))
  '
END SUB

