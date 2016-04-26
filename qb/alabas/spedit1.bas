DECLARE SUB ShiftGrid (SpriteSize%, SpriteNum%, HShift%, VShift%)
DECLARE SUB DrawPoint (GridSize%, SpriteSize%, SpriteNum%, px%, py%, tess%)
DECLARE SUB RotateGrid (SpriteSize%, SpriteNum%)
DECLARE SUB CopyGrid (SpriteSize%, SpriteNum%, ToNum%)
DECLARE SUB DrawCursor (GridSize%, SpriteSize%, px%, py%, pc%)
DECLARE SUB DrawGrid (GridSize%, SpriteSize%, SpriteNum%, tess%)
DECLARE SUB ClearGrid (SpriteSize%, SpriteNum%, pc%)
'
Scrn% = 13: GOSUB SetScreen
'
up$ = CHR$(0) + CHR$(72)
dn$ = CHR$(0) + CHR$(80)
lt$ = CHR$(0) + CHR$(75)
rt$ = CHR$(0) + CHR$(77)
'
DIM SHARED SpriteData%(SpriteSize%, SpriteSize%, NumSprites%)
'
px% = 0: py% = 0: opx% = 0: opy% = 0
pc% = 0: SpriteNum% = 0: tess% = 0
'
DrawFlag% = 2: GOSUB UpdateScreen
'
done% = 0: DO
  '
  DrawFlag% = 1
  '
  key$ = INKEY$
  '
  SELECT CASE key$
    CASE rt$
      px% = px% + 1: IF px% > SpriteSize% THEN px% = SpriteSize%
    CASE lt$
      px% = px% - 1: IF px% < 0 THEN px% = 0
    CASE dn$
      py% = py% + 1: IF py% > SpriteSize% THEN py% = SpriteSize%
    CASE up$
      py% = py% - 1: IF py% < 0 THEN py% = 0
    CASE " "
      IF SpriteData%(px%, py%, SpriteNum%) <> pc% THEN
        SpriteData%(px%, py%, SpriteNum%) = pc%
      ELSE
        SpriteData%(px%, py%, SpriteNum%) = 0
      END IF
    CASE "+", "="
      SpriteNum% = SpriteNum% + 1: IF SpriteNum% > NumSprites% THEN SpriteNum% = NumSprites%
      DrawFlag% = 2
    CASE "_", "-"
      SpriteNum% = SpriteNum% - 1: IF SpriteNum% < 0 THEN SpriteNum% = 0
      DrawFlag% = 2
    CASE ".", ">"
      pc% = pc% + 1: IF pc% > MaxColors% THEN pc% = MaxColors%
    CASE ",", "<"
      pc% = pc% - 1: IF pc% < 0 THEN pc% = 0
    CASE "t", "T"
      IF tess% = 0 THEN tess% = 1 ELSE tess% = 0
      DrawFlag% = 2
    CASE "C"
      IF PFlag% = 1 THEN SCREEN , , 0, 0
      LOCATE 23, 1: LINE INPUT "Clearing - Are you sure (y/n) ? : "; a$
      IF PFlag% = 1 THEN SCREEN , , 1, 0
      IF a$ = "y" OR a$ = "Y" THEN
        CALL ClearGrid(SpriteSize%, SpriteNum%, pc%)
      END IF
      DrawFlag% = 2
    CASE "c"
      IF PFlag% = 1 THEN SCREEN , , 0, 0
      LOCATE 23, 1: LINE INPUT "Copy to ([RETURN]=Exit) : "; ToNum$
      IF PFlag% = 1 THEN SCREEN , , 1, 0
      IF ToNum$ <> "" THEN
        ToNum% = VAL(ToNum$)
        CALL CopyGrid(SpriteSize%, SpriteNum%, ToNum%)
      END IF
      DrawFlag% = 2
    CASE "r", "R"
      CALL RotateGrid(SpriteSize%, SpriteNum%)
      DrawFlag% = 2
    CASE "6"
      CALL ShiftGrid(SpriteSize%, SpriteNum%, 1, 0)
      DrawFlag% = 2
    CASE "4"
      CALL ShiftGrid(SpriteSize%, SpriteNum%, -1, 0)
      DrawFlag% = 2
    CASE "8"
      CALL ShiftGrid(SpriteSize%, SpriteNum%, 0, -1)
      DrawFlag% = 2
    CASE "2"
      CALL ShiftGrid(SpriteSize%, SpriteNum%, 0, 1)
      DrawFlag% = 2
    CASE "q"
      done% = 1: DrawFlag% = 0
    CASE "h", "H", "/", "?"
      IF PFlag% = 1 THEN SCREEN , , 0, 0
      LOCATE 6, HOffset: PRINT "ษอออ Sprite Editor Help อออป";
      LOCATE 7, HOffset: PRINT "บ                          บ";
      LOCATE 8, HOffset: PRINT "บ  Arrow Keys Move Cursor  บ";
      LOCATE 9, HOffset: PRINT "บ  C   = Clear Grid        บ";
      LOCATE 10, HOffset: PRINT "บ  c   = Copy Sprite       บ";
      LOCATE 11, HOffset: PRINT "บ  R   = Rotate Sprite     บ";
      LOCATE 12, HOffset: PRINT "บ  >,< = Inc, Dec Color    บ";
      LOCATE 13, HOffset: PRINT "บ  +,- = Inc, Dec Sprite   บ";
      LOCATE 14, HOffset: PRINT "บ  Q   = Quit              บ";
      LOCATE 15, HOffset: PRINT "บ  H,? = This Help Screen  บ";
      LOCATE 16, HOffset: PRINT "บ                          บ";
      LOCATE 17, HOffset: PRINT "ศอออออ Press Any Key ออออออผ";
      x$ = INPUT$(1)
      IF PFlag% = 1 THEN SCREEN , , 1, 0
      DrawFlag% = 2
    CASE ELSE
      DrawFlag% = 0
  END SELECT
  '
  GOSUB UpdateScreen
  '
LOOP UNTIL done%
'
STOP
'
UpdateScreen:
  '
  IF DrawFlag% > 0 THEN
    IF DrawFlag% = 2 THEN
      CLS
      CALL DrawGrid(GridSize%, SpriteSize%, SpriteNum%, tess%)
    ELSE
      CALL DrawPoint(GridSize%, SpriteSize%, SpriteNum%, opx%, opy%, tess%)
    END IF
    CALL DrawCursor(GridSize%, SpriteSize%, px%, py%, pc%)
    opx% = px%: opy% = py%
    LOCATE 23, 1: PRINT "Sprite #"; SpriteNum%
    IF PFlag% THEN PCOPY 1, 0
  END IF
  '
RETURN
'
SetScreen:
  '
  GridSize% = 8: SpriteSize% = 7: NumSprites% = 200
  '
  HOffset = 6: ' Help Display Offset
  '
  SELECT CASE Scrn%
    CASE 7
      SCREEN 7, , 1, 0: MaxColors% = 15: PFlag% = 1
    CASE 9
      SCREEN 9, , 1, 0: MaxColors% = 15: PFlag% = 1
      GridSize% = 16
    CASE 13
      SCREEN 13: MaxColors% = 255: PFlag% = 0
  END SELECT
  '
RETURN

SUB ClearGrid (SpriteSize%, SpriteNum%, pc%)
  '
  FOR y% = 0 TO SpriteSize%
    FOR x% = 0 TO SpriteSize%
      SpriteData%(x%, y%, SpriteNum%) = pc%
    NEXT x%
  NEXT y%
  '
END SUB

SUB CopyGrid (SpriteSize%, SpriteNum%, ToNum%)
  '
  FOR y% = 0 TO SpriteSize%
    FOR x% = 0 TO SpriteSize%
      SpriteData%(x%, y%, ToNum%) = SpriteData%(x%, y%, SpriteNum%)
    NEXT x%
  NEXT y%
  '
END SUB

SUB DrawCursor (GridSize%, SpriteSize%, px%, py%, pc%)
  '
  IF pc% <> 15 THEN
    LINE (px% * GridSize% + 2, py% * GridSize% + 2)-(px% * GridSize% + GridSize% - 2, py% * GridSize% + GridSize% - 2), 15, B
  ELSE
    LINE (px% * GridSize% + 2, py% * GridSize% + 2)-(px% * GridSize% + GridSize% - 2, py% * GridSize% + GridSize% - 2), 0, B
  END IF
  '
  LINE (px% * GridSize% + 3, py% * GridSize% + 3)-(px% * GridSize% + GridSize% - 3, py% * GridSize% + GridSize% - 3), pc%, BF
  '
END SUB

SUB DrawGrid (GridSize%, SpriteSize%, SpriteNum%, tess%)
  '
  v% = 0
  FOR y% = 0 TO GridSize% * SpriteSize% STEP GridSize%
    h% = 0
    FOR x% = 0 TO GridSize% * SpriteSize% STEP GridSize%
      '
      ' Draw Grid Lines
      '
      LINE (x%, y%)-(x% + GridSize%, y% + GridSize%), 15, B
      '
      ' Fill in Grid
      '
      LINE (x% + 1, y% + 1)-(x% + GridSize% - 1, y% + GridSize% - 1), SpriteData%(h%, v%, SpriteNum%), BF
      '
      ' Fill in Actual Size Grid
      '
      PSET (GridSize% * (SpriteSize% + 1) + 5 + h%, v% + 2), SpriteData%(h%, v%, SpriteNum%)
      '
      IF tess% THEN
        PSET (GridSize% * (SpriteSize% + 1) + 6 + h% + SpriteSize%, v% + 2), SpriteData%(h%, v%, SpriteNum%)
        PSET (GridSize% * (SpriteSize% + 1) + 5 + h%, v% + 3 + SpriteSize%), SpriteData%(h%, v%, SpriteNum%)
        PSET (GridSize% * (SpriteSize% + 1) + 6 + h% + SpriteSize%, v% + 3 + SpriteSize%), SpriteData%(h%, v%, SpriteNum%)
      END IF
      h% = h% + 1
    NEXT x%
    v% = v% + 1
  NEXT y%
  '
  IF tess% THEN
    LINE (GridSize% * (SpriteSize% + 1) + 3, 0)-(GridSize% * (SpriteSize% + 1) + (SpriteSize% * 2) + 8, (SpriteSize% * 2) + 5), 15, B
  ELSE
    LINE (GridSize% * (SpriteSize% + 1) + 3, 0)-(GridSize% * (SpriteSize% + 1) + SpriteSize% + 7, SpriteSize% + 4), 15, B
  END IF
  '
END SUB

SUB DrawPoint (GridSize%, SpriteSize%, SpriteNum%, px%, py%, tess%)
  '
  ' Fill in Grid
  '
  LINE ((px% * GridSize%) + 1, (py% * GridSize%) + 1)-((px% * GridSize%) + GridSize% - 1, (py% * GridSize%) + GridSize% - 1), SpriteData%(px%, py%, SpriteNum%), BF
  '
  ' Fill in Actual Size Grid
  '
  PSET (GridSize% * (SpriteSize% + 1) + 5 + px%, py% + 2), SpriteData%(px%, py%, SpriteNum%)
  '
  IF tess% THEN
    PSET (GridSize% * (SpriteSize% + 1) + 6 + px% + SpriteSize%, py% + 2), SpriteData%(px%, py%, SpriteNum%)
    PSET (GridSize% * (SpriteSize% + 1) + 5 + px%, py% + 3 + SpriteSize%), SpriteData%(px%, py%, SpriteNum%)
    PSET (GridSize% * (SpriteSize% + 1) + 6 + px% + SpriteSize%, py% + 3 + SpriteSize%), SpriteData%(px%, py%, SpriteNum%)
  END IF
  '
END SUB

SUB RotateGrid (SpriteSize%, SpriteNum%)
  '
  DIM TempData%(SpriteSize%, SpriteSize%)
  '
  FOR x% = 0 TO SpriteSize%
    FOR y% = SpriteSize% TO 0 STEP -1
      TempData%(SpriteSize% - y%, x%) = SpriteData%(x%, y%, SpriteNum%)
    NEXT y%
  NEXT x%
  '
  FOR y% = 0 TO SpriteSize%
    FOR x% = 0 TO SpriteSize%
      SpriteData%(x%, y%, SpriteNum%) = TempData%(x%, y%)
    NEXT x%
  NEXT y%
  '
END SUB

SUB ShiftGrid (SpriteSize%, SpriteNum%, HShift%, VShift%)
  '
  DIM TempData%(SpriteSize%)
  '
  IF HShift% <> 0 THEN
    IF HShift% = 1 THEN
      FOR y% = 0 TO SpriteSize%
        TempData%(y%) = SpriteData%(SpriteSize%, y%, SpriteNum%)
      NEXT y%
      FOR y% = 0 TO SpriteSize%
        FOR x% = SpriteSize% - 1 TO 0 STEP -1
          SpriteData%(x% + 1, y%, SpriteNum%) = SpriteData%(x%, y%, SpriteNum%)
        NEXT x%
        SpriteData%(0, y%, SpriteNum%) = TempData%(y%)
      NEXT y%
    ELSE
      FOR y% = 0 TO SpriteSize%
        TempData%(y%) = SpriteData%(0, y%, SpriteNum%)
      NEXT y%
      FOR y% = 0 TO SpriteSize%
        FOR x% = 1 TO SpriteSize%
          SpriteData%(x% - 1, y%, SpriteNum%) = SpriteData%(x%, y%, SpriteNum%)
        NEXT x%
        SpriteData%(SpriteSize%, y%, SpriteNum%) = TempData%(y%)
      NEXT y%
    END IF
  ELSE
    IF VShift% <> 0 THEN
      IF VShift% = 1 THEN
        FOR x% = 0 TO SpriteSize%
          TempData%(x%) = SpriteData%(x%, SpriteSize%, SpriteNum%)
        NEXT x%
        FOR x% = 0 TO SpriteSize%
          FOR y% = SpriteSize% - 1 TO 0 STEP -1
            SpriteData%(x%, y% + 1, SpriteNum%) = SpriteData%(x%, y%, SpriteNum%)
          NEXT y%
          SpriteData%(x%, 0, SpriteNum%) = TempData%(x%)
        NEXT x%
      ELSE
        FOR x% = 0 TO SpriteSize%
          TempData%(x%) = SpriteData%(x%, 0, SpriteNum%)
        NEXT x%
        FOR x% = 0 TO SpriteSize%
          FOR y% = 1 TO SpriteSize%
            SpriteData%(x%, y% - 1, SpriteNum%) = SpriteData%(x%, y%, SpriteNum%)
          NEXT y%
          SpriteData%(x%, SpriteSize%, SpriteNum%) = TempData%(x%)
        NEXT x%
      END IF
    END IF
  END IF
  '
END SUB

