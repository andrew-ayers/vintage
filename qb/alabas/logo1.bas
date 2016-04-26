max.inst = 999
'
DIM inst$(max.inst)
'
onerad = 3.14159 / 180
'
SCREEN 12: CLS 0
'
DO
  '
  i = 0: DO
    '
    i = i + 1
    '
    PRINT USING "###"; i;
    LINE INPUT " - "; inst$(i)
    '
  LOOP UNTIL inst$(i) = ""
  '
  PRINT
  PRINT "E)dit, R)un, or Q)uit : ";
  LINE INPUT ansr$
  '
  ansr$ = UCASE$(ansr$)
  IF ansr$ = "R" THEN GOSUB run.logo
  IF ansr$ = "E" THEN GOSUB edit.logo
  IF ansr$ = "Q" THEN STOP
  '
LOOP
'
run.logo:
  '
  SCREEN 12: CLS 0
  '
  rpt.flag = 1
  '
here:
  '
  FOR i = rpt.flag TO max.inst
    IF LEFT$(inst$(i), 2) <> "to" AND LEFT$(inst$(i), 1) <> " " THEN
      cmnd$ = UCASE$(LEFT$(inst$(i), 3))
      unit$ = MID$(inst$(i), 5, 99)
      SELECT CASE cmnd$
        CASE "HOM"
          ox = 319: oy = 239
          tx = 319: ty = 239
          degs = 180: pencol = -1
          rads = onerad * degs
          LINE (tx, ty)-(tx, ty), 0
        CASE "PNU"
          pencol = -1
        CASE "PND"
          pencol = VAL(unit$)
        CASE "FWD"
          ox = tx: oy = ty
          tx = tx + SIN(rads) * VAL(unit$)
          ty = ty + COS(rads) * VAL(unit$)
          IF pencol > -1 THEN
            LINE (ox, oy)-(tx, ty), pencol
          END IF
        CASE "LFT"
          degs = degs + VAL(unit$)
          rads = onerad * degs
        CASE "RGT"
          degs = degs - VAL(unit$)
          rads = onerad * degs
        CASE "RPT"
          rpt.flag = i
          IF unit$ = " 1" THEN rpt.flag = 0
          PRINT unit$
          MID$(inst$(i), 5) = STR$(VAL(unit$) - 1)
      END SELECT
    END IF
    IF rpt.flag = 0 THEN EXIT FOR
  NEXT i
  '
  IF rpt.flag <> 0 AND rpt.flag <> 1 THEN GOTO here
  '
  RETURN

edit.logo:
  '
  '
  RETURN




