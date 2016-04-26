DECLARE SUB LoadPal ()
DECLARE SUB SavePal ()
DECLARE SUB SetPal (start.slot%, end.slot%)
DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB SelectColor (pc%)
'
TYPE RGBTriple
  red AS STRING * 1
  grn AS STRING * 1
  blu AS STRING * 1
END TYPE
'
SCREEN 13
'
CALL SelectColor(pc%)
'
CLS
'
PRINT pc%

SUB LoadPal
  '
  DIM RGB AS RGBTriple
  '
  LOCATE 21, 1: PRINT STRING$(80, " ")
  LOCATE 22, 1: PRINT STRING$(80, " ")
  LOCATE 23, 1: PRINT STRING$(80, " ");
  '
  LOCATE 23, 1: PRINT "([RETURN]=Exit)     ";
  LOCATE 21, 1: LINE INPUT "Load : "; file$
  '
  LOCATE 21, 1: PRINT STRING$(80, " ")
  LOCATE 22, 1: PRINT STRING$(80, " ")
  LOCATE 23, 1: PRINT STRING$(80, " ");
  '
  IF file$ = "" THEN EXIT SUB
  '
  OPEN file$ FOR BINARY AS 1
  '
  tt% = 1
  FOR t% = 0 TO 255
    GET #1, tt%, RGB: tt% = tt% + 3
    '
    red% = ASC(RGB.red$) - 32
    grn% = ASC(RGB.grn$) - 32
    blu% = ASC(RGB.blu$) - 32
    '
    CALL WriteRGB(red%, grn%, blu%, t%)
    '
  NEXT t%
  '
  CLOSE #1
  '
END SUB

SUB ReadRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C7, slot% ' Read RGB values from slot
  '
  red% = INP(&H3C9)
  grn% = INP(&H3C9)
  blu% = INP(&H3C9)
  '
END SUB

SUB SavePal
  '
  DIM RGB AS RGBTriple
  '
  LOCATE 21, 1: PRINT STRING$(80, " ")
  LOCATE 22, 1: PRINT STRING$(80, " ")
  LOCATE 23, 1: PRINT STRING$(80, " ");
  '
  LOCATE 23, 1: PRINT "([RETURN]=Exit)     ";
  LOCATE 21, 1: LINE INPUT "Save : "; file$
  '
  LOCATE 21, 1: PRINT STRING$(80, " ")
  LOCATE 22, 1: PRINT STRING$(80, " ")
  LOCATE 23, 1: PRINT STRING$(80, " ");
  '
  IF file$ = "" THEN EXIT SUB
  '
  OPEN file$ FOR BINARY AS 1
  '
  tt% = 1
  FOR t% = 0 TO 255
    CALL ReadRGB(red%, grn%, blu%, t%)
    '
    RGB.red$ = CHR$(red% + 32)
    RGB.grn$ = CHR$(grn% + 32)
    RGB.blu$ = CHR$(blu% + 32)
    '
    PUT #1, tt%, RGB: tt% = tt% + 3
    '
  NEXT t%
  '
  CLOSE #1
  '
END SUB

SUB SelectColor (pc%)
  '
  up$ = CHR$(0) + CHR$(72)
  dn$ = CHR$(0) + CHR$(80)
  lt$ = CHR$(0) + CHR$(75)
  rt$ = CHR$(0) + CHR$(77)
  '
  maxx = 159: maxy = 159
  stpx = 10: stpy = 10
  start.slot% = -1: end.slot% = -1
  '
  CLS 0
  '
  col% = 0
  '
  FOR y = 0 TO maxy STEP stpy
    FOR x = 0 TO maxx STEP stpx
      LINE (x + 1, y + 1)-(x + stpx - 2, y + stpy - 2), col%, BF
      col% = col% + 1
    NEXT x
  NEXT y
  '
  curx = 0: cury = 0: done = 0
  '
  LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 15, B
  '
  LOCATE 1, 22: PRINT "ษออออออออออออออออป"
  LOCATE 2, 22: PRINT "บArrow keys move บ"
  LOCATE 3, 22: PRINT "บ[RETURN] selectsบ"
  LOCATE 4, 22: PRINT "ฬออออออออออออออออน"
  LOCATE 5, 22: PRINT "บR=Change RGB    บ"
  LOCATE 6, 22: PRINT "บ[=Range Start   บ"
  LOCATE 7, 22: PRINT "บ]=Range End     บ"
  LOCATE 8, 22: PRINT "บC=Copy Range    บ"
  LOCATE 9, 22: PRINT "บ\=Shade Range   บ"
  LOCATE 10, 22: PRINT "บS=Save Palette  บ"
  LOCATE 11, 22: PRINT "บL=Load Palette  บ"
  LOCATE 12, 22: PRINT "บQ=Quit          บ"
  LOCATE 13, 22: PRINT "ศออออออออออออออออผ"
  '
  DO
    '
    col% = POINT(curx * stpx + 5, cury * stpy + 5)
    CALL ReadRGB(red%, grn%, blu%, col%)
    '
    LOCATE 22, 1: PRINT STRING$(80, " ")
    LOCATE 23, 1: PRINT STRING$(80, " ");
    '
    LOCATE 22, 1: PRINT "Color #"; col%
    LOCATE 23, 1: PRINT "RGB   #"; red%; grn%; blu%
    '
    IF start.slot% > 0 THEN
      LOCATE 16, 23: PRINT "Start :"; start.slot%
    ELSE
      LOCATE 16, 23: PRINT "                 ";
    END IF
    '
    IF end.slot% > 0 THEN
      LOCATE 18, 23: PRINT "End   :"; end.slot%
    ELSE
      LOCATE 18, 23: PRINT "                 ";
    END IF
    '
    DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
    '
    LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 0, B
    '
    SELECT CASE key$
      '
      CASE up$
        cury = cury - 1: IF cury < 0 THEN cury = INT((maxy + 1) / stpy) - 1
      CASE dn$
        cury = cury + 1: IF cury > INT((maxy + 1) / stpy) - 1 THEN cury = 0
      CASE rt$
        curx = curx + 1: IF curx > INT((maxx + 1) / stpx) - 1 THEN curx = 0
      CASE lt$
        curx = curx - 1: IF curx < 0 THEN curx = INT((maxx + 1) / stpx) - 1
      CASE CHR$(13)
        pc% = col%
        done = 1
      CASE "q", "Q"
        done = 1
      CASE "c", "C"
        IF col% > 15 AND start.slot% > 15 AND end.slot% > 15 THEN
          IF col% + (end.slot% - start.slot%) <= 255 THEN
            temp% = 0
            FOR t% = start.slot% TO end.slot%
              CALL ReadRGB(red%, grn%, blu%, t%)
              CALL WriteRGB(red%, grn%, blu%, col% + temp%)
              temp% = temp% + 1
            NEXT t%
            start.slot% = -1: end.slot% = -1
          END IF
        END IF
      CASE "r", "R"
        IF col% > 15 THEN
          LOCATE 21, 1: PRINT STRING$(80, " ")
          LOCATE 22, 1: PRINT STRING$(80, " ")
          LOCATE 23, 1: PRINT STRING$(80, " ");
          '
          LOCATE 23, 1: PRINT "([RETURN]=Exit)"
          LOCATE 21, 1: LINE INPUT "Enter RGB triple : "; a$
          '
          LOCATE 21, 1: PRINT STRING$(80, " ")
          LOCATE 22, 1: PRINT STRING$(80, " ")
          LOCATE 23, 1: PRINT STRING$(80, " ");
          '
          IF a$ <> "" THEN
            red% = VAL(MID$(a$, 1, 2))
            grn% = VAL(MID$(a$, 3, 2))
            blu% = VAL(MID$(a$, 5, 2))
            CALL WriteRGB(red%, grn%, blu%, col%)
          END IF
        END IF
      CASE "["
        IF col% > 15 THEN start.slot% = col%
      CASE "]"
        IF start.slot% > 15 AND col% >= start.slot% THEN end.slot% = col%
      CASE "\"
        IF start.slot% > 15 AND end.slot% > 15 THEN
          CALL SetPal(start.slot%, end.slot%)
          start.slot% = -1: end.slot% = -1
        END IF
      CASE "L", "l"
        CALL LoadPal
      CASE "S", "s"
        CALL SavePal
      CASE ELSE
      '
    END SELECT
    '
    LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 15, B
    '
  LOOP UNTIL done
  '
END SUB

SUB SetPal (start.slot%, end.slot%)
  '
  num.slots% = end.slot% - start.slot%
  '
  CALL ReadRGB(sr%, sg%, sb%, start.slot%)
  CALL ReadRGB(er%, eg%, eb%, end.slot%)
  '
  rr% = ABS(er% - sr%): rg% = ABS(eg% - sg%): rb% = ABS(eb% - sb%)
  rs% = SGN(er% - sr%): gs% = SGN(eg% - sg%): bs% = SGN(eb% - sb%)
  '
  stepr = (rr% / num.slots%) * rs%
  stepg = (rg% / num.slots%) * gs%
  stepb = (rb% / num.slots%) * bs%
  '
  R = sr%: g = sg%: b = sb%
  wr% = R: wg% = g: wb% = b
  '
  FOR t% = start.slot% TO end.slot%
    '
    CALL WriteRGB(wr%, wg%, wb%, t%)
    '
    R = R + stepr: wr% = R
    g = g + stepg: wg% = g
    b = b + stepb: wb% = b
    '
  NEXT t%
  '
END SUB

SUB WriteRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C8, slot% ' Write RGB values to slot
  '
  OUT &H3C9, red%
  OUT &H3C9, grn%
  OUT &H3C9, blu%
  '
END SUB

