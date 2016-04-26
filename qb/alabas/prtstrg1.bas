DECLARE SUB WindowBox (xpos1%, ypos1%, xpos2%, ypos2%, fcol%, bcol%, hcol%, style%, title$, tfcol%, tbcol%, thcol%)
DECLARE SUB Shadow (xpos1%, ypos1%, xpos2%, ypos2%)
DECLARE SUB PrintStrg (strng$, xpos%, ypos%, fcol%, bcol%, hcol%)
'
SCREEN 0, , 0, 1: CLS
'
FOR t% = 0 TO 24
  CALL PrintStrg(STRING$(80, "a"), 0, t%, 7, 4, 0)
NEXT t%
'
PCOPY 0, 1
'
CALL WindowBox(5, 5, 34, 12, 7, 1, 1, 1, " Change Colors ", 7, 2, 1)
PCOPY 0, 1
CALL WindowBox(20, 10, 49, 17, 7, 1, 1, 2, " C:\BWINDOWS\SYSTEM.CFG ", 7, 6, 1)
PCOPY 0, 1

SUB PrintStrg (strng$, xpos%, ypos%, fcol%, bcol%, hcol%)
  '
  DEF SEG = &HB800
  '
  FOR t% = 1 TO LEN(strng$)
    posn% = (ypos% * 160) + ((xpos% + (t% - 1)) * 2)
    POKE posn%, ASC(MID$(strng$, t%, 1)): POKE posn% + 1, bcol% * 16 + (fcol% + (hcol% * 8))
  NEXT
  '
  DEF SEG
  '
END SUB

SUB Shadow (xpos1%, ypos1%, xpos2%, ypos2%)
  '
  DEF SEG = &HB800
  '
  FOR v% = ypos1% TO ypos2%
    FOR h% = xpos1% TO xpos2%
      posn% = (v% * 160) + (h% * 2)
      POKE posn% + 1, 8
    NEXT
  NEXT
  '
  DEF SEG
  '
END SUB

SUB WindowBox (xpos1%, ypos1%, xpos2%, ypos2%, fcol%, bcol%, hcol%, style%, title$, tfcol%, tbcol%, thcol%)
  '
  CALL Shadow(xpos2% + 1, ypos1% + 1, xpos2% + 2, ypos2% + 1)
  CALL Shadow(xpos1% + 2, ypos2% + 1, xpos2% + 2, ypos2% + 1)
  '
  FOR v% = ypos1% TO ypos2%
    FOR h% = xpos1% TO xpos2%
      SELECT CASE style%
        CASE 0
          '
          ul$ = "Û"
          ur$ = "Û"
          ll$ = "Û"
          lr$ = "Û"
          hr$ = "Û"
          vr$ = "Û"
          fl$ = " "
          '
        CASE 1
          '
          ul$ = "Ú"
          ur$ = "¿"
          ll$ = "À"
          lr$ = "Ù"
          hr$ = "Ä"
          vr$ = "³"
          fl$ = " "
          '
        CASE 2
          '
          ul$ = "É"
          ur$ = "»"
          ll$ = "È"
          lr$ = "¼"
          hr$ = "Í"
          vr$ = "º"
          fl$ = " "
          '
      END SELECT
      '
      IF (h% <> xpos1% AND h% <> xpos2%) AND (v% <> ypos1% OR v% <> ypos2%) THEN
        CALL PrintStrg(fl$, h%, v%, fcol%, bcol%, hcol%)
      END IF
      IF (h% = xpos1% OR h% = xpos2%) AND (v% <> ypos1% AND v% <> ypos2%) THEN
        CALL PrintStrg(vr$, h%, v%, fcol%, bcol%, hcol%)
      END IF
      IF (h% <> xpos1% AND h% <> xpos2%) AND (v% = ypos1% OR v% = ypos2%) THEN
        CALL PrintStrg(hr$, h%, v%, fcol%, bcol%, hcol%)
      END IF
      IF h% = xpos1% AND v% = ypos1% THEN
        CALL PrintStrg(ul$, h%, v%, fcol%, bcol%, hcol%)
      END IF
      IF h% = xpos2% AND v% = ypos1% THEN
        CALL PrintStrg(ur$, h%, v%, fcol%, bcol%, hcol%)
      END IF
      IF h% = xpos1% AND v% = ypos2% THEN
        CALL PrintStrg(ll$, h%, v%, fcol%, bcol%, hcol%)
      END IF
      IF h% = xpos2% AND v% = ypos2% THEN
        CALL PrintStrg(lr$, h%, v%, fcol%, bcol%, hcol%)
      END IF
    NEXT
  NEXT
  '
  CALL PrintStrg(title$, xpos1% + ((xpos2% - xpos1%) \ 2) - (LEN(title$) \ 2) + 1, ypos1%, tfcol%, tbcol%, thcol%)
  '
END SUB

