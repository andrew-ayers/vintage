SCREEN 0: WIDTH 80: CLS 1
'
DEF SEG = &HB800
'
BLOAD "test.scr", 0
'
DEF SEG
'
COLOR 15, 1: LOCATE 12, 5: PRINT SPACE$(73)
COLOR 15, 1: LOCATE 12, 5: PRINT "Enter the install path : "; : LINE INPUT path$
'
COLOR 15, 1: LOCATE 12, 5: PRINT SPACE$(73)
COLOR 15, 1: LOCATE 12, 5: PRINT "Installing..."
'
SLEEP 5
'
COLOR 15, 1: LOCATE 12, 5: PRINT SPACE$(73)
COLOR 15, 1: LOCATE 12, 5: PRINT "Done!": BEEP
SLEEP 2
'
COLOR 15, 1: LOCATE 12, 5: PRINT SPACE$(73)
COLOR 15, 1: LOCATE 12, 5: PRINT "Enter [RETURN] to continue : "; : LINE INPUT trash$
'
COLOR 15, 0: SCREEN 0: WIDTH 80: CLS 0
'
PRINT "To run Swarm, change to the install directory and type"
PRINT CHR$(34) + "GO.BAT [RETURN]" + CHR$(34) + "."




SUB clr (col%)
  '
  FOR t% = 0 TO 3999 STEP 2: POKE t%, 32: POKE t% + 1, col%: NEXT
  '
END SUB

SUB SetBlock (x1%, y1%, x2%, y2%, chr%, col%)
  '
  FOR y% = y1% TO y2%
    FOR x% = x1% TO x2%
      POKE (x% * 2) + (y% * 160) + 1, col%
      POKE (x% * 2) + (y% * 160), chr%
    NEXT
  NEXT
  '
END SUB

SUB SetBox (x1%, y1%, x2%, y2%, chr%, col%)
  '
  FOR y% = y1% TO y2%
    FOR x% = x1% TO x2%
      IF (y% = y1% OR y% = y2%) OR (x% = x1% OR x% = x2%) THEN
        POKE (x% * 2) + (y% * 160) + 1, col%
        POKE (x% * 2) + (y% * 160), chr%
      END IF
    NEXT
  NEXT
  '
END SUB

