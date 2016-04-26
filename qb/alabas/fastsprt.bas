'
' Name : Fast Sprite Demo
' Date : 10/22/96
'   By : Andrew L. Ayers
'  For : QBasic, QuickBasic
'
' This is a simple routine showing how to use "Sprite" style
' graphics effectively under mode 13h, to reduce flicker, while
' keeping the speed high. This could probably be improved even
' further, so if you can, do it! The technique demonstrated really
' only works well when the background is sparse, in this case, a
' scrolling starfield. Play around with it. If the demo runs too
' fast or slow for your machine, play around with the NumStars%
' variable to raise and lower the number of stars displayed. Use
' the arrow keys move the ship, hit the spacebar to fire. "Q" exits
' the demo.
'
' Use the code/sprites if you want - just mention my name, please!
' Thanx, and have phun!
'
' PS - Include the Blast! library to make this thing SMOKE! I wrote this
' as I was coming up with the Blast library of routines, so I leave it as
' an exercise for *you* to polish it...
'
SCREEN 13
'
DIM x%(100, 4), y%(100, 4), OX%(100, 4), OY%(100, 4), LV%(4)
DIM SHARED SpriteBuffer%(130 * 252)
'
DEF SEG = VARSEG(SpriteBuffer%(0))
BLOAD "a:\ship1.spr", 0
DEF SEG
'
CLS
'
FOR t = 1 TO 4
  FOR XX = 1 TO 100
    x%(XX, t) = INT(RND * 320)
    y%(XX, t) = INT(RND * 200)
  NEXT XX
  LV%(t) = 9 - (t * 2)
NEXT t
'
up$ = CHR$(0) + CHR$(72)
dn$ = CHR$(0) + CHR$(80)
lt$ = CHR$(0) + CHR$(75)
rt$ = CHR$(0) + CHR$(77)
'
PX% = 150: PY% = 170: Click% = 0: Sprite% = 0: Fire% = 0: NumStars% = 10
'
done% = 0: DO
  FOR t% = 1 TO 4
    FOR XX% = 1 TO NumStars%
      PSET (OX%(XX%, t%), OY%(XX%, t%)), 0
      PSET (x%(XX%, t%), y%(XX%, t%)), 32 - t% * 3
      GOSUB PUTSPRITES
      OX%(XX%, t%) = x%(XX%, t%): OY%(XX%, t%) = y%(XX%, t%)
      y%(XX%, t%) = y%(XX%, t%) + LV%(t%)
      IF y%(XX%, t%) > 200 THEN y%(XX%, t%) = 0: x%(XX%, t%) = INT(RND * 320)
    NEXT XX%
  NEXT t%
  '
  LOCATE 2, 4: PRINT "A simple mode 13h game-style demo"
  LOCATE 4, 11: PRINT "by Andrew L. Ayers"
  '
LOOP UNTIL done%
'
STOP
'
PUTSPRITES:
  '
  Click% = Click% + 1: IF Click% = 500 THEN Click% = 0: Sprite% = 1 - Sprite%
  PUT (PX%, PY%), SpriteBuffer%(Sprite% * 130), PSET
  '
  IF Fire% THEN
    PUT (FX% - 7, FY%), SpriteBuffer%(260), PSET
    PUT (FX% + 6, FY%), SpriteBuffer%(260), PSET
    FY% = FY% - 1
    IF FY% <= 0 THEN
      Fire% = 0: LINE (FX% - 7, FY%)-(FX% + 31, FY% + 16), 0, BF
    END IF
  END IF
  '
  SELECT CASE INKEY$
    CASE rt$
      PX% = PX% + 4: IF PX% > 303 THEN PX% = 303
      LINE (PX% - 4, PY%)-(PX%, PY% + 15), 0, BF
    CASE lt$
      PX% = PX% - 4: IF PX% < 0 THEN PX% = 0
      LINE (PX% + 16, PY%)-(PX% + 20, PY% + 15), 0, BF
    CASE up$
      PY% = PY% - 4: IF PY% < 0 THEN PY% = 0
      LINE (PX%, PY% + 16)-(PX% + 15, PY% + 20), 0, BF
    CASE dn$
      PY% = PY% + 4: IF PY% > 183 THEN PY% = 183
      LINE (PX%, PY% - 4)-(PX% + 15, PY%), 0, BF
    CASE " "
      LINE (FX% - 8, FY%)-(FX% + 31, FY% + 16), 0, BF
      IF PY% > 8 THEN Fire% = 1: FX% = PX%: FY% = PY% - 10
    CASE "q", "Q", CHR$(27)
      done% = 1
    CASE ELSE
  END SELECT
  '
RETURN

