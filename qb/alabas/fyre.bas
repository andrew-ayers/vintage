'
' Description : Fyre - Mode 13 VGA Special Effect Routine
' Written by  : Andrew L. Ayers
' Date        : 09/10/96
'
' This shows off two techniques which may prove useful to you in game or
' demo development. The first technique is that of simulating fire. It
' uses a routine similar to that of my FirePrint! routine for doing
' burning text. I saw a post by someone (I forget who) who needed to
' know how to do fire. I hope they see this. The second technique is the
' more important of the two. It shows how to use a single buffer to hold
' multiple frames of an animation for GET/PUT. Basically, it involves using
' an offset into the buffer during the Build/GET process to store each
' frame into the buffer, then using the same offsets to replay the buffer
' frame by frame. You must know the size of each "frame" in bytes in order
' to use this, and all frames should be the same size for easiest implem-
' entation (though not necessarily). This will work for all screen modes.
'
' As always, if you use this in any of your creations, please consider your
' source and mention my name. Thanx, and have phun!
'
DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB SetPal (start.slot%, end.slot%)
'
DIM frame%(3400)
'
SCREEN 13
'
' Create an "all black" palette
'
CALL WriteRGB(0, 0, 0, 1)
CALL WriteRGB(0, 0, 0, 63)
'
CALL SetPal(1, 63)
'
' Build each frame of the "fyre"
'
LINE (0, 101)-(14, 101), 63
'
FOR t% = 0 TO 19
  '
  FOR Y% = 100 TO 80 STEP -1
    '
    FOR X% = 0 TO 14
      '
      C% = POINT(X%, Y% + 1) - INT(RND * 10): IF C% < 0 THEN C% = 0
      '
      PSET (X%, Y%), C%
      '
    NEXT
    '
  NEXT
  '
  ' Get the frame
  '
  GET (0, 80)-(14, 101), frame%(t% * 170)
  '
NEXT
'
CLS
'
' Build up "fyre" colored palette
'
CALL WriteRGB(0, 0, 0, 1)
CALL WriteRGB(63, 0, 0, 21)
CALL WriteRGB(63, 63, 0, 42)
CALL WriteRGB(58, 58, 58, 63)
'
CALL SetPal(1, 21)
CALL SetPal(21, 42)
CALL SetPal(42, 63)
'
' Display "fyre" at the bottom of the screen
'
DO
  '
  t% = t% + 1: IF t% > 19 THEN t% = 0
  '
  FOR tt% = 0 TO 300 STEP 15
    '
    PUT (tt%, 178), frame%(t% * 170), PSET
    '
  NEXT
  '
  ' Delay (may need to be adjusted)
  '
  FOR dlay = 1 TO 20000: NEXT dlay
  '
LOOP UNTIL INKEY$ <> ""

SUB ReadRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C7, slot% ' Read RGB values from slot
  '
  red% = INP(&H3C9)
  grn% = INP(&H3C9)
  blu% = INP(&H3C9)
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
  r = sr%: g = sg%: b = sb%
  wr% = r: wg% = g: wb% = b
  '
  FOR t% = start.slot% TO end.slot%
    '
    CALL WriteRGB(wr%, wg%, wb%, t%)
    '
    r = r + stepr: wr% = r
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

