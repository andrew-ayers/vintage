DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB SetPal (start.slot%, end.slot%)
'
DIM oldr%(255), oldg%(255), oldb%(255)
'
SCREEN 13
'
' Save old palette
'
FOR t% = 0 TO 255
  CALL ReadRGB(oldr%(t%), oldg%(t%), oldb%(t%), t%)
NEXT t%
'
' Create a custom 256 color palette
'
CALL WriteRGB(0, 0, 0, 0)
CALL WriteRGB(63, 0, 0, 8)
CALL WriteRGB(63, 63, 63, 15)
'
CALL WriteRGB(0, 0, 0, 16)
CALL WriteRGB(0, 63, 0, 24)
CALL WriteRGB(63, 63, 63, 31)
'
CALL WriteRGB(0, 0, 0, 32)
CALL WriteRGB(0, 0, 63, 40)
CALL WriteRGB(63, 63, 63, 47)
'
CALL SetPal(0, 8)
CALL SetPal(8, 15)
'
CALL SetPal(16, 24)
CALL SetPal(24, 31)
'
CALL SetPal(32, 40)
CALL SetPal(40, 47)
'
' Display example palette
'
FOR t% = 0 TO 47
  LINE (t% * 5, 0)-(t% * 5 + 5, 99), t%, BF
NEXT t%
'
' Draw Sprite
'
LINE (0, 120)-(31, 151), 40, BF
CIRCLE (15, 135), 10, 8
LINE (0, 120)-(31, 151), 24, B
'
' Create intensity mask
'
rad% = 20: doff% = 1: min% = -6: max% = 2
'
FOR t% = min% TO max%
  CIRCLE (65, 135), ABS(rad%), t% + 8
  PAINT (65, 135), t% + 8
  rad% = rad% - doff%
NEXT
'
' Redraw sprite at new intensity
'
FOR y% = 120 TO 151
  FOR x% = 0 TO 31
    oldval% = POINT(x%, y%)
    intensity% = POINT(50 + x%, y%) - 8
    IF oldval% > 0 THEN
      minval% = (oldval% \ 16) * 16
      maxval% = minval% + 15
      newval% = oldval% + intensity%
      IF newval% < minval% THEN
        newval% = minval%
      ELSEIF newval% > maxval% THEN
        newval% = maxval%
      END IF
      PSET (x% + 100, y%), newval%
    END IF
  NEXT
NEXT
      

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
  r = sr%: g = sg%: B = sb%
  wr% = r: wg% = g: wb% = B
  '
  FOR t% = start.slot% TO end.slot%
    '
    CALL WriteRGB(wr%, wg%, wb%, t%)
    '
    r = r + stepr: wr% = r
    g = g + stepg: wg% = g
    B = B + stepb: wb% = B
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

