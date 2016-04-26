DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB SetPal (start.slot%, end.slot%)
'
DIM oldr%(255), oldg%(255), oldb%(255)
DIM Array1%(2000), Array2%(2000)
'
SCREEN 13
'
' Save old palette
'
FOR t% = 0 TO 255
  CALL ReadRGB(oldr%(t%), oldg%(t%), oldb%(t%), t%)
NEXT t%
'
' Create a custom 63 color palette
'
CALL WriteRGB(63, 63, 63, 1)
CALL WriteRGB(63, 0, 0, 15)
CALL WriteRGB(0, 63, 0, 31)
CALL WriteRGB(0, 0, 63, 47)
CALL WriteRGB(63, 63, 63, 63)
'
CALL SetPal(1, 15)
CALL SetPal(15, 31)
CALL SetPal(31, 47)
CALL SetPal(47, 63)
'
r1% = 25: r2% = 10: c1% = 0
r3% = 10: r4% = 23: c2% = 62
r5% = 25: r6% = 23: c3% = 127
'
FOR t = .02 TO 6.282 STEP .02
  x1% = 160 + INT(SIN(t) * r1%)
  y1% = 100 + INT(COS(t) * r2%)
  x2% = 160 + INT(SIN(t) * r3%)
  y2% = 100 + INT(COS(t) * r4%)
  x3% = 160 + INT(SIN(t) * r5%)
  y3% = 100 + INT(COS(t) * r6%)
  '
  c1% = c1% + 1: IF c1% > 63 THEN c1% = 1
  c2% = c2% + 1: IF c2% > 63 THEN c2% = 1
  c3% = c3% + 1: IF c3% > 63 THEN c3% = 1
  '
  PSET (x1%, y1%), c1%
  PSET (x2%, y2%), c2%
  PSET (x3%, y3%), c3%
  '
NEXT
'
FOR t% = 4 TO 0 STEP -1
  CIRCLE (160, 100), t%, t%
  PAINT (160, 100), t%, t%
NEXT t%
'
GET (130, 70)-(190, 130), Array1%
'
' Draw the mask
'
FOR y% = 70 TO 130
  FOR x% = 130 TO 190
    IF POINT(x%, y%) THEN PSET (x%, y%), 0 ELSE PSET (x%, y%), 255
  NEXT x%
NEXT y%
GET (130, 70)-(190, 130), Array2%
'
FOR t% = 0 TO 9
  bx%(t%) = INT(RND * 100) + 20: by%(t%) = INT(RND * 100) + 20
  '
  sign1% = 1: IF INT(RND * 2) = 1 THEN sign1% = -1
  sign2% = 1: IF INT(RND * 2) = 1 THEN sign2% = -1
  '
  vx%(t%) = (INT(RND * 5) + 1) * sign1%: vy%(t%) = (INT(RND * 5) + 1) * sign2%
NEXT t%
'
CLS
'
' Move the palette for a cool effect!
'
DO
  GOSUB MoveAtom
  GOSUB MovePalette
LOOP UNTIL INKEY$ <> ""
'
CLS
'
' Reset original RGB values
'
FOR t% = 0 TO 255
  CALL WriteRGB(oldr%(t%), oldg%(t%), oldb%(t%), t%)
NEXT t%
'
STOP
'
MoveAtom:
  '
  FOR t% = 0 TO 5
    '
    PUT (bx%(t%), by%(t%)), Array2%, AND
    '
    bx%(t%) = bx%(t%) + vx%(t%)
    by%(t%) = by%(t%) + vy%(t%)
    '
    IF bx%(t%) <= 5 OR bx%(t%) >= 255 THEN vx%(t%) = -vx%(t%)
    IF by%(t%) <= 5 OR by%(t%) >= 135 THEN vy%(t%) = -vy%(t%)
    '
    PUT (bx%(t%), by%(t%)), Array2%, AND
    PUT (bx%(t%), by%(t%)), Array1%, OR
    '
  NEXT
  '
RETURN
'
MovePalette:
  '
  CALL ReadRGB(ored%, ogrn%, oblu%, 1)
  '
  FOR t% = 1 TO 62
    CALL ReadRGB(red%, grn%, blu%, t% + 1)
    CALL WriteRGB(red%, grn%, blu%, t%)
  NEXT
  '
  CALL WriteRGB(ored%, ogrn%, oblu%, 63)
  '
RETURN

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

