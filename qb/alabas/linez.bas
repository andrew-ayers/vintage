'
' Description : Linez - Mode 13 VGA Special Effect Routine
' Written by  : Andrew L. Ayers
' Date        : 09/06/96
'
' This does a pretty normal "QIX" style line effect. Play with the
' variables to see what happens!
'
' As always, if you use this in any of your creations, please consider your
' source and mention my name. Thanx, and have phun!
'
DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB SetPal (start.slot%, end.slot%)
'
CONST MAX.SPEED = 4
CONST NUM.LINES = 24
CONST NUM.SETS = 2
CONST STYLE = 0 ' 0=Lines, 1=Boxes, 2=Dotz
'
DIM x1%(NUM.LINES, NUM.SETS), y1%(NUM.LINES, NUM.SETS), x2%(NUM.LINES, NUM.SETS), y2%(NUM.LINES, NUM.SETS)
DIM xv1%(NUM.LINES, NUM.SETS), yv1%(NUM.LINES, NUM.SETS), xv2%(NUM.LINES, NUM.SETS), yv2%(NUM.LINES, NUM.SETS)
DIM c%(NUM.LINES, NUM.SETS)
'
DIM oldr%(255), oldg%(255), oldb%(255)
'
' Initialize endpoint data
'
FOR tt% = 0 TO NUM.SETS
  '
  x1%(0, tt%) = INT(RND * 300) + 10
  y1%(0, tt%) = INT(RND * 180) + 10
  x2%(0, tt%) = INT(RND * 300) + 10
  y2%(0, tt%) = INT(RND * 180) + 10
  '
  DO: xv1%(0, tt%) = (INT(RND * 3) - 1) * INT(RND * MAX.SPEED): LOOP UNTIL xv1%(0, tt%) <> 0
  DO: yv1%(0, tt%) = (INT(RND * 3) - 1) * INT(RND * MAX.SPEED): LOOP UNTIL yv1%(0, tt%) <> 0
  DO: xv2%(0, tt%) = (INT(RND * 3) - 1) * INT(RND * MAX.SPEED): LOOP UNTIL xv2%(0, tt%) <> 0
  DO: yv2%(0, tt%) = (INT(RND * 3) - 1) * INT(RND * MAX.SPEED): LOOP UNTIL yv2%(0, tt%) <> 0
  '
  c%(0, tt%) = INT(RND * 255) + 1
  '
  FOR t% = 1 TO NUM.LINES
    '
    x1%(t%, tt%) = x1%(0, tt%)
    x2%(t%, tt%) = x2%(0, tt%)
    y1%(t%, tt%) = y1%(0, tt%)
    y2%(t%, tt%) = y2%(0, tt%)
    '
    xv1%(t%, tt%) = xv1%(0, tt%)
    xv2%(t%, tt%) = xv2%(0, tt%)
    yv1%(t%, tt%) = yv1%(0, tt%)
    yv2%(t%, tt%) = yv2%(0, tt%)
    '
    c%(t%, tt%) = c%(0, tt%)
    '
  NEXT t%
  '
NEXT tt%
'
SCREEN 13: CLS 0
'
' Save old palette
'
FOR t% = 0 TO 255
  CALL ReadRGB(oldr%(t%), oldg%(t%), oldb%(t%), t%)
NEXT t%
'
' Create a custom 256 color palette
'
CALL WriteRGB(63, 63, 63, 1)   ' From all white
CALL WriteRGB(63, 0, 0, 63)    ' to red, and then
CALL WriteRGB(0, 63, 0, 127)   ' to green, then
CALL WriteRGB(0, 0, 63, 191)   ' to blue, and finally
CALL WriteRGB(63, 63, 63, 255) ' back to white...
'
CALL SetPal(1, 63)
CALL SetPal(63, 127)
CALL SetPal(127, 191)
CALL SetPal(191, 255)
'
' Start the show!
'
DO
  '
  CALL ReadRGB(ored%, ogrn%, oblu%, 1)     ' Read in slot 1.
  '
  FOR t% = 1 TO 254
    '
    IF INKEY$ <> "" THEN EXIT DO
    '
    GOSUB MoveLines
    '
    CALL ReadRGB(red%, grn%, blu%, t% + 1) ' Read slots 2-255, then
    CALL WriteRGB(red%, grn%, blu%, t%)    ' shift to slots 1-254.
    '
  NEXT t%
  '
  CALL WriteRGB(ored%, ogrn%, oblu%, 255)  ' Write what was in slot 1 to
                                           ' slot 255.
LOOP
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
MoveLines:
  '
  FOR s% = 0 TO NUM.SETS
    '
    x1%(0, s%) = x1%(0, s%) + xv1%(0, s%)
    y1%(0, s%) = y1%(0, s%) + yv1%(0, s%)
    x2%(0, s%) = x2%(0, s%) + xv2%(0, s%)
    y2%(0, s%) = y2%(0, s%) + yv2%(0, s%)
    '
    c%(0, s%) = c%(0, s%) + 1: IF c%(0, s%) > 255 THEN c%(0, s%) = 1
    '
    SELECT CASE STYLE
      CASE 0
        LINE (x1%(0, s%), y1%(0, s%))-(x2%(0, s%), y2%(0, s%)), c%(0, s%)
        '
        LINE (x1%(NUM.LINES, s%), y1%(NUM.LINES, s%))-(x2%(NUM.LINES, s%), y2%(NUM.LINES, s%)), 0
      CASE 1
        LINE (x1%(0, s%), y1%(0, s%))-(x2%(0, s%), y2%(0, s%)), c%(0, s%), B
        '
        LINE (x1%(NUM.LINES, s%), y1%(NUM.LINES, s%))-(x2%(NUM.LINES, s%), y2%(NUM.LINES, s%)), 0, B
      CASE 2
        PSET (x1%(0, s%), y1%(0, s%)), c%(0, s%): PSET (x2%(0, s%), y2%(0, s%)), c%(0, s%)
        '
        PSET (x1%(NUM.LINES, s%), y1%(NUM.LINES, s%)), 0: PSET (x2%(NUM.LINES, s%), y2%(NUM.LINES, s%)), 0
    END SELECT
    '
    FOR tt% = NUM.LINES TO 1 STEP -1
      '
      x1%(tt%, s%) = x1%(tt% - 1, s%)
      x2%(tt%, s%) = x2%(tt% - 1, s%)
      y1%(tt%, s%) = y1%(tt% - 1, s%)
      y2%(tt%, s%) = y2%(tt% - 1, s%)
      '
      xv1%(tt%, s%) = xv1%(tt% - 1, s%)
      xv2%(tt%, s%) = xv2%(tt% - 1, s%)
      yv1%(tt%, s%) = yv1%(tt% - 1, s%)
      yv2%(tt%, s%) = yv2%(tt% - 1, s%)
      c%(tt%, s%) = c%(tt% - 1, s%)
      '
    NEXT tt%
    '
    IF x1%(0, s%) < 0 OR x1%(0, s%) > 319 THEN xv1%(0, s%) = -xv1%(0, s%)
    IF y1%(0, s%) < 0 OR y1%(0, s%) > 199 THEN yv1%(0, s%) = -yv1%(0, s%)
    IF x2%(0, s%) < 0 OR x2%(0, s%) > 319 THEN xv2%(0, s%) = -xv2%(0, s%)
    IF y2%(0, s%) < 0 OR y2%(0, s%) > 199 THEN yv2%(0, s%) = -yv2%(0, s%)
    '
  NEXT s%
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

