'
' Description : Mode 13 VGA Palette Read/Write Subroutines
'               and custom palette setting routine
' Written by  : Andrew L. Ayers
' Date        : 07/24/96
'
' These read/write routines were developed from information
' provided by Eli Bennett in an ABC Code Packet. The palette
' setting (spreading?) routine is my own. These routines should
' make it easier to read/write RGB values to the VGA palette in
' mode 13 as well as in setting up palettes. If you use these
' routines, please give credit to both Mr. Bennett and myself.
' Have phun!
'
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
CALL WriteRGB(63, 63, 63, 1)   ' From all white
CALL WriteRGB(63, 0, 0, 63)    ' to red, and then
CALL WriteRGB(0, 63, 0, 127)   ' to green, then
CALL WriteRGB(0, 0, 63, 191)   ' to blue, and finally
CALL WriteRGB(63, 63, 63, 255) ' back to white...
'
CALL SetPal(1, 63)             ' Each of these lines
CALL SetPal(63, 127)           ' create a portion of
CALL SetPal(127, 191)          ' the color spread. The
CALL SetPal(191, 255)          ' two arguments are the
                               ' start and ending slots
                               ' for the spread...
'
' Display example
'
FOR t% = 1 TO 255
  LINE (t% - 1, 0)-(t% - 1, 199), t%
NEXT t%

'
' Rotate palette - this isn't how you would do it
' for speed (for more speed, inline the read/write code
' to eliminate subroutine calling overhead), but it
' does show how to do it. Notice the "sparklies" along
' the right hand edge. These are only apparent if your
' computer is fast enough. I believe this has to do with
' the registers being updated faster than the video card
' can keep up with or sothing like that. If anyone knows
' how to fix this, go for it!
'
DO
  '
  CALL ReadRGB(ored%, ogrn%, oblu%, 1)     ' Read in slot 1.
  '
  FOR t% = 1 TO 254
    CALL ReadRGB(red%, grn%, blu%, t% + 1) ' Read slots 2-255, then
    CALL WriteRGB(red%, grn%, blu%, t%)    ' shift to slots 1-254.
  NEXT t%
  '
  CALL WriteRGB(ored%, ogrn%, oblu%, 255)  ' Write what was in slot 1 to
                                           ' slot 255.
LOOP UNTIL INKEY$ <> ""
'
CLS

'
' Reset original RGB values
'
FOR t% = 0 TO 255
  CALL WriteRGB(oldr%(t%), oldg%(t%), oldb%(t%), t%)
NEXT t%

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

