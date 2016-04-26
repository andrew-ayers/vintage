'
' Description : Quadratic Plasma
' Written by  : Andrew L. Ayers
' Date        : 10/23/96
'
' This is a program to create quadratic plasma (I am unsure on
' the name here. I know I am not using quadratic equations, but
' hey, call it what you want). The images are formed using only
' math - no recursion, SIN/COS warping, or random numbers. What
' comes out is a very nice display. On machines without a co-
' processor, this may take a little while. Have phun!
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
' Display quadratic plasma
'
' Try different numbers (greater than 1) for scale!
'
'scale = 1
'scale = 10
scale = 100
'scale = 250
'
FOR y = -100 * scale TO 100 * scale STEP 1 * scale
  '
  h% = 0
  '
  FOR x = -160 * scale TO 160 * scale STEP 1 * scale
    '
    h% = h% + 1
    '
    ' Try out these equations for different effects!
    '
    c% = (SQR(x * x + y * y) AND 62) + 1
    'c% = ((SQR(x * x) + SQR(y * y)) AND 62) + 1
    'c% = (SQR(ABS(x * y) + ABS(x * y)) AND 62) + 1
    'c% = (SQR(ABS(x * x - y * y)) AND 62) + 1
    PSET (h%, v%), c%
  NEXT
  '
  GOSUB MovePalette ' Remove this line if your machine lacks a coprocessor
  v% = v% + 1
  '
NEXT
'
' Move the palette for a cool effect!
'
DO
  GOSUB MovePalette
  FOR delay = 1 TO 1000: NEXT delay' Adjust for your machine
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

