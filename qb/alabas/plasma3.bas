'
' Description : Mode 8 Sinusoidal Plasma!
' Written by  : Andrew L. Ayers
' Date        : 07/24/96
'
' This creates sinusoidal plasma in mode 8 - Shows how to screw around
' with the palette in this mode too. As always, if you use the routine
' in your own program or demo, please mention my name. Thanks, and
' have phun!
'
DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
'
DIM A%(500), C%(500), ff(10000)
'
SCREEN 8
'
FOR t% = 0 TO 7
  CALL WriteRGB(t% * 8, 0, 0, t%)
NEXT t%
'
FOR t% = 0 TO 7
  CALL WriteRGB(t% * 8, t% * 8, t% * 8, 16 + t%)
NEXT t%
'
' TPI = 2 x PI - Do NOT mess with, needed for SIN Calcs
' FREQ = Frequency (Duh!)   - Go ahead and mess with these two...
' AMPLITUDE = (Double Duh!)
'
TPI = 6.28318: FREQ% = 4: AMPLITUDE% = 15
'
' Create Sinusoidal Multicolored Backdrop Thingy!
'
SCALE = (TPI * FREQ%) / 320
'
FOR y% = 0 TO 199
  RAD = 0
  COLR% = COLR% + 1: IF COLR% > 15 THEN COLR% = 1
  LINE (0, y%)-(0, y%), COLR%
  FOR x% = 0 TO 639 STEP 8
    YPOS% = y% + SIN(RAD) * AMPLITUDE%
    LINE -(x%, YPOS%), COLR%
    RAD = RAD + (SCALE * 6)
  NEXT x%
NEXT y%
'
' Warp it sinusoidally in a horizontal fashion!
'
RAD = 0
FREQ% = 8: AMPLITUDE% = 15
SCALE = (TPI * FREQ%) / 200
'
FOR y% = 0 TO 199
  XPOS% = INT(SIN(RAD) * AMPLITUDE%)
  GET (0, y%)-(639 - XPOS%, y%), A%
  IF XPOS% >= 0 THEN
    GET (639 - XPOS%, y%)-(639, y%), C%
    PUT (XPOS%, y%), A%, PSET
    PUT (0, y%), C%, PSET
  ELSE
    GET (ABS(XPOS%), y%)-(639, y%), A%
    GET (0, y%)-(ABS(XPOS%), y%), C%
    PUT (0, y%), A%, PSET
    PUT (639 + XPOS%, y%), C%, PSET
  END IF
  RAD = RAD + SCALE
NEXT y%
'
' Mask off ugly portions
'
LINE (0, 0)-(639, 17), 0, BF
LINE (0, 174)-(639, 199), 0, BF
LINE (0, 0)-(35, 199), 0, BF
LINE (604, 0)-(639, 199), 0, BF
'
LOCATE 2, 27: PRINT "Sinusoidal Plasma Effect!"
LOCATE 23, 30: PRINT "By Andrew L. Ayers"
'
DO
  '
  CALL ReadRGB(ored%, ogrn%, oblu%, 1)     ' Read in slot 1.
  '
  FOR t% = 1 TO 6
    CALL ReadRGB(red%, grn%, blu%, t% + 1) ' Read slots 2-7, then
    CALL WriteRGB(red%, grn%, blu%, t%)    ' shift to slots 1-6.
  NEXT t%
  '
  CALL ReadRGB(red%, grn%, blu%, 16) ' Read slots 16, then
  CALL WriteRGB(red%, grn%, blu%, 7) ' shift to slot 7.
  '
  FOR t% = 16 TO 22
    CALL ReadRGB(red%, grn%, blu%, t% + 1) ' Read slots 17-23, then
    CALL WriteRGB(red%, grn%, blu%, t%)    ' shift to slots 16-22.
  NEXT t%
  '
  CALL WriteRGB(ored%, ogrn%, oblu%, 23)  ' Write what was in slot 1 to
  '                                       ' slot 23.
  '
  FOR dlay = 1 TO 12000: NEXT dlay ' May need to play with this, depending
  '                                  on your machine...
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

SUB WriteRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C8, slot% ' Write RGB values to slot
  '
  OUT &H3C9, red%
  OUT &H3C9, grn%
  OUT &H3C9, blu%
  '
END SUB

