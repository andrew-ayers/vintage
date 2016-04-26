DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
'
SCREEN 9
'
FOR t% = 0 TO 3
  CALL WriteRGB(t% * 16, 0, 0, t%)
NEXT t%
'
FOR t% = 0 TO 3
  CALL WriteRGB(0, t% * 16, 0, 4 + t%)' This is weird - slot 6 should be set
                                       'to a color, but always comes out brown
NEXT t%
'
FOR t% = 0 TO 3
  CALL WriteRGB(0, 0, t% * 16, 56 + t%)
NEXT t%
'
FOR t% = 0 TO 3
  CALL WriteRGB(t% * 16, t% * 16, t% * 16, 60 + t%)
NEXT t%
'
FOR t% = 0 TO 15
  LINE (t% * 10, 0)-(t% * 10 + 10, 199), t%, BF
NEXT t%
'

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

