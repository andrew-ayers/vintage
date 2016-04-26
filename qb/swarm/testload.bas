DECLARE SUB Init ()
DECLARE SUB LoadPal (file$)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
'
TYPE RGBTriple
  red AS STRING * 1
  grn AS STRING * 1
  blu AS STRING * 1
END TYPE
'
CALL Init
'
a$ = INPUT$(1)
'
SCREEN 0: WIDTH 80: CLS

SUB Init
  '
  SCREEN 13
  '
  CALL LoadPal("swarm1.pal")
  '
  DEF SEG = &HA000
  BLOAD "gather1.bin", 0
  DEF SEG
  '
END SUB

SUB LoadPal (file$)
  '
  DIM RGB AS RGBTriple
  '
  OPEN file$ FOR BINARY AS 1
  '
  tt% = 1
  FOR t% = 0 TO 255
    GET #1, tt%, RGB: tt% = tt% + 3
    '
    red% = ASC(RGB.red$) - 32
    grn% = ASC(RGB.grn$) - 32
    blu% = ASC(RGB.blu$) - 32
    '
    CALL WriteRGB(red%, grn%, blu%, t%)
    '
  NEXT t%
  '
  CLOSE #1
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

