'
' BurnPrint routine By Angelo KEN Pesce 1997
' Palette routine by PHOBIA
' Additional mods and comments by Andrew L. Ayers 1997
'
' You may use those routines as you wish
'
' Authors may be contacted at:
'
'   ken@uniserv.uniplan.it
'   andrewa@indirect.com
'
DECLARE SUB SetBurnPalette ()
DECLARE SUB BurnPrint (x%, y%, text$, iter%)
'
SCREEN 13
'
' Initialize pallete
'
CALL SetBurnPalette
'
CALL BurnPrint(8, 13, "BurnPrint - Press any key", 20)

SUB BurnPrint (x%, y%, text$, iter%)
  '
  ' Print the text string
  '
  COLOR 48: LOCATE y%, x%: PRINT text$
  '
  ' Wait for a keypress
  '
  DO: LOOP WHILE INKEY$ = ""
  '
  ' Calculate the text position
  '
  length% = LEN(text$)
  '
  xstart% = (x% * 8) - 8
  xend% = ((x% + length%) * 8) - 8
  ystart% = (y% * 8) - 16
  yend% = (y% * 8)
  '
  ' Loop through calculated text field
  '
  FOR r% = 1 TO iter%
    FOR y% = ystart% TO yend%
      FOR x% = xstart% TO xend%
        '
        ' Get a random value from -1 to 1
        '
        os% = INT(RND * 3)
        '
        ' Use as a slot offset to drive burn function, which is a simple
        ' pixel color interpolation routine
        '
        col% = (POINT(x% + (os% - 1), y%) + POINT(x%, y% + 1)) \ 2 - os%
        '
        ' Keep in range
        '
        IF col% < 0 THEN col% = 0
        '
        ' Plot the new color
        '
        PSET (x%, y%), col%
        '
      NEXT
    NEXT
  NEXT
  '
END SUB

SUB SetBurnPalette
  '
  ' Original routine by PHOBIA
  '
  FOR slot% = 0 TO 63
    '
    ' Fade from black to red
    '
    OUT &H3C7, slot% / 4 ' Slots 0-15
    OUT &H3C9, slot%
    OUT &H3C9, 0
    OUT &H3C9, 0
    '
    ' Fade from red to yellow
    '
    OUT &H3C7, slot% / 4 + 16 ' Slots 16-31
    OUT &H3C9, 63
    OUT &H3C9, slot%
    OUT &H3C9, 0
    '
    ' Fade from yellow to white
    '
    OUT &H3C7, slot% / 4 + 32 ' Slots 32-47
    OUT &H3C9, 63
    OUT &H3C9, 63
    OUT &H3C9, slot%
    '
  NEXT
  '
END SUB

