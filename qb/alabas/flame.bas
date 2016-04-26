DECLARE SUB SetBurnPalette ()
DECLARE SUB BurnPrint (x%, y%, text$, iter%)
'
SCREEN 13
'
' Initialize pallete
'
CALL SetBurnPalette
'
CIRCLE (159, 100), 2, 47
PAINT (159, 100), 47, 47
'
DO
  FOR y% = 98 TO 69 STEP -1
    FOR x% = 157 TO 161
      col% = POINT(x% + (INT(RND * 3) - 1), y%) + POINT(x%, y% + 1)
      col% = (col% \ 2) + INT(RND * 3) - 1
      '                               
      IF col% < 0 THEN
        col% = 0
      ELSE
        IF col% > 47 THEN col% = 47
      END IF
      '
      PSET (x%, y%), col%
    NEXT
  NEXT
  '
LOOP

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

