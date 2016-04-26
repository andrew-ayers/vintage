'
' Description : Spinner - Mode 13 VGA Special Effect Routine
' Written by  : Andrew L. Ayers
' Date        : 10/15/96
'
' This routine doesn't show anything special, code-wise, but it does
' look interesting all the same...
'
' As always, if you use this in any of your creations, please consider your
' source and mention my name. Thanx, and have phun!
'
SCREEN 13
'
x1% = 160: y1% = 100: colr% = 16
'
LOCATE 12, 6: PRINT "Spinner! - By Andrew L. Ayers"
'
t = TIMER: DO: LOOP UNTIL TIMER - t > 5
'
DO
  '
  FOR t% = 0 TO 319
    '
    LINE (x1%, y1%)-(t%, 0), colr%
    '
    colr% = colr% + 1: IF colr% > 47 THEN colr% = 17
    '
  NEXT
  '
  FOR t% = 0 TO 199
    '
    LINE (x1%, y1%)-(319, t%), colr%
    '
    colr% = colr% + 1: IF colr% > 47 THEN colr% = 16
    '
  NEXT
  '
  FOR t% = 319 TO 0 STEP -1
    '
    LINE (x1%, y1%)-(t%, 199), colr%
    '
    colr% = colr% + 1: IF colr% > 47 THEN colr% = 16
    '
  NEXT
  '
  FOR t% = 199 TO 0 STEP -1
    '
    LINE (x1%, y1%)-(0, t%), colr%
    '
    colr% = colr% + 1: IF colr% > 47 THEN colr% = 16
    '
  NEXT
  '
LOOP UNTIL INKEY$ <> ""
'
CLS

