'
' Description : Interference Demo
' Written by  : Andrew L. Ayers
' Date        : 08/15/96
'
' This little demo shows how to achieve the classic interference
' pattern so often seen in demos (not much anymore, though). It
' works best on a 486 or better!
'
' You may use this routine in any manner you like, as long
' as you give credit in an appropriate manner. Have phun!
'
DIM STAB(720), CTAB(720)
'
SCREEN 13
'
' Build SIN/COS tables for speed!
'
FOR T% = 0 TO 720
  '
  STAB(T%) = SIN((6.28 / 360) * T%)
  CTAB(T%) = COS((6.28 / 360) * T%)
  '
NEXT
'
' Begin demo...
'
done% = 0
'
DO
 '
  FOR TT% = 0 TO 360 STEP 5
    '
    X1% = 160 + STAB(TT%) * 40
    Y1% = 100 + CTAB(TT%) * 20
    X2% = 160 + STAB(TT% + 180) * 10
    Y2% = 100 + CTAB(TT% + 180) * 40
    '
    FOR T% = 0 TO 150 STEP 6
      '
      CIRCLE (OX1%, OY1%), T%, 0
      CIRCLE (X1%, Y1%), T%, 9
      CIRCLE (OX2%, OY2%), T%, 0
      CIRCLE (X2%, Y2%), T%, 12
      '
    NEXT
    '
    LOCATE 2, 4: PRINT "Interference Demo by Andrew Ayers"
    '
    OX1% = X1%: OY1% = Y1%
    OX2% = X2%: OY2% = Y2%
    '
    IF INKEY$ <> "" THEN done% = 1: EXIT FOR
    '
  NEXT
  '
LOOP UNTIL done%

