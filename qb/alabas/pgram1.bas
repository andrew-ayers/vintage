SCREEN 13
'
Size = 1
'
R1% = 50 * Size: R2% = 45 * Size
'
' Compute increment table
'
FOR T% = 360 TO 0 STEP -72
  '
  PT(TT%) = (T% * 3.14159) / 180
  TT% = TT% + 1
  '
NEXT
'
' Draw 5 pointed star
'
FOR TT% = 0 TO 4 STEP 2
  X% = 160 - CINT(SIN(PT(TT%)) * R1%)
  Y% = 100 - CINT(COS(PT(TT%)) * R2%)
  IF TT% = 0 THEN
    SX% = X%: SY% = Y%
    LINE (X%, Y%)-(X%, Y%), 15
  ELSE
    LINE -(X%, Y%), 15
  END IF
NEXT
'
FOR TT% = 1 TO 3 STEP 2
  X% = 160 - CINT(SIN(PT(TT%)) * R1%)
  Y% = 100 - CINT(COS(PT(TT%)) * R2%)
  LINE -(X%, Y%), 15
NEXT
LINE -(SX%, SY%), 15
'
' Draw Circle
'
FOR T% = 360 TO 0 STEP -1
  RAD = (T% * 3.14159) / 180
  X% = 160 - CINT(SIN(RAD) * R1%)
  Y% = 100 - CINT(COS(RAD) * R2%)
  IF T% = 0 THEN
    LINE (X%, Y%)-(X%, Y%), 15
  ELSE
    LINE -(X%, Y%), 15
  END IF
NEXT


