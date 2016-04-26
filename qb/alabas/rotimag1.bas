DECLARE SUB RotateImage (angle!)
'
CONST PI = 3.14159
'
SCREEN 7, , 1, 0
'
FOR t% = 0 TO 7
  LINE (7 - t%, 7 - t%)-(8 + t%, 8 + t%), t% * 2, B
NEXT
'
DO
  FOR angle = 0 TO 2 * PI STEP PI / 15
    LINE (159, 99)-(189, 129), 0, BF
    CALL RotateImage(angle)
    IF INKEY$ <> "" THEN END
    PCOPY 1, 0
  NEXT
LOOP

SUB RotateImage (angle)
  '
  cx1% = 8
  cy1% = 8
  cx2% = 15
  cy2% = 15
  '
  FOR x2% = 0 TO cx2% - 1
    '  
    FOR y2% = 0 TO cy2% - 1
      '
      IF x2% = 0 THEN
        a = PI / 2
      ELSE
        a = ATN(y2% / x2%)
      END IF
      '
      r = SQR(x2% * x2% + y2% * y2%)
      '  
      x1% = CINT(r * COS(a + angle))
      y1% = CINT(r * SIN(a + angle))
      '
      c0% = POINT(cx1% + x1%, cy1% + y1%)
      c1% = POINT(cx1% - x1%, cy1% - y1%)
      c2% = POINT(cx1% + y1%, cy1% - x1%)
      c3% = POINT(cx1% - y1%, cy1% + x1%)
      '
      IF c0% <> -1 THEN PSET (159 + cx2% + x2%, 99 + cy2% + y2%), c0%
      IF c1% <> -1 THEN PSET (159 + cx2% - x2%, 99 + cy2% - y2%), c1%
      IF c2% <> -1 THEN PSET (159 + cx2% + y2%, 99 + cy2% - x2%), c2%
      IF c3% <> -1 THEN PSET (159 + cx2% - y2%, 99 + cy2% + x2%), c3%
      '
    NEXT
    '
  NEXT
  '
END SUB

