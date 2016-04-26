DECLARE SUB DrawBox (x1%, y1%, x2%, y2%, flag%)
'
SCREEN 12
'
CALL DrawBox(0, 0, 639, 479, 0)
CALL DrawBox(0, 0, 639, 15, 1)
'
CALL DrawBox(3, 3, 12, 12, 2)
'
CALL DrawBox(200, 100, 515, 300, 1)
CALL DrawBox(200, 100, 515, 115, 1)
CALL DrawBox(498, 117, 513, 298, 2)
CALL DrawBox(499, 135, 512, 150, 1)
CALL DrawBox(202, 117, 496, 298, 2)
CALL DrawBox(484, 102, 496, 113, 1)
CALL DrawBox(498, 102, 513, 113, 2)
'
LINE (203, 118)-(495, 297), 15, BF

SUB DrawBox (x1%, y1%, x2%, y2%, flag%)
  '
  LINE (x1%, y1%)-(x2%, y2%), 7, BF
  '
  col1% = 7: col2% = 7
  IF flag% = 1 THEN col1% = 15: col2% = 8
  IF flag% = 2 THEN col1% = 8: col2% = 15
  '
  LINE (x1%, y1%)-(x2%, y1%), col1%
  LINE (x1%, y1%)-(x1%, y2%), col1%
  '
  LINE (x2%, y1%)-(x2%, y2%), col2%
  LINE (x1% + 1, y2%)-(x2%, y2%), col2%
  '
END SUB

