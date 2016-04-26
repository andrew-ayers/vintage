DEFINT A-Z
'
SCREEN 12
'
LINE (0, 0)-(639, 0), 14
LINE (0, 0)-(0, 479), 14
'
idist = 2: wid = 32: hgt = 32: tile = 2
'
FOR tiley = 0 TO 13
  FOR tilex = 0 TO 17
    '
    x1 = ((tilex + 1) * idist) + (tilex * wid)
    x2 = x1 + (wid - 1)
    y1 = ((tiley + 1) * idist) + (tiley * hgt)
    y2 = y1 + hgt
    '
    LINE (x1, y1)-(x2, y2), 15, B
  NEXT
NEXT
'
DO: LOOP UNTIL INKEY$ <> ""

