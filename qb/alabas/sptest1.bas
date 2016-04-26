DEFINT A-Z
'
num.sprites = 3
'
DIM sprites(130 * num.sprites)
'
SCREEN 13
'
' Sprite 0
'
CLS
LINE (0, 0)-(15, 15), 4, BF
CIRCLE (7, 7), 7, 15
PAINT (7, 7), 11, 15
'
GET (0, 0)-(15, 15), sprites(0)
'
' Sprite 1
'
CLS
LINE (0, 0)-(15, 15), 4, BF
LINE (7, 0)-(15, 15), 15
LINE -(0, 15), 15
LINE -(7, 0), 15
PAINT (7, 7), 5, 15
'
GET (0, 0)-(15, 15), sprites(130)
'
' Sprite 2
'
CLS
LINE (0, 0)-(15, 15), 4, BF
'
GET (0, 0)-(15, 15), sprites(260)
'
CLS
'
DO
  FOR y = 0 TO 9
    FOR x = 0 TO 9
      sp = INT(RND * 3)
      PUT (x * 16, y * 16), sprites(sp * 130), PSET
    NEXT x
  NEXT y
LOOP UNTIL INKEY$ <> ""

