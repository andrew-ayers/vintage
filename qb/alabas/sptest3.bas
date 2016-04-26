DEFINT A-Z
'
num.sprites = 3
'
DIM sprites(130 * num.sprites)
'
DEF SEG = VARSEG(sprites(0))
BLOAD "c:\myfiles\data\text\qbasic\sptest2.dat", VARPTR(sprites(0))
DEF SEG
'
SCREEN 13
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

