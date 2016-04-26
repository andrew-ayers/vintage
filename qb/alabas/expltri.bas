'
' Description : Exploding Pieces - Special Effect Routine
' Written by  : Andrew L. Ayers
' Date        : 09/12/96
'
' This shows off a pretty CPU intensive "explosion" of colored
' triangles, all spinning and moving away from a common center at
' different velocitys. Perhaps it could be modified to be used in
' a game or something. I wrote it after reading a post on one of
' the newsgroups about someone needing to create an explosion -
' I hope this helps some...
'
' As always, if you use this in any of your creations, please consider your
' source and mention my name. Thanx, and have phun!
'
SCREEN 7, , 1, 0
'
CONST MAX.NUM = 60
'
' Define a triangle
'
TYPE triangle
  px1 AS INTEGER
  py1 AS INTEGER
  px2 AS INTEGER
  py2 AS INTEGER
  px3 AS INTEGER
  py3 AS INTEGER
  '
  nx1 AS INTEGER
  ny1 AS INTEGER
  nx2 AS INTEGER
  ny2 AS INTEGER
  nx3 AS INTEGER
  ny3 AS INTEGER
  '
  r AS INTEGER
  '
  x AS INTEGER
  y AS INTEGER
  '
  vx AS INTEGER
  vy AS INTEGER
END TYPE
'
' Set up a whole mess of 'em
'
DIM pieces(MAX.NUM) AS triangle
'
' Initialize them
'
FOR t% = 0 TO MAX.NUM
  '
  pieces(t%).px1 = 0: pieces(t%).py1 = -5
  pieces(t%).px2 = 5: pieces(t%).py2 = 5
  pieces(t%).px3 = -5: pieces(t%).py3 = 5
  '
  pieces(t%).nx1 = 0: pieces(t%).ny1 = -5
  pieces(t%).nx2 = 5: pieces(t%).ny2 = 5
  pieces(t%).nx3 = -5: pieces(t%).ny3 = 5
  '
  pieces(t%).r = INT(RND * 360)
  '
  pieces(t%).x = 160
  pieces(t%).y = 100
  '
  DO
    '
    pieces(t%).vx = (INT(RND * 17) - 8)
    pieces(t%).vy = (INT(RND * 17) - 8)
    '
  LOOP UNTIL pieces(t%).vx <> 0 AND pieces(t%).vy <> 0
  '
NEXT
'
' Build SIN/COS tables for speed
'
DIM sintable(359), costable(359)
'
rad = (3.14159 * 2) / 360
'
FOR t% = 0 TO 359
  '
  sintable(t%) = SIN(t% * rad)
  costable(t%) = COS(t% * rad)
  '
NEXT t%
'
r% = 0
'
' Explode!
'
DO
  '
  done% = 1
  '
  ' ERASE LAST IMAGE
  '
  LINE (0, 0)-(319, 199), 0, BF
  '
  FOR t% = 0 TO MAX.NUM
    '
    IF pieces(t%).x >= 0 AND pieces(t%).x <= 319 AND pieces(t%).y >= 0 AND pieces(t%).y <= 199 THEN
      '
      ' CALCULATE POSITION OF NEW IMAGE
      '
      lx1 = pieces(t%).px1 * sintable(r%) + pieces(t%).py1 * costable(r%)
      ly1 = pieces(t%).px1 * costable(r%) - pieces(t%).py1 * sintable(r%)
      lx2 = pieces(t%).px2 * sintable(r%) + pieces(t%).py2 * costable(r%)
      ly2 = pieces(t%).px2 * costable(r%) - pieces(t%).py2 * sintable(r%)
      lx3 = pieces(t%).px3 * sintable(r%) + pieces(t%).py3 * costable(r%)
      ly3 = pieces(t%).px3 * costable(r%) - pieces(t%).py3 * sintable(r%)
      '
      pieces(t%).nx1 = pieces(t%).x + lx1
      pieces(t%).ny1 = pieces(t%).y + ly1
      pieces(t%).nx2 = pieces(t%).x + lx2
      pieces(t%).ny2 = pieces(t%).y + ly2
      pieces(t%).nx3 = pieces(t%).x + lx3
      pieces(t%).ny3 = pieces(t%).y + ly3
      '
      ' DRAW NEW IMAGE
      '
      c% = INT(RND * 16)
      '
      LINE (pieces(t%).nx1, pieces(t%).ny1)-(pieces(t%).nx2, pieces(t%).ny2), c%
      LINE -(pieces(t%).nx3, pieces(t%).ny3), c%
      LINE -(pieces(t%).nx1, pieces(t%).ny1), c%
      '
      PAINT (pieces(t%).x, pieces(t%).y), c%
      '
      pieces(t%).x = pieces(t%).x + pieces(t%).vx
      pieces(t%).y = pieces(t%).y + pieces(t%).vy
      '
      done% = 0
      '
    END IF
    '
  NEXT
  '
  r% = r% + 15: IF r% > 359 THEN r% = 0
  '
  ' SHOW IMAGE ON SCREEN
  '
  PCOPY 1, 0
  '
LOOP UNTIL done%



