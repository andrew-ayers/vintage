z'
' Description : Explosions - VGA mode 13 special effect
' Written by  : Andrew L. Ayers
' Date        : 10/22/96
'
' The name says it all!
'
' You may use this routine in any manner you like, as long
' as you give credit in an appropriate manner. Have phun!
'
SCREEN 13
'
' Set up arrays for our explosion data
'
DIM x(50), y(50), xv(50), yv(50), ox(50), oy(50)
'
DO
  '
  ' Initialize an explosion
  '
  FOR t% = 0 TO 50
    x(t%) = 0
    y(t%) = 0
    dir = RND * 6.28: vel = INT(RND * 5) + 1
    xv(t%) = SIN(dir) * vel
    yv(t%) = COS(dir) * vel
  NEXT t%
  '
  ' Initialize offsets and color
  '
  tx% = INT(RND * 320)
  ty% = INT(RND * 200)
  c% = 31: done% = 0
  '
  ' Print the title
  '
  LOCATE 1, 6: PRINT "Explosions by Andrew L. Ayers"
  LOCATE 23, 8: PRINT "Press any key to exit demo"
  '
  ' Explode!
  '
  DO
    '
    ' Move all the pieces
    '
    FOR t% = 0 TO 50
      '
      ' Erase an old piece
      '
      LINE (ox(t%) + tx%, oy(t%) + ty%)-(x(t%) + tx%, y(t%) + ty%), 0
      ox(t%) = x(t%): oy(t%) = y(t%)
      '
      ' Move the piece
      '
      x(t%) = x(t%) + xv(t%)
      y(t%) = y(t%) + yv(t%)
      '
      ' Draw it at new position
      '
      LINE -(x(t%) + tx%, y(t%) + ty%), c%
      '
    NEXT
    '
    ' Decrement color to "fade"
    '
    c% = c% - 1: IF c% < 16 THEN done% = 1 ' Do another explosion if done
    '
    IF INKEY$ <> "" THEN done% = 2 ' Exit on any key press
    '
    FOR dlay = 1 TO 5000: NEXT dlay ' Change to suit your machine
    '
  LOOP UNTIL done%
  '
LOOP UNTIL done% = 2
'
CLS

