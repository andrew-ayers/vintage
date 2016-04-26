DIM x(3), y(3), ox(3), oy(3)
'
rx = 100
ry = 20
xc = 320
yc = 175
height = 80
stp = (6.28 / 4)
'
SCREEN 9, , 0, 1
'
DO
  FOR t = 0 TO stp STEP .1
    '
    FOR points = 0 TO 3
      x(points) = xc + SIN(t + (stp * points)) * rx
      y(points) = yc + COS(t + (stp * points)) * ry
      '
      ' Draw Lines
      '
      LINE (x(points), y(points))-(x(points), (yc + height) - y(points)), 15
      '
      IF points > 0 THEN
        LINE (ox(points - 1), oy(points - 1))-(x(points), y(points)), 15
        LINE (ox(points - 1), (yc + height) - oy(points - 1))-(x(points), (yc + height) - y(points)), 15
      END IF
      '
      IF points = 3 THEN
        LINE (x(points), y(points))-(ox(0), oy(0)), 15
        LINE (x(points), (yc + height) - y(points))-(ox(0), (yc + height) - oy(0)), 15
      END IF
      '
      ox(points) = x(points): oy(points) = y(points)
      '
    NEXT points
    '
    PCOPY 0, 1
    '
    FOR points = 0 TO 3
      '
      x(points) = xc + SIN(t + (stp * points)) * rx
      y(points) = yc + COS(t + (stp * points)) * ry
      '
      ' Erase Lines
      '
      LINE (ox(points), oy(points))-(ox(points), (yc + height) - oy(points)), 0
      '
      IF points > 0 THEN
        LINE (ox(points - 1), oy(points - 1))-(x(points), y(points)), 0
        LINE (ox(points - 1), (yc + height) - oy(points - 1))-(x(points), (yc + height) - y(points)), 0
      END IF
      '
      IF points = 3 THEN
        LINE (x(points), y(points))-(ox(0), oy(0)), 0
        LINE (x(points), (yc + height) - y(points))-(ox(0), (yc + height) - oy(0)), 0
      END IF
      '
      ox(points) = x(points): oy(points) = y(points)
      '
    NEXT points
    '
  NEXT t
LOOP

