TYPE DPoint
  x AS LONG
  y AS LONG
  z AS LONG
  xx AS INTEGER
  yy AS INTEGER
  col AS INTEGER
END TYPE

DIM Stars(1500) AS DPoint

SCREEN 13

FOR t% = 0 TO 1500
  Stars(t%).x = INT(RND * 4096) - 2048
  Stars(t%).y = INT(RND * 2048) - 1024
  Stars(t%).z = INT(RND * 32) + 1
NEXT t%

dir% = 2

DO
  FOR t% = 0 TO 200
    PSET (Stars(t%).xx, Stars(t%).yy), 0

    Stars(t%).xx = 160 + Stars(t%).x \ Stars(t%).z
    Stars(t%).yy = 100 + Stars(t%).y \ Stars(t%).z

    PSET (Stars(t%).xx, Stars(t%).yy), Stars(t%).col

    Stars(t%).z = Stars(t%).z - dir%
   
    IF Stars(t%).z < 1 THEN
      Stars(t%).x = INT(RND * 4096) - 2048
      Stars(t%).y = INT(RND * 2048) - 1024
      Stars(t%).z = 32
    END IF
   
    IF Stars(t%).z > 32 THEN
      Stars(t%).x = INT(RND * 4096) - 2048
      Stars(t%).y = INT(RND * 2048) - 1024
      Stars(t%).z = 1
    END IF

    Stars(t%).col = 31 - (Stars(t%).z \ 2): IF Stars(t%).col < 16 THEN Stars(t%).col = 16

  NEXT t%

  t = TIMER: DO: LOOP UNTIL TIMER > t

LOOP

