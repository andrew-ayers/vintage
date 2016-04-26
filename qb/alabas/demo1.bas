DECLARE SUB dlin (x1%, y1%, x2%, y2%, colr%)
DECLARE SUB FirePrint (h%, v%, a$, tilt%)
'
SCREEN 13
'
' Set up an all "red" palette
'
FOR T = 0 TO 63: PALETTE T, T: NEXT T
'
' Call the routine once for a simple "flame" effect,
' or over and over (as done here) for a great "burning"
' effect! Use uppercase for best effect.
'
DO
  CALL FirePrint(9, 12, "FLAMIN! - A GRAFIX DEMO", 0)
  CALL FirePrint(8, 14, "BY ANDREW L. AYERS - 1996", 0)
LOOP UNTIL INKEY$ <> ""
'
DIM a%(500), b%(500)
DIM bx%(40), by%(40), dx%(40), dy%(40)
'
RANDOMIZE TIMER
'
CLS
'
FOR T = 0 TO 63: PALETTE T, 0: NEXT T
'
SCREEN 13: CLS : COLOR 15
'
' Create "Sphere"
'
o = 0
FOR T% = 14 TO 0 STEP -1
  c% = 31 - T%
  CIRCLE (15 + o, 15 + o), T%, c%
  PAINT (15 + o, 15 + o), c%
  o = o - .4
NEXT T%
'
GET (0, 0)-(30, 30), a%
'
' Create Mask
'
LINE (0, 0)-(30, 30), 255, BF
CIRCLE (15, 15), 14, 0: PAINT (15, 15), 0
'
GET (0, 0)-(30, 30), b%
'
PALETTE
'
' Initialize Ballz
'
FOR T = 0 TO 39
  bx%(T) = INT(RND * 200) + 50: by%(T) = INT(RND * 100) + 50
  DO
    dx%(T) = INT(RND * 11) - 5: dy%(T) = INT(RND * 11) - 5
  LOOP UNTIL dx%(T) <> 0 AND dy%(T) <> 0
NEXT T
'
' Move Ballz
'
numbalz% = 10: tick% = 0: show% = 1
'
LOCATE 12, 12: PRINT "I have ballz, man!"
T = INT(TIMER)
DO: LOOP UNTIL INT(TIMER - T) > 2
'
DO
  WAIT &H3DA, 8              ' Wait for vertical retrace
  'LINE (0, 0)-(319, 199), 0, BF
  IF show% THEN LOCATE 12, 12: PRINT "This is" + STR$(numbalz%) + " ballz!"
  FOR T% = 0 TO numbalz% - 1
    PUT (bx%(T%), by%(T%)), b%, AND
    bx%(T%) = bx%(T%) + dx%(T%): by%(T%) = by%(T%) + dy%(T%)
    IF bx%(T%) > 280 OR bx%(T%) < 10 THEN dx%(T%) = -dx%(T%)
    IF by%(T%) > 160 OR by%(T%) < 10 THEN dy%(T%) = -dy%(T%)
    PUT (bx%(T%), by%(T%)), b%, AND: PUT (bx%(T%), by%(T%)), a%, OR
  NEXT T%
  tick% = tick% + 1: IF tick% = 100 THEN tick% = 0: numbalz% = numbalz% + 10
  IF numbalz% > 30 THEN numbalz% = 30: show% = 0
LOOP UNTIL INKEY$ <> ""
'
CLS
'
SCREEN 7, , 1, 0
'
DIM px(10), py(10), PZ(10), YAW(10), PIT(10), ROL(10), NX(10), NY(10)
DIM STAB(359), CTAB(359)
'
VD = 250: DCOR = 30
'
px(1) = 50: px(2) = 50: px(3) = -50: px(4) = -50
py(1) = 50: py(2) = 50: py(3) = 50: py(4) = 50
PZ(1) = 50: PZ(2) = -50: PZ(3) = -50: PZ(4) = 50
'
FOR T = 4 TO 1 STEP -1
  px(T + 4) = px(5 - T)
  py(T + 4) = py(5 - T) - 100
  PZ(T + 4) = PZ(5 - T)
NEXT T
'
FOR T = 1 TO 8
  YAW(T) = 0
  PIT(T) = 0
  ROL(T) = 0
NEXT T
'
FOR T = 0 TO 359
  STAB(T) = SIN((6.282 / 360) * T)
  CTAB(T) = COS((6.282 / 360) * T)
NEXT T
'
DO
  '
  ' ERASE LAST IMAGE
  '
  LINE (NX(1), NY(1))-(NX(2), NY(2)), 0
  LINE -(NX(3), NY(3)), 0
  LINE -(NX(4), NY(4)), 0
  LINE -(NX(1), NY(1)), 0
  '
  LINE (NX(5), NY(5))-(NX(6), NY(6)), 0
  LINE -(NX(7), NY(7)), 0
  LINE -(NX(8), NY(8)), 0
  LINE -(NX(5), NY(5)), 0
  '
  LINE (NX(1), NY(1))-(NX(8), NY(8)), 0
  LINE (NX(2), NY(2))-(NX(7), NY(7)), 0
  LINE (NX(3), NY(3))-(NX(6), NY(6)), 0
  LINE (NX(4), NY(4))-(NX(5), NY(5)), 0
  '
  ' CALCULATE POSITION OF NEW IMAGE
  '
  FOR T = 1 TO 8
    '
    ' TRANSLATE, THEN ROTATE
    '
    TX = px(T)
    TY = -py(T)
    TZ = PZ(T)
    '
    ' ROTATE (PIT)
    '
    SX = TY * STAB(PIT(T)) + TX * CTAB(PIT(T))
    SY = TY * CTAB(PIT(T)) - TX * STAB(PIT(T))
    SZ = TZ
    '
    ' ROTATE (ROL)
    '
    QX = SX
    QY = SZ * STAB(ROL(T)) + SY * CTAB(ROL(T))
    QZ = SZ * CTAB(ROL(T)) - SY * STAB(ROL(T))
    '
    ' ROTATE (YAW)
    '
    RX = QZ * STAB(YAW(T)) + QX * CTAB(YAW(T))
    RY = QY
    RZ = QZ * CTAB(YAW(T)) - QX * STAB(YAW(T))
    '
    ' ROTATE, THEN TRANSLATE
    '
    'RX = RX + 50
    RZ = RZ + 50
    '
    NX(T) = 160 + ((VD * RX) / (RZ + VD))
    NY(T) = 100 + ((VD * RY) / (RZ + VD + DCOR))
    '
    PIT(T) = PIT(T) + 3
    IF PIT(T) > 359 THEN PIT(T) = 0
    '
    YAW(T) = YAW(T) + 2
    IF YAW(T) > 359 THEN YAW(T) = 0
    '
    ROL(T) = ROL(T) + 4
    IF ROL(T) > 359 THEN ROL(T) = 0
    '
  NEXT T
  '
  ' DRAW NEW IMAGE
  '
  LINE (NX(1), NY(1))-(NX(2), NY(2)), 15
  LINE -(NX(3), NY(3)), 15
  LINE -(NX(4), NY(4)), 15
  LINE -(NX(1), NY(1)), 15
  '
  LINE (NX(5), NY(5))-(NX(6), NY(6)), 15
  LINE -(NX(7), NY(7)), 15
  LINE -(NX(8), NY(8)), 15
  LINE -(NX(5), NY(5)), 15
  '
  LINE (NX(1), NY(1))-(NX(8), NY(8)), 15
  LINE (NX(2), NY(2))-(NX(7), NY(7)), 15
  LINE (NX(3), NY(3))-(NX(6), NY(6)), 15
  LINE (NX(4), NY(4))-(NX(5), NY(5)), 15
  '
  ' SHOW IMAGE ON SCREEN
  '
  COLOR 4: LOCATE 23, 8: PRINT "A SIMPLE WIREFRAME CUBE..."
  '
  PCOPY 1, 0
  '
LOOP UNTIL INKEY$ <> ""
'
SCREEN 13: CLS
'
DO
  x1% = INT(RND * 320)
  x2% = INT(RND * 320)
  y1% = INT(RND * 200)
  y2% = INT(RND * 200)
  colr% = INT(RND * 16)
  '
  CALL dlin(x1%, y1%, x2%, y2%, colr%)
  '
  LOCATE 12, 9: PRINT "A QUICK LINE ROUTINE..."
  '
LOOP UNTIL INKEY$ <> ""
'
CLS
'
DEFINT A-Z
'
SCREEN 13
'
REDIM px(1000), py(1000), PZ(1000), NX(1000), NY(1000)
'
VD = 100
'
FOR T = 1 TO 1000
  px(T) = (INT(RND * 300) - 150) * VD: py(T) = (INT(RND * 300) - 150) * VD: PZ(T) = INT(RND * 300) + 1
NEXT T
'
DO
  FOR T = 1 TO 1000
    PSET (NX(T), NY(T)), 0
    NX(T) = 160 + (px(T)) / PZ(T)
    NY(T) = 100 + (py(T)) / PZ(T)
    PSET (NX(T), NY(T)), 15
    PZ(T) = PZ(T) - 15
    IF PZ(T) <= 0 THEN PZ(T) = 300
  NEXT T
  COLOR 5: LOCATE 13, 13: PRINT "Look! 1000 Starz!"
LOOP UNTIL INKEY$ <> ""
'
CLS
'
' Set the maximum number of particles to use, may need to be
' adjusted for different gravity and pressure vectors for "water"
' to flow right
'
Max% = 300
'
REDIM px(Max%), py%(Max%), ox(Max%), oy%(Max%), xv(Max%), yv%(Max%), c%(Max%)
'
gravity% = 1: pressure% = 12 ' Gravity and water pressure vectors - play with!
'
' Initialize Particles
'
FOR TT% = 0 TO Max% - 1
  py%(TT%) = 999
NEXT TT%
'
SCREEN 7, , 1, 0
'
DO
  CLS 0
  '
  ' Draw the fountain pipe
  '
  LINE (156, 141)-(164, 200), 8, BF
  LINE (157, 141)-(163, 200), 7, BF
  LINE (159, 141)-(161, 200), 15, BF
  '
  ' Get some particle ready
  '
  FOR num% = 1 TO 10
    GOSUB NewParticle
  NEXT num%
  '
  ' Draw the particles
  '
  FOR TT% = 0 TO Max% - 1
    '
    IF py%(TT%) <> 999 THEN
      '
      ' Only draw if not off-screen
      '
      IF px(TT%) >= 0 AND px(TT%) <= 319 AND py%(TT%) >= 0 THEN
        '
        ' I originally thought particles (ie, pset) would look more
        ' like water, but lines actually work better.
        '
        LINE (ox(TT%), oy%(TT%))-(px(TT%), py%(TT%)), c%(TT%)
      END IF
      '
      ox(TT%) = px(TT%): oy%(TT%) = py%(TT%) ' Endpoint of line
      '
      ' Add velocity vectors to the position coords
      '
      px(TT%) = px(TT%) + xv(TT%)
      py%(TT%) = py%(TT%) + yv%(TT%)
      '
      ' Is the particle in the basin? If so, then reset particle
      '
      IF py%(TT%) > 199 THEN py%(TT%) = 999
      '
      ' Apply gravity vector to the y-velocity vector
      '
      yv%(TT%) = yv%(TT%) + gravity%
    END IF
  NEXT TT%
  '
  ' Now draw the basin
  '
  LINE (30, 170)-(290, 200), 8, BF
  LINE (70, 170)-(250, 200), 7, BF
  LINE (120, 170)-(200, 200), 15, BF
  '
  ' Wait for verticle retrace before copying to visible page
  '
  COLOR 9: LOCATE 2, 13: PRINT "A WATER FOUNTAIN!"
  '
  WAIT &H3DA, 8
  PCOPY 1, 0
LOOP UNTIL INKEY$ <> ""
'
CLS
'
DEFINT A-Z
'
SCREEN 13: PALETTE: CLS
'
LOCATE 12, 12: COLOR 15: PRINT "THAT'S ALL, FOLKS!"
'
T& = INT(TIMER)
DO: LOOP UNTIL INT(TIMER - T&) > 2
'
FOR T = 31 TO 16 STEP -1
  LOCATE 12, 12: COLOR T: PRINT "THAT'S ALL, FOLKS!"
  FOR TT& = 1 TO 50000: NEXT TT&
NEXT T
'
STOP
'
NewParticle:
  '
  ' Find first available slot and fill it
  '
  FOR TT% = 0 TO Max% - 1
    IF py%(TT%) = 999 THEN ' Empty slots marked as "999"
      '
      ' Set initial starting position at top of pipe
      '
      px(TT%) = 160 + (INT(RND * 7) - 3): py%(TT%) = 140
      ox(TT%) = px(TT%): oy%(TT%) = py%(TT%)
      '
      ' Set horizontal and vertical velocity vectors
      '
      xv(TT%) = (INT(RND * 7) - 3) * RND
      yv%(TT%) = -pressure%
      '
      ' Choose color for particle
      '
      cc% = INT(RND * 8): col% = 15 ' Default white
      '
      SELECT CASE cc%
        CASE 1, 2
          col% = 1 ' Dark blue
        CASE 3, 4
          col% = 9 ' Light blue
      END SELECT
      '
      c%(TT%) = col%
      '
      EXIT FOR
    END IF
  NEXT TT%
  '
RETURN

DEFSNG A-Z
SUB dlin (x1%, y1%, x2%, y2%, colr%)
  '
  h% = x1%: v% = y1%
  '
  xdiff% = x2% - x1%
  ydiff% = y2% - y1%
  '
  IF xdiff% < 0 THEN
    xdiff% = -xdiff%
    xunit% = -1
  ELSE
    xunit% = 1
  END IF
  '
  IF ydiff% < 0 THEN
    ydiff% = -ydiff%
    yunit% = -1
  ELSE
    yunit% = 1
  END IF
  '
  errorterm% = 0
  '
  IF xdiff% > ydiff% THEN
    length% = xdiff% + 1
    FOR i% = 1 TO length%
      PSET (h%, v%), colr%
      h% = h% + xunit%
      errorterm% = errorterm% + ydiff%
      IF errorterm% > xdiff% THEN
        v% = v% + yunit%
        errorterm% = errorterm% - xdiff%
      END IF
    NEXT i%
  ELSE
    length% = ydiff% + 1
    FOR i% = 1 TO length%
      PSET (h%, v%), colr%
      v% = v% + yunit%
      errorterm% = errorterm% + xdiff%
      IF errorterm% > 0 THEN
        h% = h% + xunit%
        errorterm% = errorterm% - ydiff%
      END IF
    NEXT i%
  END IF
  '
END SUB

SUB FirePrint (h%, v%, a$, tilt%)
  '
  ' Print the string in bright "red"
  '
  COLOR 63: LOCATE v%, h%: PRINT a$
  '
  ' Set up start and end locations for the burn
  '
  SY% = (v% * 8) - 8: ey% = (v% * 8) - 16
  SX% = (h% * 8) - 8: ex% = ((h% + LEN(a$)) * 8) - 8
  '
  FOR y% = SY% TO ey% STEP -1
    FOR x% = SX% TO ex%
      '
      ' Take the current color, subtract a random amount for
      ' red flame "fade", and plot the new point
      '
      col% = POINT(x%, y%) - RND * 25: IF col% < 0 THEN col% = 0
      '
      PSET (x% + tilt%, y% - 1), col%
      '
    NEXT x%
  NEXT y%
  '
END SUB

