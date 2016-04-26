'
' Set the maximum number of particles to use, may need to be
' adjusted for different gravity and pressure vectors for "water"
' to flow right
'
Max% = 300
'
DIM px(Max%), py%(Max%), ox(Max%), oy%(Max%), xv(Max%), yv%(Max%), c%(Max%)
'
gravity% = 1: pressure% = 12 ' Gravity and water pressure vectors - play with!
'
' Initialize Particles
'
FOR tt% = 0 TO Max% - 1
  py%(tt%) = 999
NEXT tt%
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
  FOR tt% = 0 TO Max% - 1
    '
    IF py%(tt%) <> 999 THEN
      '
      ' Only draw if not off-screen
      '
      IF px(tt%) >= 0 AND px(tt%) <= 319 AND py%(tt%) >= 0 THEN
        '
        ' I originally thought particles (ie, pset) would look more
        ' like water, but lines actually work better.
        '
        LINE (ox(tt%), oy%(tt%))-(px(tt%), py%(tt%)), c%(tt%)
      END IF
      '
      ox(tt%) = px(tt%): oy%(tt%) = py%(tt%) ' Endpoint of line
      '
      ' Add velocity vectors to the position coords
      '
      px(tt%) = px(tt%) + xv(tt%)
      py%(tt%) = py%(tt%) + yv%(tt%)
      '
      ' Is the particle in the basin? If so, then reset particle
      '
      IF py%(tt%) > 199 THEN py%(tt%) = 999
      '
      ' Apply gravity vector to the y-velocity vector
      '
      yv%(tt%) = yv%(tt%) + gravity%
    END IF
  NEXT tt%
  '
  ' Now draw the basin
  '
  LINE (30, 170)-(290, 200), 8, BF
  LINE (70, 170)-(250, 200), 7, BF
  LINE (120, 170)-(200, 200), 15, BF
  '
  ' Wait for verticle retrace before copying to visible page
  '
  WAIT &H3DA, 8
  PCOPY 1, 0
LOOP UNTIL INKEY$ <> ""
'
STOP
'

NewParticle:
  '
  ' Find first available slot and fill it
  '
  FOR tt% = 0 TO Max% - 1
    IF py%(tt%) = 999 THEN ' Empty slots marked as "999"
      '
      ' Set initial starting position at top of pipe
      '
      px(tt%) = 160 + (INT(RND * 7) - 3): py%(tt%) = 140
      ox(tt%) = px(tt%): oy%(tt%) = py%(tt%)
      '
      ' Set horizontal and vertical velocity vectors
      '
      xv(tt%) = (INT(RND * 7) - 3) * RND
      yv%(tt%) = -pressure%
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
      c%(tt%) = col%
      '
      EXIT FOR
    END IF
  NEXT tt%
  '
RETURN

