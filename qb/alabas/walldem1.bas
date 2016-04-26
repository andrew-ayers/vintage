DECLARE SUB draw.maze (map%(), xview%, yview%, viewing.angle%, atn.table%(), sin.table!(), cos.table!())
'
CONST WALL.HEIGHT = 64
CONST VIEWER.HEIGHT = 32
CONST VIEWER.DISTANCE = 200
CONST VIEWPORT.LEFT = 0
CONST VIEWPORT.RIGHT = 319
CONST VIEWPORT.TOP = 0
CONST VIEWPORT.BOT = 199
CONST VIEWPORT.HEIGHT = 200
CONST VIEWPORT.CENTER = 100
CONST VIEWPORT.RES = 4
CONST VIEWER.SPEED = 5
CONST RENDER.TYPE = 1 ' 0=Line, 1=Direct Video Memory Access
'
DIM map%(15, 15)
'
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,2,0,3,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,5,0,7,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,2,0,4,0,0,1
DATA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,2,2,0,2,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,3,0,0,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,4,0,3,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,2,0,2,0,0,0,0,0,1
DATA 1,0,0,0,0,0,0,7,0,0,0,0,0,0,0,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
'
FOR y% = 0 TO 15
  FOR x% = 0 TO 15
    READ map%(x%, y%)
  NEXT
NEXT
'
' Pre-Calc SIN, COS, ATN Tables
'
dpr! = 360 / (2 * 3.14159)' Degrees per Radian
'
DIM atn.table%(320), cos.table!(360), sin.table!(360)
'
FOR column% = VIEWPORT.LEFT TO VIEWPORT.RIGHT - 1 STEP VIEWPORT.RES
  '
  degrees% = INT(ATN((column% - 160) / VIEWER.DISTANCE) * dpr!)
  '
  IF degrees% > 359 THEN
    degrees% = degrees% - 360
  ELSE
    IF degrees% < 0 THEN degrees% = 360 + degrees%
  END IF
  '
  atn.table%(column%) = degrees%
  '
NEXT column%
'
FOR t% = 0 TO 359
  cos.table!(t%) = COS(t% / dpr!)
  sin.table!(t%) = SIN(t% / dpr!)
NEXT t%
'
up$ = CHR$(0) + CHR$(72)
dn$ = CHR$(0) + CHR$(80)
lt$ = CHR$(0) + CHR$(75)
rt$ = CHR$(0) + CHR$(77)
esc$ = CHR$(27)
'
viewing.angle% = 0
xview% = 6 * 64
yview% = 8 * 64
'
SCREEN 13
'
done% = 0: DO
  CALL draw.maze(map%(), xview%, yview%, viewing.angle%, atn.table%(), sin.table!(), cos.table!())
  '
  ' Get position BEFORE player moves
  '
  oxview% = xview%: oyview% = yview%
  '
  LOCATE 1, 1: PRINT "QBasic Ray Caster 1.0 by Andrew L. Ayers"
  '
  DO: k$ = INKEY$: LOOP UNTIL k$ <> ""
  '
  SELECT CASE k$
    CASE rt$
      viewing.angle% = viewing.angle% + VIEWER.SPEED
      IF viewing.angle% > 359 THEN viewing.angle% = 360 - viewing.angle%
    CASE lt$
      viewing.angle% = viewing.angle% - VIEWER.SPEED
      IF viewing.angle% < 0 THEN viewing.angle% = 360 + viewing.angle%
    CASE up$
      xview% = xview% + (cos.table!(viewing.angle%) * (3 * VIEWER.SPEED))
      yview% = yview% + (sin.table!(viewing.angle%) * (3 * VIEWER.SPEED))
    CASE dn$
      xview% = xview% - (cos.table!(viewing.angle%) * (3 * VIEWER.SPEED))
      yview% = yview% - (sin.table!(viewing.angle%) * (3 * VIEWER.SPEED))
    CASE esc$
      done% = 1
  END SELECT
  '
  ' Check for wallz
  '
  IF map%(INT(xview% / 64), INT(yview% / 64)) <> 0 THEN
    '
    ' Player hit a wall, reset position
    '
    SOUND 100, .5
    xview% = oxview%: yview% = oyview%
  END IF
  '
LOOP UNTIL done%

SUB draw.maze (map%(), xview%, yview%, viewing.angle%, atn.table%(), sin.table!(), cos.table!())
  '
  LINE (VIEWPORT.LEFT, VIEWPORT.TOP)-(VIEWPORT.RIGHT, VIEWPORT.CENTER), 6, BF
  LINE (VIEWPORT.LEFT, VIEWPORT.CENTER)-(VIEWPORT.RIGHT, VIEWPORT.BOT), 8, BF
  '
  IF RENDER.TYPE THEN DEF SEG = &HA000
  '
  FOR column% = VIEWPORT.LEFT TO VIEWPORT.RIGHT - 1 STEP VIEWPORT.RES
    '
    column.angle% = atn.table%(column%)
    degrees% = viewing.angle% + column.angle%
    IF degrees% > 359 THEN
      degrees% = degrees% - 360
    ELSE
      IF degrees% < 0 THEN degrees% = 360 + degrees%
    END IF
    '
    x2% = INT(2048 * cos.table!(degrees%))
    y2% = INT(2048 * sin.table!(degrees%))
    '
    x2% = x2% + xview%
    y2% = y2% + yview%
    '
    x% = xview%
    y% = yview%
    '
    xdiff% = x2% - xview%
    ydiff% = y2% - yview%
    '
    IF xdiff% = 0 THEN xdiff% = 1
    '
    slope! = ydiff% / xdiff%
    '
    IF slope! = 0 THEN slope! = .0001
    '
    DO
      '
      IF xdiff% > 0 THEN
        grid.x% = (x% AND &HFFC0) + 64
      ELSE
        grid.x% = (x% AND &HFFC0) - 1
      END IF
      '
      IF ydiff% > 0 THEN
        grid.y% = (y% AND &HFFC0) + 64
      ELSE
        grid.y% = (y% AND &HFFC0) - 1
      END IF
      '
      xcross.x% = grid.x%
      xcross.y! = y% + slope! * (grid.x% - x%)
      '
      ycross.x! = x% + (grid.y% - y%) / slope!
      ycross.y% = grid.y%
      '
      xd% = xcross.x% - x%
      yd! = xcross.y! - y%
      xdist& = INT(SQR(xd% * xd% + yd! * yd!))
      '
      xd! = ycross.x! - x%
      yd% = ycross.y% - y%
      ydist& = INT(SQR(xd! * xd! + yd% * yd%))
      '
      IF xdist& < ydist& THEN
        '
        xmaze% = INT(xcross.x% / 64)
        ymaze% = INT(xcross.y! / 64)
        '
        x% = xcross.x%
        y% = INT(xcross.y!)
        '
      ELSE
        '
        xmaze% = INT(ycross.x! / 64)
        ymaze% = INT(ycross.y% / 64)
        '
        x% = INT(ycross.x!)
        y% = ycross.y%
        '
      END IF
      '
      IF map%(xmaze%, ymaze%) THEN EXIT DO
      '                   
    LOOP
    '
    colr% = map%(xmaze%, ymaze%)
    '
    xd& = x% - xview%
    yd& = y% - yview%
    distance& = INT(SQR(xd& * xd& + yd& * yd&) * cos.table!(column.angle%))
    IF distance& = 0 THEN distance& = 1
    '
    height% = INT(VIEWER.DISTANCE * WALL.HEIGHT / distance&)
    bot% = INT(VIEWER.DISTANCE * VIEWER.HEIGHT / distance&) + VIEWPORT.CENTER
    '
    top% = bot% - height%
    '
    IF top% < VIEWPORT.TOP THEN
      height% = height% - (VIEWPORT.TOP - top%)
      top% = VIEWPORT.TOP
    END IF
    '
    IF (top% + height%) > VIEWPORT.BOT THEN
      height% = height% - (bot% - VIEWPORT.BOT)
    END IF
    '
    IF NOT (RENDER.TYPE) THEN
      LINE (column%, top%)-(column% + (VIEWPORT.RES - 1), top% + height%), colr%, BF
    ELSE
      FOR ii% = 0 TO (VIEWPORT.RES - 1)
        offset& = top% * 320 + column% + ii%
        '
        FOR i% = 0 TO height% - 1
          '
          POKE offset&, colr%
          '
          offset& = offset& + 320
          '
        NEXT
      NEXT
    END IF
    '
  NEXT
  '
  IF RENDER.TYPE THEN DEF SEG
  '
END SUB

