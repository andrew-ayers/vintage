DECLARE SUB draw.maze (xview%, yview%, viewing.angle%)
'
CONST WALL.HEIGHT = 64
CONST VIEWER.HEIGHT = 32
CONST VIEWER.DISTANCE = 200
CONST VIEWPORT.LEFT = 0
CONST VIEWPORT.RIGHT = 319
CONST VIEWPORT.TOP = 20
CONST VIEWPORT.BOT = 179
CONST VIEWPORT.HEIGHT = 160
CONST VIEWPORT.CENTER = 99
CONST VIEWPORT.RES = 5
CONST VIEWER.SPEED = 5
CONST RENDER.TYPE = 0 ' 0=Line, -1=Direct Video Memory Access
'
DIM SHARED map%(15, 15)
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
DIM SHARED atn.table%(320), cos.table!(360), sin.table!(360)
'
FOR column% = VIEWPORT.LEFT TO VIEWPORT.RIGHT - 1 STEP VIEWPORT.RES
  '
  degrees% = INT(ATN((column% - 160) / VIEWER.DISTANCE) * dpr!)
  '
  IF degrees% > 360 THEN
    degrees% = degrees% - 360
  ELSE
    IF degrees% < 0 THEN degrees% = 360 + degrees%
  END IF
  '
  atn.table%(column%) = degrees%
  '
NEXT column%
'
FOR t% = 0 TO 360
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
  '
  CALL draw.maze(xview%, yview%, viewing.angle%)
  '
  oxview% = xview%: oyview% = yview%
  '
  LOCATE 1, 1: PRINT "QBasic Ray Caster 1.1 by Andrew L. Ayers"
  '
  DO: k$ = INKEY$: LOOP UNTIL k$ <> ""
  '
  SELECT CASE k$
    CASE rt$
      viewing.angle% = viewing.angle% + VIEWER.SPEED
      IF viewing.angle% > 359 THEN viewing.angle% = viewing.angle% - 360
    CASE lt$
      viewing.angle% = viewing.angle% - VIEWER.SPEED
      IF viewing.angle% < 0 THEN viewing.angle% = viewing.angle% + 360
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
  ' Check for wall
  '
  IF map%(INT(xview% / 64), INT(yview% / 64)) <> 0 THEN
    '
    ' Hit wall! Reset position
    '
    SOUND 100, .5
    xview% = oxview%: yview% = oyview%
  END IF
  '
LOOP UNTIL done%

SUB draw.maze (xview%, yview%, viewing.angle%)
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
    x2% = CINT(2048 * cos.table!(degrees%))
    y2% = CINT(2048 * sin.table!(degrees%))
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
    dangle% = degrees% ' Distance Calculation Angle
    '
    sin.dangle! = sin.table!(dangle%)
    cos.dangle! = cos.table!(dangle%)
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
      xcross.x& = grid.x%
      xcross.y& = CLNG(y% + slope! * (grid.x% - x%))
      '
      ycross.x& = CLNG(x% + (grid.y% - y%) / slope!)
      ycross.y& = grid.y%
      '
      xd& = xcross.x& - x%
      yd& = xcross.y& - y%
      IF sin.dangle! <> 0 THEN
        xdist! = xd& / sin.dangle!
      ELSE
        xdist! = yd& / cos.dangle!
      END IF
      '
      xd& = ycross.x& - x%
      yd& = ycross.y& - y%
      IF sin.dangle! <> 0 THEN
        ydist! = xd& / sin.dangle!
      ELSE
        ydist! = yd& / cos.dangle!
      END IF
      '
      IF ABS(xdist!) < ABS(ydist!) THEN
        '
        xmaze% = INT(xcross.x& / 64)
        ymaze% = INT(xcross.y& / 64)
        '
        x% = CINT(xcross.x&)
        y% = CINT(xcross.y&)
        '
      ELSE
        '
        xmaze% = INT(ycross.x& / 64)
        ymaze% = INT(ycross.y& / 64)
        '
        x% = CINT(ycross.x&)
        y% = CINT(ycross.y&)
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
    distance& = CLNG(SQR(xd& * xd& + yd& * yd&) * cos.table!(column.angle%))
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
      LINE (column%, VIEWPORT.TOP)-(column% + (VIEWPORT.RES - 1), top%), 6, BF
      LINE (column%, top%)-(column% + (VIEWPORT.RES - 1), top% + height%), colr%, BF
      LINE (column%, top% + height%)-(column% + (VIEWPORT.RES - 1), VIEWPORT.BOT), 8, BF
    ELSE
      FOR ii% = 0 TO (VIEWPORT.RES - 1)
        '
        offset1& = VIEWPORT.TOP * 320 + column% + ii%
        '
        FOR i% = 0 TO top% - 1
          '
          POKE offset1&, 6
          '
          offset1& = offset1& + 320
          '
        NEXT
        '
        offset2& = top% * 320 + column% + ii%
        '
        FOR i% = 0 TO height% - 1
          '
          POKE offset2&, colr%
          '
          offset2& = offset2& + 320
          '
        NEXT
        '
        offset3& = (top% + height%)
        offset3& = offset3& * 320 + column% + ii%
        '
        FOR i% = (top% + height%) TO VIEWPORT.BOT - 1
          '
          POKE offset3&, 8
          '
          offset3& = offset3& + 320
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

